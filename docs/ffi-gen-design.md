# Compiler-driven JVM FFI binding generation

## 1. Background and Motivation

### Current state

Every JVM interop point in idris-jvm is a hand-written `%foreign` declaration: a
descriptor string plus a matching Idris type signature. For example, from
`src/Compiler/Jvm/Asm.idr`:

```idris
%foreign "jvm:.add(i:java/util/Collection java/lang/Object Bool),java/util/Collection"
prim_add : Collection a -> Object -> PrimIO Bool

%foreign "jvm:<init>(java/util/ArrayList),java/util/ArrayList"
prim_newArrayList : PrimIO ArrayList
```

Writing these by hand is tedious and fragile. The descriptor string encodes the
JVM method name, parameter types, return type, dispatch kind (static / virtual /
interface / constructor / field) and owner class — and any mismatch between the
descriptor and the real class on the classpath surfaces only at **runtime**, as a
`NoSuchMethodError` or a linkage failure, never at compile time.

### Goal

Generate these bindings automatically. The user names the JVM classes they want to
import, and the compiler produces a normal Idris module of typed `%foreign`
declarations plus idiomatic wrappers — derived from the actual bytecode on the
classpath, so the descriptors are correct by construction.

```sh
idris2 --cg jvm --jvm-ffi-import java/util/ArrayList,java/util/List Main.idr
```

```idris
-- Main.idr
import Java.Util

main : IO ()
main = do
  xs <- ArrayList.new
  _  <- ArrayList.add xs "hello"   -- String accepted where Object is expected
  n  <- List.size xs               -- ArrayList used through the List interface
  putStrLn "size = \{show n}"
```

## 2. Why not elaborator reflection?

The obvious tool is elaborator reflection (`%macro` / the `Elab` monad): inspect a
type and emit declarations at compile time. It is the **wrong** tool here, for one
decisive reason:

> **`Elab` cannot reach the JVM.** A reflection script can manipulate `TTImp`
> syntax and query the *Idris* typing context, but it has no Java reflection, no
> FFI, no way to read a `.class` file. It can only *synthesize* from metadata it is
> already handed — it cannot *discover* a class's API.

The information we need (method names, parameter/return types, static-vs-instance,
interface-vs-class) lives in JVM bytecode, and reading bytecode is I/O that `Elab`
does not have.

### The key observation

The idris2-jvm compiler **runs as a JVM process**. So while an `Elab` script cannot
touch the JVM, the **compiler driver can** — it already FFIs into a Java "assembler"
library (`io.github.mmhelloworld.idrisjvm.assembler.*`) that uses ASM. We put the
class introspection *there*, in the driver, and have the driver emit a normal Idris
source module before compilation proceeds. This is strictly more capable than the
reflection route and produces inspectable, committable source.

## 3. End-to-end flow

```
idris2 --cg jvm --jvm-ffi-import java/util/ArrayList,java/util/List  Main.idr
   │
   ├─ getCmdOpts            CommandLine.idr   → CLOpt: JvmFfiImport [..]
   ├─ stMain / updateEnv    Driver.idr:156    (search paths configured)
   ├─ preOptions  ◄──────── SetOptions.idr:390   ★ generation hook
   │     reflect classpath (ASM)
   │       → metadata ADTs
   │       → group by package
   │       → render one Idris module per package
   │       → write Java/Util.idr, Java/Lang.idr, …
   └─ loadMainFile          Driver.idr:202    (Main.idr imports Java.Util, now present)
```

`.` is on `extra_dirs` by default (`Core/Options.idr`), so `import Java.Util`
resolves with no extra flags. The hook runs in `preOptions` — after `updateEnv`
has configured paths but before `loadMainFile` triggers import resolution
(`Core/Directory.idr:209 nsToSource`) — which guarantees the generated module
exists by the time it is imported.

## 4. Module & namespace layout

Generated bindings are organized by Java package and class:

- **One Idris module per Java package.** Module name = the package with each
  segment capitalized: `java/util/*` → `Java.Util` (file `Java/Util.idr`);
  `java/lang/*` → `Java.Lang`.
- **One namespace per class** inside that module, named after the simple class
  name: `java.util.List` → `namespace List`. Members are `List.add`, `List.size`.
- **One marker type per class** at module top-level, named after the class
  (`data List : Type where [external]`). The top-level type `List` and the
  same-named `namespace List` coexist — this is exactly what an Idris `record`
  does (it yields a type `Foo` *and* a namespace `Foo` for its projections), so it
  is sound and the result reads like Java: `xs : List`, `List.add xs x`.
- **Cross-package reference types** are pulled in with `import`: `java.lang.Object`
  → `import Java.Lang`, used as `Object` (qualified `Java.Lang.Object` on clash).
- Because each class is its own namespace, **method-name overloading only needs
  disambiguation within a class** — `List.add` and `ArrayList.add` never collide.

### Generated module example

For `--jvm-ffi-import java/util/ArrayList,java/util/List`, the compiler writes
`Java/Util.idr`:

```idris
module Java.Util

import Java.Lang   -- Object marker type

%cg jvm

public export data List      : Type where [external]
public export data ArrayList : Type where [external]

namespace List
  %foreign "jvm:.add(i:java/util/List java/lang/Object boolean),java/util/List"
  prim__add : List -> Object -> PrimIO Bool
  export %inline
  add : HasIO io => List -> Object -> io Bool
  add xs x = primIO (prim__add xs x)

  %foreign "jvm:.size(i:java/util/List int),java/util/List"
  prim__size : List -> PrimIO Int
  export %inline
  size : HasIO io => List -> io Int
  size xs = primIO (prim__size xs)

namespace ArrayList
  %foreign "jvm:<init>(java/util/ArrayList),java/util/ArrayList"
  prim__new : PrimIO ArrayList
  export %inline
  new : HasIO io => io ArrayList
  new = primIO prim__new

  %foreign "jvm:.add(java/util/ArrayList java/lang/Object boolean),java/util/ArrayList"
  prim__add : ArrayList -> Object -> PrimIO Bool
  export %inline
  add : HasIO io => ArrayList -> Object -> io Bool
  add xs x = primIO (prim__add xs x)
```

Note the renderer used `i:java/util/List` (interface receiver →
`invokeinterface`) for `List`, but plain `java/util/ArrayList` (class receiver) for
`ArrayList`, driven by each owner's `ACC_INTERFACE` flag.

### Cyclic packages: umbrella module + re-export stubs

The "one module per package" layout above assumes the cross-package import graph is a
DAG. It is not in general: the JVM package graph is genuinely cyclic — e.g.
`java.lang.Iterable.iterator()` returns `java.util.Iterator`, while `java.util` types
name `java.lang.Object`, so `Java.Lang` and `Java.Util` would import each other. Idris
forbids cyclic module imports (there is no `.hs-boot`/forward-declaration escape hatch),
so the cyclic packages must share one compilation unit.

The renderer condenses the import graph into **strongly-connected components** and emits
one module per SCC:

- A **singleton** SCC (no cycle) renders exactly as the per-package module above — the
  common case (e.g. `--jvm-ffi-import java/util/ArrayList,java/util/List`) is unchanged.
- A **multi-package** SCC renders as a single **umbrella module**, named after the
  members' common path prefix (e.g. `Java`), with each package's declarations in a nested
  `namespace` (`namespace Lang`, `namespace Util`, `namespace Util.Function`). Nesting
  keeps fully-qualified names identical (`Java.Util.List` etc.), and the module imports
  only packages *outside* its SCC — an SCC is closed under mutual reachability, so the
  condensed graph (and the stub re-exports below) stay acyclic.
- For each member package a thin **re-export stub** is emitted so user `import Java.Util`
  keeps resolving: `module Java.Util` / `import public Java`.

Within the umbrella, declarations are emitted in **kind-grouped passes** (all opaque
`data` markers, then the functional-interface synonyms + `Inherits`/lambda infra, then the
`Inherits` instances, then the methods), each pass reopening every namespace. Because the
package graph is cyclic, no per-namespace ordering avoids forward references; grouping by
declaration kind makes every cross-namespace reference *backward*. (`mutual` is
insufficient on its own — it does not resolve forward references in interface-
implementation headers.)

Two related details, independent of cycles but exercised by the same imports:

- **Builtin-modeled types** (`java.lang.String` → Idris `String`) get no marker, namespace
  or `Inherits` instances — `String` is reserved and the builtin stands in for it (the
  `Inherits String Object` infra connects it). They still drive the import graph.
- A functional-interface synonym whose SAM returns a *parameterised* type (e.g.
  `Iterable.iterator() : Iterator<T>`) erases the element to `Object`
  (`Iterator Java.Lang.Object`) rather than leaking an unbound type variable into the
  synonym's right-hand side.

## 5. Components

### 5.1 `ClasspathReflector.java` — the introspection half

Location: `idris-jvm-assembler/src/main/java/io/github/mmhelloworld/idrisjvm/assembler/`
(ASM is already a dependency, `idris-jvm-assembler/pom.xml`).

Given a binary class name it:

1. Resolves the class bytes — context classloader resource first, with a `jrt:/`
   filesystem fallback for JDK platform classes (see §7).
2. Runs a `ClassReader` + `ClassVisitor` to collect: owner internal name,
   `ACC_INTERFACE`, and per public member — name, raw JVM descriptor `(...)ret`,
   `ACC_STATIC`, constructor/method/field kind, declared exceptions.
3. Resolves, for each distinct reference type that appears, whether it is an
   interface (needed for the `i:` prefix, which the method descriptor alone cannot
   tell us).
4. Emits a **flat line-protocol string** (one member per line, `|`-delimited, plus
   an interface-type set). The Java side deliberately returns *raw* JVM info and
   does **not** translate to idris `jvm:` tokens — translation stays in Idris so it
   is unit-testable against the hand-written strings in `Asm.idr`.

### 5.2 `src/Compiler/Jvm/Reflection.idr` — the FFI bridge

```idris
%foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/ClasspathReflector" "reflect" "String" "String"
prim_reflect : String -> PrimIO String

reflectClass : HasIO io => String -> io ClassInfo   -- parse line-protocol → ADTs
```

### 5.3 `src/Compiler/Jvm/FfiGen.idr` — metadata ADTs + pure renderer

- ADTs: `JType` (`JPrim` / `JClass` / `JIface` / `JArray`), `MemberKind`,
  `MemberInfo`, `ClassInfo`.
- `descriptor : ClassInfo -> MemberInfo -> String` — assembles the `jvm:` string
  per the grammar in §6. **The load-bearing correctness piece**, golden-tested
  byte-for-byte against `Asm.idr`.
- `idrisSig : MemberInfo -> TTImp/String` — Idris type: `int→Int`, `boolean→Bool`,
  `double→Double`, `void→()`; reference types → the opaque per-class marker type;
  prims return `PrimIO ret`.
- `renderPackage : (pkg : String) -> List ClassInfo -> String` — emits one module:
  header, cross-package imports, marker `data` decls, then a `namespace <Class>`
  block per class with each member's low-level `%foreign` prim **plus** an idiomatic
  wrapper.
- Wrapper layer: `void→IO ()`; intra-class overload disambiguation; **subtyping
  via `Inherits`** (see below); and **null-safety** — a reference return that may be
  null is surfaced as `io (Maybe a)` (see §5.8). Further marshalling
  (`java.util` collections ⇄ `List`, `throws` → `IO (Either JThrowable a)`) is future work.

### 5.6 Parametric types from Java generics

The reflector emits each class's generic type-parameter names (the trailing field of
the `C|` line, e.g. `K,V`) and each method's raw generic signature (the trailing field
of the `M|` line, e.g. `<K:…;V:…>(TK;TV;)Ljava/util/Map<TK;TV;>;`). The generator
parses these (JVMS §4.7.9.1) so that marker types and signatures carry their type
parameters instead of being erased to `Object`:

- **Parameterised markers**: `data List : Type -> Type`, `data Map : Type -> Type -> Type`.
- **Shared type variables**: a Java type variable maps to one Idris type variable, so
  `Map.of(K,V,K,V,…)` — 20 *arguments* but only **two type parameters** — renders as
  `of : k -> v -> k -> v -> … -> io (Map k v)`, with *no* per-argument constraints.
  (This is what makes the elaborator's implicit binder tractable; no arity cap needed.)
- **Typed returns**: `ArrayList.get : ArrayList e -> Int -> io e` and `Map.get :
  Map k v -> a -> io v` — you get the element / value type back, not `Object`.
- Type-variable *names* clash with `Prelude` (`List`, `Map`, `Maybe`, …), so every type
  *reference* is fully qualified (`Java.Util.List e`); only the marker declaration and
  the namespace keep the bare name.
- Marker arities come from the `C|` line for imported classes and are otherwise inferred
  from generic-signature usage; erased usages of a parameterised type are padded with
  fresh type variables.

### 5.7 Subtyping (`Inherits`)

The generator reuses the compiler's own `Inherits`/`subtyping` idiom (as in `Asm.idr`):

- **Infra** in `Java.Lang`: `interface Inherits child parent` with `subtyping =
  believe_me`, the reflexive `Inherits a a`, and `Inherits String Object`. A universal
  `Inherits a Object` is deliberately *avoided* — it overlaps the reflexive instance on
  `Inherits Object Object` and makes resolution ambiguous; instead each class gets an
  explicit `Inherits (Child cv..) Object`.
- **Parameterised instances** from the reflected transitive supertypes:
  `Inherits (ArrayList e) (List e)`, `Inherits (HashMap k v) (Map k v)`, …
- **Receivers are direct** (`ArrayList e`, not an `Inherits`-bounded variable). This
  threads the type parameter cleanly from construction through use; a generic receiver
  would leave a fresh `ArrayList ?` unpinned and break inference. Subtyping is applied to
  reference-typed **parameters** (pass an `ArrayList` where a `Collection` is expected),
  and a value can be upcast explicitly with `subtyping`. Each subtyped argument is
  ascribed to the prim's concrete type (`the (Collection e) (subtyping x)`) to discharge
  the constraint and pin the prim's type variables.

Note: a bare `String`/numeric literal into a freshly-constructed generic container needs
a type hint (annotate the constructor — `the (IO (ArrayList String)) ArrayList.new` — or
the first element). This is the usual literal-defaulting interaction with parameterised
FFI types, not specific to the generator.

Cross-package imports are emitted as **`import public`**: a generated module's API surfaces
other packages' types (`Java.Lang.Object`) and the `Inherits` interface/instances, so importers
— user code and sibling generated modules — must see them transitively to discharge those
constraints. (A plain `import` left `Inherits` invisible at the call site: `Undefined name
Java.Lang.Inherits`.)

### 5.8 Null-safety (`io (Maybe a)`)

A Java method may return `null`; if its binding is typed as a bare reference, the null leaks
into Idris as a non-`Maybe` value and crashes far from the cause. Reference returns judged
nullable are surfaced as `io (Maybe a)`:

- **Detection** is *annotation + curated list*. The reflector (`ClasspathReflector`) reads any
  return annotation whose simple name is `Nullable`/`CheckForNull` — method-level
  (`visitAnnotation`, JSR-305 / JetBrains) and TYPE_USE on the return (`visitTypeAnnotation` with
  `METHOD_RETURN`, jspecify) — and emits a trailing nullable field on the `M|` line. Because
  vanilla JDK bytecode carries no such annotations, the generator additionally consults a small
  curated list (`jdkNullableMethods`: `Map.{get,put,remove,putIfAbsent,replace}`,
  `Queue.{poll,peek}`, `Deque.*`, `NavigableSet.*`), matched against the class **and its
  reflected supertypes** so concrete subclasses (`HashMap`, `ArrayDeque`, …) inherit the policy.
- **Marshalling** reuses the existing idiom from `Asm.idr`: the prim keeps the raw return
  (`PrimIO a`, possibly null) and the wrapper maps `nullableToMaybe` over it
  (`nullableToMaybe <$> primIO …`), where `nullableToMaybe value = if isNull (believe_me value)
  then Nothing else Just value`. `isNull`/`nullableToMaybe` are emitted once into `Java.Lang`
  alongside the `Inherits`/`jlambda` infra. Only reference returns are wrapped — primitives/void
  never are.

So `HashMap.get : HashMap k v -> a -> io (Maybe v)`: a present key yields `Just v`, a missing
key `Nothing`.

### 5.9 Demand-driven generation (source reference-scan)

Emitting each imported class's *entire* public API made compiling against a few JDK classes slow
(whole-program elaboration of dozens of unused members). The generator now emits **only the
members user code references**:

- Before rendering, the driver (`generateJvmFfiBindings`) scans the project sources — the entry
  files named on the command line plus every `.idr` under their directories — for `Class.member`
  tokens (`scanReferences`, lexical and deliberately over-approximating; qualified uses like
  `Java.Util.ArrayList.add` are matched too). Generated modules are excluded from the scan (their
  `@generated` header) so a stale prior generation does not re-pin every member.
- Members are uniquified over the **full** member list first (so generated names like `add_2`
  stay stable regardless of what is pruned) and only then filtered. A namespace with nothing kept
  is dropped. **Markers, `Inherits` instances, and the `Java.Lang` infra are always emitted** —
  only member bindings are pruned.
- For `tests/jvm/ffigen` (`ArrayList` using only `new`/`add`/`size`/`sort`/`get`) this cut the
  generated `Java.Util` from ~290 lines to ~100.

Tradeoff: the generated source now depends on usage, so it is less stable as a committed
artifact, and a reference the scan cannot see (member used fully unqualified, or reached only
through re-export) would be pruned and break the build. The scan is intentionally generous, and
over-approximation (emitting a spare member) is the safe direction. `renderAll`'s `refs`
parameter is `Maybe`: `Nothing` restores the unfiltered whole-API form.

### 5.4 CLI flag — `src/Idris/CommandLine.idr`

Add `JvmFfiImport (List String)` to `CLOpt` and a `MkOpt ["--jvm-ffi-import"] …`
entry (modeled on the existing `--source-dir` option). Comma-separated class list.

### 5.5 Generation hook — `src/Idris/SetOptions.idr`

```idris
preOptions (JvmFfiImport classes :: opts) = do
  infos <- traverse reflectClass classes              -- ClassInfo carries its package
  d <- getDirs
  let root = fromMaybe "." (source_dir d)
  for_ (groupByPackage infos) $ \(pkg, cs) => do      -- one module per package
    let modPath = root </> pkgToPath pkg ++ ".idr"     -- "java/util" → "Java/Util.idr"
    coreLift $ do mkdirAll (dirOf modPath); writeFile modPath (renderPackage pkg cs)
  preOptions opts
```

## 6. The `jvm:` descriptor grammar (reference)

From `src/Compiler/Jvm/Foreign.idr` and `InferredType.idr`:

```
jvm:<spec>(<type> <type> … <ret>),<owner>
```

- **spec prefix**: *(none)* = static · `.` = instance · `<init>` = constructor ·
  `#name` = field get · `#=name` = field set.
- **Instance members list the receiver as the FIRST type** inside the parens; the
  return type is always **last**.
- **Constructor**: parens = params ++ [retClass]; owner = the constructed class.
- **Type tokens**: primitives spelled out (`int boolean void char long double …`);
  reference types as slash-FQN (`java/lang/String`), nested with `$`
  (`java/util/AbstractMap$SimpleImmutableEntry`); arrays prefix `[`; interface
  receiver/argument prefix `i:`.

## 7. Risks & limitations

- **JDK-module resolution (primary risk).** On JDK 9+, `java.*` classes live in
  modules, not on the classpath, so `getResourceAsStream("java/util/ArrayList.class")`
  returns null. The reflector falls back to the `jrt:/` filesystem
  (`FileSystems.getFileSystem(URI.create("jrt:/"))`, reading
  `/modules/<module>/<path>.class`) for platform classes, and uses the classloader
  resource for user jars.
- **Interface params.** The receiver's interface-ness comes from the owner's
  `ACC_INTERFACE`. Whether each *argument* type is an interface requires resolving
  that type; the reflector emits an interface-type set so the renderer can mark
  `i:` correctly. The initial spike may mark only the receiver.
- **Generics** are parsed from generic signatures into parameterised types (§5.6).
  Remaining gaps: wildcards/variance are erased to their bound; non-identity generic
  supertypes (`class Foo<X> implements Bar<List<X>>`) fall back to fresh type vars rather
  than the precise argument; inner-class type signatures (`Map.Entry<K,V>`) keep the
  outer type only; and the high-arity `Inherits`-binder cost is gone (no cap needed).
- **User jars.** JDK classes always resolve; user classes must be on the compiler
  process's JVM classpath (via the launcher / `JAVA_OPTS` / `CLASSPATH`).
- **Static methods on interfaces** (`List.of`, `List.copyOf`, `Map.of`, …) — *fixed*.
  The JVM requires these to be invoked via an `InterfaceMethodref`. The generator now
  prefixes the static owner with `i:` (`jvm:of(…),i:java/util/List`) when the class is an
  interface, and `Codegen.JvmStaticMethodCall` detects that prefix, strips it, and passes
  `isInterface=true` to `invokeMethod` (it was hardcoded `False`). The two-line backend
  change in `src/Compiler/Jvm/Codegen.idr` is part of this feature.
- **High-arity subtyping cap.** Wrappers genericize each reference arg into an `Inherits`
  constraint; beyond `maxGenericArgs` (8) the member falls back to concrete types so the
  elaborator's implicit binder does not overflow (e.g. `Map.of`'s 20-parameter overload).
- **Cost.** *Largely addressed* (§5.9). The generator now emits only the members user code
  references (source reference-scan), rather than each class's entire public API, which is the
  dominant cost under idris-jvm's whole-program compilation. Transitive supertype *markers* are
  still all emitted (they are one line each and needed for `Inherits`). Remaining gap: the scan
  is lexical and over-approximating, and does not see fully-unqualified or re-exported references
  — such a reference would be pruned and break the build, so qualified member access is assumed.

## 8. Verification

1. **Golden descriptors (pure, fast):** unit-check `descriptor` output against the
   hand-written strings in `Asm.idr` (ArrayList / Collection / Map), byte-for-byte.
2. **Reflector incl. JDK:** `ClasspathReflector.reflect("java/util/ArrayList")`
   returns members (proves the `jrt:/` platform fallback).
3. **Generation wired:** the `--jvm-ffi-import` run writes `Java/Util.idr` (module
   `Java.Util`, namespaces `ArrayList`/`List`) before `Main` compiles, and
   `import Java.Util` resolves.
4. **Runtime:** `Main` builds an `ArrayList`, `add`s via generated wrappers, reads
   `size`, sorts via a `jlambda` `Comparator`, reads a typed element; then exercises
   **null-safety** — `HashMap.get` returns `io (Maybe v)`, a hit prints `Just`'s value and a
   miss prints on `Nothing`. The `run` script also asserts **demand-driven pruning** (referenced
   `size` kept, unreferenced `clear` omitted). Diff against `tests/jvm/ffigen/expected`.
5. Build with `IDRIS2_BOOT=~/bin/idris2-0.8.3/exec/idris2`. Reflector (Java) changes need the
   assembler jar rebuilt and copied into `build/exec/idris2_app/` (`make` does not refresh it).
