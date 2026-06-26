# Idris 2 JVM v0.8.4

Release notes for changes since [v0.8.3](https://github.com/mmhelloworld/idris-jvm/releases/tag/0.8.3).

## Highlights

### JVM FFI binding generator

You no longer have to hand-write `%foreign` declarations to call Java. Name the
classes you want and the compiler reflects them off the classpath and generates
idiomatic, typed Idris binding modules **before** compilation — so the
descriptors are correct by construction and mismatches surface at compile time
instead of as a runtime `NoSuchMethodError`.

```sh
idris2 --cg jvm --jvm-ffi-import java/util/ArrayList,java/util/List Main.idr
```

```idris
import Java.Util

main : IO ()
main = do
  xs <- ArrayList.new
  _  <- ArrayList.add xs "hello"   -- String accepted where Object is expected
  n  <- List.size xs               -- ArrayList used through the List interface
  putStrLn "size = \{show n}"
```

What the generator produces and handles:

- **One module per Java package, one namespace per class.** `java/util/*` →
  `Java.Util` (`Java/Util.idr`), with `List.add`, `ArrayList.new`, etc. `.` is on
  the default search path, so `import Java.Util` just works.
- **Typed marker types from Java generics** — `Map : Type -> Type -> Type`, with
  shared type variables (`Map.of` has 2 type params, not 20 constraints) and
  `Inherits`-based subtyping instances so subclasses pass where a supertype is
  expected.
- **Method overloading** via namespace-based resolution — `add(E)` and
  `add(int, E)` are both `add`, disambiguated by Idris at the call site.
- **Java lambdas / functional interfaces** are bound as Idris functions.
- **Nullable returns are typed as `Maybe`**, so Java `null` is handled safely.
- **Module cycles** between mutually-referencing packages are broken
  automatically.
- **On-demand generation:** bindings are generated as needed rather than all up
  front.

New CLI flags:

- `--jvm-ffi-import <classes>` — generate FFI bindings for comma-separated
  internal class names (e.g. `java/util/ArrayList`) before compiling.
- `--jvm-ffi-list <classes>` — print the callable member catalog (signatures
  only) for the given classes, then exit.
- `--jvm-classpath <classpath>` — jars/dirs the FFI reflector resolves project
  (non-JDK) classes against; JDK platform classes are found via a `jrt:`
  fallback with no extra configuration.

Under the hood: a new ASM-based `ClasspathReflector` (Java) reads class
metadata, transitive supertypes, and generic signatures; `Compiler.Jvm.FfiGen`
is a pure renderer + JVM descriptor/generic-signature parser; and
`Compiler.Jvm.Reflection` bridges the two. See
[`docs/ffi-gen-design.md`](../docs/ffi-gen-design.md) for the full design.

### IDE support and the IntelliJ plugin

This release pairs with the new
[**Idris 2 (JVM)** plugin for IntelliJ IDEA](https://plugins.jetbrains.com/plugin/32261-idris-2-jvm),
which is backed by this compiler. It provides syntax highlighting, code
completion, interactive editing (case split, add clause, proof search, generate
definition, holes), go-to-definition, and documentation lookup.

It also surfaces the JVM FFI directly in the editor: completing on a Java class
offers its full set of callable members and generates the binding on demand. To
support this, a new `jvm-ffi-list` IDE-mode command lists FFI members
(signatures only, with a classpath argument) without writing any files.

### Static interface methods

Static methods declared on an interface (e.g. `List.of`, `Map.of`) are now
invoked with an `InterfaceMethodref` constant instead of a regular
`Methodref`, fixing an `IncompatibleClassChangeError` at runtime. The
`jvm:` foreign descriptor gained an `i:` owner prefix to flag interface owners,
which the FFI generator emits automatically for static interface methods.

## Optimizer / codegen

- **Family-typed return refinement.** `Con` body return types are now
  specialized, and specialized return types propagate across call chains, with
  dead code eliminated as a result — continuing the monomorphization work from
  prior releases.

## Documentation

- README now documents the IntelliJ plugin and IDE support, and notes that the
  compiler is also published to Maven Central.
- New `docs/README.md` (Sphinx build instructions) and
  `docs/ffi-gen-design.md` (FFI generator design).
- Install docs updated.
