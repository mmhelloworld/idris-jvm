# Changelog Next

This CHANGELOG describes the merged but unreleased changes.  Please see
[CHANGELOG](./CHANGELOG.md) for changes to all previously released versions of
Idris2, as well as the sub-headings typically used for changes.  All new PRs
should target this file (`CHANGELOG_NEXT`).

## [Next version]

### Compiler changes

* Common-subexpression elimination no longer merges structurally identical
  effectful closed terms (terms that create or thread a `%World`, e.g.
  `unsafePerformIO (newIORef Nothing)`). Previously two such top-level
  constants collapsed into one shared, once-evaluated constant, silently
  aliasing what should be distinct mutable references.
* Positivity-check errors now report the location of the `data` declaration
  being checked instead of no location at all. When the offending type
  constructor has not been loaded (it is referred to through a type alias in
  an imported module whose own imports are not re-exported), the error
  explains that its defining module must be imported, instead of the
  misleading "not a data type".
* Compiled expressions retain far more source locations, so code generators
  can emit usable debug line information (previously a breakpoint on a clause
  like `area (Circle r) = pi * r * r` had no line to bind to):
  - `Compiler.CompileExpr` no longer discards an application's location when
    rebuilding the spine for arity: `expandToArity` used the head's location,
    which is empty for resolved interface-method references — so every
    operator application (`x * y`, `a + b`, `m >>= f`) compiled without one.
    The application term's own location is restored on the rebuilt root.
  - Case-tree clauses attribute their right-hand side to the clause: when an
    RHS root has no location, `Core.Case.CaseBuilder` stamps the matched
    pattern's location onto it.
  - `Compiler.Inline` attributes an inlined body's root to its call site
    (callee locations are meaningless in the caller's line table, and
    primitive wrappers have none at all).
  - Dev tooling: `IDRIS_INLINE_DEBUG=<name-fragment>` dumps the location tree
    of matching definitions after each frontend pass; on the JVM backend
    `IDRIS_JVM_DEBUG` additionally dumps location trees during scope
    inference.
* Pattern-bound variables keep their usernames through case-tree
  compilation: `Core.Env.close` now derives each machine name's root from
  the clause binder it closes over (`{radius:0}` instead of `{pat0::0}`),
  and the case-tree builder's `nextNames` propagates those roots into the
  tree binders. The JVM backend strips the uniqueness counter when emitting
  the `LocalVariableTable` where unambiguous, so debuggers show
  `radius`/`width`/`height` for `area (Circle radius)` instead of
  `e$0`/`e$1`. Note: the textual rendering of case trees and coverage
  errors now shows the user-derived names, which may require regenerating
  golden test expectations.
* The JVM backend emits a line number at each function-application site (in
  addition to scope starts), so step-over inside a clause body or a
  do-block advances statement by statement like javac-compiled code, and
  every `let`/statement line is a valid breakpoint target.
* Fixed the `LocalVariableTable` recording logical variable indices instead
  of physical JVM slots: any variable declared after a `double` or `long`
  in the same method was reported one slot low (inside the wide variable),
  so debuggers showed garbage for it (e.g. `0.0` for a `let`-bound double
  following a double argument). The table now uses the same
  logical-to-physical slot translation as the generated loads and stores.

### JVM backend changes

* `div` and `mod` on signed integer types now follow Euclidean semantics,
  matching the Chez and other reference backends: the remainder is always
  non-negative (`-11 `mod` 10` is `9`, not `-1`; `-11 `div` 10` is `-2`,
  not `-1`). Previously the backend emitted the JVM's truncated
  division/remainder instructions (`idiv`/`irem`/`ldiv`/`lrem` and
  `BigInteger.divide`/`remainder`), whose results take the dividend's sign.
  Signed division and modulo now call `IdrisMath.euclidDiv`/`euclidMod`;
  unsigned types are unaffected.

* Fixed `Bool` conversions at the Java boundary when the Java side uses boxed
  `java/lang/Boolean`: exported functions returning `Boolean` failed class
  verification (`VerifyError: Bad return type` — the Idris `Bool`, an `int` at
  the JVM level, was boxed with `Integer.valueOf`), and foreign calls passing a
  `Bool` to a `Boolean` parameter threw `ClassCastException` at runtime.
  `Bool` in a foreign signature now maps to the JVM `boolean` type instead of
  `Object`, and an `int` converted to a `Boolean` target boxes via
  `Boolean.valueOf`.

* Fixed loading an `int` variable into a `boolean` slot: `loadVar` had no
  `int -> boolean` case, so an `int`-typed variable feeding a primitive
  `boolean` parameter (e.g. a `Bool` foreign argument) fell through to the
  boxing catch-all and was boxed with `Integer.valueOf`, failing verification
  (`VerifyError: Bad type on operand stack`) against the `Z` descriptor. Since `Bool` foreign parameters now map to `boolean`, this
  surfaced when the compiler compiled itself — an interface-method wrapper
  passing its `Bool` parameter to a `boolean` foreign method — breaking the
  self-hosting build. `int` and `boolean` share the JVM int stack
  representation, so the load is now a plain `iload`.

* Fixed Java lambda (`jlambda`) type derivation when common-subexpression
  elimination lifts the shared functional-interface tuple type or the lambda's
  function type into a `csegen` definition: the definition lookup now uses the
  program-qualified name (previously a `NullPointerException`, or a SAM
  descriptor degraded to all-`Object` causing `AbstractMethodError` at runtime
  for interfaces with non-`Object` erased signatures).

* Fixed `%export` of functions returning `PrimIO`: the generated Java wrapper
  now calls the compiled Idris function with its actual inferred JVM
  signature. Previously the wrapper fabricated an all-`Object` descriptor and
  boxed the synthesized world argument, while the compiled method takes the
  world as a primitive `int`, causing `NoSuchMethodError` at runtime.
  Primitive return types of exported functions are handled as well.
* Fixed a runtime `NullPointerException` in foreign static and instance field
  setters (`jvm:#=...`) used at `PrimIO ()` types: the generated code pushed a
  null and converted it to the unit `int` via `Conversion.toInt`. Setters now
  produce their result the same way void method calls do.

* Debug info fixes, making compiled programs debuggable with standard JVM
  debuggers (jdb, IntelliJ):
  - The `LocalVariableTable` now declares each `let`-bound variable,
    case-alternative binding and switch temporary live from just after its
    store instruction rather than from its scope's start. Debuggers read
    locals through JVMTI, which rejects reading a slot before it is written
    (`INVALID_SLOT`), so a single premature entry made the whole variables
    view fail at function entry.
  - The `LineNumberTable` no longer carries more than one entry per bytecode
    offset. Nested scopes starting at the same offset (function entry plus its
    first `let`/case scope) each emitted their line; debuggers keep only one
    entry per offset, so the shadowed line silently stopped being a valid
    breakpoint target ("no executable code at line N").
  - The `SourceFile` attribute is now derived from the class's own module
    name. Previously it was whichever definition first created the class
    writer, so a class first touched by a generated or cross-module
    definition was stamped with a different module's file (e.g. a user
    module's class claiming `Compiled from "Main.idr"`), sending debuggers to
    the wrong source file.
  - Compiler-generated code with no source location (`EmptyFC`) no longer
    emits line numbers pretending to be line 1 of `Main.idr`; such code now
    has no line entry at all.
  - Cross-module inlined code no longer emits its original file's line
    numbers into the inlining method. A method's line table is interpreted
    against its class's single SourceFile, so a Prelude line number inside a
    user function sent debuggers to arbitrary positions in the user's file
    (e.g. "line 235" of a 23-line `Main.idr`) and made stepping jump wildly.
    Scope lines are now kept only when they belong to the function's own
    source file; inlined code has no line entry (a JSR-45 SMAP could map such
    lines to their real files in the future).

