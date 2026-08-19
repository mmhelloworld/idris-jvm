# Changelog Next

This CHANGELOG describes the merged but unreleased changes.  Please see
[CHANGELOG](./CHANGELOG.md) for changes to all previously released versions of
Idris2, as well as the sub-headings typically used for changes.  All new PRs
should target this file (`CHANGELOG_NEXT`).

## [Next version]

### Compiler changes

* The JVM backend now emits primitive constants for integer literals instead
  of runtime `BigInteger` values. Previously every `Integer` literal reaching
  an `int` or `long` context was compiled as
  `new BigInteger("<literal>").intValue()`, parsing a string on each
  evaluation — a measured 13x slowdown in literal-heavy hot paths such as
  `fib`'s `n < 2` test. Literals whose target type is primitive are now
  folded to `iconst`/`ldc` constants (including literals under
  `cast Integer -> Int*/Bits*/String` operations, applying the same
  truncation as the runtime conversions), and remaining `BigInteger` literal
  loads use `BigInteger.valueOf(long)` when the value fits in a long,
  reserving string parsing for literals beyond 64 bits. `fib 38` improves
  from 1.87s to 0.20s, matching equivalent Java (0.13s) up to JVM startup.

* The JVM backend now emits nullary data constructors as shared singletons
  (a `public static final INSTANCE` per generated class) instead of
  allocating a fresh object at every use, and bakes the constructor id into
  `getConstructorId()` as a per-class constant instead of storing it as a
  field in every instance, shrinking constructor objects and their `<init>`
  signatures. Tail-recursion continuation constructors (`TcContinue_<arity>`,
  which reuse one name per arity with the tag selecting the continuation)
  are renamed to per-tag classes to keep constructor ids per-class constants,
  and the backend now fails at compile time if a constructor class is ever
  reused with a different id. Allocation on the `matMul` JMH benchmark drops
  by 19% B/op.

* The JVM backend now compiles one- and two-case switches (booleans,
  if/else, and two-constructor types such as `List` and `Maybe`) to direct
  compare-and-branch instructions instead of `lookupswitch`, and fuses
  `case` over a primitive comparison into a single conditional jump instead
  of materializing 0/1 and re-dispatching on it. Generated methods shrink
  substantially — `Prelude`'s `Int` less-than drops from 32 to 8 bytecode
  bytes — keeping hot combinators inside the JIT's bytecode-size-driven
  inlining budgets: +16% throughput on the sievePrimes JMH benchmark and
  +12% on matMul with no allocation change.

* The JVM backend's higher-order specialisation now reaches the standard
  list combinators. Call-site argument types are normalized before being
  logged for the specialisation plan, so one argument the plan cannot
  narrow (e.g. a `Nil` accumulator) no longer blocks a typed-callback
  specialisation for the others — `mapAppend`/`filterAppend` now get
  `$sp` variants whose callbacks are invoked through typed `Fn$` interfaces
  instead of the boxed `Function.apply` bridge, which in turn lets the
  constructor-specialisation plan store list elements in primitive-typed
  `CONS$…` cells. Functions whose bodies dispatch on type constructors are
  now explicitly excluded from specialisation (previously an accident of
  the acceptance rules).

* The JVM backend now represents Idris `Integer` (and therefore `Nat`)
  values as boxed `long`s while they fit in 64 bits, promoting to
  `BigInteger` only on overflow — the standard small-integer strategy.
  Arithmetic, comparisons, casts, and hash-based `case` dispatch go
  through a new runtime `IdrisInteger` class whose fast paths use
  primitive `long` operations with overflow checks; FFI boundaries
  declared as `BigInteger` convert in both directions automatically.
  Idiomatic `Nat`-heavy code speeds up dramatically — a `Nat`
  microbenchmark (tail-recursive counting, naive `Nat` fib, a
  big-Integer multiply chain) runs 3.2x faster end to end including JVM
  startup — and the self-hosted compiler builds the base library about
  10% faster than the 0.8.5 release.

* The JVM backend's laziness machinery is dramatically cheaper.
  `MemoizedDelayed` uses double-checked locking — the post-initialization
  read is a volatile flag check plus a field read instead of re-entering
  a synchronized closure through a mutable field — and `Inf` (codata)
  delays are now plain closures with no memoization wrapper, matching
  the Chez reference backend's default laziness (`Lazy` values and
  top-level constants keep memoization). Thunk reads disappear from
  stream-heavy profiles entirely: +10% throughput and -7% allocation on
  the matMul JMH benchmark.

* The compile-time evaluator now folds signed `div`/`mod` constants with
  explicit Euclidean semantics instead of delegating to the host compiler's
  operators. This fixes REPL evaluation and type-level `div`/`mod` on negative
  operands disagreeing with generated code when the compiler is bootstrapped
  with a pre-0.8.5 release (e.g. `the Int (-11) `mod` 10` evaluated to `-1` at
  the type level but `9` at runtime in the 0.8.5 release), and keeps the
  evaluator correct in the same release as any future semantics change.




* Generated launchers are tuned for the JVM's actual behavior on
  Idris workloads. Executables now run with `-XX:+UseParallelGC`
  (measured 4-10% faster than the default G1 on compile workloads —
  a single compute thread with a high allocation rate wants a
  throughput collector; override at runtime via
  `JAVA_OPTS="-XX:-UseParallelGC ..."`), and enable class data
  sharing: the generated classes are packed into a
  `<program>-classes.jar` so the classpath is jar-only, and the
  launcher passes `-XX:+AutoCreateSharedArchive` so the first run
  trains a CDS archive next to the app jars and later runs map it.
  Short-lived invocations start ~20-25% faster (a small `--check`
  drops from ~1.5s to ~1.2s); the archive regenerates itself
  automatically after a JDK or classpath change, and CDS is skipped
  when `IDRIS2_JVM_CLASSPATH` is set since user-supplied class
  directories cannot be archived.

* Mutually tail-recursive functions run on a reusable mutable trampoline
  frame instead of allocating a continuation object per iteration. The
  `Compiler.TailRec` transform now emits calls to static runtime frame
  helpers (`Runtime.TcFrame`: an int function-index plus an argument
  array, reused across all iterations of a trampoline run) and dispatches
  on an integer switch, replacing the per-iteration `TcContinue_*`
  constructor allocations and their megamorphic `getConstructorId`/
  `getProperty` dispatch with monomorphic field and array accesses.
  Trampoline continuations were ~28% of all allocation when the compiler
  typechecks its own sources (the evaluator's `eval`/`evalLocal`/
  `evalTree` group runs at arity 10); the self-hosted compiler
  typechecks the compiler sources ~9% faster, narrowing the gap to the
  Chez backend on the same workload from ~2.2x to ~2.05x.
