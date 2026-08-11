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

* The compile-time evaluator now folds signed `div`/`mod` constants with
  explicit Euclidean semantics instead of delegating to the host compiler's
  operators. This fixes REPL evaluation and type-level `div`/`mod` on negative
  operands disagreeing with generated code when the compiler is bootstrapped
  with a pre-0.8.5 release (e.g. `the Int (-11) `mod` 10` evaluated to `-1` at
  the type level but `9` at runtime in the 0.8.5 release), and keeps the
  evaluator correct in the same release as any future semantics change.



