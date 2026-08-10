# Changelog Next

This CHANGELOG describes the merged but unreleased changes.  Please see
[CHANGELOG](./CHANGELOG.md) for changes to all previously released versions of
Idris2, as well as the sub-headings typically used for changes.  All new PRs
should target this file (`CHANGELOG_NEXT`).

## [Next version]

### Compiler changes

* The compile-time evaluator now folds signed `div`/`mod` constants with
  explicit Euclidean semantics instead of delegating to the host compiler's
  operators. This fixes REPL evaluation and type-level `div`/`mod` on negative
  operands disagreeing with generated code when the compiler is bootstrapped
  with a pre-0.8.5 release (e.g. `the Int (-11) `mod` 10` evaluated to `-1` at
  the type level but `9` at runtime in the 0.8.5 release), and keeps the
  evaluator correct in the same release as any future semantics change.



