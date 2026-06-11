# Recursive slot types for synthetic constructor shapes

Status: **future work — not started.**  Companion to
[monomorphization-design.md](monomorphization-design.md) §12 (Phase 2d /
v1.4).  This document expands the one-line future-work entry there into
the full problem statement, the candidate designs, and the
recommendation.

## 1. Goal

Phase 2d types the RECURSIVE slots of an ordinary DATACON's spec class
at the family-instantiation interface: `Node$I$L$L.property1 : Tree$F`
instead of `Object`, so the `$idrisTailRec` reassignment of a
family-typed loop variable is cast-free.  The same treatment for the
*synthetic* constructor shapes would give prelude lists the headline
example the original Phase 2d sketch used: `CONS$I$L.property1 :
List$I$L`, eliminating one `checkcast` per iteration in every
tail-recursive list loop.

## 2. Why synthetic shapes are blocked today

Three facts combine into the blocker:

1. **One class serves many source types.**  `tryIntrinsic`
   (`Compiler/Opts/Constructor.idr`) rewrites every CONS-shaped
   constructor to the single intrinsic Name `_builtin/CONS` before the
   JVM backend sees anything.  And `calcListy`
   (`TTImp/ProcessData.idr`) is deliberately loose about what
   "CONS-shaped" means — any constructor with two unerased arguments
   qualifies, *including non-recursive ones*; pairs are the documented
   case ("Note they don't have to be recursive!  It's just about
   whether we can pair cheaply").  So one spec class `CONS$I$L` holds
   int-list cells, `(Int, String)` pairs, `Vect` cells, and every
   user-defined two-slot type that matched the shape.

2. **A field descriptor is per-class, not per-instance.**  Typing
   `property1` as `List$I$L` is a claim about *every* instance of the
   class.  The first `(5, "five")` pair would hit the constructor's
   `checkcast List$I$L` on a `String` and throw.  A slot of a class
   whose instances disagree about what lives there cannot be refined.

3. **The disambiguating evidence is erased before the backend runs.**
   For ordinary DATACONs, Phase 2d consults the DCon's telescope in the
   Ctxt (`findRecursiveConSlots`) to prove a slot recursive.
   `_builtin/CONS` has no Ctxt entry — and even if it did, there is no
   single correct answer, because the intrinsic Name aggregates source
   types with *contradictory* answers (List's tail is recursive, Pair's
   second slot is not).  The original DCon Name, which would
   disambiguate, is gone by the time `NmCon` reaches the backend.

## 3. Option A: split the intrinsic classes per source type

Undo (or bypass) the name collapse so each source type keeps its own
constructor class: `Prelude.List.(::)` gets its own class with a Ctxt
entry, `MkPair` its own, and so on.  Lists then become ordinary
DATACONs from the backend's perspective — the existing v1.4 + Phase 2d
machinery (eligibility gate, instantiation-keyed `$F` families,
telescope walk) applies with **no new theory**, and `List`'s tail slot
is typed for free.

Costs — this is a real trade, not a cleanup:

- **It forfeits exactly what the intrinsic was built for.**  Every
  list-shaped type currently shares one natural class and one spec
  class per slot shape.  Splitting multiplies emitted class files and
  spreads JIT type profiles across many classes where today's call
  sites are monomorphic on `CONS`.
- **Runtime interop.**  Hand-written runtime classes
  (`IdrisList$Cons`, the string-unpacking paths, FFI helpers)
  construct and consume the shared representation.  Mixed
  representations do work through the `IdrisObject` interface, but
  every place that benefits from "a cons is a CONS" needs re-auditing.
- **It is not a backend-local change.**  The rewrite happens in a
  shared compiler optimisation pass, and the rewritten constructors may
  be baked into TTC-cached compiled defs.  If so, the boot compiler's
  library TTCs hand the new backend old-style names — requiring a TTC
  version bump and a staged bootstrap.

## 4. Option B: per-instance recursion proofs at construction sites

Keep the shared classes but pick a layout per *site*: where the tail
expression's inferred type already witnesses family membership (it is
statically `List$I$L`, or a freshly constructed `Nil`), construct a
"recursive variant" class with a typed tail; otherwise construct the
generic one.

This mostly collapses under its consumer side:

- Since a field type is per-class, "per-instance" really means **two
  spec classes per shape** (generic `CONS$I$L` plus a recursive
  variant).  A `List$I$L`-typed discriminant in a Cons alt can then be
  *either* class, so `findUniqueFamilyMember`'s uniqueness check fails
  and the alt falls back to boxed `getProperty` — trading one checkcast
  per loop iteration for losing typed accessors entirely.
- Recovering the win needs a global guarantee that every cell of a
  given list value used the recursive variant — a semantic-type
  property the untyped `NamedCExp` backend cannot see.  Local site
  evidence cannot supply it: one perfectly recursive list built through
  a polymorphic helper (whose tail is `Object`-typed at that site)
  poisons the whole value.

## 5. Recommended middle path: split by recursion evidence, not source type

A two-way split keeps nearly all the sharing: teach the shape
classification to distinguish recursive from non-recursive CONS *at the
point where the real types are still available*, and emit **two
intrinsic names** — `_builtin/CONS` (non-recursive or unknown: pairs)
and `_builtin/RCONS` (second argument provably the type itself).  All
genuinely list-like types still share one class (`RCONS`); all
pair-like types share the other.  The JVM backend can then safely
refine `RCONS$I$L.property1` to the synthetic `List$I$L` family,
because every instance of that class is recursive *by construction* —
the per-class/per-instance conflict of §2 dissolves.

This preserves the cheap-pair sharing (a 2-way split, not per-type),
gives prelude lists the typed tail, and reuses the existing
synthetic-family naming unchanged.

Wrinkles to resolve before believing in it:

- **TTC staging.**  Where does `tryIntrinsic` run relative to TTC
  caching of compiled defs?  If rewritten names are persisted, library
  TTCs produced by the boot compiler contain only `_builtin/CONS`, and
  the classification cannot be recovered downstream (the original Name
  is gone) — a TTC bump and a staged bootstrap are required, as with
  Option A but with a much smaller blast radius.
- **Tag assignment.**  `conInfoNameTag` pins CONS = tag 1; `RCONS` must
  agree so the existing tag-switch dispatch (`getConstructorId`) is
  unaffected and CONS/RCONS instances of one family can flow through
  the same pattern matches.
- **Family unity.**  `deriveTConFamily`'s synthetic path and
  `computeNaturalToTConIfaces`'s synthetic family keying must treat
  CONS and RCONS as the same "List" family, so `NIL` keeps implementing
  every active instantiation of both shapes.
- **Runtime-constructed lists.**  `IdrisList` cells built from Java
  (string unpacking, FFI) remain a third representation — fine via
  `IdrisObject` dispatch, but they keep `naturalConsLive`-style
  conservatism alive on those paths.

## 6. Why this is deferred (all three designs)

The prize is precisely **one guaranteed-to-succeed `checkcast` per
`$idrisTailRec` iteration** on list-tail loops.  Modern JITs make that
nearly free (monomorphic subtype-check caches; the check is often
hoisted out of the loop).  The risk surface, by contrast, is the
hottest shared representation in the runtime plus a bootstrap-staging
dance.

**Prerequisite:** the benchmarks item in monomorphization-design.md §13.
Profile a list-heavy workload (the self-hosted compiler is the natural
one) and confirm the tail checkcast is visible at all before paying for
any of the designs above.  If it is, pursue §5; Options A and B are
recorded here mainly to explain why they were rejected.
