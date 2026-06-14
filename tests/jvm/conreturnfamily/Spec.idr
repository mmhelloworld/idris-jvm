module Main

-- Family-typed return propagation (design doc §14).
--
-- `Box` is an ordinary multi-constructor DATACON family: the data-carrying
-- `Full` has three Int fields (so it escapes the listy/maybe shapes that
-- become shared `_builtin` intrinsics).  `mkBox` is a PRODUCER whose
-- declared result is the single TCon `Box`; its body builds only
-- `Full$I$I$I` cells (and the nullary `Empty`), so the return-type
-- refinement narrows `mkBox`'s return from Object to the family interface
-- `Box$F`.
--
-- The CONSUMER `useBox` is fed `mkBox`'s result at a call site that is NOT
-- a direct construction (`useBox (mkBox 5)`).  Only because `mkBox`'s
-- refined return advertises `Box$F` does that call log a family-typed
-- argument, earning `useBox$sp(Box$F)` whose `Full` alt reads the fields
-- with typed `getInt0/1/2` accessors instead of boxed `getProperty` +
-- `Conversion.toInt`.
data Box = Empty | Full Int Int Int

mkBox : Int -> Box
mkBox 0 = Empty
mkBox n = Full n (n + 1) (n + 2)

useBox : Box -> Int
useBox Empty = 0
useBox (Full a b c) = a + b + c

-- Regression guard 1: a RECORD must NOT be return-refined — only multi-con
-- family INTERFACES are valid targets.  A record has no marker interface,
-- only its concrete DCon spec class, whose identity is per-site field-type
-- inference (`MkPFR$L$L$I` vs natural `MkPFR`).  Refining a record return
-- commits consumers to one class → `ClassCastException`/`VerifyError` when a
-- differently-inferred sibling flows in (the self-host `Text.Bounded` and
-- `Idris.Package.partitionOpts` failures).  Both a PARAMETRIC record
-- (`Wrap a`) and a NON-PARAMETRIC one (`NRec`) must keep an Object return.
record Wrap a where
  constructor MkWrap
  here : a
  lo : Int
  hi : Int

wrap : a -> Wrap a
wrap x = MkWrap x 1 2

-- Reads the parametric `here` field too, so the `int` instantiation
-- (`MkWrap$I$I$I`) is genuinely forced.
useWrap : Int -> Int
useWrap n = let w = wrap n in here w + lo w + hi w

-- A genuine non-parametric RECORD: THREE fields, so `calcListy`/`calcMaybe`
-- don't reinterpret it as a synthetic CONS/JUST cell.  Its spec class is
-- per-site field-type inference, so it must NOT be return-refined (§14).
record NRec where
  constructor MkNRec
  a : Int
  b : Int
  c : Int

mkRec : Int -> NRec
mkRec n = MkNRec n (n + 1) (n + 2)

useRec : Int -> Int
useRec n = let r = mkRec n in a r + b r + c r

main : IO ()
main = do
  printLn (useBox (mkBox 5))
  printLn (useWrap 7)
  printLn (useRec 9)
