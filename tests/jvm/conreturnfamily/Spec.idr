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

main : IO ()
main = printLn (useBox (mkBox 5))
