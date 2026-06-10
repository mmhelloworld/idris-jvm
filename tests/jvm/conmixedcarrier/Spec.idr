module Main

-- Mixed-carrier sum type: BOTH constructors carry data, with different
-- slot shapes.  The family interface is keyed by the TCon instantiation
-- (Run has no type params -> single interface Run$F), so Done$I and
-- Step$I$L both implement it and family-typed tail recursion across
-- siblings is sound.  Under the old DCon-slot-suffix keying this
-- program crashed with `Done$I cannot be cast to Run$I$L`.
data Run = Done Int | Step Int Run

sumRun : Int -> Run -> Int
sumRun acc (Done n) = acc + n
sumRun acc (Step n rest) = sumRun (acc + n) rest

mkRun : Int -> Run
mkRun 0 = Done 0
mkRun n = Step n (mkRun (n - 1))

main : IO ()
main = do
  printLn (sumRun 0 (Step 1 (Step 2 (Done 3))))
  printLn (sumRun 0 (mkRun 100))
