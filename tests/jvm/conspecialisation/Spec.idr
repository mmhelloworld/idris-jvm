module Main

data Triple = MkT Int Int Int

sumT : Triple -> Int
sumT (MkT a b c) = a + b + c

main : IO ()
main = do
  putStrLn (show (sumT (MkT 6 7 8)))
  putStrLn (show (sumT (MkT 100 23 4)))
