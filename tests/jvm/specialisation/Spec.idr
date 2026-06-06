module Main

mulInts : Int -> Int -> Int
mulInts x y = x * y

addInts : Int -> Int -> Int
addInts x y = x + y

squareInt : Int -> Int
squareInt n = mulInts n n

main : IO ()
main = do
  printLn (mulInts 6 7)
  printLn (addInts 100 23)
  printLn (squareInt 9)
