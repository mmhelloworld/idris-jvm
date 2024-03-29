module Main

import Data.Vect
import Data.Stream

foo : List Char
foo = unpack $ pack $ take 4000 (repeat 'a')

factorialAux : Integer -> Integer -> Integer
factorialAux 0 a = a
factorialAux n a = factorialAux (n-1) (a*n)

factorial : Integer -> Integer
factorial n = factorialAux n 1

sum : Nat -> Nat
sum n = go 0 n where
  go : Nat -> Nat -> Nat
  go acc Z = acc
  go acc n@(S k) = go (acc + n) k

main : IO ()
main =
   do
      printLn $ factorial 100
      printLn $ factorial 10000
      printLn $ show $ the (Vect 3 String) ["red", "green", "blue"]
      printLn foo
      printLn (sum 50000)
