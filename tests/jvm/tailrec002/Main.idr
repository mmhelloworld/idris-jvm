{-

Test that some library functions don't cause stack overflow errors.

So far, only Prelude.List.++ is tested.

-}

module Main

import Data.List
import Data.SnocList

values : List Nat
values = replicate 50000 1

seulav : SnocList Nat
seulav = Lin <>< values

mutual
    isEven : Nat -> Bool
    isEven Z = True
    isEven (S k) = isOdd k

    isOdd : Nat -> Bool
    isOdd Z = False
    isOdd (S k) = isEven k

-- tail recursive equivalent of library function so that we can test the tail call optimization of `go` below
takeBefore : (n -> Bool) -> Stream n -> List n
takeBefore p xs = reverse $ go [] xs where
    go : List n -> Stream n -> List n
    go acc (x :: xs) = if p x then go (x :: acc) xs else acc

-- This is to test trampolines resulting in splitting a function. JVM backend splits deeply nested cases into multiple
-- functions to avoid method size getting larger than JVM limit.
go : List Int -> Int
go [] = 0
go (a :: b :: c :: d :: e :: f :: g :: h :: rest) = go (a + b + c + d + e + f + g + h :: rest)
go (n :: rest) = go rest

main : IO ()
main = do
  printLn $ length $ values ++ [1]
  printLn $ length $ map Just values
  printLn $ length $ mapMaybe Just values
  printLn $ length $ filter (const True) values
  printLn $ length $ seulav ++ [<1]
  printLn $ length $ map Just seulav
  printLn $ length $ mapMaybe Just seulav
  printLn $ length $ filter (const True) seulav
  printLn $ isOdd 100000
  printLn $ (go $ Main.takeBefore (<= 200100) [1..]) > 100
