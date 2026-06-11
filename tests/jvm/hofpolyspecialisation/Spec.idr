module Main

-- Polymorphic higher-order function: the callback slot has no concrete
-- declared type, so the typed signature is derived from the literal
-- lambda's body (the parameter's uses in primitive operations).
myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

mySum : List Int -> Int
mySum [] = 0
mySum (x :: xs) = x + mySum xs

main : IO ()
main = do
  printLn (mySum (myMap (\x => x * 2) [1, 2, 3]))
  printLn (myMap (\s => s ++ "!") ["a", "b"])
