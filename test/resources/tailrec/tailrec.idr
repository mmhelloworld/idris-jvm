module Main

sum : Nat -> Nat
sum n = go 0 n where
  go : Nat -> Nat -> Nat
  go acc Z = acc
  go acc n@(S k) = go (acc + n) k
  
main : IO ()
main = printLn (sum 50000)
