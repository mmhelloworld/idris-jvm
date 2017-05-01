module Main

mutual
  evenT : Nat -> IO Bool
  evenT Z = pure True
  evenT (S k) = oddT k

  oddT : Nat -> IO Bool
  oddT Z = pure False
  oddT (S k) = evenT k

main : IO ()
main = evenT 99999 >>= printLn
