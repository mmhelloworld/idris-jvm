module Main

import IdrisJvm.FFI
import IdrisJvm.IO

-- Returning an additional Nat to test that `par` returns a complex type correctly
fibonacci : Integer -> (Nat, Integer)
fibonacci 0 = (0, 0)
fibonacci 1 = (1, 1)
fibonacci n =
  let (fibNat1, fib1) = par $ fibonacci (n - 1) -- Evaluate in parallel using Java's ForkJoin
      (fibNat2, fib2) = par $ fibonacci (n - 2) -- Evaluate in parallel
      fibNat = fibNat1 + fibNat2
  in (fibNat * 2, fib1 + fib2)

shutdownExecutor : JVM_IO ()
shutdownExecutor = javacall (Static clz "shutdownExecutor") (JVM_IO ()) where
  clz = Class "io/github/mmhelloworld/idrisjvm/runtime/Concurrent"

main : JVM_IO ()
main = do
  fork $ printLn "a" -- Run in a new thread using Java's ExecutorService
  fork $ printLn "b" -- Run in another thread
  fork $ printLn $ fibonacci 10
  fork $ printLn "cde"
  fork $ printLn "fg"
  printLn "h"
  shutdownExecutor
