module Main

import IdrisJvm.IO
import Java.Lang

mutual
  evenT : Nat -> JVM_IO ()
  evenT Z = printLn True
  evenT (S k) = do
    setProperty "bar" (show k)
    oddT k

  oddT : Nat -> JVM_IO ()
  oddT Z = printLn False
  oddT (S k) = do
    setProperty "foo" (show k)
    evenT k

main : JVM_IO ()
main = evenT 99999
