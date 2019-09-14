import IdrisJvm.IO
import IdrisJvm.Data.IORef

count : Nat -> IORef Integer -> JVM_IO ()
count Z ref
    = do x <- readIORef ref
         printLn x
count (S k) ref
    = do modifyIORef ref (+1)
         count k ref

main : JVM_IO ()
main = do r <- newIORef 0
          count 1000000 r
