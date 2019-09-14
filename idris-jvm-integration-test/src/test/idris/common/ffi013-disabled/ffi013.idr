module Main

import IdrisJvm.IO
import IdrisJvm.File

main : JVM_IO ()
main = do b <- fRemove "remove.me"
          putStrLn $ if b then "success" else "failure"
