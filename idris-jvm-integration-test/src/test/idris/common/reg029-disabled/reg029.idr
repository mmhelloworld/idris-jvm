module Main

import IdrisJvm.IO
import IdrisJvm.System

main : JVM_IO ()
main = do
  printLn !(getEnv "IDRIS_REG029_NONEXISTENT_VAR")
  printLn !(getEnv "IDRIS_REG029_EXISTENT_VAR")
