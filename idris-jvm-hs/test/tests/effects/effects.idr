module Main

import Effects
import Effect.StdIO
import IdrisJvm.IO
import IdrisJvm.Effects

hello : Eff () [STDIO]
hello = do
  putStrLn "Hello world!"
  putChar 'b'
  putStrLn "ar"

main : JVM_IO ()
main = run hello
