module Main

-- Generated at build time from the package file's `jvmimports` (see tests/jvm/ffigen for the
-- command-line form). Nothing under src/ is written by the generator.
import Java.Util

main : IO ()
main = do
  xs <- the (IO (ArrayList String)) ArrayList.new
  _  <- ArrayList.add xs "banana"
  _  <- ArrayList.add xs "apple"
  n  <- ArrayList.size xs
  printLn n
  m <- the (IO (HashMap String Int)) HashMap.new
  _ <- HashMap.put m "answer" 42
  Just v <- HashMap.get m "answer"
    | Nothing => putStrLn "unexpected miss"
  printLn v
