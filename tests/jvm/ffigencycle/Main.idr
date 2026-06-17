module Main

-- `java.lang` and `java.util` reference each other (e.g. `java.lang.Iterable.iterator()`
-- returns `java.util.Iterator`, while `java.util` types name `java.lang.Object`), so a
-- per-package module would form an import cycle. The generator merges the cyclic packages
-- into one umbrella module (`Java`) with the per-package `import Java.Util` / `import Java.Lang`
-- preserved as thin re-export stubs.
import Java.Util
import Java.Lang

main : IO ()
main = do
  xs <- the (IO (ArrayList String)) ArrayList.new
  _  <- ArrayList.add xs "alpha"
  _  <- ArrayList.add xs "beta"
  n  <- ArrayList.size xs
  printLn n
