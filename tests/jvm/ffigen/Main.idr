module Main

-- `Java.Util` is generated at compile time by `--jvm-ffi-import java/util/ArrayList`
-- (see preOptions in src/Idris/SetOptions.idr). The generator parses Java generic
-- signatures, so `ArrayList` is a parameterised marker (`ArrayList : Type -> Type`)
-- and `get` returns the element type rather than `Object`.
import Java.Util

main : IO ()
main = do
  -- annotate the element type once; the type parameter then flows through (a bare
  -- literal into a fresh generic container needs a hint, as with any parametric FFI).
  xs <- the (IO (ArrayList String)) ArrayList.new
  _  <- ArrayList.add xs "hello"
  _  <- ArrayList.add xs "world"
  n  <- ArrayList.size xs
  -- get : ArrayList e -> Int -> io e  — returns String here, no Object/cast
  first <- ArrayList.get xs 0
  printLn n
  putStrLn first
