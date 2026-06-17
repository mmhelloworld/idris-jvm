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
  _  <- ArrayList.add xs "banana"
  _  <- ArrayList.add xs "apple"
  _  <- ArrayList.add xs "cherry"
  n  <- ArrayList.size xs
  printLn n
  -- sort : ArrayList e -> (e -> e -> Int) -> io ()  — an Idris function is bridged to a
  -- java.util.Comparator via `jlambda` (the functional-interface binding).
  ArrayList.sort xs (\a, b => if a < b then -1 else if a > b then 1 else 0)
  -- get : ArrayList e -> Int -> io e  — returns String here, no Object/cast
  first <- ArrayList.get xs 0
  putStrLn first

  -- Null-safety: HashMap.get is a curated JDK nullable method (HashMap <: java.util.Map), so it
  -- is generated as `... -> io (Maybe v)` — a missing key surfaces as `Nothing`, not a raw null.
  m <- the (IO (HashMap String String)) HashMap.new
  _ <- HashMap.put m "fruit" "apple"
  Just hit <- HashMap.get m "fruit"
    | Nothing => putStrLn "unexpected miss"
  putStrLn hit
  Nothing <- HashMap.get m "veg"
    | Just _ => putStrLn "unexpected hit"
  putStrLn "veg: Nothing"
