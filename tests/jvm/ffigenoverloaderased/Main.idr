module Main

-- Regression: java.lang.StringBuilder has `append` overloads that ERASE to the same Idris prim
-- type — e.g. append(char[]) and append(Object) both become
-- `StringBuilder -> Object -> PrimIO StringBuilder`. The wrappers intentionally share the name
-- `append` (resolved by argument type), but their prims must NOT, or a wrapper body's
-- `prim__append` call elaborates ambiguously. Each overload now gets a distinct prim
-- (`prim__append`, `prim__Append1`, …), so the whole group compiles.
import Java.Lang

main : IO ()
main = do
  sb <- StringBuilder.new
  _  <- StringBuilder.append sb "idris"     -- append(String)
  _  <- StringBuilder.append sb "-on-jvm"
  _  <- StringBuilder.reverse sb
  s  <- StringBuilder.toString sb
  putStrLn s
