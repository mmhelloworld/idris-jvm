module Main

-- `org.objectweb.asm.Type` has a simple name that is reserved in Idris, so the generator
-- names its marker and namespace `Type_` (same rule for keywords such as `data`).
import Org.Objectweb.Asm

main : IO ()
main = do
  t <- Type_.getObjectType "java/lang/String"
  d <- Type_.getDescriptor t
  putStrLn d
  n <- Type_.getClassName t
  putStrLn n
