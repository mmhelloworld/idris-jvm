module Main

import IdrisJvm.FFI

mathMax : Int -> Int -> Int
mathMax a b = unsafePerformIO $ javacall (JStatic "java/lang/Math" "max") (Int -> Int -> JVM_IO Int) a b

-- Takes a property name and a default value and returns the property value
-- if it exists otherwise returns the default value
systemGetProperty : String -> String -> JVM_IO String
systemGetProperty key default = javacall (JStatic "java/lang/System" "getProperty") (String -> String -> JVM_IO String) key default

main : JVM_IO ()
main = do
  printLn $ mathMax 3 5
  systemGetProperty "idris_jvm_ffi_invalid_prop" "foo" >>= printLn
