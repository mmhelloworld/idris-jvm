module Main

import IdrisJvm.FFI

namespace Java.Lang.Math
  MathClass : JVM_NativeTy
  MathClass = Class "java/lang/Math"

  mathMax : Int -> Int -> Int
  mathMax a b = unsafePerformIO $ javacall (Static MathClass "max") (Int -> Int -> JVM_IO Int) a b


namespace Java.Lang.System

  SystemClass : JVM_NativeTy
  SystemClass = Class "java/lang/System"

  -- Takes a property name and a default value and returns the property value
  -- if it exists otherwise returns the default value
  getProperty : String -> String -> JVM_IO String
  getProperty key default = javacall (Static SystemClass "getProperty") (String -> String -> JVM_IO String) key default


namespace Java.Util.Objects

  ObjectsClass : JVM_NativeTy
  ObjectsClass = Class "java/util/Objects"

  -- Respects inheritance so that this function can be called on all types
  -- that extend java.lang.Object, which is everything.
  toString : Inherits Object that => that -> String
  toString obj = unsafePerformIO $ javacall (Static ObjectsClass "toString") (Object -> JVM_IO String) (believe_me obj)


namespace Java.Math.BigInteger
  BigIntegerClass : JVM_NativeTy
  BigIntegerClass = Class "java/math/BigInteger"

  BigInteger : Type
  BigInteger = JVM_Native BigIntegerClass

  -- Constructor example
  bigIntegerFromString : String -> JVM_IO BigInteger
  bigIntegerFromString s = new (String -> JVM_IO BigInteger) s

Inherits Object BigInteger where {}

main : JVM_IO ()
main = do
  printLn $ max 3 5
  printLn !(getProperty "idris_jvm_ffi_invalid_prop" "foo")
  bigNum <- bigIntegerFromString "100000000000099999" -- Invokes BigInteger constructor
  printLn $ toString bigNum -- Calling Objects.toString(Object) with BigInteger respecting inheritance
