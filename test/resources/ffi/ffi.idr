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
  objToString : Inherits Object that => that -> String
  objToString obj = unsafePerformIO $ javacall (Static ObjectsClass "toString") (Object -> JVM_IO String) (believe_me obj)


namespace Java.Math.BigInteger
  BigIntegerClass : JVM_NativeTy
  BigIntegerClass = Class "java/math/BigInteger"

  BigInteger : Type
  BigInteger = JVM_Native BigIntegerClass

  -- Constructor example
  fromString : String -> JVM_IO BigInteger
  fromString s = new (String -> JVM_IO BigInteger) s

  toString : BigInteger -> String
  toString bigInt = unsafePerformIO $ javacall (Instance "toString") (BigInteger -> JVM_IO String) bigInt

  add : BigInteger -> BigInteger -> BigInteger
  add b1 b2 = unsafePerformIO $ javacall (Instance "add") (BigInteger -> BigInteger -> JVM_IO BigInteger) b1 b2


Inherits Object BigInteger where {}


namespace Java.Util.List
  ListClass : JVM_NativeTy
  ListClass = Interface "java/util/List"

  JList : Type
  JList = JVM_Native ListClass

  size : Inherits JList list => list -> JVM_IO Nat
  size list = cast <$> javacall (Instance "size") (JList -> JVM_IO Int) (believe_me list)

Inherits Object JList where {}


namespace Java.Util.ArrayList
  ArrayListClass : JVM_NativeTy
  ArrayListClass = Class "java/util/ArrayList"

  ArrayList : Type
  ArrayList = JVM_Native ArrayListClass

  emptyArrayList : JVM_IO ArrayList
  emptyArrayList = new (JVM_IO ArrayList)

Inherits JList ArrayList where {}
Inherits Object ArrayList where {}

main : JVM_IO ()
main = do
  printLn $ max 3 5
  printLn !(getProperty "idris_jvm_ffi_invalid_prop" "foo")
  bigInt1 <- fromString "11111111111111111111" -- Invokes BigInteger constructor
  bigInt2 <- fromString "11111111111111111111"
  printLn $ toString $ add bigInt1 bigInt2 -- bigInt1.add(bigInt2).toString()
  jlist <- emptyArrayList
  printLn !(size jlist)
