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

BooleanClass : JVM_NativeTy
BooleanClass = Class "java/lang/Boolean"

ByteClass : JVM_NativeTy
ByteClass = Class "java/lang/Byte"

CharacterClass : JVM_NativeTy
CharacterClass = Class "java/lang/Character"

StringClass : JVM_NativeTy
StringClass = Class "java/lang/String"

ShortClass : JVM_NativeTy
ShortClass = Class "java/lang/Short"

LongClass : JVM_NativeTy
LongClass = Class "java/lang/Long"

parseBool : String -> Bool
parseBool s = unsafePerformIO $ javacall (Static BooleanClass "parseBoolean") (String -> JVM_IO Bool) s

boolToString : Bool -> String
boolToString b = unsafePerformIO $ javacall (Static StringClass "valueOf") (Bool -> JVM_IO String) b

charAt : String -> Int -> Char
charAt str index = unsafePerformIO $ javacall (Instance "charAt") (String -> Int -> JVM_IO Char) str index

charToString : Char -> String
charToString b = unsafePerformIO $ javacall (Static StringClass "valueOf") (Char -> JVM_IO String) b

parseByte : String -> Bits8
parseByte s = unsafePerformIO $ javacall (Static ByteClass "parseByte") (String -> JVM_IO Bits8) s

byteToString : Bits8 -> String
byteToString b = unsafePerformIO $ javacall (Static ByteClass "toString") (Bits8 -> JVM_IO String) b

parseShort : String -> Bits16
parseShort s = unsafePerformIO $ javacall (Static ShortClass "parseShort") (String -> JVM_IO Bits16) s

shortToString : Bits16 -> String
shortToString b = unsafePerformIO $ javacall (Static ShortClass "toString") (Bits16 -> JVM_IO String) b

parseLong : String -> Bits64
parseLong s = unsafePerformIO $ javacall (Static LongClass "parseLong") (String -> JVM_IO Bits64) s

longToString : Bits64 -> String
longToString b = unsafePerformIO $ javacall (Static LongClass "toString") (Bits64 -> JVM_IO String) b

main : JVM_IO ()
main = do
  printLn $ max 3 5
  printLn $ boolToString $ parseBool "true"
  printLn $ charToString $ charAt "foobarbaz" 3
  printLn $ byteToString $ parseByte "127"
  printLn $ shortToString $ parseShort "32767"
  printLn $ longToString $ parseLong "9223372036854775807"
  printLn !(getProperty "idris_jvm_ffi_invalid_prop" "foo")
  bigInt1 <- fromString "11111111111111111111" -- Invokes BigInteger constructor
  bigInt2 <- fromString "11111111111111111111"
  printLn $ toString $ add bigInt1 bigInt2 -- bigInt1.add(bigInt2).toString()
  jlist <- emptyArrayList
  printLn !(size jlist)
