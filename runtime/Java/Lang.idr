module Java.Lang

import IdrisJvm.FFI

%access public export

namespace Boolean
  BooleanClass : JVM_NativeTy
  BooleanClass = Class "java/lang/Boolean"

  parseBool : String -> Bool
  parseBool s = unsafePerformIO $ invokeStatic BooleanClass "parseBoolean" (String -> JVM_IO Bool) s

namespace Byte
  ByteClass : JVM_NativeTy
  ByteClass = Class "java/lang/Byte"

  parseByte : String -> Bits8
  parseByte s = unsafePerformIO $ invokeStatic ByteClass "parseByte" (String -> JVM_IO Bits8) s

namespace Character
  CharacterClass : JVM_NativeTy
  CharacterClass = Class "java/lang/Character"

namespace JInteger
  IntegerClass : JVM_NativeTy
  IntegerClass = Class "java/lang/Integer"

  parseInt : String -> Int
  parseInt s = unsafePerformIO $ invokeStatic IntegerClass "parseInt" (String -> JVM_IO Int) s

namespace Short
  ShortClass : JVM_NativeTy
  ShortClass = Class "java/lang/Short"

  parseShort : String -> Bits16
  parseShort s = unsafePerformIO $ invokeStatic ShortClass "parseShort" (String -> JVM_IO Bits16) s

namespace Long
  LongClass : JVM_NativeTy
  LongClass = Class "java/lang/Long"

  parseLong : String -> Bits64
  parseLong s = unsafePerformIO $ invokeStatic LongClass "parseLong" (String -> JVM_IO Bits64) s

namespace Class

  ClassClass : JVM_NativeTy
  ClassClass = Class "java/lang/Class"

  Class : Type
  Class = JVM_Native ClassClass

  forName : String -> Class
  forName className = unsafePerformIO $ invokeStatic ClassClass "forName" (String -> JVM_IO Class) className

namespace Math

  MathClass : JVM_NativeTy
  MathClass = Class "java/lang/Math"

  Math : Type
  Math = JVM_Native MathClass

  maxInt : Int -> Int -> Int
  maxInt a b = unsafePerformIO $ invokeStatic MathClass "max" (Int -> Int -> JVM_IO Int) a b

  maxDouble : Double -> Double -> Double
  maxDouble a b = unsafePerformIO $ invokeStatic MathClass "max" (Double -> Double -> JVM_IO Double) a b

  maxFloat : JFloat -> JFloat -> JFloat
  maxFloat a b = unsafePerformIO $ invokeStatic MathClass "max" (JFloat -> JFloat -> JVM_IO JFloat) a b

namespace Object
  ObjectClass : JVM_NativeTy
  ObjectClass = Class "java/lang/Object"

  Object : Type
  Object = JVM_Native ObjectClass

  ObjectArray : Type
  ObjectArray = JVM_Native (Array ObjectClass)

Inherits Object Object where {}

namespace System

  SystemClass : JVM_NativeTy
  SystemClass = Class "java/lang/System"

  -- Takes a property name and a default value and returns the property value
  -- if it exists otherwise returns the default value
  getProperty : String -> String -> JVM_IO String
  getProperty = invokeStatic SystemClass "getProperty" (String -> String -> JVM_IO String)

namespace Thread

  ThreadClass : JVM_NativeTy
  ThreadClass = Class "java/lang/Thread"

  Thread : Type
  Thread = JVM_Native ThreadClass

namespace JavaString

  StringClass : JVM_NativeTy
  StringClass = Class "java/lang/String"

  StringArray : Type
  StringArray = JVM_Native (Array StringClass)

  boolToString : Bool -> String
  boolToString b = unsafePerformIO $ invokeStatic StringClass "valueOf" (Bool -> JVM_IO String) b

  charAt : String -> Int -> Char
  charAt str index = unsafePerformIO $ invokeInstance "charAt" (String -> Int -> JVM_IO Char) str index

  charToString : Char -> String
  charToString b = unsafePerformIO $ invokeStatic StringClass "valueOf" (Char -> JVM_IO String) b

  byteToString : Bits8 -> String
  byteToString b = unsafePerformIO $ invokeStatic ByteClass "toString" (Bits8 -> JVM_IO String) b

  shortToString : Bits16 -> String
  shortToString b = unsafePerformIO $ invokeStatic ShortClass "toString" (Bits16 -> JVM_IO String) b

  longToString : Bits64 -> String
  longToString b = unsafePerformIO $ invokeStatic LongClass "toString" (Bits64 -> JVM_IO String) b

Inherits Object String where {}

Inherits ObjectArray StringArray where {}
