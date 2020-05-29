module Java.Lang

import IdrisJvm.FFI
import Data.Vect

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
  integerClass : String
  integerClass = "java/lang/Integer"

  IntegerClass : JVM_NativeTy
  IntegerClass = Class integerClass

  JInteger : Type
  JInteger = JVM_Native IntegerClass

  parseInt : String -> Int
  parseInt s = unsafePerformIO $ invokeStatic IntegerClass "parseInt" (String -> JVM_IO Int) s

  valueOf : Int -> JInteger
  valueOf n = unsafePerformIO $ invokeStatic IntegerClass "valueOf" (Int -> JVM_IO JInteger) n

namespace Short
  ShortClass : JVM_NativeTy
  ShortClass = Class "java/lang/Short"

  Short : Type
  Short = JVM_Native ShortClass

  parseShort : String -> Bits16
  parseShort s = unsafePerformIO $ invokeStatic ShortClass "parseShort" (String -> JVM_IO Bits16) s

  valueOf : Bits16 -> Short
  valueOf n = unsafePerformIO $ invokeStatic ShortClass "valueOf" (Bits16 -> JVM_IO Short) n

namespace Long
  LongClass : JVM_NativeTy
  LongClass = Class "java/lang/Long"

  Long : Type
  Long = JVM_Native LongClass

  parseLong : String -> Bits64
  parseLong s = unsafePerformIO $ invokeStatic LongClass "parseLong" (String -> JVM_IO Bits64) s

  valueOf : Bits64 -> Long
  valueOf n = unsafePerformIO $ invokeStatic LongClass "valueOf" (Bits64 -> JVM_IO Long) n

  intValue : Bits64 -> Int
  intValue n = unsafePerformIO $ invokeInstance "intValue" (Long -> JVM_IO Int) (Long.valueOf n)

namespace JDouble
  DoubleClass : JVM_NativeTy
  DoubleClass = Class "java/lang/Double"

  JDouble : Type
  JDouble = JVM_Native DoubleClass

  parseDouble : String -> Double
  parseDouble s = unsafePerformIO $ invokeStatic DoubleClass "parseDouble" (String -> JVM_IO Double) s

  valueOf : Double -> JDouble
  valueOf n = unsafePerformIO $ invokeStatic DoubleClass "valueOf" (Double -> JVM_IO JDouble) n

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

namespace Closeable

  Closeable : Type
  Closeable = JVM_Native (Interface "java/io/Closeable")

  close : Inherits Closeable closeable => closeable -> JVM_IO ()
  close closeable = invokeInstance "close" (Closeable -> JVM_IO ()) (believe_me closeable)

namespace Flushable

  Flushable : Type
  Flushable = JVM_Native (Interface "java/io/Flushable")

  flush : Inherits Flushable flushable => flushable -> JVM_IO ()
  flush flushable = invokeInstance "flush" (Flushable -> JVM_IO ()) (believe_me flushable)

namespace InputStream

  InputStream : Type
  InputStream = JVM_Native (Class "java/io/InputStream")

  Inherits Closeable InputStream where {}

namespace PrintStream

  PrintStream : Type
  PrintStream = JVM_Native $ Class "java/io/PrintStream"

  println : PrintStream -> String -> JVM_IO ()
  println = invokeInstance "println" (PrintStream -> String -> JVM_IO ())

  printCh : PrintStream -> Char -> JVM_IO ()
  printCh = invokeInstance "print" (PrintStream -> Char -> JVM_IO ())

  Inherits Flushable PrintStream where {}

namespace System

  systemClass : String
  systemClass = "java/lang/System"

  SystemClass : JVM_NativeTy
  SystemClass = Class systemClass

  getProperty : String -> JVM_IO (Maybe String)
  getProperty p = nullableStringToMaybe <$> invokeStatic SystemClass "getProperty" (String -> JVM_IO String) p

  getenv : String -> JVM_IO (Maybe String)
  getenv p = nullableStringToMaybe <$> invokeStatic SystemClass "getenv" (String -> JVM_IO String) p

  setProperty : String -> String -> JVM_IO (Maybe String)
  setProperty name value = nullableStringToMaybe <$>
    invokeStatic SystemClass "setProperty" (String -> String -> JVM_IO String) name value

  -- Takes a property name and a default value and returns the property value
  -- if it exists otherwise returns the default value
  getPropertyWithDefault : String -> String -> JVM_IO String
  getPropertyWithDefault = invokeStatic SystemClass "getProperty" (String -> String -> JVM_IO String)

  stdout : PrintStream
  stdout = unsafePerformIO $ getStaticField SystemClass "out" (JVM_IO PrintStream)

  stderr : PrintStream
  stderr = unsafePerformIO $ getStaticField SystemClass "err" (JVM_IO PrintStream)

  stdin : InputStream
  stdin = unsafePerformIO $ getStaticField SystemClass "in" (JVM_IO InputStream)

  lineSeparator : JVM_IO String
  lineSeparator = invokeStatic SystemClass "lineSeparator" (JVM_IO String)

  exit : Int -> JVM_IO ()
  exit = invokeStatic SystemClass "exit" (Int -> JVM_IO ())

namespace Runnable
  Runnable : Type
  Runnable = javaInterface "java/lang/Runnable"

  %inline
  jlambda : JVM_IO () -> Runnable
  jlambda f = javalambda "run" (JVM_IO ()) g
    where
      g : () -> ()
      g x = unsafePerformIO f

  run : Runnable -> JVM_IO ()
  run runnable = invokeInstance "run" (Runnable -> JVM_IO ()) runnable

namespace Thread

  ThreadClass : JVM_NativeTy
  ThreadClass = Class "java/lang/Thread"

  Thread : Type
  Thread = JVM_Native ThreadClass

  fromRunnable : Runnable -> JVM_IO Thread
  fromRunnable runnable = FFI.new (Runnable -> JVM_IO Thread) runnable

  start : Thread -> JVM_IO ()
  start thread = invokeInstance "start" (Thread -> JVM_IO ()) thread

  sleep : Bits64 -> JVM_IO ()
  sleep millis = invokeStatic ThreadClass "sleep" (Bits64 -> JVM_IO ()) millis

  currentThread : JVM_IO Thread
  currentThread = invokeStatic ThreadClass "currentThread" (JVM_IO Thread)

  getId : Thread -> JVM_IO Bits64
  getId thread = invokeInstance "getId" (Thread -> JVM_IO Bits64) thread

  join : Thread -> JVM_IO ()
  join thread = invokeInstance "join" (Thread -> JVM_IO ()) thread

namespace JavaString

  stringClass : String
  stringClass = "java/lang/String"

  StringClass : JVM_NativeTy
  StringClass = Class stringClass

  StringArray : Type
  StringArray = JVM_Array String

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

  getBytes : String -> JVM_IO (JVM_Array Bits8)
  getBytes str = invokeInstance "getBytes" (String -> String -> JVM_IO (JVM_Array Bits8)) str "UTF-8"

  format : String -> JVM_Array Object -> String
  format fmt args = unsafePerformIO $ invokeStatic StringClass "format" (String -> JVM_Array Object -> JVM_IO String) fmt args

namespace StringBuilder
    StringBuilderClass : JVM_NativeTy
    StringBuilderClass = Class "java/lang/StringBuilder"

    StringBuilder : Type
    StringBuilder = JVM_Native StringBuilderClass

    new : Int -> JVM_IO StringBuilder
    new = FFI.new (Int -> JVM_IO StringBuilder)

    appendString : StringBuilder -> String -> JVM_IO StringBuilder
    appendString = invokeInstance "append" (StringBuilder -> String -> JVM_IO StringBuilder)

    toString : StringBuilder -> JVM_IO String
    toString = invokeInstance "toString" (StringBuilder -> JVM_IO String)

%inline
vectToArray : Vect n elemTy -> {auto jvmType: JVM_Types elemTy} -> JVM_IO (JVM_Array elemTy)
vectToArray {elemTy} xs
    = vectToArray' xs
        (\n => newArray elemTy n)
        (setArray (JVM_Array elemTy -> Int -> elemTy -> JVM_IO ()))
  where
    vectToArray' : Vect n elemTy
                -> (Nat -> JVM_IO (JVM_Array elemTy))
                -> (JVM_Array elemTy -> Int -> elemTy -> JVM_IO ())
                -> JVM_IO (JVM_Array elemTy)
    vectToArray' {n} xs createArray setArray' = do
        arr <- createArray n
        go arr 0 xs
      where
        go : JVM_Array elemTy -> Int -> Vect m elemTy -> JVM_IO (JVM_Array elemTy)
        go arr index [] = pure arr
        go arr index (x :: xs) = do
          setArray' arr index x
          go arr (index + 1) xs

%inline
listToArray : List elemTy -> {auto jvmType: JVM_Types elemTy} -> JVM_IO (JVM_Array elemTy)
listToArray {elemTy} xs
    = assert_total $ listToArray' xs
        (newArray elemTy (length xs))
        (setArray (JVM_Array elemTy -> Int -> elemTy -> JVM_IO ()))
  where
    listToArray' : List elemTy
                -> JVM_IO (JVM_Array elemTy)
                -> (JVM_Array elemTy -> Int -> elemTy -> JVM_IO ())
                -> JVM_IO (JVM_Array elemTy)
    listToArray' xs createArray setArray' = do
        arr <- createArray
        go arr 0 xs
      where
        go : JVM_Array elemTy -> Int -> List elemTy -> JVM_IO (JVM_Array elemTy)
        go arr index [] = pure arr
        go arr index (x :: xs) = do
          setArray' arr index x
          go arr (index + 1) xs

%inline
vectToArray2d : Vect m (Vect n elemTy) -> {auto jvmType: JVM_Types elemTy} -> JVM_IO (JVM_Array (JVM_Array elemTy))
vectToArray2d {elemTy} xs
    = vectToArray2d' xs
        (\m, n => newMultiArray (Int -> Int -> JVM_IO (JVM_Array (JVM_Array elemTy))) (cast m) (cast n))
        (setArray (JVM_Array (JVM_Array elemTy) -> Int -> Int -> elemTy -> JVM_IO ()))
  where
    vectToArray2d' : Vect m (Vect n elemTy)
                  -> (Nat -> Nat -> JVM_IO (JVM_Array (JVM_Array elemTy)))
                  -> (JVM_Array (JVM_Array elemTy) -> Int -> Int -> elemTy -> JVM_IO ())
                  -> JVM_IO (JVM_Array (JVM_Array elemTy))
    vectToArray2d' {m} {n} xs createArray setArray' = do
        arr <- createArray m n
        go arr 0 xs
      where
        createRow : JVM_Array (JVM_Array elemTy) -> Int -> Int -> Vect a elemTy -> JVM_IO ()
        createRow arr row col [] = pure ()
        createRow arr row col (x :: xs) = do
          setArray' arr row col x
          createRow arr row (col + 1) xs

        go : JVM_Array (JVM_Array elemTy) -> Int -> Vect a (Vect b elemTy) -> JVM_IO (JVM_Array (JVM_Array elemTy))
        go {b} arr row [] = pure arr
        go {b} arr row (xs :: xxs) = do
          createRow arr row 0 xs
          go arr (row + 1) xxs

%inline
arrayToVect : JVM_Array elemTy -> {auto jvmType: JVM_Types elemTy} -> JVM_IO (n ** Vect n elemTy)
arrayToVect {elemTy} arr = do
    len <- arrayLength arr
    v <- f 0 len (getArray (JVM_Array elemTy -> Int -> JVM_IO elemTy) arr)
    pure (_ ** v)
  where
    f : (index: Nat) -> (n: Nat) -> (Int -> JVM_IO elemTy) -> JVM_IO (Vect n elemTy)
    f index Z getter = pure []
    f index (S k) getter = do
      rest <- f (index + 1) k getter
      curr <- getter (cast index)
      pure (curr :: rest)
