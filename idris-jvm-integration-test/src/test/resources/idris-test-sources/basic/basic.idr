module Main

import IdrisJvm.IO
import Java.Lang
import Java.Util
import Data.Vect

numTest : (Num a, Show a) => a -> a -> JVM_IO ()
numTest a b = do
  putStrLn $ show a ++ " + " ++ show b ++ " = " ++ show (a + b)
  putStrLn $ show a ++ " * " ++ show b ++ " = " ++ show (a * b)

integralTest : (Integral a, Show a) => a -> a -> JVM_IO ()
integralTest a b = do
  putStrLn $ show a ++ " div " ++ show b ++ " = " ++ show (a `div` b)
  -- putStrLn $ show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b) -- not working due to a defect in Idris portable codegen

fractionalTest : (Fractional a, Show a) => a -> a -> JVM_IO ()
fractionalTest a b = do
  putStrLn $ show a ++ " / " ++ show b ++ " = " ++ show (a / b)
  putStrLn $ "recip " ++ show a ++ " = " ++ show (recip a)
  putStrLn $ "recip " ++ show b ++ " = " ++ show (recip b)

showOrdering : Ordering -> String
showOrdering EQ = "EQ"
showOrdering LT = "LT"
showOrdering GT = "GT"

ordTest : (Ord a, Show a) => a -> a -> JVM_IO ()
ordTest a b = putStrLn $ "compare " ++ show a ++ " " ++ show b ++ " = " ++ showOrdering (compare a b)

int1 : Int
int1 = 2147483647

int2 : Int
int2 = -2147483648

int3 : Int
int3 = 4

integer1 : Integer
integer1 = 100000000000000009

integer2 : Integer
integer2 = 200000000000000008

bits8_1: Bits8
bits8_1 = 255

bits8_2: Bits8
bits8_2 = 255

bits16_1: Bits16
bits16_1 = 4097

bits16_2: Bits16
bits16_2 = 512

bits32_1: Bits32
bits32_1 = 8388608

bits32_2: Bits32
bits32_2 = 524288

bits64_1: Bits64
bits64_1 = 9223372036854775807

bits64_2: Bits64
bits64_2 = 281474976710656

double1 : Double
double1 = 24.234234

double2 : Double
double2 = 75676.23232

printHeader : String -> JVM_IO ()
printHeader header = do
  putStrLn ""
  putStrLn header
  putStrLn $ concat $ List.replicate (length header) "="

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y * y == z * z]
main : JVM_IO ()
main = do
  print "Hello "
  putStrLn "world!"
  printHeader "numTest on Int"
  numTest int1 int2

  printHeader "numTest on Integer"
  numTest integer1 integer2

  printHeader "numTest on Bits8"
  numTest bits8_1 bits8_2

  printHeader "numTest on Bits16"
  numTest bits16_1 bits16_2

  printHeader "numTest on Bits32"
  numTest bits32_1 bits32_2

  printHeader "numTest on Bits64"
  numTest bits64_1 bits64_2

  printHeader "numTest on Double"
  numTest double1 double2

  printHeader "integralTest on Int"
  integralTest int1 int2

  printHeader "integralTest on Integer"
  integralTest integer1 integer2

  printHeader "integralTest on Bits8"
  integralTest bits8_1 bits8_2

  printHeader "integralTest on Bits16"
  integralTest bits16_1 bits16_2

  printHeader "integralTest on Bits32"
  integralTest bits32_1 bits32_2

  printHeader "integralTest on Bits64"
  integralTest bits64_1 bits64_2

  printHeader "fractionalTest on Double"
  fractionalTest double1 double2

  printHeader "ordTest on Int"
  ordTest int1 int2

  printHeader "ordTest on Integer"
  ordTest integer1 integer2

  printHeader "ordTest on Bits8"
  ordTest bits8_1 bits8_2

  printHeader "ordTest on Bits16"
  ordTest bits16_1 bits16_2

  printHeader "ordTest on Bits32"
  ordTest bits32_1 bits32_2

  printHeader "ordTest on Bits64"
  ordTest bits64_1 bits64_2

  printHeader "ordTest on Double"
  ordTest double1 double2

  putStrLn ""
  printLn $ pythag 50

  -- Array tests start
  printLn !(arrayToVect !(vectToArray ['d', 'c', 'a']))
  printLn !(arrayToVect !(vectToArray $ the (Vect _ Int) $ [3, 4, 5]))
  printLn !(arrayToVect !(vectToArray (the (Vect _ Bits8) [2, 4, 6, 5, 3])))
  printLn !(arrayToVect !(vectToArray (the (Vect _ Bits16) [5, 3])))
  printLn !(arrayToVect !(vectToArray (the (Vect _ Bits32) [2, 5, 3])))
  printLn !(arrayToVect !(vectToArray (the (Vect _ Bits64) [2, 4, 6, 5])))
  printLn !(arrayToVect !(vectToArray [Float 0.5, Float 2.0, Float 4.5]))
  printLn !(arrayToVect !(vectToArray [1.3, 1.5, 1.9, 2.5]))
  printLn !(arrayToVect !(vectToArray ["hello", "world", "!"]))

  marr <- newMultiArray (Int -> Int -> Int -> Int -> JVM_IO (JVM_Array (JVM_Array (JVM_Array (JVM_Array String))))) 3 2 2 4
  setArray (JVM_Array (JVM_Array (JVM_Array (JVM_Array String))) -> Int -> Int -> Int -> Int -> String -> JVM_IO ()) marr 0 0 0 3 "foo"
  printLn !(Arrays.deepToString marr)
  printLn !(getArray (JVM_Array (JVM_Array (JVM_Array (JVM_Array String))) -> Int -> Int -> Int -> Int -> JVM_IO String) marr 0 0 0 3)
  printLn !(arrayLength !(getArray (JVM_Array (JVM_Array (JVM_Array (JVM_Array String))) -> Int -> Int -> JVM_IO (JVM_Array (JVM_Array String))) marr 0 0))
  printLn !(arrayLength marr)

  vectArr2d <- vectToArray2d (the (Vect _ (Vect _ Int)) [[3, 33], [4, 44], [5, 55]])
  printLn !(Arrays.deepToString vectArr2d)
  -- Array tests end
