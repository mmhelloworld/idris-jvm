module Main

numTest : (Num a, Show a) => a -> a -> IO ()
numTest a b = do
  putStrLn $ show a ++ " + " ++ show b ++ " = " ++ show (a + b)
  putStrLn $ show a ++ " * " ++ show b ++ " = " ++ show (a * b)

integralTest : (Integral a, Show a) => a -> a -> IO ()
integralTest a b = do
  putStrLn $ show a ++ " div " ++ show b ++ " = " ++ show (a `div` b)
  putStrLn $ show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)

fractionalTest : (Fractional a, Show a) => a -> a -> IO ()
fractionalTest a b = do
  putStrLn $ show a ++ " / " ++ show b ++ " = " ++ show (a / b)
  putStrLn $ "recip " ++ show a ++ " = " ++ show (recip a)
  putStrLn $ "recip " ++ show b ++ " = " ++ show (recip b)

showOrdering : Ordering -> String
showOrdering EQ = "EQ"
showOrdering LT = "LT"
showOrdering GT = "GT"

ordTest : (Ord a, Show a) => a -> a -> IO ()
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

printHeader : String -> IO ()
printHeader header = do
  putStrLn ""
  putStrLn header
  putStrLn $ concat $ replicate (length header) "="

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y * y == z * z]
main : IO ()
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
