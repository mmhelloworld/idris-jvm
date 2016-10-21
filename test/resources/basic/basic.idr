module Main

numTest : (Num a, Show a) => a -> a -> IO ()
numTest a b = do
  putStrLn $ show a ++ " + " ++ show b ++ " = " ++ show (a + b)
  putStrLn $ show a ++ " * " ++ show b ++ " = " ++ show (a * b)

integralTest : (Integral a, Show a) => a -> a -> IO ()
integralTest a b = do
  putStrLn $ show a ++ " div " ++ show b ++ " = " ++ show (a `div` b)
  putStrLn $ show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)

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

bits16_1: Bits8
bits16_1 = 4097

bits16_2: Bits8
bits16_2 = 512

bits32_1: Bits8
bits32_1 = 8388608

bits32_2: Bits8
bits32_2 = 524288

bits64_1: Bits8
bits64_1 = 9223372036854775807

bits64_2: Bits8
bits64_2 = 281474976710656

main : IO ()
main = do
  print "Hello "
  putStrLn "world!"
  numTest int1 int2
  numTest integer1 integer2
  numTest bits8_1 bits8_2

  -- The following commented code fails with (even on the C backend):
  -- *** ./Prelude/Interfaces.idr:356:18:unmatched case in Prelude.Interfaces.case
  -- block in divB8 at ./Prelude/Interfaces.idr:356:18 ***
  --numTest bits16_1 bits16_2
  --numTest bits32_1 bits32_2
  --numTest bits64_1 bits64_2

  integralTest int1 int2
  integralTest integer1 integer2
  integralTest bits8_1 bits8_2

  -- The following commented code fails with (even on the C backend):
  -- *** ./Prelude/Interfaces.idr:356:18:unmatched case in Prelude.Interfaces.case
  -- block in divB8 at ./Prelude/Interfaces.idr:356:18 ***
  --integralTest bits16_1 bits16_2
  --integralTest bits32_1 bits32_2
  --integralTest bits64_1 bits64_2
