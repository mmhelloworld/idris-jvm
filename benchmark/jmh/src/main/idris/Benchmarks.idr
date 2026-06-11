module Benchmarks

import Data.List
import System
import System.FFI
import Java.Lang
import Java.Util

%export
    """
    jvm:import
    org/openjdk/jmh/annotations/Benchmark
    org/openjdk/jmh/infra/Blackhole
    main/BenchmarkMain
    """
jvmImports : List String
jvmImports = []

-- Sieve of Eratosthenes over List Int: exercises polymorphic list
-- construction and filtering with boxed numeric elements.
sieve : List Int -> List Int
sieve [] = []
sieve (p :: xs) = p :: sieve (filter (\x => mod x p /= 0) xs)

sieveLimit : Int
sieveLimit = 4096

countPrimes : Int -> Int
countPrimes n = cast (length (sieve [2 .. n]))

-- Matrix multiplication over List (List Double): exercises higher-order
-- functions (map, zipWith, foldl, transpose) with boxed doubles.
matrixSize : Int
matrixSize = 50

mkMatrix : Int -> List (List Double)
mkMatrix n = map (\i => map (\j => cast (i * n + j)) [0 .. n - 1]) [0 .. n - 1]

dot : List Double -> List Double -> Double
dot xs ys = foldl (+) 0 (zipWith (*) xs ys)

mult : List (List Double) -> List (List Double) -> List (List Double)
mult a b = let bt = transpose b in map (\row => map (dot row) bt) a

sumMatrix : List (List Double) -> Double
sumMatrix = foldl (\acc, row => foldl (+) acc row) 0

matMulChecksum : Int -> Double
matMulChecksum n = sumMatrix (mult (mkMatrix n) (mkMatrix n))

namespace Blackhole
    public export
    Blackhole : Type
    Blackhole = Struct "org/openjdk/jmh/infra/Blackhole" []

    %foreign "jvm:.consume"
    prim_consumeInt : Blackhole -> Int -> PrimIO ()

    %foreign "jvm:.consume"
    prim_consumeDouble : Blackhole -> Double -> PrimIO ()

    export %inline
    consumeInt : HasIO io => Blackhole -> Int -> io ()
    consumeInt blackhole value = primIO $ prim_consumeInt blackhole value

    export %inline
    consumeDouble : HasIO io => Blackhole -> Double -> io ()
    consumeDouble blackhole value = primIO $ prim_consumeDouble blackhole value

%foreign "jvm:main,org/openjdk/jmh/Main"
prim_jmhMain : Array String -> PrimIO ()

jmhMain : HasIO io => Array String -> io ()
jmhMain args = primIO $ prim_jmhMain args

namespace BenchmarkMain
    %export """
            jvm:public BenchmarkMain
            {
                "annotations": [
                    {"NoArgsConstructor": {}}
                ]
            }
            """
    public export
    BenchmarkMain : Type
    BenchmarkMain = Struct "main/BenchmarkMain" []

    %export """
             jvm:public sievePrimes
             {
                 "annotations": [
                     {"Benchmark": {}}
                 ],
                 "enclosingType": "BenchmarkMain",
                 "arguments": [
                     { "type": "BenchmarkMain" },
                     { "type": "Blackhole" }
                 ],
                 "returnType": "void"
             }
         """
    sievePrimes : BenchmarkMain -> Blackhole -> IO ()
    sievePrimes _ blackhole = consumeInt blackhole (countPrimes sieveLimit)

    %export """
             jvm:public matMul
             {
                 "annotations": [
                     {"Benchmark": {}}
                 ],
                 "enclosingType": "BenchmarkMain",
                 "arguments": [
                     { "type": "BenchmarkMain" },
                     { "type": "Blackhole" }
                 ],
                 "returnType": "void"
             }
         """
    matMul : BenchmarkMain -> Blackhole -> IO ()
    matMul _ blackhole = consumeDouble blackhole (matMulChecksum matrixSize)

main : IO ()
main = do
  args <- Arrays.fromList String !getArgs
  jmhMain args
