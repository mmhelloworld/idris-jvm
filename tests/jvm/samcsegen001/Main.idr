module Main

import Java.Lang
import System.FFI

-- A SAM whose erased method signature has primitive types:
-- int applyAsInt(int). The lambda bridge must derive this precise descriptor
-- even when CSE lifts the shared type terms into csegen definitions (the
-- interface tuple and the function type occur at several jlambda sites).
public export %inline
IntUnaryOperator : Type
IntUnaryOperator = (Struct "java/util/function/IntUnaryOperator applyAsInt" [], Int -> PrimIO Int)

%foreign "jvm:.applyAsInt(i:java/util/function/IntUnaryOperator int int),java/util/function/IntUnaryOperator"
prim_applyAsInt : Object -> Int -> PrimIO Int

runOperator : IntUnaryOperator -> Int -> IO ()
runOperator operator value = do
    result <- primIO $ prim_applyAsInt (believe_me operator) value
    printLn result

increment : Int -> PrimIO Int
increment value = toPrim $ pure (value + 1)

double : Int -> PrimIO Int
double value = toPrim $ pure (value * 2)

main : IO ()
main = do
    runOperator (the IntUnaryOperator (jlambda increment)) 41
    runOperator (the IntUnaryOperator (jlambda double)) 21
