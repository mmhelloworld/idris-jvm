module Compiler.Jvm.Variable

import Core.Context
import Core.Core
import Core.FC

import Compiler.Jvm.Asm
import Compiler.Jvm.InferredType

import Data.Maybe
import Data.List

getLocTy : Map Int InferredType -> Int -> IO InferredType
getLocTy typesByIndex varIndex = do
    optTy <- Map.get typesByIndex varIndex
    pure $ fromMaybe IUnknown $ nullableToMaybe optTy

getVarIndex : Map Int InferredType -> Int -> IO Int
getVarIndex types index = go 0 0 where
  go : Int -> Int -> IO Int
  go pos currVarIndex =
    if currVarIndex == index then
        pure pos
    else do
        currVarTy <- getLocTy types currVarIndex
        let isTwoWordTy = currVarTy == IDouble || currVarTy == ILong
        let nextPos = if isTwoWordTy then pos + 2 else succ pos
        go nextPos (succ currVarIndex)

opWithWordSize : {auto stateRef: Ref AsmState AsmState} -> Map Int InferredType -> (Int -> Core ()) -> Int -> Core ()
opWithWordSize types op var = do
    newPos <- coreLift $ getVarIndex types var
    op newPos

boxDouble : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxDouble = invokeMethod InvokeStatic "java/lang/Double" "valueOf" "(D)Ljava/lang/Double;" False

boxFloat : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxFloat = invokeMethod InvokeStatic "java/lang/Float" "valueOf" "(F)Ljava/lang/Float;" False

boxBool : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxBool = invokeMethod InvokeStatic "java/lang/Boolean" "valueOf" "(Z)Ljava/lang/Boolean;" False

boxByte : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxByte = invokeMethod InvokeStatic "java/lang/Byte" "valueOf" "(B)Ljava/lang/Byte;" False

boxChar : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxChar = invokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

boxInt : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxInt = invokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False

boxShort : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxShort = invokeMethod InvokeStatic "java/lang/Short" "valueOf" "(S)Ljava/lang/Short;" False

boxLong : {auto stateRef: Ref AsmState AsmState} -> Core ()
boxLong = invokeMethod InvokeStatic "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;" False

unboxBool : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxBool = invokeMethod InvokeVirtual "java/lang/Boolean" "booleanValue" "()Z" False

unboxByte : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxByte = invokeMethod InvokeVirtual "java/lang/Byte" "byteValue" "()B" False

unboxInt : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxInt = invokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False

unboxChar : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxChar = invokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False

unboxShort : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxShort = invokeMethod InvokeVirtual "java/lang/Short" "shortValue" "()S" False

unboxLong : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxLong = invokeMethod InvokeVirtual "java/lang/Long" "longValue" "()J" False

unboxDouble : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxDouble = invokeMethod InvokeVirtual "java/lang/Double" "doubleValue" "()D" False

unboxFloat : {auto stateRef: Ref AsmState AsmState} -> Core ()
unboxFloat = invokeMethod InvokeVirtual "java/lang/Float" "floatValue" "()F" False

%inline
export
conversionClass : String
conversionClass = "io/github/mmhelloworld/idrisjvm/runtime/Conversion"

boolObjToBool : {auto stateRef: Ref AsmState AsmState} -> Core ()
boolObjToBool = invokeMethod InvokeStatic conversionClass "toBoolean" "(Ljava/lang/Object;)Z" False

boolToInt : {auto stateRef: Ref AsmState AsmState} -> Core ()
boolToInt = invokeMethod InvokeStatic conversionClass "boolToInt1" "(Z)I" False

objToInt : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToInt = invokeMethod InvokeStatic conversionClass "toInt" "(Ljava/lang/Object;)I" False

objToChar : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToChar = invokeMethod InvokeStatic conversionClass "toChar" "(Ljava/lang/Object;)C" False

objToBoolean : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToBoolean = invokeMethod InvokeStatic conversionClass "toBoolean" "(Ljava/lang/Object;)Z" False

objToByte : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToByte = invokeMethod InvokeStatic conversionClass "toByte" "(Ljava/lang/Object;)B" False

charObjToChar : {auto stateRef: Ref AsmState AsmState} -> Core ()
charObjToChar = do checkcast "java/lang/Character"; unboxChar

objToShort : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToShort = invokeMethod InvokeStatic conversionClass "toShort" "(Ljava/lang/Object;)S" False

objToLong : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToLong = invokeMethod InvokeStatic conversionClass "toLong" "(Ljava/lang/Object;)J" False

objToFloat : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToFloat = invokeMethod InvokeStatic conversionClass "toFloat" "(Ljava/lang/Object;)F" False

objToDouble : {auto stateRef: Ref AsmState AsmState} -> Core ()
objToDouble = invokeMethod InvokeStatic conversionClass "toDouble" "(Ljava/lang/Object;)D" False

export
asmCast : {auto stateRef: Ref AsmState AsmState} -> (sourceType: InferredType) -> (targetType: InferredType) -> Core ()

asmCast ty1@(IRef class1 _ _) ty2@(IRef class2 _ _) = when (class1 /= class2) (checkcast class2)

asmCast IUnknown ty@(IRef clazz _ _) = checkcast clazz

asmCast IBool IBool     = pure ()
asmCast IByte IByte     = pure ()
asmCast IChar IChar     = pure ()
asmCast IShort IShort   = pure ()
asmCast IInt IBool      = pure ()
asmCast IInt IInt       = pure ()
asmCast IChar IInt      = pure ()
asmCast IBool IInt      = boolToInt
asmCast IVoid IInt      = iconst 0 -- for primitive functions returning void, Idris return type will be int
asmCast ILong ILong     = pure ()
asmCast IFloat IFloat   = pure ()
asmCast IDouble IDouble = pure ()
asmCast (IArray _) (IArray _) = pure ()

asmCast IInt IChar = i2c
asmCast IInt IByte = i2b
asmCast IInt IShort = i2s
asmCast IFloat IDouble = f2d
asmCast IDouble IFloat = d2f

asmCast ty IBool = objToBoolean

asmCast ty IByte = objToByte

asmCast ty IChar = objToChar

asmCast ty IShort = objToShort

asmCast ty IInt = objToInt

asmCast ty ILong = objToLong

asmCast ty IFloat = objToFloat

asmCast ty IDouble = objToDouble

asmCast IBool ty = boxBool

asmCast IByte ty = boxByte

asmCast IChar ty = boxChar

asmCast IShort ty = boxShort

asmCast IInt ty =
    if ty == inferredBigIntegerType then do
        i2l
        invokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
    else boxInt

asmCast ILong ty = boxLong

asmCast IFloat ty = boxFloat

asmCast IDouble ty = boxDouble

asmCast (IRef _ _ _) arr@(IArray _) = checkcast $ getJvmTypeDescriptor arr
asmCast (IArray _) (IRef clazz _ _) = checkcast clazz

asmCast _ IVoid = pure ()
asmCast IVoid (IRef _ _ _) = aconstnull
asmCast IVoid IUnknown = aconstnull
asmCast ty IUnknown = pure ()

asmCast ty1 ty2 = throw $ GenericMsg emptyFC $ "Cannot convert from " ++ show ty1 ++ " to " ++ show ty2

loadAndBox : {auto stateRef: Ref AsmState AsmState} -> (Int -> Core ()) -> Core () -> Map Int InferredType
           -> Int -> Core ()
loadAndBox loadOp boxOp sourceLocTys var = let op = \index => do loadOp index; boxOp
                                           in opWithWordSize sourceLocTys op var

loadAndBoxBool : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndBoxBool ty = loadAndBox iload boxBool

loadAndBoxByte : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndBoxByte ty = loadAndBox iload boxByte

loadAndBoxChar : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndBoxChar ty = loadAndBox iload boxChar

loadAndBoxShort : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndBoxShort ty = loadAndBox iload boxShort

loadAndBoxInt : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndBoxInt ty = loadAndBox iload boxInt

loadAndBoxLong : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndBoxLong ty = loadAndBox lload boxLong

loadAndBoxFloat : {auto stateRef: Ref AsmState AsmState} -> Map Int InferredType -> Int -> Core ()
loadAndBoxFloat = loadAndBox fload boxFloat

loadAndBoxDouble : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndBoxDouble ty = loadAndBox dload boxDouble

loadAndUnboxBool : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndUnboxBool ty sourceLocTys var =
    let loadInstr = \index => do aload index; boolObjToBool
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxByte : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndUnboxByte ty sourceLocTys var =
    let loadInstr = \index => do aload index; objToByte
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxChar : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndUnboxChar ty sourceLocTys var =
    let loadInstr = \index => do aload index; objToChar
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxShort : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndUnboxShort ty sourceLocTys var =
    let loadInstr = \index => do aload index; objToShort
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxInt : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndUnboxInt ty sourceLocTys var =
    let loadInstr = \index => do aload index; objToInt
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxDouble : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Map Int InferredType -> Int -> Core ()
loadAndUnboxDouble ty sourceLocTys var =
    let loadInstr = \index => do aload index; objToDouble
    in opWithWordSize sourceLocTys loadInstr var

export
loadVar : {auto stateRef: Ref AsmState AsmState} -> Map Int InferredType -> (srcTy: InferredType) -> (targetTy: InferredType) -> Int -> Core ()
loadVar sourceLocTys IBool IBool var = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IByte IByte var = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IChar IChar var  = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IShort IShort var = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IBool IInt  var = opWithWordSize sourceLocTys (\var => do iload var; boolToInt) var
loadVar sourceLocTys IByte IInt  var = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IChar IInt  var = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IShort IInt  var = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IInt IInt  var = opWithWordSize sourceLocTys iload var
loadVar sourceLocTys IInt IChar  var = opWithWordSize sourceLocTys (\var => do iload var; i2c) var
loadVar sourceLocTys IInt IByte  var = opWithWordSize sourceLocTys (\var => do iload var; i2b) var
loadVar sourceLocTys IInt IShort  var = opWithWordSize sourceLocTys (\var => do iload var; i2s) var
loadVar sourceLocTys ILong ILong  var = opWithWordSize sourceLocTys lload var
loadVar sourceLocTys IFloat IFloat var = opWithWordSize sourceLocTys fload var
loadVar sourceLocTys IFloat IDouble var = opWithWordSize sourceLocTys (\var => do fload var; f2d) var
loadVar sourceLocTys IDouble IDouble var = opWithWordSize sourceLocTys dload var
loadVar sourceLocTys IDouble IFloat var = opWithWordSize sourceLocTys (\var => do dload var; d2f) var
loadVar sourceLocTys ty1@(IArray _) ty2@(IArray _) var = opWithWordSize sourceLocTys aload var

loadVar sourceLocTys IBool ty var = loadAndBoxBool ty sourceLocTys var
loadVar sourceLocTys IByte ty var = loadAndBoxByte ty sourceLocTys var
loadVar sourceLocTys IChar ty var = loadAndBoxChar ty sourceLocTys var
loadVar sourceLocTys IShort ty var  = loadAndBoxShort ty sourceLocTys var
loadVar sourceLocTys IInt ty var  = loadAndBoxInt ty sourceLocTys var

loadVar sourceLocTys ILong ty var = loadAndBoxLong ty sourceLocTys var

loadVar sourceLocTys IFloat _ var = loadAndBoxFloat sourceLocTys var

loadVar sourceLocTys IDouble ty var = loadAndBoxDouble ty sourceLocTys var

loadVar sourceLocTys ty IBool var = loadAndUnboxBool ty sourceLocTys var

loadVar sourceLocTys ty IByte var = loadAndUnboxByte ty sourceLocTys var

loadVar sourceLocTys ty IChar var = loadAndUnboxChar ty sourceLocTys var

loadVar sourceLocTys ty IShort var = loadAndUnboxShort ty sourceLocTys var

loadVar sourceLocTys ty IInt var = loadAndUnboxInt ty sourceLocTys var

loadVar sourceLocTys ty ILong var =
    let loadInstr = \index => do aload index; objToLong
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys (IRef "java/math/BigInteger" _ _) (IRef "java/math/BigInteger" _ _) var =
  opWithWordSize sourceLocTys aload var

loadVar sourceLocTys _ (IRef "java/math/BigInteger" _ _) var =
    let loadInstr = \index => do
      aload index
      invokeMethod InvokeStatic conversionClass "toInteger" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys ty IFloat var =
    let loadInstr = \index => do aload index; objToFloat
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys ty IDouble var = loadAndUnboxDouble ty sourceLocTys var

loadVar sourceLocTys _ arr@(IArray _) var =
    let loadInstr = \index => do aload index; checkcast $ getJvmTypeDescriptor arr
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown ty2@(IRef _ _ _) var =
    let loadInstr = \index => do aload index; asmCast IUnknown ty2
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys (IArray _) (IRef _ _ _) var = opWithWordSize sourceLocTys aload var
loadVar sourceLocTys (IArray _) IUnknown var = opWithWordSize sourceLocTys aload var

loadVar sourceLocTys (IRef _ _ _) IUnknown var = opWithWordSize sourceLocTys aload var
loadVar sourceLocTys IUnknown IUnknown var = opWithWordSize sourceLocTys aload var

loadVar sourceLocTys ty1@(IRef _ _ _) ty2@(IRef _ _ _) var =
    let loadInstr = \index => do aload index; asmCast ty1 ty2
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys ty1 ty2 var = throw $ GenericMsg emptyFC ("Cannot load variable " ++ show var ++ " of type " ++ show ty1 ++
    " to type " ++ show ty2)

storeVarWithWordSize : {auto stateRef: Ref AsmState AsmState} -> (Int -> Core ()) -> Int -> Core ()
storeVarWithWordSize storeOp var = opWithWordSize !getVariableTypes storeOp var

boxStore : {auto stateRef: Ref AsmState AsmState} -> Core () -> Int -> Core ()
boxStore boxOp var = storeVarWithWordSize (\index => do boxOp; astore index) var

export
storeVar : {auto stateRef: Ref AsmState AsmState} -> (srcTy: InferredType) -> (targetTy: InferredType) -> Int -> Core ()
storeVar IBool IBool     var = do types <- getVariableTypes; opWithWordSize types istore var
storeVar IByte IByte     var = do types <- getVariableTypes; opWithWordSize types istore var
storeVar IChar IChar     var = do types <- getVariableTypes; opWithWordSize types istore var
storeVar IShort IShort   var = do types <- getVariableTypes; opWithWordSize types istore var
storeVar IInt IInt       var = do types <- getVariableTypes; opWithWordSize types istore var
storeVar ILong ILong     var = do types <- getVariableTypes; opWithWordSize types lstore var
storeVar IFloat IFloat   var = do types <- getVariableTypes; opWithWordSize types fstore var
storeVar IDouble IDouble var = do types <- getVariableTypes; opWithWordSize types dstore var
storeVar (IArray _) (IArray _) var = do types <- getVariableTypes; opWithWordSize types astore var

storeVar IBool ty var = boxStore boxBool var
storeVar IByte ty var = boxStore boxByte var
storeVar IChar ty var = boxStore boxChar var
storeVar IShort ty var = boxStore boxShort var
storeVar IInt ty var = boxStore boxInt var
storeVar ILong ty var = boxStore boxLong var
storeVar IFloat ty var = boxStore boxFloat var
storeVar IDouble ty var = boxStore boxDouble var

storeVar ty IBool var = storeVarWithWordSize (\index => do asmCast ty IBool; istore index) var

storeVar ty IByte var = storeVarWithWordSize (\index => do asmCast ty IByte; istore index) var

storeVar ty IChar var = storeVarWithWordSize (\index => do asmCast ty IChar; istore index) var

storeVar ty IShort var = storeVarWithWordSize (\index => do asmCast ty IShort; istore index) var

storeVar ty IInt var = storeVarWithWordSize (\index => do asmCast ty IInt; istore index) var

storeVar ty ILong var = storeVarWithWordSize (\index => do asmCast ty ILong; lstore index) var

storeVar ty IFloat var = storeVarWithWordSize (\index => do asmCast ty IFloat; fstore index) var

storeVar ty IDouble var = storeVarWithWordSize (\index => do asmCast ty IDouble; dstore index) var

storeVar ty arr@(IArray elemTy) var =
    storeVarWithWordSize (\index => do checkcast $ getJvmTypeDescriptor arr; astore index) var

storeVar ty targetTy@(IRef _ _ _) var = do
    types <- getVariableTypes
    asmCast ty targetTy
    opWithWordSize types astore var

storeVar _ _ var = opWithWordSize !getVariableTypes astore var
