module Compiler.Jvm.Variable

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

opWithWordSize : Map Int InferredType -> (Int -> Asm ()) -> Int -> Asm ()
opWithWordSize types op var = do
    newPos <- LiftIo $ getVarIndex types var
    op newPos

boxDouble : Asm ()
boxDouble = InvokeMethod InvokeStatic "java/lang/Double" "valueOf" "(D)Ljava/lang/Double;" False

boxFloat : Asm ()
boxFloat = InvokeMethod InvokeStatic "java/lang/Float" "valueOf" "(F)Ljava/lang/Float;" False

boxBool : Asm ()
boxBool = InvokeMethod InvokeStatic "java/lang/Boolean" "valueOf" "(Z)Ljava/lang/Boolean;" False

boxByte : Asm ()
boxByte = InvokeMethod InvokeStatic "java/lang/Byte" "valueOf" "(B)Ljava/lang/Byte;" False

boxChar : Asm ()
boxChar = InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

boxInt : Asm ()
boxInt = InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False

boxShort : Asm ()
boxShort = InvokeMethod InvokeStatic "java/lang/Short" "valueOf" "(S)Ljava/lang/Short;" False

boxLong : Asm ()
boxLong = InvokeMethod InvokeStatic "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;" False

unboxBool : Asm ()
unboxBool = InvokeMethod InvokeVirtual "java/lang/Boolean" "booleanValue" "()Z" False

unboxByte : Asm ()
unboxByte = InvokeMethod InvokeVirtual "java/lang/Byte" "byteValue" "()B" False

unboxInt : Asm ()
unboxInt = InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False

unboxChar : Asm ()
unboxChar = InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False

unboxShort : Asm ()
unboxShort = InvokeMethod InvokeVirtual "java/lang/Short" "shortValue" "()S" False

unboxLong : Asm ()
unboxLong = InvokeMethod InvokeVirtual "java/lang/Long" "longValue" "()J" False

unboxDouble : Asm ()
unboxDouble = InvokeMethod InvokeVirtual "java/lang/Double" "doubleValue" "()D" False

unboxFloat : Asm ()
unboxFloat = InvokeMethod InvokeVirtual "java/lang/Float" "floatValue" "()F" False

%inline
export
conversionClass : String
conversionClass = "io/github/mmhelloworld/idrisjvm/runtime/Conversion"

boolObjToBool : Asm ()
boolObjToBool = InvokeMethod InvokeStatic conversionClass "toBoolean" "(Ljava/lang/Object;)Z" False

boolToInt : Asm ()
boolToInt = InvokeMethod InvokeStatic conversionClass "boolToInt1" "(Z)I" False

objToInt : Asm ()
objToInt = InvokeMethod InvokeStatic conversionClass "toInt" "(Ljava/lang/Object;)I" False

objToChar : Asm ()
objToChar = InvokeMethod InvokeStatic conversionClass "toChar" "(Ljava/lang/Object;)C" False

objToBoolean : Asm ()
objToBoolean = InvokeMethod InvokeStatic conversionClass "toBoolean" "(Ljava/lang/Object;)Z" False

objToByte : Asm ()
objToByte = InvokeMethod InvokeStatic conversionClass "toByte" "(Ljava/lang/Object;)B" False

charObjToChar : Asm ()
charObjToChar = do Checkcast "java/lang/Character"; unboxChar

objToShort : Asm ()
objToShort = InvokeMethod InvokeStatic conversionClass "toShort" "(Ljava/lang/Object;)S" False

objToLong : Asm ()
objToLong = InvokeMethod InvokeStatic conversionClass "toLong" "(Ljava/lang/Object;)J" False

objToFloat : Asm ()
objToFloat = InvokeMethod InvokeStatic conversionClass "toFloat" "(Ljava/lang/Object;)F" False

objToDouble : Asm ()
objToDouble = InvokeMethod InvokeStatic conversionClass "toDouble" "(Ljava/lang/Object;)D" False

export
checkcast : String -> Asm ()
checkcast "java/lang/Object" = pure ()
checkcast cname              = Checkcast cname

export
asmCast : (sourceType: InferredType) -> (targetType: InferredType) -> Asm ()

asmCast ty1@(IRef class1 _ _) ty2@(IRef class2 _ _) = when (class1 /= class2) (checkcast class2)

asmCast IUnknown ty@(IRef clazz _ _) = checkcast clazz

asmCast IBool IBool     = Pure ()
asmCast IByte IByte     = Pure ()
asmCast IChar IChar     = Pure ()
asmCast IShort IShort   = Pure ()
asmCast IInt IBool      = Pure ()
asmCast IInt IInt       = Pure ()
asmCast ILong ILong     = Pure ()
asmCast IFloat IFloat   = Pure ()
asmCast IDouble IDouble = Pure ()
asmCast (IArray _) (IArray _) = Pure ()

asmCast IBool IInt = boolToInt
asmCast IInt IChar = I2c
asmCast IInt IByte = I2b
asmCast IInt IShort = I2s
asmCast IFloat IDouble = F2d
asmCast IDouble IFloat = D2f

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
        I2l
        InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
    else boxInt

asmCast ILong ty = boxLong

asmCast IFloat ty = boxFloat

asmCast IDouble ty = boxDouble

asmCast (IRef _ _ _) arr@(IArray _) = Checkcast $ getJvmTypeDescriptor arr
asmCast (IArray _) (IRef clazz _ _) = Checkcast clazz

asmCast _ IVoid = Pure ()
asmCast IVoid IVoid = Pure ()
asmCast IVoid (IRef _ _ _) = Aconstnull
asmCast IVoid IUnknown = Aconstnull
asmCast ty IUnknown = Pure ()

asmCast ty1 ty2 = Throw emptyFC $ "Cannot convert from " ++ show ty1 ++ " to " ++ show ty2

loadAndBox : (Int -> Asm ()) -> Asm () -> Map Int InferredType -> Int -> Asm ()
loadAndBox loadOp boxOp sourceLocTys var = let op = \index => do loadOp index; boxOp
                                           in opWithWordSize sourceLocTys op var

loadAndBoxBool : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndBoxBool ty = loadAndBox Iload boxBool

loadAndBoxByte : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndBoxByte ty = loadAndBox Iload boxByte

loadAndBoxChar : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndBoxChar ty = loadAndBox Iload boxChar

loadAndBoxShort : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndBoxShort ty = loadAndBox Iload boxShort

loadAndBoxInt : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndBoxInt ty = loadAndBox Iload boxInt

loadAndBoxLong : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndBoxLong ty = loadAndBox Lload boxLong

loadAndBoxFloat : Map Int InferredType -> Int -> Asm ()
loadAndBoxFloat = loadAndBox Fload boxFloat

loadAndBoxDouble : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndBoxDouble ty = loadAndBox Dload boxDouble

loadAndUnboxBool : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndUnboxBool ty sourceLocTys var =
    let loadInstr = \index => do Aload index; boolObjToBool
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxByte : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndUnboxByte ty sourceLocTys var =
    let loadInstr = \index => do Aload index; objToByte
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxChar : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndUnboxChar ty sourceLocTys var =
    let loadInstr = \index => do Aload index; objToChar
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxShort : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndUnboxShort ty sourceLocTys var =
    let loadInstr = \index => do Aload index; objToShort
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxInt : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndUnboxInt ty sourceLocTys var =
    let loadInstr = \index => do Aload index; objToInt
    in opWithWordSize sourceLocTys loadInstr var

loadAndUnboxDouble : InferredType -> Map Int InferredType -> Int -> Asm ()
loadAndUnboxDouble ty sourceLocTys var =
    let loadInstr = \index => do Aload index; objToDouble
    in opWithWordSize sourceLocTys loadInstr var

export
loadVar : Map Int InferredType -> (srcTy: InferredType) -> (targetTy: InferredType) -> Int -> Asm ()
loadVar sourceLocTys IBool IBool var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IByte IByte var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IChar IChar var  = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IShort IShort var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IBool IInt  var = opWithWordSize sourceLocTys (\var => do Iload var; boolToInt) var
loadVar sourceLocTys IByte IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IChar IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IShort IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IInt IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IInt IChar  var = opWithWordSize sourceLocTys (\var => do Iload var; I2c) var
loadVar sourceLocTys IInt IByte  var = opWithWordSize sourceLocTys (\var => do Iload var; I2b) var
loadVar sourceLocTys IInt IShort  var = opWithWordSize sourceLocTys (\var => do Iload var; I2s) var
loadVar sourceLocTys ILong ILong  var = opWithWordSize sourceLocTys Lload var
loadVar sourceLocTys IFloat IFloat var = opWithWordSize sourceLocTys Fload var
loadVar sourceLocTys IFloat IDouble var = opWithWordSize sourceLocTys (\var => do Fload var; F2d) var
loadVar sourceLocTys IDouble IDouble var = opWithWordSize sourceLocTys Dload var
loadVar sourceLocTys IDouble IFloat var = opWithWordSize sourceLocTys (\var => do Dload var; D2f) var
loadVar sourceLocTys ty1@(IArray _) ty2@(IArray _) var = opWithWordSize sourceLocTys Aload var

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
    let loadInstr = \index => do Aload index; objToLong
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys _ (IRef "java/math/BigInteger" _ _) var =
    let loadInstr = \index => do
      Aload index
      InvokeMethod InvokeStatic conversionClass "toInteger" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys ty IFloat var =
    let loadInstr = \index => do Aload index; objToFloat
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys ty IDouble var = loadAndUnboxDouble ty sourceLocTys var

loadVar sourceLocTys _ arr@(IArray _) var =
    let loadInstr = \index => do Aload index; checkcast $ getJvmTypeDescriptor arr
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown ty2@(IRef _ _ _) var =
    let loadInstr = \index => do Aload index; asmCast IUnknown ty2
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys (IArray _) (IRef _ _ _) var = opWithWordSize sourceLocTys Aload var
loadVar sourceLocTys (IArray _) IUnknown var = opWithWordSize sourceLocTys Aload var

loadVar sourceLocTys (IRef _ _ _) IUnknown var = opWithWordSize sourceLocTys Aload var
loadVar sourceLocTys IUnknown IUnknown var = opWithWordSize sourceLocTys Aload var

loadVar sourceLocTys ty1@(IRef _ _ _) ty2@(IRef _ _ _) var =
    let loadInstr = \index => do Aload index; asmCast ty1 ty2
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys ty1 ty2 var = Throw emptyFC ("Cannot load variable " ++ show var ++ " of type " ++ show ty1 ++
    " to type " ++ show ty2)

storeVarWithWordSize : (Int -> Asm ()) -> Int -> Asm ()
storeVarWithWordSize storeOp var = opWithWordSize !getVariableTypes storeOp var

boxStore : Asm () -> Int -> Asm ()
boxStore boxOp var = storeVarWithWordSize (\index => do boxOp; Astore index) var

export
storeVar : (srcTy: InferredType) -> (targetTy: InferredType) -> Int -> Asm ()
storeVar IBool IBool     var = do types <- getVariableTypes; opWithWordSize types Istore var
storeVar IByte IByte     var = do types <- getVariableTypes; opWithWordSize types Istore var
storeVar IChar IChar     var = do types <- getVariableTypes; opWithWordSize types Istore var
storeVar IShort IShort   var = do types <- getVariableTypes; opWithWordSize types Istore var
storeVar IInt IInt       var = do types <- getVariableTypes; opWithWordSize types Istore var
storeVar ILong ILong     var = do types <- getVariableTypes; opWithWordSize types Lstore var
storeVar IFloat IFloat   var = do types <- getVariableTypes; opWithWordSize types Fstore var
storeVar IDouble IDouble var = do types <- getVariableTypes; opWithWordSize types Dstore var
storeVar (IArray _) (IArray _) var = do types <- getVariableTypes; opWithWordSize types Astore var

storeVar IBool ty var = boxStore boxBool var
storeVar IByte ty var = boxStore boxByte var
storeVar IChar ty var = boxStore boxChar var
storeVar IShort ty var = boxStore boxShort var
storeVar IInt ty var = boxStore boxInt var
storeVar ILong ty var = boxStore boxLong var
storeVar IFloat ty var = boxStore boxFloat var
storeVar IDouble ty var = boxStore boxDouble var

storeVar ty IBool var = storeVarWithWordSize (\index => do asmCast ty IBool; Istore index) var

storeVar ty IByte var = storeVarWithWordSize (\index => do asmCast ty IByte; Istore index) var

storeVar ty IChar var = storeVarWithWordSize (\index => do asmCast ty IChar; Istore index) var

storeVar ty IShort var = storeVarWithWordSize (\index => do asmCast ty IShort; Istore index) var

storeVar ty IInt var = storeVarWithWordSize (\index => do asmCast ty IInt; Istore index) var

storeVar ty ILong var = storeVarWithWordSize (\index => do asmCast ty ILong; Lstore index) var

storeVar ty IFloat var = storeVarWithWordSize (\index => do asmCast ty IFloat; Fstore index) var

storeVar ty IDouble var = storeVarWithWordSize (\index => do asmCast ty IDouble; Dstore index) var

storeVar ty arr@(IArray elemTy) var =
    storeVarWithWordSize (\index => do checkcast $ getJvmTypeDescriptor arr; Astore index) var

storeVar ty targetTy@(IRef _ _ _) var = do
    types <- getVariableTypes
    asmCast ty targetTy
    opWithWordSize types Astore var

storeVar _ _ var = opWithWordSize !getVariableTypes Astore var
