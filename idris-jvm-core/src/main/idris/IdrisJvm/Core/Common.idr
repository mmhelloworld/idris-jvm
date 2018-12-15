module IdrisJvm.Core.Common

import Data.SortedMap
import IdrisJvm.Core.Asm
import IdrisJvm.IR.Types
import IdrisJvm.IO
import Java.Lang

%access public export

jtrace : Show a => a -> b -> b
jtrace x val = unsafePerformIO {ffi=FFI_JVM} (do printLn x; pure val)

jerror : String -> a
jerror msg = believe_me . unsafePerformIO $ invokeStatic RuntimeClass "error" (Object -> JVM_IO Object) (believe_me msg)

sep : String -> List String -> String
sep x xs = cast $ intercalate (cast x) $ map cast xs

IdrisToJavaNameConverterClass : JVM_NativeTy
IdrisToJavaNameConverterClass = Class "IdrisJvm/Core/IdrisToJavaNameConverter"

jname : String -> JMethodName
jname s =
  let [cname, mname] = split (== ',') . unsafePerformIO $ invokeStatic IdrisToJavaNameConverterClass "idrisClassMethodName" (String -> JVM_IO String) s
  in MkJMethodName cname mname

locIndex : LVar -> Int
locIndex (Loc i) = i
locIndex _       = jerror "Unexpected global variable"

classSig : String -> String
classSig className = "L" ++ className ++ ";"

rtFuncSig : String
rtFuncSig = classSig $ rtClass "Function"

rtThunkSig : String
rtThunkSig = classSig thunkClass

utilClass : String
utilClass = rtClass "Util"

intRange : Int -> Int -> List Int
intRange from to = if from < to then [from .. to] else []

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

repeatString : Nat -> String -> String
repeatString n s = concat (replicate n s)

repeatObjectDesc : Nat -> String
repeatObjectDesc n = repeatString n "Ljava/lang/Object;"

sig : Nat -> String
sig nArgs = "(" ++ repeatObjectDesc nArgs ++  ")Ljava/lang/Object;"

metafactoryDesc : Descriptor
metafactoryDesc =
  concat [ "("
         , "Ljava/lang/invoke/MethodHandles$Lookup;"
         , "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodHandle;"
         , "Ljava/lang/invoke/MethodType;"
         , ")"
         , "Ljava/lang/invoke/CallSite;"
         ]

unrolledConstructorPropsCount : Nat
unrolledConstructorPropsCount = 10

inferredTypeToFieldTypeDesc : InferredType -> FieldTypeDescriptor
inferredTypeToFieldTypeDesc IBool = FieldTyDescBoolean
inferredTypeToFieldTypeDesc IByte = FieldTyDescByte
inferredTypeToFieldTypeDesc IChar = FieldTyDescChar
inferredTypeToFieldTypeDesc IShort = FieldTyDescShort
inferredTypeToFieldTypeDesc IInt = FieldTyDescInt
inferredTypeToFieldTypeDesc ILong = FieldTyDescLong
inferredTypeToFieldTypeDesc IFloat = FieldTyDescFloat
inferredTypeToFieldTypeDesc IDouble = FieldTyDescDouble
inferredTypeToFieldTypeDesc (Ref refTy) = FieldTyDescReference (ClassDesc refTy)
inferredTypeToFieldTypeDesc IUnknown = FieldTyDescReference (ClassDesc "java/lang/Object")
inferredTypeToFieldTypeDesc (IArray elemTy) = FieldTyDescReference (ArrayDesc $ inferredTypeToFieldTypeDesc elemTy)

getInferredTyDesc : InferredType -> String
getInferredTyDesc = asmFieldTypeDesc . inferredTypeToFieldTypeDesc

toFrameLocalVarType : InferredType -> String
toFrameLocalVarType IBool = "INTEGER"
toFrameLocalVarType IByte = "INTEGER"
toFrameLocalVarType IChar = "INTEGER"
toFrameLocalVarType IShort = "INTEGER"
toFrameLocalVarType IInt = "INTEGER"
toFrameLocalVarType ILong = "LONG"
toFrameLocalVarType IFloat = "FLOAT"
toFrameLocalVarType IDouble = "DOUBLE"
toFrameLocalVarType (Ref clsName) = clsName
toFrameLocalVarType (IArray elemTy) = "[" ++ getInferredTyDesc elemTy
toFrameLocalVarType _ = "java/lang/Object"

toFrameLocalVarTypes : InferredTypeStore -> List String
toFrameLocalVarTypes types = toFrameLocalVarType <$> (values types)

fullFrame : Asm ()
fullFrame = do
  nlocalVars <- GetLocalVarCount
  locTypes <- GetFunctionLocTypes
  let frameLocTypes = toFrameLocalVarTypes locTypes
  Frame FFull nlocalVars frameLocTypes 0 []

addFrame : Asm ()
addFrame = do
  needFrame <- ShouldDescribeFrame
  if needFrame
    then do
      fullFrame
      UpdateShouldDescribeFrame False
    else Frame FSame 0 [] 0 []

defaultConstructor : ClassName -> ClassName -> Asm ()
defaultConstructor cname parent = do
  CreateMethod [Public] cname "<init>" "()V" Nothing Nothing [] []
  MethodCodeStart
  Aload 0
  InvokeMethod InvokeSpecial parent "<init>" "()V" False
  Return
  MaxStackAndLocal (-1) (-1) -- Let the asm calculate
  MethodCodeEnd

invokeError : String -> Asm ()
invokeError x = do
  Ldc $ StringConst x
  InvokeMethod InvokeStatic (rtClass "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False

getPrimitiveClass : String -> Asm ()
getPrimitiveClass clazz = Field FGetStatic clazz "TYPE" "Ljava/lang/Class;"

getInferredFunDesc : List InferredType -> InferredType -> String
getInferredFunDesc [] retTy = "()" ++ getInferredTyDesc retTy
getInferredFunDesc argTypes retTy =
    let argDescs = getInferredTyDesc <$> argTypes
        retTyDesc = getInferredTyDesc retTy
    in "(" ++ (the String $ concat argDescs) ++ ")" ++ retTyDesc

box : InferredType -> Asm ()
box IBool   = boxInt
box IByte   = boxByte
box IChar   = boxChar
box IShort  = boxShort
box IInt    = boxInt
box ILong   = boxLong
box IFloat  = boxFloat
box IDouble = boxDouble
box _       = pure ()

getVarIndex : InferredTypeStore -> LVar -> Int
getVarIndex types (Loc index) = go 0 0 where
  go : Int -> Int -> Int
  go pos currVarIndex =
    if currVarIndex == index then
        pos
    else
        let currVarTy = getLocTy types (Loc currVarIndex)
            isTwoWordTy = currVarTy == IDouble || currVarTy == ILong
            nextPos = if isTwoWordTy then pos + 2 else succ pos
        in go nextPos (succ currVarIndex)

opWithWordSize : InferredTypeStore -> (Int -> Asm ()) -> LVar -> Asm ()
opWithWordSize types op var = do
    let newPos = getVarIndex types var
    op newPos

boolObjToBool : Asm ()
boolObjToBool = InvokeMethod InvokeStatic utilClass "toBoolean" "(Ljava/lang/Object;)Z" False

boolToInt : Asm ()
boolToInt = InvokeMethod InvokeStatic utilClass "boolToInt" "(Z)I" False

objToInt : Asm ()
objToInt = InvokeMethod InvokeStatic utilClass "toInt" "(Ljava/lang/Object;)I" False

objToByte : Asm ()
objToByte = InvokeMethod InvokeStatic utilClass "toByte" "(Ljava/lang/Object;)B" False

charObjToChar : Asm ()
charObjToChar = do Checkcast "java/lang/Character"; unboxChar

objToShort : Asm ()
objToShort = InvokeMethod InvokeStatic utilClass "toShort" "(Ljava/lang/Object;)S" False

longObjToLong : Asm ()
longObjToLong = do Checkcast "java/lang/Long"; unboxLong

objToFloat : Asm ()
objToFloat = InvokeMethod InvokeStatic utilClass "toFloat" "(Ljava/lang/Object;)F" False

objToDouble : Asm ()
objToDouble = InvokeMethod InvokeStatic utilClass "toDouble" "(Ljava/lang/Object;)D" False

checkcast : String -> Asm ()
checkcast "java/lang/Object" = pure ()
checkcast cname              = Checkcast cname

cgCast : InferredType -> InferredType -> Asm ()
cgCast IUnknown IBool = boolObjToBool
cgCast (Ref _) IBool = boolObjToBool

cgCast IUnknown IByte = objToByte
cgCast (Ref _) IByte = objToByte

cgCast IUnknown IChar = charObjToChar
cgCast (Ref _) IChar = charObjToChar

cgCast IUnknown IShort = objToShort
cgCast (Ref _) IShort = objToShort

cgCast IUnknown IInt = objToInt
cgCast (Ref _) IInt = objToInt

cgCast IUnknown ILong = longObjToLong
cgCast (Ref _) ILong = longObjToLong

cgCast IUnknown IFloat = objToFloat
cgCast (Ref _) IFloat = objToFloat

cgCast IUnknown IDouble = objToDouble
cgCast (Ref _) IDouble = objToDouble

cgCast IBool (Ref _) = boxBool
cgCast IBool IUnknown = boxBool

cgCast IByte (Ref _) = boxByte
cgCast IByte IUnknown = boxByte

cgCast IChar (Ref _) = boxChar
cgCast IChar IUnknown = boxChar

cgCast IShort (Ref _) = boxShort
cgCast IShort IUnknown = boxShort

cgCast IInt (Ref _) = boxInt
cgCast IInt IUnknown = boxInt

cgCast ILong (Ref _) = boxLong
cgCast ILong IUnknown = boxLong

cgCast IFloat (Ref _) = boxFloat
cgCast IFloat IUnknown = boxFloat

cgCast IDouble (Ref _) = boxDouble
cgCast IDouble IUnknown = boxDouble

cgCast IInt IByte = I2b
cgCast IInt IShort = I2s
cgCast IFloat IDouble = F2d
cgCast IDouble IFloat = D2f

cgCast IBool IInt = boolToInt

cgCast (Ref _) arr@(IArray _) = Checkcast $ getInferredTyDesc arr
cgCast IUnknown arr@(IArray _) = Checkcast $ getInferredTyDesc arr

cgCast IUnknown (Ref ty) = checkcast ty

cgCast (Ref ty1) (Ref ty2) = when (ty1 /= ty2) $ checkcast ty2

cgCast _ _ = pure ()

loadVar : InferredTypeStore -> (srcTy: InferredType) -> (targetTy: InferredType) -> LVar -> Asm ()
loadVar sourceLocTys IBool IBool var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IByte IByte var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IChar IChar var  = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IShort IShort var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IByte IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IChar IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IShort IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IInt IInt  var = opWithWordSize sourceLocTys Iload var
loadVar sourceLocTys IInt IByte  var = opWithWordSize sourceLocTys (\var => do Iload var; I2b) var
loadVar sourceLocTys IInt IShort  var = opWithWordSize sourceLocTys (\var => do Iload var; I2s) var
loadVar sourceLocTys ILong ILong  var = opWithWordSize sourceLocTys Lload var
loadVar sourceLocTys IFloat IFloat var = opWithWordSize sourceLocTys Fload var
loadVar sourceLocTys IFloat IDouble var = opWithWordSize sourceLocTys (\var => do Fload var; F2d) var
loadVar sourceLocTys IDouble IDouble var = opWithWordSize sourceLocTys Dload var
loadVar sourceLocTys IDouble IFloat var = opWithWordSize sourceLocTys (\var => do Dload var; D2f)  var

loadVar sourceLocTys IBool IUnknown  var =
    let loadInstr = \index => do Iload index; boxBool
    in opWithWordSize sourceLocTys loadInstr var 
loadVar sourceLocTys IBool (Ref _) var =
    let loadInstr = \index => do Iload index; boxBool
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IByte IUnknown  var =
    let loadInstr = \index => do Iload index; boxByte
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys IByte (Ref _) var =
    let loadInstr = \index => do Iload index; boxByte
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IChar IUnknown var =
    let loadInstr = \index => do Iload index; boxChar
    in opWithWordSize sourceLocTys loadInstr var
    
loadVar sourceLocTys IChar (Ref _) var =
    let loadInstr = \index => do Iload index; boxChar
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IShort IUnknown var  =
    let loadInstr = \index => do Iload index; boxShort
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys IShort (Ref _) var =
    let loadInstr = \index => do Iload index; boxShort
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IInt IUnknown var  =
    let loadInstr = \index => do Iload index; boxInt
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys IInt (Ref _) var =
    let loadInstr = \index => do Iload index; boxInt
    in opWithWordSize sourceLocTys loadInstr var
    
loadVar sourceLocTys ILong IUnknown var =
    let loadInstr = \index => do Lload index; boxLong
    in opWithWordSize sourceLocTys loadInstr var
    
loadVar sourceLocTys ILong (Ref _) var =
    let loadInstr = \index => do Lload index; boxLong
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IFloat IUnknown var =
    let loadInstr = \index => do Fload index; boxFloat
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys IFloat (Ref _) var =
    let loadInstr = \index => do Fload index; boxFloat
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IDouble IUnknown var =
    let loadInstr = \index => do Dload index; boxDouble
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys IDouble (Ref _) var =
    let loadInstr = \index => do Dload index; boxDouble
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown IBool var =
    let loadInstr = \index => do Aload index; boolObjToBool
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) IBool var =
    let loadInstr = \index => do Aload index; boolObjToBool
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown IByte var =
    let loadInstr = \index => do Aload index; objToByte
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) IByte var =
    let loadInstr = \index => do Aload index; objToByte
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown IChar var =
    let loadInstr = \index => do Aload index; charObjToChar
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) IChar var =
    let loadInstr = \index => do Aload index; charObjToChar
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown IShort var =
    let loadInstr = \index => do Aload index; objToShort
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) IShort var =
    let loadInstr = \index => do Aload index; objToShort
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown IInt var =
    let loadInstr = \index => do Aload index; objToInt
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IBool IInt var =
    let loadInstr = \index => do Iload index; boolToInt
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys (Ref _) IInt var =
    let loadInstr = \index => do Aload index; objToInt
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown ILong var = 
    let loadInstr = \index => do Aload index; longObjToLong
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) ILong var = 
    let loadInstr = \index => do Aload index; longObjToLong
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown IFloat var =
    let loadInstr = \index => do Aload index; objToFloat
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) IFloat var =
    let loadInstr = \index => do Aload index; objToFloat
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown IDouble var = 
    let loadInstr = \index => do Aload index; objToDouble
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) IDouble var = 
    let loadInstr = \index => do Aload index; objToDouble
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys IUnknown arr@(IArray _) var =
    let loadInstr = \index => do Aload index; Checkcast $ getInferredTyDesc arr
    in opWithWordSize sourceLocTys loadInstr var
loadVar sourceLocTys (Ref _) arr@(IArray _) var =
    let loadInstr = \index => do Aload index; Checkcast $ getInferredTyDesc arr
    in opWithWordSize sourceLocTys loadInstr var 

loadVar sourceLocTys IUnknown (Ref ty2) var = 
    let loadInstr = \index => do Aload index; checkcast ty2
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys (Ref ty1) (Ref ty2) var = 
    let loadInstr = \index => do Aload index; when (ty1 /= ty2) $ checkcast ty2
    in opWithWordSize sourceLocTys loadInstr var

loadVar sourceLocTys _ _ var = opWithWordSize sourceLocTys Aload var

loadVars : InferredTypeStore -> InferredTypeStore -> List LVar -> Asm ()
loadVars _ _ []  = pure ()
loadVars sourceLocTys targetLocTys vars  = do
    let nVars = the Int $ cast $ length vars
    let targetVarTys = if nVars > 0 then getLocTy targetLocTys <$> (Loc <$> [0 .. pred nVars]) else []
    let sourceVarTys = getLocTy sourceLocTys <$> vars
    let varsWithSourceTargetTys = List.zip vars $ List.zip sourceVarTys targetVarTys
    go varsWithSourceTargetTys
  where
    go : List (LVar, InferredType, InferredType) -> Asm ()
    go [] = pure ()
    go ((var, srcTy, targetTy) :: vs) = do
      loadVar sourceLocTys srcTy targetTy var
      go vs

storeVarWithWordSize : (Int -> Asm ()) -> LVar -> Asm ()
storeVarWithWordSize storeOp var = do
    types <- GetFunctionLocTypes
    opWithWordSize types storeOp var

boxStore : Asm () -> LVar -> Asm ()
boxStore boxOp var = storeVarWithWordSize (\index => do boxOp; Astore index) var

storeVar : (srcTy: InferredType) -> (targetTy: InferredType) -> LVar -> Asm ()
storeVar IBool IBool     var = do types <- GetFunctionLocTypes; opWithWordSize types Istore var
storeVar IByte IByte     var = do types <- GetFunctionLocTypes; opWithWordSize types Istore var
storeVar IChar IChar     var = do types <- GetFunctionLocTypes; opWithWordSize types Istore var
storeVar IShort IShort   var = do types <- GetFunctionLocTypes; opWithWordSize types Istore var
storeVar IInt IInt       var = do types <- GetFunctionLocTypes; opWithWordSize types Istore var
storeVar ILong ILong     var = do types <- GetFunctionLocTypes; opWithWordSize types Lstore var
storeVar IFloat IFloat   var = do types <- GetFunctionLocTypes; opWithWordSize types Fstore var
storeVar IDouble IDouble var = do types <- GetFunctionLocTypes; opWithWordSize types Dstore var

storeVar IBool IUnknown var = boxStore boxBool var
storeVar IBool (Ref _) var = boxStore boxBool var

storeVar IByte IUnknown var = boxStore boxByte var
storeVar IByte (Ref _) var = boxStore boxByte var

storeVar IChar IUnknown var = boxStore boxChar var
storeVar IChar (Ref _) var = boxStore boxChar var

storeVar IShort IUnknown var = boxStore boxShort var
storeVar IShort (Ref _) var = boxStore boxShort var

storeVar IInt IUnknown var = boxStore boxInt var
storeVar IInt (Ref _) var = boxStore boxInt var

storeVar ILong IUnknown var = boxStore boxLong var
storeVar ILong (Ref _) var = boxStore boxLong var

storeVar IFloat IUnknown var = boxStore boxFloat var
storeVar IFloat (Ref _) var = boxStore boxFloat var

storeVar IDouble IUnknown var = boxStore boxDouble var
storeVar IDouble (Ref _) var = boxStore boxDouble var

storeVar IUnknown IBool var = storeVarWithWordSize (\index => do cgCast IUnknown IBool; Istore index) var
storeVar srcTy@(Ref _) IBool var = storeVarWithWordSize (\index => do cgCast srcTy IBool; Istore index) var

storeVar IUnknown IByte var = storeVarWithWordSize (\index => do cgCast IUnknown IByte; Istore index) var
storeVar srcTy@(Ref _) IByte var = storeVarWithWordSize (\index => do cgCast srcTy IByte; Istore index) var

storeVar IUnknown IChar var = storeVarWithWordSize (\index => do cgCast IUnknown IChar; Istore index) var
storeVar srcTy@(Ref _) IChar var = storeVarWithWordSize (\index => do cgCast srcTy IChar; Istore index) var

storeVar IUnknown IShort var = storeVarWithWordSize (\index => do cgCast IUnknown IShort; Istore index) var
storeVar srcTy@(Ref _) IShort var = storeVarWithWordSize (\index => do cgCast srcTy IShort; Istore index) var

storeVar IUnknown IInt var = storeVarWithWordSize (\index => do cgCast IUnknown IInt; Istore index) var
storeVar srcTy@(Ref _) IInt var = storeVarWithWordSize (\index => do cgCast srcTy IInt; Istore index) var

storeVar IUnknown ILong var = storeVarWithWordSize (\index => do cgCast IUnknown ILong; Lstore index) var
storeVar srcTy@(Ref _) ILong var = storeVarWithWordSize (\index => do cgCast srcTy ILong; Lstore index) var

storeVar IUnknown IFloat var = storeVarWithWordSize (\index => do cgCast IUnknown IFloat; Fstore index) var
storeVar srcTy@(Ref _) IFloat var = storeVarWithWordSize (\index => do cgCast srcTy IFloat; Fstore index) var

storeVar IUnknown IDouble var = storeVarWithWordSize (\index => do cgCast IUnknown IDouble; Dstore index) var
storeVar srcTy@(Ref _) IDouble var = storeVarWithWordSize (\index => do cgCast srcTy IDouble; Dstore index) var

storeVar IUnknown arr@(IArray elemTy) var =
    storeVarWithWordSize (\index => do Checkcast $ getInferredTyDesc arr; Astore index) var
storeVar (Ref _) arr@(IArray elemTy) var =
    storeVarWithWordSize (\index => do Checkcast $ getInferredTyDesc arr; Astore index) var

storeVar IUnknown targetTy@(Ref _) var = do
    types <- GetFunctionLocTypes
    cgCast IUnknown targetTy
    opWithWordSize types Astore var

storeVar srcTy@(Ref _) targetTy@(Ref _) var = do
    types <- GetFunctionLocTypes
    cgCast srcTy targetTy
    opWithWordSize types Astore var

storeVar _ _ var = do types <- GetFunctionLocTypes; opWithWordSize types Astore var

idrisObjectProperty : Int -> Int -> Asm ()
idrisObjectProperty object propertyIndex = do
  locTypes <- GetFunctionLocTypes
  loadVar locTypes inferredObjectType inferredObjectType (Loc object)
  Iconst propertyIndex
  InvokeMethod InvokeStatic idrisObjectType "getProperty" "(Ljava/lang/Object;I)Ljava/lang/Object;" False

assign : (lhs: List LVar) -> (rhs: List LVar) -> Asm ()
assign lhs rhs = do
    locTypes <- GetFunctionLocTypes
    go locTypes lhs rhs
  where
    go : InferredTypeStore -> (lhs: List LVar) -> (rhs: List LVar) -> Asm ()
    go types [] [] = pure ()
    go types [] _  = pure ()
    go types _  [] = pure ()
    go types (lhs :: lhsRest) (rhs :: rhsRest) = do
      let lhsTy = getLocTy types lhs
      let rhsTy = getLocTy types rhs
      loadVar types rhsTy lhsTy rhs
      storeVar lhsTy lhsTy lhs
      go types lhsRest rhsRest

aload : Int -> Asm ()
aload index = do
  types <- GetFunctionLocTypes
  opWithWordSize types Aload (Loc index)
