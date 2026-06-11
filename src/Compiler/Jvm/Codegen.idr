module Compiler.Jvm.Codegen

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.NoMangle

import Core.Context
import Core.Core
import Core.Directory
import Core.Name
import Core.Options
import Core.TT
import Core.TT.Primitive

import Data.List
import Data.List.Lazy
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect

import Debug.Trace

import Libraries.Data.NameMap
import Libraries.Data.SortedMap
import Libraries.Data.SortedSet
import Libraries.Utils.Path

import System.File
import System.FFI
import System.Info

import Compiler.Jvm.Asm
import Compiler.Jvm.Export
import Compiler.Jvm.ExtPrim
import Compiler.Jvm.FunctionTree
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.Math
import Compiler.Jvm.Optimizer
import Compiler.Jvm.ShowUtil
import Compiler.Jvm.Tree
import Compiler.Jvm.Tuples
import Compiler.Jvm.Variable

import Idris.Syntax

%default covering

%hide Core.Name.Scoped.Scope
%hide System.FFI.runtimeClass

addScopeLocalVariables : {auto stateRef: Ref AsmState AsmState} -> Scope -> Core ()
addScopeLocalVariables scope = do
    let scopeIndex = index scope
    let (lineNumberStart, lineNumberEnd) = lineNumbers scope
    let (labelStart, labelEnd) = labels scope
    nameAndIndices <- coreLift $ Map.toList $ variableIndices scope
    go labelStart labelEnd nameAndIndices
  where
    go : String -> String -> List (String, Int) -> Core ()
    go _ _ [] = pure ()
    go labelStart labelEnd ((name, varIndex) :: rest) = do
        variableType <- getVariableTypeAtScope (index scope) name
        localVariable name (getJvmTypeDescriptor variableType) Nothing labelStart labelEnd varIndex
        go labelStart labelEnd rest

addLocalVariables : {auto stateRef: Ref AsmState AsmState} -> Int -> Core ()
addLocalVariables scopeIndex = do
    scope <- getScope scopeIndex
    addScopeLocalVariables scope
    traverse_ addLocalVariables $ childIndices scope

enterScope : {auto stateRef: Ref AsmState AsmState} -> Core ()
enterScope = do
    scopeIndex <- newScopeIndex
    updateCurrentScopeIndex scopeIndex

exitScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Core ()
exitScope = updateCurrentScopeIndex

withScope : {auto stateRef: Ref AsmState AsmState} -> Lazy (Core ()) -> Core ()
withScope op = do
    scopeIndex <- getCurrentScopeIndex
    enterScope
    op
    exitScope scopeIndex

shouldUseTableSwitch : Int64 -> Int64 -> Int64 -> Bool
shouldUseTableSwitch low high labelCount =
  let tableSpaceCost = 4 + (high - low + 1)
      tableTimeCost = 3
      lookupSpaceCost = 3 + 2 * labelCount
      lookupTimeCost = labelCount
  in labelCount > 0 &&
       tableSpaceCost + 3 * tableTimeCost <=
           lookupSpaceCost + 3 * lookupTimeCost

defaultValue : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Core ()
defaultValue IBool = iconst 0
defaultValue IByte = iconst 0
defaultValue IChar = iconst 0
defaultValue IShort = iconst 0
defaultValue IInt = iconst 0
defaultValue ILong = ldc $ Int64Const 0
defaultValue IFloat = fconst 0
defaultValue IDouble = dconst 0
defaultValue _ = aconstnull

assembleArray : {auto stateRef: Ref AsmState AsmState} -> (elemTy: InferredType) -> Core ()
assembleArray IBool = anewbooleanarray
assembleArray IByte = anewbytearray
assembleArray IChar = anewchararray
assembleArray IShort = anewshortarray
assembleArray IInt = anewintarray
assembleArray ILong = anewlongarray
assembleArray IFloat = anewfloatarray
assembleArray IDouble = anewdoublearray
assembleArray (IRef ty _ _) = anewarray ty
assembleArray (IArray ty) = anewarray (getJvmTypeDescriptor ty)
assembleArray (IFunction (MkJavaLambdaType (IRef ty _ _) _ _ _)) = anewarray ty
assembleArray _ = anewarray "java/lang/Object"

storeArray : {auto stateRef: Ref AsmState AsmState} -> (elemTy: InferredType) -> Core ()
storeArray IBool = bastore
storeArray IByte = bastore
storeArray IChar = castore
storeArray IShort = sastore
storeArray IInt = iastore
storeArray ILong = lastore
storeArray IFloat = fastore
storeArray IDouble = dastore
storeArray _ = aastore

loadArray : {auto stateRef: Ref AsmState AsmState} -> (elemTy: InferredType) -> Core ()
loadArray IBool = baload
loadArray IByte = baload
loadArray IChar = caload
loadArray IShort = saload
loadArray IInt = iaload
loadArray ILong = laload
loadArray IFloat = faload
loadArray IDouble = daload
loadArray _ = aaload

jIntKind : PrimType -> IntKind
jIntKind ty = fromMaybe (Signed (P 32)) (intKind ty)

multiValueMap : Ord k => (a -> k) -> (a -> v) -> List a -> SortedMap k (List v)
multiValueMap f g xs = go SortedMap.empty xs where
  go : SortedMap k (List v) -> List a -> SortedMap k (List v)
  go acc [] = acc
  go acc (x :: xs) =
    let key = f x
        value = g x
        vs = fromMaybe [] $ SortedMap.lookup key acc
        newAcc = SortedMap.insert key (value :: vs) acc
    in go newAcc xs

constantAltIntExpr : {auto stateRef: Ref AsmState AsmState} -> FC -> NamedConstAlt -> Core (String, Int, NamedConstAlt)
constantAltIntExpr fc alt@(MkNConstAlt constant _) = do
        constExpr <- getIntConstantValue fc constant
        label <- newLabel
        pure (label, constExpr, alt)

%foreign jvm' "java/lang/Long" "hashCode" "long" "int"
int64HashCode : Int64 -> Int

%foreign jvm' "java/lang/Long" "hashCode" "long" "int"
bits64HashCode : Bits64 -> Int

hashCode : Primitive.Constant -> Maybe Int
hashCode (BI value) = Just $ Object.hashCode value
hashCode (I64 value) = Just $ int64HashCode value
hashCode (B64 value) = Just $ bits64HashCode value
hashCode (Str value) = Just $ Object.hashCode value
hashCode x = Nothing

getHashCodeSwitchClass : {auto stateRef: Ref AsmState AsmState} -> FC -> InferredType -> Core String
getHashCodeSwitchClass fc (IRef "java/lang/String" _ _) = pure stringClass
getHashCodeSwitchClass fc (IRef "java/math/BigInteger" _ _) = pure bigIntegerClass
getHashCodeSwitchClass fc ILong = pure"java/lang/Long"
getHashCodeSwitchClass fc constantType = asmCrash ("Constant type " ++ show constantType ++ " cannot be compiled to 'Switch'.")

assembleHashCodeSwitchConstant : {auto stateRef: Ref AsmState AsmState} -> FC -> Primitive.Constant -> Core ()
assembleHashCodeSwitchConstant _ (BI value) = newBigInteger $ show value
assembleHashCodeSwitchConstant _ (I64 value) = ldc $ Int64Const value
assembleHashCodeSwitchConstant _ (B64 value) = ldc $ Bits64Const value
assembleHashCodeSwitchConstant _ (Str value) = ldc $ StringConst value
assembleHashCodeSwitchConstant fc constant =
    asmCrash $ "Constant " ++ show constant ++ " cannot be compiled to 'switch'"

conAltIntExpr : {auto stateRef: Ref AsmState AsmState} -> NamedConAlt -> Core (String, Int, NamedConAlt)
conAltIntExpr alt@(MkNConAlt name conInfo tag _ expr) = do
    label <- newLabel
    intValue <- case conInfo of
      NOTHING => pure 0
      NIL => pure 0
      JUST => pure 1
      CONS => pure 1
      _ => maybe (asmCrash $ "Missing constructor tag " ++ show name) pure tag
    pure (label, intValue, alt)

createDefaultLabel : {auto stateRef: Ref AsmState AsmState} -> Core String
createDefaultLabel = do
    label <- newLabel
    createLabel label
    pure label

getSwitchCasesWithEndLabel : List1 (String, Int, a) -> List1 String -> List1 (String, Int, a, String)
getSwitchCasesWithEndLabel switchCases labelStarts = getSwitchCaseWithEndLabel <$> endLabels
  where
    endLabels : List1 ((String, Int, a), String)
    endLabels = zip switchCases (lappend (tail labelStarts) (singleton methodEndLabel))

    getSwitchCaseWithEndLabel : ((String, Int, a), String) -> (String, Int, a, String)
    getSwitchCaseWithEndLabel ((labelStart, constExpr, body), labelEnd) = (labelStart, constExpr, body, labelEnd)

getCasesWithLabels : (mapper: a -> Core (String, Int, a)) -> (alts: List1 a) -> Core (List1 (String, Int, a))
getCasesWithLabels mapper alts = do
    cases <- traverse mapper (toList alts)
    case fromList $ sortBy (comparing second) cases of
      Nothing => asmCrash "Unexpected empty switch cases"
      Just sorted => pure sorted

assembleCaseWithScope : {auto stateRef: Ref AsmState AsmState} -> String -> String -> (body: Core ()) -> Core ()
assembleCaseWithScope lblStart lblEnd body = withScope $ do
    scopeIndex <- getCurrentScopeIndex
    scope <- getScope scopeIndex
    let (lineNumberStart, lineNumberEnd) = lineNumbers scope
    labelStart lblStart
    updateScopeStartLabel scopeIndex lblStart
    addLineNumber lineNumberStart lblStart
    updateScopeEndLabel scopeIndex lblEnd
    body

labelHashCodeAlt : {auto stateRef: Ref AsmState AsmState} -> (Int, a) -> Core (String, Int, a)
labelHashCodeAlt (hash, expressions) = pure (!newLabel, hash, expressions)

getHashCodeCasesWithLabels : {auto stateRef: Ref AsmState AsmState} -> SortedMap Int (List (Int, a))
                           -> Core (List1 (String, Int, List (Int, a)))
getHashCodeCasesWithLabels positionAndAltsByHash = do
  cases@(first :: rest) <- traverse labelHashCodeAlt $ SortedMap.toList positionAndAltsByHash
    | [] => asmCrash ("Unexpected empty switch cases")
  pure $ (first ::: rest)

toUnsignedInt : {auto stateRef: Ref AsmState AsmState} -> Int -> Core ()
toUnsignedInt bits = do
  iconst bits
  invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False

assembleInt : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> Int -> Core ()
assembleInt isTailCall returnType value = do
    iconst value
    asmCast IInt returnType
    when isTailCall $ asmReturn returnType

assembleInt64 : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> Int64 -> Core ()
assembleInt64 isTailCall returnType value = do
    ldc $ Int64Const value
    asmCast ILong returnType
    when isTailCall $ asmReturn returnType

assembleBits64 : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> Bits64 -> Core ()
assembleBits64 isTailCall returnType value = do
    ldc $ Bits64Const value
    asmCast ILong returnType
    when isTailCall $ asmReturn returnType

isInterfaceInvocation : InferredType -> Bool
isInterfaceInvocation (IRef _ Interface _) = True
isInterfaceInvocation _ = False

getDynamicVariableIndex : {auto stateRef: Ref AsmState AsmState} -> (variablePrefix: String) -> Core Int
getDynamicVariableIndex variablePrefix = do
    suffixIndex <- newDynamicVariableIndex
    let variableName = variablePrefix ++ show suffixIndex
    getVariableIndex variableName

assembleIdentityLambda : {auto stateRef: Ref AsmState AsmState} -> (isTailCall : Bool) -> Core ()
assembleIdentityLambda isTailCall = do
  field GetStatic functionsClass "IDENTITY" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleIdentity1Lambda : {auto stateRef: Ref AsmState AsmState} -> (isTailCall : Bool) -> Core ()
assembleIdentity1Lambda isTailCall = do
  field GetStatic functionsClass "IDENTITY_1" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleIdentity2Lambda : {auto stateRef: Ref AsmState AsmState} -> (isTailCall : Bool) -> Core ()
assembleIdentity2Lambda isTailCall = do
  field GetStatic functionsClass "IDENTITY_2" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleConstantLambda : {auto stateRef: Ref AsmState AsmState} -> (isTailCall : Bool) -> Core ()
assembleConstantLambda isTailCall = do
  field GetStatic functionsClass "CONSTANT" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleConstant1Lambda : {auto stateRef: Ref AsmState AsmState} -> (isTailCall : Bool) -> Core ()
assembleConstant1Lambda isTailCall = do
  field GetStatic functionsClass "CONSTANT_1" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

getLambdaTypeByArity: (arity: Nat) -> LambdaType
getLambdaTypeByArity 2 = Function2Lambda
getLambdaTypeByArity 3 = Function3Lambda
getLambdaTypeByArity 4 = Function4Lambda
getLambdaTypeByArity 5 = Function5Lambda
getLambdaTypeByArity _ = FunctionLambda

assembleClassLiteral : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Core ()
assembleClassLiteral IByte   = field GetStatic "java/lang/Byte" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IChar   = field GetStatic "java/lang/Character" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IShort  = field GetStatic "java/lang/Short" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IBool   = field GetStatic "java/lang/Boolean" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IDouble = field GetStatic "java/lang/Double" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IFloat  = field GetStatic "java/lang/Float" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IInt    = field GetStatic "java/lang/Integer" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral ILong   = field GetStatic "java/lang/Long" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IVoid   = field GetStatic "java/lang/Void" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral type    = ldc $ TypeConst $ getJvmTypeDescriptor type

intToBigInteger : {auto stateRef: Ref AsmState AsmState} -> Core ()
intToBigInteger = do
  i2l
  invokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False

unsignedIntToBigInteger : {auto stateRef: Ref AsmState AsmState} -> Core ()
unsignedIntToBigInteger = do
    invokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
    invokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False

unsignedIntToString : {auto stateRef: Ref AsmState AsmState} -> Core ()
unsignedIntToString = invokeMethod InvokeStatic "java/lang/Integer" "toUnsignedString" "(I)Ljava/lang/String;" False

bigIntegerToInt : {auto stateRef: Ref AsmState AsmState} -> Core () -> Core ()
bigIntegerToInt op = do
  invokeMethod InvokeVirtual "java/math/BigInteger" "intValue" "()I" False
  op

compareUnsignedLong : {auto stateRef: Ref AsmState AsmState} -> (String -> Core ()) -> String -> Core ()
compareUnsignedLong op label = do longCompareUnsigned; op label

compareUnsignedInt : {auto stateRef: Ref AsmState AsmState} -> (String -> Core ()) -> String -> Core ()
compareUnsignedInt op label = do integerCompareUnsigned; op label

compareSignedLong : {auto stateRef: Ref AsmState AsmState} -> (String -> Core ()) -> String -> Core ()
compareSignedLong op label = do lcmp; op label

getCastAsmOp : {auto stateRef: Ref AsmState AsmState} -> PrimType -> PrimType -> Core ()
getCastAsmOp IntegerType Bits8Type = do
    iconst 8
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(Ljava/math/BigInteger;I)I" False
getCastAsmOp IntegerType Bits16Type = do
    iconst 16
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(Ljava/math/BigInteger;I)I" False
getCastAsmOp IntegerType Bits32Type = do
    iconst 32
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(Ljava/math/BigInteger;I)I" False
getCastAsmOp IntegerType Bits64Type = do
    iconst 64
    invokeMethod InvokeStatic conversionClass "toUnsignedLong" "(Ljava/math/BigInteger;I)J" False
getCastAsmOp IntegerType Int64Type = invokeMethod InvokeVirtual "java/math/BigInteger" "longValue" "()J" False
getCastAsmOp IntegerType Int16Type = bigIntegerToInt i2s
getCastAsmOp IntegerType Int32Type = bigIntegerToInt (pure ())
getCastAsmOp IntegerType Int8Type = bigIntegerToInt i2b
getCastAsmOp IntegerType IntType = bigIntegerToInt (pure ())
getCastAsmOp IntegerType CharType =
  bigIntegerToInt (invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False)
getCastAsmOp IntegerType DoubleType = invokeMethod InvokeVirtual "java/math/BigInteger" "doubleValue" "()D" False
getCastAsmOp IntegerType StringType = invokeMethod InvokeVirtual "java/math/BigInteger" "toString" "()Ljava/lang/String;" False

getCastAsmOp Int8Type Bits64Type = i2l
getCastAsmOp Int8Type IntegerType = intToBigInteger
getCastAsmOp Int8Type Int64Type = i2l
getCastAsmOp Int8Type DoubleType = i2d
getCastAsmOp Int8Type CharType = invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

getCastAsmOp Int16Type Int8Type = i2b
getCastAsmOp Int16Type IntegerType = intToBigInteger
getCastAsmOp Int16Type Bits64Type = i2l
getCastAsmOp Int16Type Int64Type = i2l
getCastAsmOp Int16Type DoubleType = i2d
getCastAsmOp Int16Type CharType = invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

getCastAsmOp Int32Type Int8Type = i2b
getCastAsmOp Int32Type Int16Type = i2s
getCastAsmOp Int32Type Int64Type = i2l
getCastAsmOp Int32Type Bits64Type = i2l
getCastAsmOp Int32Type Bits16Type = toUnsignedInt 16
getCastAsmOp Int32Type Bits8Type = toUnsignedInt 8
getCastAsmOp Int32Type IntegerType = intToBigInteger
getCastAsmOp Int32Type DoubleType = i2d
getCastAsmOp Int32Type CharType = invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

getCastAsmOp IntType Int8Type = i2b
getCastAsmOp IntType Int16Type = i2s
getCastAsmOp IntType Int64Type = i2l
getCastAsmOp IntType Bits64Type = i2l
getCastAsmOp IntType Bits16Type = toUnsignedInt 16
getCastAsmOp IntType Bits8Type = toUnsignedInt 8
getCastAsmOp IntType IntegerType = intToBigInteger
getCastAsmOp IntType DoubleType = i2d
getCastAsmOp IntType CharType = invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

getCastAsmOp DoubleType StringType =
  invokeMethod InvokeStatic "java/lang/Double" "toString" "(D)Ljava/lang/String;" False
getCastAsmOp DoubleType IntegerType = do
    invokeMethod InvokeStatic "java/math/BigDecimal" "valueOf" "(D)Ljava/math/BigDecimal;" False
    invokeMethod InvokeVirtual "java/math/BigDecimal" "toBigInteger" "()Ljava/math/BigInteger;" False
getCastAsmOp DoubleType Bits8Type = do d2i; toUnsignedInt 8
getCastAsmOp DoubleType Bits16Type = do d2i; toUnsignedInt 16
getCastAsmOp DoubleType Bits32Type = do d2l; l2i
getCastAsmOp DoubleType Bits64Type = invokeMethod InvokeStatic conversionClass "toLong" "(D)J" False
getCastAsmOp DoubleType IntType = do d2l; l2i
getCastAsmOp DoubleType Int8Type = do d2i; i2b
getCastAsmOp DoubleType Int16Type = do d2i; i2s
getCastAsmOp DoubleType Int32Type = do d2l; l2i
getCastAsmOp DoubleType Int64Type = invokeMethod InvokeStatic conversionClass "toLong" "(D)J" False
getCastAsmOp DoubleType _ = d2i

getCastAsmOp CharType IntegerType = do
    i2l
    invokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
getCastAsmOp CharType Bits64Type = i2l
getCastAsmOp CharType Int64Type = i2l
getCastAsmOp CharType DoubleType = i2d
getCastAsmOp CharType StringType =
  invokeMethod InvokeStatic "java/lang/Character" "toString" "(C)Ljava/lang/String;" False
getCastAsmOp CharType _ = pure ()

getCastAsmOp Bits8Type Bits16Type = do
    iconst 16
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp Bits8Type Bits32Type = do
    i2l
    iconst 32
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Bits8Type Bits64Type = do
    iconst 64
    invokeMethod InvokeStatic conversionClass "toUnsignedLong" "(II)J" False
getCastAsmOp Bits8Type IntegerType = unsignedIntToBigInteger
getCastAsmOp Bits8Type Int8Type = i2b
getCastAsmOp Bits8Type Int16Type = i2s
getCastAsmOp Bits8Type Int64Type = i2l
getCastAsmOp Bits8Type CharType = invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
getCastAsmOp Bits8Type StringType = unsignedIntToString
getCastAsmOp Bits8Type DoubleType = i2d
getCastAsmOp Bits8Type _ = pure ()

getCastAsmOp Bits16Type Bits8Type = do
    iconst 8
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp Bits16Type Bits32Type = do
    i2l
    iconst 32
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Bits16Type IntType = pure ()

getCastAsmOp Bits16Type Bits64Type = do
    iconst 64
    invokeMethod InvokeStatic conversionClass "toUnsignedLong" "(II)J" False
getCastAsmOp Bits16Type IntegerType = unsignedIntToBigInteger
getCastAsmOp Bits16Type Int8Type = i2b
getCastAsmOp Bits16Type Int16Type = i2s
getCastAsmOp Bits16Type Int64Type = i2l
getCastAsmOp Bits16Type CharType = invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
getCastAsmOp Bits16Type DoubleType = i2d
getCastAsmOp Bits16Type StringType = unsignedIntToString

getCastAsmOp Bits32Type Bits8Type = do
    iconst 8
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp Bits32Type Bits16Type = do
    iconst 16
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp Bits32Type IntType = pure ()
getCastAsmOp Bits32Type Bits64Type =
    invokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
getCastAsmOp Bits32Type IntegerType = unsignedIntToBigInteger
getCastAsmOp Bits32Type Int8Type = i2b
getCastAsmOp Bits32Type Int16Type = i2s
getCastAsmOp Bits32Type Int64Type = invokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
getCastAsmOp Bits32Type DoubleType = do
  invokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
  l2d
getCastAsmOp Bits32Type CharType = invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
getCastAsmOp Bits32Type StringType = unsignedIntToString

getCastAsmOp Bits64Type Bits8Type = do
    iconst 8
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Bits64Type Bits16Type = do
    iconst 16
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Bits64Type Bits32Type = do
    iconst 32
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Bits64Type IntegerType =
    invokeMethod InvokeStatic conversionClass "toUnsignedBigInteger" "(J)Ljava/math/BigInteger;" False
getCastAsmOp Bits64Type Int64Type = pure ()
getCastAsmOp Bits64Type Int8Type = l2i
getCastAsmOp Bits64Type Int16Type = l2i
getCastAsmOp Bits64Type Int32Type = l2i
getCastAsmOp Bits64Type IntType = l2i
getCastAsmOp Bits64Type DoubleType = invokeMethod InvokeStatic conversionClass "unsignedLongToDouble" "(J)D" False
getCastAsmOp Bits64Type CharType = do
  l2i
  invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
getCastAsmOp Bits64Type StringType =
    invokeMethod InvokeStatic "java/lang/Long" "toUnsignedString" "(J)Ljava/lang/String;" False

getCastAsmOp Int64Type IntegerType =
    invokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
getCastAsmOp Int64Type Bits8Type = do
    iconst 8
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Int64Type Bits16Type = do
    iconst 16
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Int64Type Bits32Type = do
    iconst 32
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp Int64Type Bits64Type = pure ()
getCastAsmOp Int64Type Int8Type = l2i
getCastAsmOp Int64Type Int16Type = l2i
getCastAsmOp Int64Type Int32Type = l2i
getCastAsmOp Int64Type IntType = l2i
getCastAsmOp Int64Type DoubleType = l2d
getCastAsmOp Int64Type CharType = do l2i; invokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
getCastAsmOp Int64Type StringType =
    invokeMethod InvokeStatic "java/lang/Long" "toString" "(J)Ljava/lang/String;" False

getCastAsmOp StringType IntegerType =
  invokeMethod InvokeStatic conversionClass "toInteger" "(Ljava/lang/String;)Ljava/math/BigInteger;" False
getCastAsmOp StringType Bits8Type = do
  invokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
  iconst 8
  invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp StringType Bits16Type = do
  invokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
  iconst 16
  invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp StringType Bits32Type = do
  invokeMethod InvokeStatic "java/lang/Long" "parseLong" "(Ljava/lang/String;)J" False
  l2i
getCastAsmOp StringType Bits64Type =
  invokeMethod InvokeStatic conversionClass "toLong" "(Ljava/lang/String;)J" False
getCastAsmOp StringType Int8Type = do
  invokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
  i2b
getCastAsmOp StringType Int16Type = do
  invokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
  i2s
getCastAsmOp StringType Int32Type = do
  invokeMethod InvokeStatic "java/lang/Long" "parseLong" "(Ljava/lang/String;)J" False
  l2i
getCastAsmOp StringType IntType = invokeMethod InvokeStatic conversionClass "toInt" "(Ljava/lang/String;)I" False
getCastAsmOp StringType Int64Type =
  invokeMethod InvokeStatic conversionClass "toLong" "(Ljava/lang/String;)J" False
getCastAsmOp StringType DoubleType =
  invokeMethod InvokeStatic "java/lang/Double" "parseDouble" "(Ljava/lang/String;)D" False
getCastAsmOp StringType CharType = do
  iconst 0
  invokeMethod InvokeVirtual "java/lang/String" "charAt" "(Ljava/lang/String;I)C" False
getCastAsmOp StringType _ =
    invokeMethod InvokeStatic conversionClass "toInt" "(Ljava/lang/String;)I" False

getCastAsmOp _ Bits8Type = do
    iconst 8
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp _ Bits16Type = do
    iconst 16
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
getCastAsmOp _ Bits32Type = do
    i2l
    iconst 32
    invokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
getCastAsmOp _ Bits64Type =
    invokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
getCastAsmOp _ Int64Type = i2l
getCastAsmOp _ IntegerType = do
    i2l
    invokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
getCastAsmOp _ DoubleType = i2d
getCastAsmOp _ StringType =
    invokeMethod InvokeStatic "java/lang/Integer" "toString" "(I)Ljava/lang/String;" False
getCastAsmOp _ _ = pure ()

assembleMissingDefault : {auto stateRef: Ref AsmState AsmState} -> InferredType -> FC -> String -> Core ()
assembleMissingDefault returnType fc defaultLabel = do
    labelStart defaultLabel
    defaultValue returnType
    asmReturn returnType

fillTableSwitchLabels : (defaultLabel: String) -> (labels: List1 String) -> (cases: List1 Int) -> List1 String
fillTableSwitchLabels defaultLabel labels cases = reverse $ go (head cases) (zip cases labels) where

  go1 : List1 String -> Int -> List (Int, String) -> List1 String
  go1 acc _ [] = acc
  go1 acc currentValue cases@((caseValue, label) :: rest) =
    if currentValue == caseValue
       then go1 (cons label acc) (currentValue + 1) rest
       else go1 (cons defaultLabel acc) (currentValue + 1) cases

  go : Int -> List1 (Int, String) -> List1 String
  go currentValue cases@((caseValue, label) ::: rest) =
    if currentValue == caseValue
       then go1 (singleton label) (currentValue + 1) rest
       else go1 (singleton defaultLabel) (currentValue + 1) (toList cases)

assembleBranch : {auto stateRef: Ref AsmState AsmState} -> (defaultLabel: String) -> (labels: List1 String)
               -> (cases: List1 Int) -> Core ()
assembleBranch defaultLabel labels cases =
  let min = head cases
      max = last cases
      isTableSwitch = shouldUseTableSwitch (cast min) (cast max) (cast $ length labels)
  in if isTableSwitch
        then tableSwitch min max defaultLabel $ fillTableSwitchLabels defaultLabel labels cases
        else lookupSwitch defaultLabel labels cases

parameters {auto c : Ref Ctxt Defs} {auto s : Ref Syn SyntaxInfo} {auto stateRef: Ref AsmState AsmState}
  mutual
    -- The applied variable's static type when it is a typed callback
    -- interface of the given arity (higher-order specialisation); Nothing
    -- routes the application to the boxed `Function.apply` path.
    callbackSigOfVariableWithArity : Nat -> NamedCExp -> Core (Maybe (String, InferredFunctionType))
    callbackSigOfVariableWithArity arity (NmLocal _ var) = do
      varType <- getVariableType (jvmSimpleName var)
      pure $ case (varType, parseCallbackIfaceType varType) of
        (IRef cls _ _, Just sig) =>
          if length sig.parameterTypes == arity then Just (cls, sig) else Nothing
        _ => Nothing
    callbackSigOfVariableWithArity _ _ = pure Nothing

    assembleExpr : (isTailCall: Bool) -> InferredType -> NamedCExp -> Core ()
    assembleExpr isTailCall returnType (NmDelay _ _ expr) =
        assembleSubMethodWithScope isTailCall returnType Nothing Nothing expr
    assembleExpr isTailCall returnType (NmLocal _ loc) = do
        let variableName = jvmSimpleName loc
        index <- getVariableIndex variableName
        variableType <- getVariableType variableName
        loadVar !getVariableTypes variableType returnType index
        when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType (NmApp _ (NmLam _ var body) [argument]) =
        assembleSubMethodWithScope isTailCall returnType (Just argument) (Just var) body
    assembleExpr isTailCall returnType (NmLam _ parameter body) =
        assembleSubMethodWithScope isTailCall returnType Nothing (Just parameter) body

    assembleExpr isTailCall returnType (NmLet _ var value expr) = do
        valueScopeStartLabel <- newLabel
        createLabel valueScopeStartLabel
        targetExprScopeStartLabel <- newLabel
        createLabel targetExprScopeStartLabel
        letScopeIndex <- getCurrentScopeIndex
        let variableName = jvmSimpleName var
        variableType <- getVariableType variableName
        variableIndex <- getVariableIndex variableName

        withScope $ do
            valueScopeIndex <- getCurrentScopeIndex
            scope <- getScope valueScopeIndex
            let (lineNumberStart, lineNumberEnd) = lineNumbers scope
            updateScopeStartLabel valueScopeIndex valueScopeStartLabel
            updateScopeEndLabel valueScopeIndex targetExprScopeStartLabel
            labelStart valueScopeStartLabel
            addLineNumber lineNumberStart valueScopeStartLabel
            assembleExpr False variableType value
            storeVar variableType variableType variableIndex

        withScope $ do
            targetExprScopeIndex <- getCurrentScopeIndex
            scope <- getScope targetExprScopeIndex
            let (lineNumberStart, lineNumberEnd) = lineNumbers scope
            updateScopeStartLabel targetExprScopeIndex targetExprScopeStartLabel
            labelStart targetExprScopeStartLabel
            addLineNumber lineNumberStart targetExprScopeStartLabel
            updateScopeEndLabel targetExprScopeIndex methodEndLabel
            assembleExpr isTailCall returnType expr

    -- Tail recursion. Store arguments and recur to the beginning of the method
    assembleExpr _ returnType app@(NmApp fc (NmRef _ (UN (Basic "$idrisTailRec"))) args) =
        case length args of
            Z => goto methodStartLabel
            (S lastArgIndex) => do
                jname <- name <$> getCurrentFunction
                parameterTypes <- getFunctionParameterTypes jname
                let argsWithTypes = zip args parameterTypes
                variableTypes <- getVariableTypes
                let argIndices = [0 .. the Int $ cast lastArgIndex]
                targetVariableIndices <- traverse (storeParameter variableTypes) $ zip argIndices argsWithTypes
                traverse_ (assign variableTypes) $ zip targetVariableIndices $ zip argIndices parameterTypes
                goto methodStartLabel
              where
                assign : Map Int InferredType -> (Int, Int, InferredType) -> Core ()
                assign types (targetVariableIndex, argIndex, ty) =
                    when (targetVariableIndex /= argIndex) $ do
                        loadVar types ty ty targetVariableIndex
                        storeVar ty ty argIndex

    assembleExpr isTailCall returnType (NmApp _ (NmRef _ idrisName) []) =
        assembleNmAppNilArity isTailCall returnType idrisName
    assembleExpr isTailCall returnType (NmApp _ (NmRef _ idrisName) args) =
      if isSuperCall idrisName args then do aconstnull; when isTailCall $ asmReturn returnType
      else do
        let jname = jvmName !getProgramName idrisName
        functionType <- case !(findFunctionType jname) of
            Just ty => pure ty
            Nothing => pure $ MkInferredFunctionType inferredObjectType $ replicate (length args) inferredObjectType
        let paramTypes = parameterTypes functionType
        if paramTypes == []
            then assembleNmAppNilArity isTailCall returnType idrisName
            else do
                -- Mirror of inferAppArgs: literal lambdas at typed
                -- callback slots (spec callee or decl-driven) are
                -- assembled expecting the typed interface; the
                -- invokestatic descriptor below keeps the callee's actual
                -- parameter types (the typed value is assignable to a
                -- natural Function slot since the interface extends it).
                declSigs <- fromMaybe [] . SortedMap.lookup (jvmSimpleName idrisName) <$> getCallbackSlotSigs
                let expectedTypes = adjustCallbackSlotTypes !getProgramName declSigs args paramTypes
                let argsWithTypes = zip args expectedTypes
                traverse_ assembleParameter argsWithTypes
                let methodReturnType = InferredFunctionType.returnType functionType
                let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType methodReturnType paramTypes
                invokeMethod InvokeStatic (className jname) (methodName jname) methodDescriptor False
                asmCast methodReturnType returnType
                when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType (NmApp _ inner@(NmApp _ lambdaVariable@(NmLocal _ _) [x]) [y]) = do
        -- Typed apply (arity-2 higher-order specialisation): `f x y` —
        -- nested unary application — on a variable statically typed as a
        -- two-parameter callback interface invokes the typed `apply` in
        -- one call: no intermediate partial-application closure, no
        -- boxing.  Mirrors inferExprApp's nested variable-head case;
        -- every other shape replays the nested boxed path.
        case !(callbackSigOfVariableWithArity 2 lambdaVariable) of
          Just (ifaceName, sig) => do
            createIdrisFunctionInterface ifaceName (getMethodDescriptor sig)
            assembleExpr False (IRef ifaceName Interface []) lambdaVariable
            let (t1, t2) = case sig.parameterTypes of
                             [a, b] => (a, b)
                             _ => (inferredObjectType, inferredObjectType)
            assembleExpr False t1 x
            assembleExpr False t2 y
            invokeMethod InvokeInterface ifaceName "apply" (getMethodDescriptor sig) True
            asmCast sig.returnType returnType
          Nothing => do
            assembleExpr False inferredLambdaType inner
            assembleExpr False IUnknown y
            invokeMethod InvokeInterface "java/util/function/Function" "apply" "(Ljava/lang/Object;)Ljava/lang/Object;" True
            asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType (NmApp _ lambdaVariable [arg]) = do
        -- Typed apply (higher-order specialisation): when the applied
        -- value is a variable statically typed as an arity-1 callback
        -- interface (e.g. a spec's callback parameter), invoke the typed
        -- `apply` directly — argument cast to the typed parameter,
        -- primitive result with no boxing.  Mirrors inferExprApp's
        -- variable-head case; every other shape — including a partial
        -- application of an arity-2 typed callback — stays on the boxed
        -- `Function.apply` path, which remains correct for typed
        -- callbacks through the interface's default bridge method.
        case !(callbackSigOfVariableWithArity 1 lambdaVariable) of
          Just (ifaceName, sig) => do
            createIdrisFunctionInterface ifaceName (getMethodDescriptor sig)
            assembleExpr False (IRef ifaceName Interface []) lambdaVariable
            assembleExpr False (fromMaybe inferredObjectType (head' sig.parameterTypes)) arg
            invokeMethod InvokeInterface ifaceName "apply" (getMethodDescriptor sig) True
            asmCast sig.returnType returnType
          Nothing => do
            assembleExpr False inferredLambdaType lambdaVariable
            assembleExpr False IUnknown arg
            invokeMethod InvokeInterface "java/util/function/Function" "apply" "(Ljava/lang/Object;)Ljava/lang/Object;" True
            asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType expr@(NmCon fc name conInfo tag args) = assembleCon isTailCall returnType fc name tag args

    assembleExpr isTailCall returnType (NmOp fc fn args) = do
        assembleExprOp returnType fc fn args
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmExtPrim fc p args) = do
        jvmExtPrim fc returnType (toPrim p) args
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmForce _ _ expr) = do
        assembleExpr False delayedType expr
        invokeMethod InvokeStatic runtimeClass "force" "(Ljava/lang/Object;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType (NmConCase fc sc [] Nothing) = do
        defaultValue returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmConCase fc sc [] (Just expr)) = do
        ignore $ assembleConstructorSwitchExpr sc
        assembleExpr isTailCall returnType expr
    assembleExpr _ returnType (NmConCase fc sc [MkNConAlt name conInfo _ args expr] Nothing) = do
        idrisObjectVariableIndex <- assembleConstructorSwitchExpr sc
        assembleConCaseExpr returnType name conInfo idrisObjectVariableIndex args expr
    assembleExpr _ returnType (NmConCase fc sc alts def) = assembleConCase returnType fc sc alts def

    assembleExpr isTailCall returnType (NmConstCase fc sc [] Nothing) = do
        defaultValue returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmConstCase fc sc [] (Just expr)) = assembleExpr isTailCall returnType expr
    assembleExpr _ returnType (NmConstCase fc sc alts@(_ :: _) def) = do
        constantType <- getConstantType alts
        assembleConstantSwitch returnType constantType fc sc alts def

    assembleExpr isTailCall returnType (NmPrimVal fc (I value)) = assembleInt isTailCall returnType value
    assembleExpr isTailCall returnType (NmPrimVal fc (I8 value)) = assembleInt isTailCall returnType (cast value)
    assembleExpr isTailCall returnType (NmPrimVal fc (I16 value)) = assembleInt isTailCall returnType (cast value)
    assembleExpr isTailCall returnType (NmPrimVal fc (I32 value)) = assembleInt isTailCall returnType (cast value)
    assembleExpr isTailCall returnType (NmPrimVal fc (I64 value)) = assembleInt64 isTailCall returnType value
    assembleExpr isTailCall returnType (NmPrimVal fc (B8 value)) = assembleInt isTailCall returnType (cast value)
    assembleExpr isTailCall returnType (NmPrimVal fc (B16 value)) = assembleInt isTailCall returnType (cast value)
    assembleExpr isTailCall returnType (NmPrimVal fc (B32 value)) = assembleInt isTailCall returnType (cast value)
    assembleExpr isTailCall returnType (NmPrimVal fc (B64 value)) = assembleBits64 isTailCall returnType value
    assembleExpr isTailCall returnType (NmPrimVal fc (BI value)) = do
        loadBigInteger value
        asmCast inferredBigIntegerType returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmPrimVal fc (Str value)) = do
        ldc $ StringConst value
        asmCast inferredStringType returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmPrimVal fc (Ch value)) = do
        iconst $ cast value
        asmCast IChar returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmPrimVal fc (Db value)) = do
        ldc $ DoubleConst value
        asmCast IDouble returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmPrimVal _ _) = assembleInt isTailCall returnType 0
    assembleExpr isTailCall returnType (NmErased _) = assembleInt isTailCall returnType 0
    assembleExpr isTailCall returnType (NmCrash _ msg) = do
        ldc $ StringConst msg
        invokeMethod InvokeStatic runtimeClass "crash" "(Ljava/lang/String;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType
    assembleExpr _ _ expr = throw $ GenericMsg (getFC expr) $ "Cannot compile " ++ show expr ++ " yet"

    assembleDefault : InferredType -> String -> NamedCExp -> Core ()
    assembleDefault returnType defaultLabel expr =
      assembleCaseWithScope defaultLabel methodEndLabel (assembleExpr True returnType expr)

    castInt : InferredType -> Core () -> NamedCExp -> Core ()
    castInt returnType conversionOp expr = jassembleCast returnType IInt IInt conversionOp expr

    jassembleCast : InferredType -> InferredType -> InferredType -> Core () -> NamedCExp -> Core ()
    jassembleCast returnType from to conversionOp expr = do
        assembleExpr False from expr
        conversionOp
        asmCast to returnType

    assembleNmAppNilArity : (isTailCall : Bool) -> InferredType -> Name -> Core ()
    assembleNmAppNilArity isTailCall returnType idrisName = do
        let javaName = jvmName !getProgramName idrisName
        field GetStatic (className javaName) (methodName javaName)
            "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
        invokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate"
            "()Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    -- Cheap, side-effect-free emission-time type lookup for a constructor's
    -- argument expressions.  We only need to know whether each slot is
    -- primitive (to match a con-spec) — anything more sophisticated would
    -- duplicate the type inference machinery.  Falls back to Object for
    -- expression shapes we can't classify locally.
    shallowExprType : NamedCExp -> Core InferredType
    shallowExprType (NmLocal _ var) = retrieveVariableType (jvmSimpleName var)
    shallowExprType (NmPrimVal _ (I _))   = pure IInt
    shallowExprType (NmPrimVal _ (I8 _))  = pure IInt
    shallowExprType (NmPrimVal _ (I16 _)) = pure IInt
    shallowExprType (NmPrimVal _ (I32 _)) = pure IInt
    shallowExprType (NmPrimVal _ (I64 _)) = pure ILong
    shallowExprType (NmPrimVal _ (B8 _))  = pure IInt
    shallowExprType (NmPrimVal _ (B16 _)) = pure IInt
    shallowExprType (NmPrimVal _ (B32 _)) = pure IInt
    shallowExprType (NmPrimVal _ (B64 _)) = pure ILong
    shallowExprType (NmPrimVal _ (Db _))  = pure IDouble
    shallowExprType (NmPrimVal _ (Ch _))  = pure IChar
    shallowExprType _ = pure inferredObjectType

    -- Look up a matching constructor spec for the given Name and inferred
    -- arg types.  Slots match either when the types are identical or when
    -- the spec slot is primitive and the call-site type is the same primitive.
    -- (Reference-typed slots in a spec collapse to a single L letter so all
    -- spec ref slots match any call-site ref type.)
    findMatchingConSpec : Name -> List InferredType -> ConSpecialisationPlan -> Maybe SpecialisedConstructor
    findMatchingConSpec name argTypes plan =
      do entries <- SortedMap.lookup name plan
         find (\sc => slotsMatch sc.paramTypes argTypes) entries
      where
        slotMatch : InferredType -> InferredType -> Bool
        slotMatch spec arg =
          if isPrimitive spec then spec == arg
          else not (isPrimitive arg)
        slotsMatch : List InferredType -> List InferredType -> Bool
        slotsMatch xs ys =
          length xs == length ys
          && all (uncurry slotMatch) (zip xs ys)

    -- Spec-class emission path: stack args with their typed slot, then
    -- `new` + `invokespecial <init>(constructorIdType, slotTypes...)V`.
    assembleConSpec : (isTailCall: Bool) -> InferredType -> Maybe Int
                   -> SpecialisedConstructor -> List NamedCExp -> Core ()
    assembleConSpec isTailCall returnType tag spec args = do
        let constructorIdType = maybe inferredStringType (const IInt) tag
        new spec.specClassName
        dup
        maybe (ldc . StringConst $ spec.specClassName) iconst tag
        -- `fieldTypes`, not `paramTypes`: the emitted class's `<init>`
        -- descriptor carries the refined slot types, and
        -- `assembleParameter`'s `asmCast` emits the Object→family-interface
        -- checkcast for refined recursive slots at construction.
        let constructorTypes = constructorIdType :: spec.fieldTypes
        traverse_ assembleParameter (zip args spec.fieldTypes)
        let descriptor = getMethodDescriptor $ MkInferredFunctionType IVoid constructorTypes
        invokeMethod InvokeSpecial spec.specClassName "<init>" descriptor False
        -- The spec class is a sibling of the natural constructor class (both
        -- extend Object and implement IdrisObject) — it is NOT a subclass.
        -- A `checkcast` to the natural class would either throw
        -- NoClassDefFoundError at runtime (the natural is never emitted when
        -- every construction site is specialised) or, if the natural exists,
        -- ClassCastException.  Cast only to targets the spec actually is —
        -- itself, Object, or IdrisObject — and leave the value untouched
        -- otherwise.  Downstream uses go through Object/IdrisObject (function
        -- args take Object, pattern matching dispatches via IdrisObject), so
        -- the untouched spec instance is consumed correctly.
        let specType = IRef spec.specClassName Class []
        let castTarget = case returnType of
                          IRef cls _ _ =>
                            if cls == spec.specClassName
                               || cls == "java/lang/Object"
                               || cls == idrisObjectClass
                              then returnType
                              else specType
                          _ => returnType
        asmCast specType castTarget
        when isTailCall $ asmReturn returnType

    assembleCon : (isTailCall: Bool) -> InferredType -> FC -> Name -> (tag : Maybe Int) -> List NamedCExp -> Core ()
    assembleCon isTailCall returnType fc name tag args = do
        -- `inferExprCon` logs deeply-inferred argument types per construction
        -- site keyed by `(fc, name)`.  Prefer that log over `shallowExprType`
        -- so emission picks the same spec the analysis pass discovered.
        -- Otherwise the two disagree (e.g. `MkT 100 23 4` with `100 :
        -- Integer` cast to `Int` — deep inference sees `[int, int, int]`
        -- and adds a spec; shallow sees `[Object, Object, Object]` and
        -- builds the natural class) and `assembleConCaseExpr`'s typed-
        -- accessor narrowing crashes at runtime because the natural class
        -- is unexpectedly alive.
        conLog <- getConSiteLog
        argTypes <- case find (\(f, n, _, _, _) => f == fc && n == name) conLog of
                      Just (_, _, _, _, ts) => pure ts
                      Nothing => traverse shallowExprType args
        conPlan <- getConSpecialisationPlan
        case findMatchingConSpec name argTypes conPlan of
          Just spec => assembleConSpec isTailCall returnType tag spec args
          Nothing => do
            let fileName = fst $ getSourceLocationFromFc fc
            let constructorClassName = getIdrisConstructorClassName !getProgramName (jvmSimpleName name)
            let constructorIdType = maybe inferredStringType (const IInt) tag
            new constructorClassName
            dup
            maybe (ldc . StringConst $ constructorClassName) iconst tag
            let constructorParameterCountNat = length args
            let constructorParameterCount = the Int $ cast constructorParameterCountNat
            let constructorTypes = constructorIdType :: replicate constructorParameterCountNat inferredObjectType
            let argsWithTypes = zip args $ drop 1 constructorTypes
            traverse_ assembleParameter argsWithTypes
            let descriptor = getMethodDescriptor $ MkInferredFunctionType IVoid constructorTypes
            hasConstructor <- coreLift $ AsmGlobalState.hasConstructor constructorClassName
            when (not hasConstructor) $ do
                coreLift $ AsmGlobalState.addConstructor constructorClassName
                natIfaces <- getNaturalToTConIfaces
                let ifaces = fromMaybe [] $ SortedMap.lookup constructorClassName natIfaces
                createIdrisConstructorClassWithIfaces constructorClassName (isNothing tag) constructorParameterCount ifaces
            invokeMethod InvokeSpecial constructorClassName "<init>" descriptor False
            let constructorType = IRef constructorClassName Class []
            asmCast constructorType returnType
            when isTailCall $ asmReturn returnType

    assembleConstructorSwitchExpr : NamedCExp -> Core Int
    assembleConstructorSwitchExpr (NmLocal _ loc) = getVariableIndex $ jvmSimpleName loc
    assembleConstructorSwitchExpr sc = do
        idrisObjectVariableIndex <- getVariableIndex $ "constructorSwitchValue" ++ show !newDynamicVariableIndex
        assembleExpr False idrisObjectType sc
        storeVar idrisObjectType idrisObjectType idrisObjectVariableIndex
        pure idrisObjectVariableIndex

    assembleExprBinaryOp : InferredType -> InferredType -> Core () -> NamedCExp -> NamedCExp -> Core ()
    assembleExprBinaryOp returnType exprType operator expr1 expr2 = do
        assembleExpr False exprType expr1
        assembleExpr False exprType expr2
        operator
        asmCast exprType returnType

    assembleExprBinaryBoolOp : InferredType -> InferredType -> (String -> Core ()) -> NamedCExp -> NamedCExp -> Core ()
    assembleExprBinaryBoolOp returnType exprType operator expr1 expr2 = do
        assembleExpr False exprType expr1
        assembleExpr False exprType expr2
        ifLabel <- newLabel
        createLabel ifLabel
        elseLabel <- newLabel
        createLabel elseLabel
        endLabel <- newLabel
        createLabel endLabel
        operator elseLabel
        labelStart ifLabel
        iconst 1
        goto endLabel
        labelStart elseLabel
        iconst 0
        labelStart endLabel
        asmCast IInt returnType

    assembleExprComparableBinaryBoolOp : InferredType -> String -> (String -> Core ()) -> NamedCExp -> NamedCExp
                                       -> Core ()
    assembleExprComparableBinaryBoolOp returnType className operator expr1 expr2 = do
        let exprType = IRef className Class []
        assembleExpr False exprType expr1
        assembleExpr False exprType expr2
        ifLabel <- newLabel
        createLabel ifLabel
        elseLabel <- newLabel
        createLabel elseLabel
        endLabel <- newLabel
        createLabel endLabel
        invokeMethod InvokeVirtual className "compareTo" ("(L" ++ className ++ ";)I") False
        operator elseLabel
        labelStart ifLabel
        iconst 1
        goto endLabel
        labelStart elseLabel
        iconst 0
        labelStart endLabel
        asmCast IInt returnType

    assembleExprUnaryOp : InferredType -> InferredType -> Core () -> NamedCExp -> Core ()
    assembleExprUnaryOp returnType exprType operator expr = do
        assembleExpr False exprType expr
        operator
        asmCast exprType returnType

    assembleStrCons : InferredType -> (char: NamedCExp) -> (str: NamedCExp) -> Core ()
    assembleStrCons returnType char str = do
        new "java/lang/StringBuilder"
        dup
        invokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
        assembleExpr False IChar char
        invokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(C)Ljava/lang/StringBuilder;" False
        assembleExpr False inferredStringType str
        invokeMethod InvokeVirtual "java/lang/StringBuilder" "append"
            "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
        invokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
        asmCast inferredStringType returnType

    assembleStrReverse : InferredType -> NamedCExp -> Core ()
    assembleStrReverse returnType str = do
        new "java/lang/StringBuilder"
        dup
        assembleExpr False inferredStringType str
        invokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "(Ljava/lang/String;)V" False
        invokeMethod InvokeVirtual "java/lang/StringBuilder" "reverse" "()Ljava/lang/StringBuilder;" False
        invokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
        asmCast inferredStringType returnType

    assembleCast : InferredType -> FC -> PrimType -> PrimType -> NamedCExp -> Core ()
    assembleCast returnType fc from to x =
      jassembleCast returnType (getInferredType from) (getInferredType to) (getCastAsmOp from to) x

    assembleExprOp : InferredType -> FC -> PrimFn arity -> Vect arity NamedCExp -> Core ()
    assembleExprOp returnType fc (Neg Bits64Type) [x] = assembleExprUnaryOp returnType ILong lneg x
    assembleExprOp returnType fc (ShiftR Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong (do l2i; lushr) x y
    assembleExprOp returnType fc (BAnd Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong land x y
    assembleExprOp returnType fc (BOr Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong lor x y
    assembleExprOp returnType fc (BXOr Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong lxor x y

    assembleExprOp returnType fc (Neg Int64Type) [x] = assembleExprUnaryOp returnType ILong lneg x
    assembleExprOp returnType fc (ShiftR Int64Type) [x, y] = assembleExprBinaryOp returnType ILong (do l2i; lshr) x y
    assembleExprOp returnType fc (BAnd Int64Type) [x, y] = assembleExprBinaryOp returnType ILong land x y
    assembleExprOp returnType fc (BOr Int64Type) [x, y] = assembleExprBinaryOp returnType ILong lor x y
    assembleExprOp returnType fc (BXOr Int64Type) [x, y] = assembleExprBinaryOp returnType ILong lxor x y

    assembleExprOp returnType fc (Neg IntegerType) [x] =
        let op = invokeMethod InvokeVirtual "java/math/BigInteger" "negate" "()Ljava/math/BigInteger;" False
        in assembleExprUnaryOp returnType inferredBigIntegerType op x
    assembleExprOp returnType fc (ShiftR IntegerType) [x, y] = do
        let op = do
            invokeMethod InvokeVirtual "java/math/BigInteger" "intValueExact" "()I" False
            invokeMethod InvokeVirtual "java/math/BigInteger" "shiftRight" "(I)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y
    assembleExprOp returnType fc (BAnd IntegerType) [x, y] = do
        let op = invokeMethod InvokeVirtual "java/math/BigInteger" "and"
                    "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y
    assembleExprOp returnType fc (BOr IntegerType) [x, y] = do
        let op = invokeMethod InvokeVirtual "java/math/BigInteger" "or"
                    "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y
    assembleExprOp returnType fc (BXOr IntegerType) [x, y] = do
        let op = invokeMethod InvokeVirtual "java/math/BigInteger" "xor"
                    "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y

    assembleExprOp returnType fc (Add DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble dadd x y
    assembleExprOp returnType fc (Sub DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble dsub x y
    assembleExprOp returnType fc (Mul DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble dmul x y
    assembleExprOp returnType fc (Div DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble ddiv x y
    assembleExprOp returnType fc (Neg DoubleType) [x] = assembleExprUnaryOp returnType IDouble dneg x

    assembleExprOp returnType fc (Add ty) [x, y] =
      assembleExprBinaryOp returnType (getInferredType ty) (add (jIntKind ty)) x y
    assembleExprOp returnType fc (Sub ty) [x, y] =
      assembleExprBinaryOp returnType (getInferredType ty) (sub (jIntKind ty)) x y
    assembleExprOp returnType fc (Mul ty) [x, y] =
      assembleExprBinaryOp returnType (getInferredType ty) (mul (jIntKind ty)) x y
    assembleExprOp returnType fc (Div ty) [x, y] =
      assembleExprBinaryOp returnType (getInferredType ty) (div (jIntKind ty)) x y
    assembleExprOp returnType fc (Mod ty) [x, y] =
      assembleExprBinaryOp returnType (getInferredType ty) (mod (jIntKind ty)) x y
    assembleExprOp returnType fc (Neg ty) [x] = assembleExprUnaryOp returnType IInt ineg x

    assembleExprOp returnType fc (ShiftL ty) [x, y] =
      assembleExprBinaryOp returnType (getInferredType ty) (shl (jIntKind ty)) x y
    assembleExprOp returnType fc (ShiftR Bits32Type) [x, y] = assembleExprBinaryOp returnType IInt iushr x y
    assembleExprOp returnType fc (ShiftR ty) [x, y] = assembleExprBinaryOp returnType IInt ishr x y
    assembleExprOp returnType fc (BAnd ty) [x, y] = assembleExprBinaryOp returnType IInt iand x y
    assembleExprOp returnType fc (BOr ty) [x, y] = assembleExprBinaryOp returnType IInt ior x y
    assembleExprOp returnType fc (BXOr ty) [x, y] = assembleExprBinaryOp returnType IInt ixor x y

    assembleExprOp returnType fc (LT DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do dcmpg; ifge label) x y
    assembleExprOp returnType fc (LTE DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do dcmpg; ifgt label) x y
    assembleExprOp returnType fc (EQ DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do dcmpl; ifne label) x y
    assembleExprOp returnType fc (GTE DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do dcmpl; iflt label) x y
    assembleExprOp returnType fc (GT DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do dcmpl; ifle label) x y

    assembleExprOp returnType fc (LT IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass ifge x y
    assembleExprOp returnType fc (LTE IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass ifgt x y
    assembleExprOp returnType fc (EQ IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass ifne x y
    assembleExprOp returnType fc (GTE IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass iflt x y
    assembleExprOp returnType fc (GT IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass ifle x y

    assembleExprOp returnType fc (LT StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass ifge x y
    assembleExprOp returnType fc (LTE StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass ifgt x y
    assembleExprOp returnType fc (EQ StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass ifne x y
    assembleExprOp returnType fc (GTE StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass iflt x y
    assembleExprOp returnType fc (GT StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass ifle x y

    assembleExprOp returnType fc (LT Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong ifge) x y
    assembleExprOp returnType fc (LTE Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong ifgt) x y
    assembleExprOp returnType fc (EQ Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong ifne) x y
    assembleExprOp returnType fc (GTE Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong iflt) x y
    assembleExprOp returnType fc (GT Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong ifle) x y

    assembleExprOp returnType fc (LT Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong ifge) x y
    assembleExprOp returnType fc (LTE Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong ifgt) x y
    assembleExprOp returnType fc (EQ Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong ifne) x y
    assembleExprOp returnType fc (GTE Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong iflt) x y
    assembleExprOp returnType fc (GT Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong ifle) x y

    assembleExprOp returnType fc (LT Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt ifge) x y
    assembleExprOp returnType fc (LTE Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt ifgt) x y
    assembleExprOp returnType fc (EQ Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt ifne) x y
    assembleExprOp returnType fc (GTE Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt iflt) x y
    assembleExprOp returnType fc (GT Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt ifle) x y

    assembleExprOp returnType fc (LT ty) [x, y] = assembleExprBinaryBoolOp returnType IInt ificmpge x y
    assembleExprOp returnType fc (LTE ty) [x, y] = assembleExprBinaryBoolOp returnType IInt ificmpgt x y
    assembleExprOp returnType fc (EQ ty) [x, y] = assembleExprBinaryBoolOp returnType IInt ificmpne x y
    assembleExprOp returnType fc (GTE ty) [x, y] = assembleExprBinaryBoolOp returnType IInt ificmplt x y
    assembleExprOp returnType fc (GT ty) [x, y] = assembleExprBinaryBoolOp returnType IInt ificmple x y

    assembleExprOp returnType fc StrLength [x] = do
        assembleExpr False inferredStringType x
        invokeMethod InvokeVirtual "java/lang/String" "length" "()I" False
        asmCast IInt returnType

    assembleExprOp returnType fc StrHead [x] = do
        assembleExpr False inferredStringType x
        iconst 0
        invokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
        asmCast IChar returnType

    assembleExprOp returnType fc StrTail [x] = do
        assembleExpr False inferredStringType x
        iconst 1
        invokeMethod InvokeVirtual "java/lang/String" "substring" "(I)Ljava/lang/String;" False
        asmCast inferredStringType returnType

    assembleExprOp returnType fc StrIndex [x, i] = do
        assembleExpr False inferredStringType x
        assembleExpr False IInt i
        invokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
        asmCast IChar returnType

    assembleExprOp returnType fc StrCons [x, y] = assembleStrCons returnType x y

    assembleExprOp returnType fc StrAppend [x, y] =
        let op = invokeMethod InvokeVirtual "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;" False
        in assembleExprBinaryOp returnType inferredStringType op x y

    assembleExprOp returnType fc StrReverse [x] = assembleStrReverse returnType x

    assembleExprOp returnType fc StrSubstr [offset, len, str] =do
        assembleExpr False IInt offset
        assembleExpr False IInt len
        assembleExpr False inferredStringType str
        invokeMethod InvokeStatic (getRuntimeClass "Strings") "substring"
            "(IILjava/lang/String;)Ljava/lang/String;" False
        asmCast inferredStringType returnType

    -- `e` is Euler's number, which approximates to: 2.718281828459045
    assembleExprOp returnType fc DoubleExp [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "exp" "(D)D" False -- Base is `e`. Same as: `pow(e, x)
    assembleExprOp returnType fc DoubleLog [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "log" "(D)D" False -- Base is `e`.
    assembleExprOp returnType fc DoublePow [x, y] =
        let op = invokeMethod InvokeStatic "java/lang/Math" "pow" "(DD)D" False
        in assembleExprBinaryOp returnType IDouble op x y
    assembleExprOp returnType fc DoubleSin [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "sin" "(D)D" False
    assembleExprOp returnType fc DoubleCos [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "cos" "(D)D" False
    assembleExprOp returnType fc DoubleTan [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "tan" "(D)D" False
    assembleExprOp returnType fc DoubleASin [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "asin" "(D)D" False
    assembleExprOp returnType fc DoubleACos [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "acos" "(D)D" False
    assembleExprOp returnType fc DoubleATan [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "atan" "(D)D" False
    assembleExprOp returnType fc DoubleSqrt [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "sqrt" "(D)D" False
    assembleExprOp returnType fc DoubleFloor [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "floor" "(D)D" False
    assembleExprOp returnType fc DoubleCeiling [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Core ()
        op = invokeMethod InvokeStatic "java/lang/Math" "ceil" "(D)D" False

    assembleExprOp returnType fc (Cast from to) [arg] = assembleCast returnType fc from to arg

    assembleExprOp returnType fc BelieveMe [_,_,x] = do
        assembleExpr False IUnknown x
        asmCast IUnknown returnType

    assembleExprOp returnType fc Crash [_, msg] = do
        assembleExpr False inferredStringType msg
        invokeMethod InvokeStatic runtimeClass "crash" "(Ljava/lang/String;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType

    assembleExprOp returnType fc op _ = throw $ GenericMsg fc ("Unsupported operator " ++ show op)

    assembleParameter : (NamedCExp, InferredType) -> Core ()
    assembleParameter (param, ty) = assembleExpr False ty param

    -- The `tailRecArg` temporaries are typed during inference from the
    -- argument expression, but specialised callees store them with the
    -- callee's parameter type (e.g. a primitive double unboxed from a
    -- closure result).  Record that type in the by-index map driving
    -- physical-slot translation (`opWithWordSize`) so a two-word temp
    -- shifts every later variable by two slots instead of one —
    -- otherwise the next temp's astore clobbers the double's second
    -- slot and the verifier rejects the method.
    storeParameter : Map Int InferredType -> (Int, NamedCExp, InferredType) -> Core Int
    storeParameter variableTypes (var, (NmLocal _ loc), ty) = do
        let valueVariableName = jvmSimpleName loc
        valueVariableIndex <- getVariableIndex valueVariableName
        if var /= valueVariableIndex
            then do
                valueVariableType <- getVariableType valueVariableName
                loadVar variableTypes valueVariableType ty valueVariableIndex
                targetVariableIndex <- getDynamicVariableIndex "tailRecArg"
                ignore $ coreLift $ Map.put variableTypes targetVariableIndex ty
                storeVar ty ty targetVariableIndex
                pure targetVariableIndex
            else pure var
    storeParameter variableTypes (var, param, ty) = do
        assembleExpr False ty param
        targetVariableIndex <- getDynamicVariableIndex "tailRecArg"
        ignore $ coreLift $ Map.put variableTypes targetVariableIndex ty
        storeVar ty ty targetVariableIndex
        pure targetVariableIndex

    createMethodReference : (isTailCall: Bool) -> (arity: Nat) -> Name -> Core ()
    createMethodReference isTailCall arity name = do
        let javaName = jvmName !getProgramName name
        functionType <- case !(findFunctionType javaName) of
            Just ty => pure ty
            Nothing => pure $ MkInferredFunctionType inferredObjectType $ replicate arity inferredObjectType
        let methodReturnType = InferredFunctionType.returnType functionType
        let paramTypes = parameterTypes functionType
        let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType methodReturnType paramTypes
        let functionInterface = getFunctionInterface arity
        let invokeDynamicDescriptor = getMethodDescriptor $ MkInferredFunctionType functionInterface []
        asmInvokeDynamic (className javaName) (methodName javaName) "apply" invokeDynamicDescriptor
           (getSamDesc (getLambdaTypeByArity arity)) methodDescriptor methodDescriptor
        when (arity > 1) $ do
          let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType inferredLambdaType [functionInterface]
          invokeMethod InvokeStatic functionsClass "curry" methodDescriptor False
        when isTailCall $ asmReturn inferredLambdaType

    assembleSubMethodWithScope1 : (isTailCall: Bool) -> InferredType
                                -> (parameterName : Maybe Name) -> NamedCExp -> Core ()
    assembleSubMethodWithScope1 isTailCall returnType parameterName body = do
        parentScope <- getScope !getCurrentScopeIndex
        withScope $ assembleSubMethod isTailCall returnType Nothing parameterName parentScope body

    assembleMethodReference : (isTailCall: Bool) -> InferredType
                            -> (isMethodReference : Bool) -> (arity: Nat) -> (functionName: Name)
                            -> (parameterName : Maybe Name) -> NamedCExp -> Core ()
    assembleMethodReference isTailCall returnType isMethodReference arity functionName parameterName body =
      -- A lambda expected at a typed callback interface must NOT take the
      -- method-reference shortcut: createMethodReference emits an
      -- invokedynamic against the natural `Function`, which is not an
      -- instance of the typed interface (the subtyping only goes the
      -- other way), so the value would fail the interface cast at
      -- runtime.  The seeded inference path (inferCallbackLambda) never
      -- takes the method-reference shape either, so the scope created
      -- there is consumed by the sub-method path here.
      if isMethodReference && isNothing (parseCallbackIfaceType returnType)
        then createMethodReference isTailCall arity functionName
        else assembleSubMethodWithScope1 isTailCall returnType parameterName body

    assembleSubMethodWithScope : (isTailCall: Bool) -> InferredType
                               -> (parameterValue: Maybe NamedCExp) -> (parameterName : Maybe Name)
                               -> NamedCExp -> Core ()
    assembleSubMethodWithScope isTailCall returnType (Just value) (Just name) body = do
        parentScope <- getScope !getCurrentScopeIndex
        let shouldGenerateVariable = name == extractedMethodArgumentName
        parameterValueVariable <-
            if shouldGenerateVariable
                then pure $ jvmSimpleName name ++ show !newDynamicVariableIndex
                else pure $ jvmSimpleName name
        let parameterValueVariableName = UN $ Basic parameterValueVariable
        withScope $ assembleSubMethod isTailCall returnType (Just (assembleValue parentScope parameterValueVariable))
            (Just parameterValueVariableName) parentScope
            (substituteVariableSubMethodBody (NmLocal (getFC body) parameterValueVariableName) body)
      where
          assembleValue : Scope -> String -> Core ()
          assembleValue enclosingScope variableName = do
            lambdaScopeIndex <- getCurrentScopeIndex
            updateCurrentScopeIndex (index enclosingScope)
            assembleExpr False !(getVariableTypeAtScope lambdaScopeIndex variableName) value
            updateCurrentScopeIndex lambdaScopeIndex

    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmLam _ p4 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1,
        NmLocal _ arg2, NmLocal _ arg3, NmLocal _ arg4]))))) = do
          isMethodReference <- canUseMethodReference name [arg0, arg1, arg2, arg3, arg4] p0 [p1, p2, p3, p4]
          assembleMethodReference isTailCall returnType isMethodReference 5 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2,
        NmLocal _ arg3])))) = do
          isMethodReference <- canUseMethodReference name [arg0, arg1, arg2, arg3] p0 [p1, p2, p3]
          assembleMethodReference isTailCall returnType isMethodReference 4 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmLam _ p2 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2]))) = do
        isMethodReference <- canUseMethodReference name [arg0, arg1, arg2] p0 [p1, p2]
        assembleMethodReference isTailCall returnType isMethodReference 3 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1])) = do
        isMethodReference <- canUseMethodReference name [arg0, arg1] p0 [p1]
        assembleMethodReference isTailCall returnType isMethodReference 2 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0 body@(NmApp _ (NmRef _ name) [NmLocal _ arg0]) = do
      isMethodReference <- canUseMethodReference name [arg0] p0 []
      assembleMethodReference isTailCall returnType isMethodReference 1 name p0 body

    assembleSubMethodWithScope isTailCall returnType _ parameterName body =
      assembleSubMethodWithScope1 isTailCall returnType parameterName body

    assembleSubMethod : (isTailCall: Bool) -> InferredType -> (parameterValueExpr: Maybe (Core ()))
                      -> (parameterName: Maybe Name) -> Scope -> NamedCExp -> Core ()
    assembleSubMethod isTailCall lambdaReturnType parameterValueExpr parameterName declaringScope expr = do
        scope <- getScope !getCurrentScopeIndex
        maybe (pure ()) (setScopeCounter . succ) (parentIndex scope)
        let lambdaBodyReturnType = returnType scope
        let lambdaType = getLambdaTypeByParameter parameterName
        when (lambdaType == DelayedLambda) $ do
            new "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed"
            dup
        let isExtracted = isJust parameterValueExpr
        -- Higher-order specialisation: a real lambda whose expected type
        -- is a typed callback interface is created against that interface
        -- — typed SAM descriptor, primitive-typed implementation method.
        -- The parameter types below come from the scope that
        -- inferCallbackLambda seeded with the signature's parameter
        -- types, so the descriptors here and inference agree.  An
        -- arity-2 signature comes with the nested two-lambda shape
        -- (inference enforced it via sigMatchesLambdaShape): the inner
        -- lambda dissolves into a second parameter of the SAME
        -- implementation method and the body is the inner lambda's body.
        let mCallbackSig = if isExtracted || lambdaType /= FunctionLambda
                             then Nothing
                             else parseCallbackIfaceType lambdaReturnType
        -- The interface class file must exist in the output whenever a
        -- lambda is created against it — including at sites that never
        -- earn a spec (decl-driven or lambda-derived signatures).  The
        -- assembler dedups by class name, so this is idempotent.
        case mCallbackSig of
          Just sig => do
            ifaceName <- getJvmReferenceTypeName lambdaReturnType
            createIdrisFunctionInterface ifaceName (getMethodDescriptor sig)
          Nothing => pure ()
        (parameterNames, bodyExpr) <- the (Core (List Name, NamedCExp)) $
            case (mCallbackSig, expr) of
              (Just sig, NmLam _ p2 inner) =>
                if length sig.parameterTypes == 2
                  then pure (toList parameterName ++ [p2], inner)
                  else pure (toList parameterName, expr)
              (Just sig, _) =>
                if length sig.parameterTypes == 2
                  then asmCrash "Arity-2 callback lambda without the nested two-lambda shape"
                  else pure (toList parameterName, expr)
              _ => pure (toList parameterName, expr)
        let lambdaInterfaceType = maybe (getLambdaInterfaceType lambdaType) (const lambdaReturnType) mCallbackSig
        parameterTypes <- traverse (getVariableType . jvmSimpleName) parameterNames
        variableTypes <- coreLift $ Map.values {key=Int} !(loadClosures declaringScope scope)
        maybe (pure ()) id parameterValueExpr
        let invokeDynamicDescriptor = getMethodDescriptor $ MkInferredFunctionType lambdaInterfaceType variableTypes
        let implementationMethodReturnType =
            if isExtracted
              then lambdaBodyReturnType
              else maybe (getLambdaImplementationMethodReturnType lambdaType) (.returnType) mCallbackSig
        let implementationMethodDescriptor =
            getMethodDescriptor $
                MkInferredFunctionType implementationMethodReturnType (variableTypes ++ parameterTypes)
        let methodPrefix = if isExtracted then "extr" else "lambda"
        lambdaClassMethodName <- getLambdaImplementationMethodName methodPrefix
        let lambdaMethodName = methodName lambdaClassMethodName
        let lambdaClassName = className lambdaClassMethodName
        let interfaceMethodName = getLambdaInterfaceMethodName lambdaType
        let indy = the (Core ()) $ do
            let instantiatedMethodDescriptor = getMethodDescriptor $
                MkInferredFunctionType implementationMethodReturnType parameterTypes
            asmInvokeDynamic lambdaClassName lambdaMethodName interfaceMethodName invokeDynamicDescriptor
                (maybe (getSamDesc lambdaType) getMethodDescriptor mCallbackSig)
                implementationMethodDescriptor instantiatedMethodDescriptor
            when (lambdaType == DelayedLambda) $
                invokeMethod InvokeSpecial "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "<init>"
                    "(Lio/github/mmhelloworld/idrisjvm/runtime/Delayed;)V" False
        let staticCall = do
             invokeMethod InvokeStatic lambdaClassName lambdaMethodName implementationMethodDescriptor False
             asmCast lambdaBodyReturnType lambdaReturnType
        maybe indy (const staticCall) parameterValueExpr
        when isTailCall $ if isExtracted then asmReturn lambdaReturnType else asmReturn lambdaInterfaceType
        let oldLineNumberLabels = lineNumberLabels !getState
        newLineNumberLabels <- coreLift $ Map.newTreeMap {key=Int} {value=String}
        updateState $ { lineNumberLabels := newLineNumberLabels }
        let accessModifiers = if isExtracted then [Public, Static] else [Public, Static, Synthetic]
        createMethod accessModifiers "" lambdaClassName lambdaMethodName implementationMethodDescriptor
            Nothing Nothing [] []
        methodCodeStart
        let labelStart = methodStartLabel
        let labelEnd = methodEndLabel
        addLambdaStartLabel scope labelStart
        maybe (pure ()) (\parentScopeIndex => updateScopeStartLabel parentScopeIndex labelStart) (parentIndex scope)
        let lambdaReturnType =
            if isExtracted
              then lambdaBodyReturnType
              else maybe inferredObjectType (.returnType) mCallbackSig
        assembleExpr True lambdaReturnType bodyExpr
        addLambdaEndLabel scope labelEnd
        maybe (pure ()) (\parentScopeIndex => updateScopeEndLabel parentScopeIndex labelEnd) (parentIndex scope)
        addLocalVariables $ fromMaybe (index scope) (parentIndex scope)
        maxStackAndLocal (-1) (-1)
        methodCodeEnd
        updateState $ { lineNumberLabels := oldLineNumberLabels }
      where
        addLambdaStartLabel : Scope -> String -> Core ()
        addLambdaStartLabel scope label = do
            let scopeIndex = index scope
            let lineNumberStart = fst $ lineNumbers scope
            createLabel label
            labelStart label
            addLineNumber lineNumberStart label
            updateScopeStartLabel scopeIndex label

        addLambdaEndLabel : Scope -> String -> Core ()
        addLambdaEndLabel scope label = do
            let scopeIndex = index scope
            let lineNumberEnd = snd $ lineNumbers scope
            createLabel label
            labelStart label
            updateScopeEndLabel scopeIndex label

        readSourceTargetType : Maybe (Entry InferredType InferredType) -> IO (InferredType, InferredType)
        readSourceTargetType Nothing = pure (IUnknown, IUnknown)
        readSourceTargetType (Just entry) = Entry.toTuple {k=InferredType} {v=InferredType} entry

        loadVariables : Map Int InferredType -> Map Int (Entry InferredType InferredType) -> List Int -> Core ()
        loadVariables _ _ [] = pure ()
        loadVariables declaringScopeVariableTypes types (var :: vars) = do
            sourceTargetTypeEntry <- coreLift $ Map.get types var
            (sourceType, targetType) <- coreLift $ readSourceTargetType $ nullableToMaybe sourceTargetTypeEntry
            loadVar declaringScopeVariableTypes sourceType targetType var
            loadVariables declaringScopeVariableTypes types vars

        loadClosures : Scope -> Scope -> Core (Map Int InferredType)
        loadClosures declaringScope currentScope = case parentIndex currentScope of
                Just parentScopeIndex => do
                    parentScope <- getScope parentScopeIndex
                    variableNames <- coreLift $ Map.keys {value=Int} $ variableIndices parentScope
                    variableNameAndIndex <- traverse getVariableNameAndIndex variableNames
                    typesByIndex <- getIndexAndType variableNameAndIndex
                    declaringScopeVariableTypes <- getVariableTypesAtScope (index declaringScope)
                    indices <- coreLift $ Map.keys {value=Entry InferredType InferredType} typesByIndex
                    loadVariables declaringScopeVariableTypes typesByIndex indices
                    coreLift $ Map.getValue2 {k=Int} {v1=InferredType} {v2=InferredType} typesByIndex
                Nothing => coreLift $ Map.newTreeMap {key=Int} {value=InferredType}
            where
                getVariableNameAndIndex : String -> Core (String, Int)
                getVariableNameAndIndex name = do
                    variableIndex <- getVariableIndexAtScope (index declaringScope) name
                    pure (name, variableIndex)

                getIndexAndType : List (String, Int) -> Core (Map Int (Entry InferredType InferredType))
                getIndexAndType nameAndIndices = do
                    typesByIndexMap <- coreLift $ Map.newTreeMap {key=Int} {value=Entry InferredType InferredType}
                    go typesByIndexMap
                    pure typesByIndexMap
                  where
                    go : Map Int (Entry InferredType InferredType) -> Core ()
                    go typesByIndexMap = go1 nameAndIndices where
                        go1 : List (String, Int) -> Core ()
                        go1 [] = pure ()
                        go1 ((name, varIndex) :: rest) = do
                            targetType <- getVariableType name
                            sourceType <- getVariableTypeAtScope (index declaringScope) name
                            entry <- coreLift $ Entry.new sourceType targetType
                            _ <- coreLift $ Map.put typesByIndexMap varIndex entry
                            go1 rest

    assembleSwitch : (returnType: InferredType) -> FC -> Maybe (Core ())
                   -> (mapper: a -> Core (String, Int, a)) -> (caseAssembler: (String, Int, a, String) -> Core ())
                   -> List a -> Maybe NamedCExp -> Core ()
    assembleSwitch _ fc _ _ _ [] _ = throw $ GenericMsg fc "Empty cases"
    assembleSwitch returnType fc exprAsm caseIntMapper caseAssembler (alt :: rest) def = do
      fromMaybe (pure ()) exprAsm
      switchCases <- getCasesWithLabels caseIntMapper (alt ::: rest)
      let labels = fst <$> switchCases
      let exprs = second <$> switchCases
      traverse_ createLabel (toList labels)
      defaultLabel <- createDefaultLabel
      assembleBranch defaultLabel labels exprs
      let switchCasesWithEndLabel = getSwitchCasesWithEndLabel switchCases labels
      traverse_ caseAssembler $ toList switchCasesWithEndLabel
      maybe (assembleMissingDefault returnType fc defaultLabel) (assembleDefault returnType defaultLabel) def

    assembleConstantSwitch : (returnType: InferredType) -> (switchExprType: InferredType) -> FC -> NamedCExp
                           -> List NamedConstAlt -> Maybe NamedCExp -> Core ()
    assembleConstantSwitch returnType IInt fc sc alts def = do
        let switchExprAsm = Just $ assembleExpr False IInt sc
        let caseIntMapper = constantAltIntExpr fc
        assembleSwitch returnType fc switchExprAsm caseIntMapper assembleExprConstAlt alts def
      where
        assembleExprConstAlt : (String, Int, NamedConstAlt, String) -> Core ()
        assembleExprConstAlt (labelStart, _, (MkNConstAlt _ expr), labelEnd) =
            assembleCaseWithScope labelStart labelEnd (assembleExpr True returnType expr)

    assembleConstantSwitch returnType constantType fc sc alts def = do
        hashPositionAndAlts <- traverse (constantAltHashCodeExpr fc) $
            zip [0 .. the Int $ cast $ length $ drop 1 alts] alts
        let positionAndAltsByHash = multiValueMap fst snd hashPositionAndAlts
        hashCodeSwitchCases <- getHashCodeCasesWithLabels positionAndAltsByHash
        let labels = fst <$> hashCodeSwitchCases
        let exprs = second <$> hashCodeSwitchCases
        switchEndLabel <- newLabel
        createLabel switchEndLabel
        traverse_ createLabel (toList labels)
        assembleExpr False constantType sc
        constantExprVariableSuffixIndex <- newDynamicVariableIndex
        let constantExprVariableName = "constantCaseExpr" ++ show constantExprVariableSuffixIndex
        constantExprVariableIndex <- getVariableIndex constantExprVariableName
        hashCodePositionVariableSuffixIndex <- newDynamicVariableIndex
        let hashCodePositionVariableName = "hashCodePosition" ++ show hashCodePositionVariableSuffixIndex
        hashCodePositionVariableIndex <- getVariableIndex hashCodePositionVariableName
        storeVar constantType constantType constantExprVariableIndex
        constantClass <- getHashCodeSwitchClass fc constantType
        iconst (-1)
        storeVar IInt IInt hashCodePositionVariableIndex
        loadVar !getVariableTypes constantType constantType constantExprVariableIndex
        let isLong = constantClass == "java/lang/Long"
        let invocationType = if isLong then InvokeStatic else InvokeVirtual
        let signature = if isLong then "(J)I" else "()I"
        invokeMethod invocationType constantClass "hashCode" signature False
        lookupSwitch switchEndLabel labels exprs
        traverse_
            (assembleHashCodeSwitchCases fc constantClass constantExprVariableIndex hashCodePositionVariableIndex
                switchEndLabel)
            (toList hashCodeSwitchCases)
        scope <- getScope !getCurrentScopeIndex
        let lineNumberStart = fst $ lineNumbers scope
        labelStart switchEndLabel
        addLineNumber lineNumberStart switchEndLabel
        assembleConstantSwitch returnType IInt fc (NmLocal fc $ UN $ Basic hashCodePositionVariableName)
            (hashPositionSwitchAlts hashPositionAndAlts) def
      where
          constantAltHashCodeExpr : FC
                                  -> (Int, NamedConstAlt) -> Core (Int, Int, NamedConstAlt)
          constantAltHashCodeExpr fc positionAndAlt@(position, MkNConstAlt constant _) = case hashCode constant of
              Just hashCodeValue => pure (hashCodeValue, position, snd positionAndAlt)
              Nothing => asmCrash ("Constant " ++ show constant ++ " cannot be compiled to 'Switch'.")

          hashPositionSwitchAlts : List (Int, Int, NamedConstAlt) -> List NamedConstAlt
          hashPositionSwitchAlts exprPositionAlts = reverse $ go [] exprPositionAlts where
              go : List NamedConstAlt -> List (Int, Int, NamedConstAlt) -> List NamedConstAlt
              go acc [] = acc
              go acc ((_, position, (MkNConstAlt _ expr)) :: alts) =
                  go (MkNConstAlt (I position) expr :: acc) alts

          assembleHashCodeSwitchCases : FC -> String -> Int -> Int
                                      -> String -> (String, Int, List (Int, NamedConstAlt)) -> Core ()
          assembleHashCodeSwitchCases fc _ _ _ _ (_, _, []) = throw $ GenericMsg fc "Empty cases"
          assembleHashCodeSwitchCases fc constantClass constantExprVariableIndex hashCodePositionVariableIndex
              switchEndLabel (label, _, positionAndAlts) = go label positionAndAlts where

                  {-
                  Returns whether the comparison is using comparator or "equals". Comparators return 0 when the
                  values are equal but `equals` returns boolean (1 being true in bytecode) so the bytecode condition
                  following the comparison should be `ifne` for comparator but `ifeq` for `equals`.
                  Currently only for `long`, comparator is used. For String and BigInteger, `equals` is used.
                  -}
                  isComparator : String -> Bool
                  isComparator constantClass = constantClass == "java/lang/Long"

                  compareConstant : String -> Core ()
                  compareConstant "java/lang/Long" = lcmp
                  compareConstant "java/lang/String" =
                    invokeMethod InvokeVirtual stringClass "equals" "(Ljava/lang/Object;)Z" False
                  compareConstant "java/math/BigInteger" =
                    invokeMethod InvokeVirtual bigIntegerClass "equals" "(Ljava/lang/Object;)Z" False
                  compareConstant clazz = asmCrash ("Unknown constant class " ++ clazz ++ " for switch")

                  switchBody : String -> String -> Int -> NamedConstAlt -> Core ()
                  switchBody label nextLabel position (MkNConstAlt constant _) = do
                    scope <- getScope !getCurrentScopeIndex
                    let lineNumberStart = fst $ lineNumbers scope
                    labelStart label
                    addLineNumber lineNumberStart label
                    loadVar !getVariableTypes constantType constantType constantExprVariableIndex
                    assembleHashCodeSwitchConstant fc constant
                    compareConstant constantClass
                    let condition = if isComparator constantClass then ifne else ifeq
                    condition nextLabel
                    iconst position
                    storeVar IInt IInt hashCodePositionVariableIndex
                    goto switchEndLabel

                  go : String -> List (Int, NamedConstAlt) -> Core ()
                  go _ [] = pure ()
                  go label ((position, alt) :: []) = switchBody label switchEndLabel position alt
                  go label ((position, alt) :: positionAndAlts) = do
                      nextLabel <- newLabel
                      switchBody label nextLabel position alt
                      go nextLabel positionAndAlts

    assembleConCase : InferredType -> FC -> (sc : NamedCExp) -> List NamedConAlt -> Maybe NamedCExp -> Core ()
    assembleConCase returnType fc sc alts def = do
        idrisObjectVariableIndex <- assembleConstructorSwitchExpr sc
        variableTypes <- getVariableTypes
        optTy <- coreLift $ Map.get variableTypes idrisObjectVariableIndex
        let idrisObjectVariableType = fromMaybe IUnknown $ nullableToMaybe optTy
        let isIdrisObject = idrisObjectVariableType /= IUnknown && idrisObjectVariableType /= inferredObjectType
        let targetType = if (not isIdrisObject) then idrisObjectType else idrisObjectVariableType
        loadVar variableTypes idrisObjectVariableType targetType idrisObjectVariableIndex
        when (not isIdrisObject) $ do
            storeVar idrisObjectType idrisObjectType idrisObjectVariableIndex
            loadVar !getVariableTypes idrisObjectType idrisObjectType idrisObjectVariableIndex
        let hasTypeCase = any isTypeCase alts
        let constructorIdType = if hasTypeCase then "Ljava/lang/String;" else "I"
        let constructorGetter = if hasTypeCase then "getStringConstructorId" else "getConstructorId"
        invokeMethod InvokeInterface idrisObjectClass constructorGetter ("()" ++ constructorIdType) True
        if hasTypeCase
            then assembleStringConstructorSwitch returnType fc idrisObjectVariableIndex alts def
            else assembleConstructorSwitch returnType fc idrisObjectVariableIndex alts def

    -- When the discriminant's static type already pins down a specialised
    -- constructor class (`idrisObjectVariableType` is an IRef whose class
    -- equals some `specClassName` in the plan), use that spec's slot types
    -- and class name — pattern matching then extracts primitive fields via
    -- typed accessors (`getInt0()I`, …) instead of the boxed
    -- `getProperty(I)Object`.  Otherwise fall back to the natural class.
    findConSpecByClass : Name -> String -> ConSpecialisationPlan -> Maybe SpecialisedConstructor
    findConSpecByClass name className plan =
      do entries <- SortedMap.lookup name plan
         find (\sc => sc.specClassName == className) entries

    isSingleConstructor : ConInfo -> Bool
    isSingleConstructor RECORD = True
    isSingleConstructor UNIT   = True
    isSingleConstructor _      = False

    assembleConCaseExpr : InferredType -> Name -> ConInfo -> Int -> List Name -> NamedCExp -> Core ()
    assembleConCaseExpr returnType name conInfo idrisObjectVariableIndex args expr = do
            variableTypes <- getVariableTypes
            optTy <- coreLift $ Map.get variableTypes idrisObjectVariableIndex
            let idrisObjectVariableType = fromMaybe IUnknown $ nullableToMaybe optTy
            programName <- getProgramName
            let naturalType@(IRef naturalClassName _ _) = getConstructorType programName name conInfo
                  | _ => asmCrash "Not a reference type for Idris constructor \{show name}"
            conPlan <- getConSpecialisationPlan
            natLive <- getNaturalConsLive
            let entries = fromMaybe [] $ SortedMap.lookup name conPlan
            -- Pick a spec class for the discriminant when one is statically provable:
            --   (a) variable type is already an IRef of a spec class for this Name; or
            --   (b) The constructor belongs to a single-constructor type
            --       (`RECORD`/`UNIT`), `name` has exactly one spec entry, and
            --       the natural class is dead (no construction site produced
            --       an instance not covered by a spec).  Every runtime value
            --       must be that spec instance, so an explicit `checkcast`
            --       from `Object`/`IdrisObject` to the spec class is safe.
            --
            -- The single-constructor guard mirrors `inferConCaseExpr`'s — a
            -- sibling constructor's slot value could otherwise leak into the
            -- typed-accessor path at runtime, and the field's static slot
            -- type would not match the value's real shape.
            let uniqueSafe : Maybe SpecialisedConstructor
                uniqueSafe = if not (isSingleConstructor conInfo)
                              then Nothing
                              else case entries of
                                     [sc] => if SortedSet.contains name natLive
                                                then Nothing
                                                else Just sc
                                     _    => Nothing
            let mSpecByClass : Maybe SpecialisedConstructor
                mSpecByClass = case idrisObjectVariableType of
                                 IRef varClassName _ _ => findConSpecByClass name varClassName conPlan
                                 _ => Nothing
            -- (c) Family-interface dispatch: the discriminant is statically
            --     typed as a TCon family-instantiation interface (e.g.
            --     `Maybe$I`, `Run$F`).  `findUniqueFamilyMember` requires
            --     the candidate to be the UNIQUE entry of this constructor
            --     that can inhabit the interface (distinct slot shapes can
            --     share an instantiation — `A : a -> a -> T a` observed
            --     `[I,L]` and `[L,I]` are both `T$F$I` — and
            --     under-determined sibling shapes implement every
            --     instantiation).  The checkcast is sound PROVIDED the
            --     natural class is also dead:
            --     `computeNaturalToTConIfaces` teaches live natural
            --     siblings to `implements` every active family
            --     instantiation, so a `Maybe$I`-typed value may hold a
            --     natural `Just` when `Just` is natural-live, and a
            --     checkcast to `JUST$I` would throw.
            let mSpecByFamily : Maybe SpecialisedConstructor
                mSpecByFamily = case idrisObjectVariableType of
                                  IRef varClassName _ _ =>
                                    if SortedSet.contains name natLive
                                      then Nothing
                                      else findUniqueFamilyMember conPlan name varClassName
                                  _ => Nothing
            let mSpec : Maybe SpecialisedConstructor
                mSpec = mSpecByClass <|> mSpecByFamily <|> uniqueSafe
            let constructorClassName : String
                constructorClassName = maybe naturalClassName specClassName mSpec
            let constructorType : InferredType
                constructorType = maybe naturalType (\sc => IRef sc.specClassName Class []) mSpec
            let mSlotTypes : Maybe (List InferredType)
                mSlotTypes = (\sc => sc.fieldTypes) <$> mSpec
            -- When `uniqueSafe` or `mSpecByFamily` selected the spec, the
            -- discriminant's static type doesn't equal `constructorType` but
            -- the cast is provably safe — force the typed-accessor path.
            let forceCast = isJust uniqueSafe || isJust mSpecByFamily
            bindArg forceCast constructorClassName constructorType mSlotTypes idrisObjectVariableType variableTypes 0 args
            assembleExpr True returnType expr
        where

          bindArg : Bool -> String -> InferredType -> Maybe (List InferredType)
                 -> InferredType -> Map Int InferredType -> Int -> List Name -> Core ()
          bindArg _ _ _ _ _ _ _ [] = pure ()
          bindArg forceCast constructorClassName constructorType mSlotTypes idrisObjectVariableType variableTypes index (var :: vars) = do
              let variableName = jvmSimpleName var
              when (used variableName expr) $ do
                -- Only emit a direct invokevirtual on the constructor's class
                -- when we're sure the discriminant has that exact class — i.e.
                -- the variable's inferred type matches `constructorType`, or a
                -- global analysis (`uniqueSafe` above) has proved an explicit
                -- checkcast is sound.  Otherwise fall back to IdrisObject
                -- interface dispatch: foreign FFI methods can return runtime-
                -- defined IdrisObject impls (e.g. Strings$CharacterUnconsResult
                -- for UnconsResult.Character) whose class differs from the
                -- Idris-generated constructor class the compiler would
                -- otherwise cast to.
                let safeToCast = forceCast || idrisObjectVariableType == constructorType
                let mSlotType : Maybe InferredType
                    mSlotType = do slots <- mSlotTypes
                                   case drop (cast index) slots of
                                     (ty :: _) => Just ty
                                     [] => Nothing
                if safeToCast
                  then case mSlotType of
                    -- Spec class with a known slot type → typed accessor.
                    -- Slot type is set during inference (`inferConCaseExpr`
                    -- in Optimizer.idr) so `updateScopeVariableTypes` syncs
                    -- it into every scope's `allVariableTypes` and the
                    -- body's `NmLocal` load uses the right instruction.
                    Just slotTy => do
                      loadVar variableTypes idrisObjectVariableType constructorType idrisObjectVariableIndex
                      invokeMethod InvokeVirtual constructorClassName
                        (specConAccessorName slotTy index)
                        ("()" ++ getJvmTypeDescriptor slotTy) False
                      variableIndex <- getVariableIndex variableName
                      storeVar slotTy slotTy variableIndex
                    -- Non-spec class → existing boxed getProperty.
                    Nothing => do
                      loadVar variableTypes idrisObjectVariableType constructorType idrisObjectVariableIndex
                      iconst index
                      invokeMethod InvokeVirtual constructorClassName "getProperty" "(I)Ljava/lang/Object;" False
                      variableIndex <- getVariableIndex variableName
                      storeVar inferredObjectType !(getVariableType variableName) variableIndex
                  else do
                    loadVar variableTypes idrisObjectVariableType idrisObjectType idrisObjectVariableIndex
                    iconst index
                    invokeMethod InvokeInterface idrisObjectClass "getProperty" "(I)Ljava/lang/Object;" True
                    variableIndex <- getVariableIndex variableName
                    storeVar inferredObjectType !(getVariableType variableName) variableIndex
              bindArg forceCast constructorClassName constructorType mSlotTypes idrisObjectVariableType variableTypes (index + 1) vars

    assembleConstructorSwitch : InferredType -> FC -> Int -> List NamedConAlt -> Maybe NamedCExp -> Core ()
    assembleConstructorSwitch _ fc _ [] _ = throw $ GenericMsg fc "Empty cases"
    assembleConstructorSwitch returnType fc idrisObjectVariableIndex alts def = do
        assembleSwitch returnType fc Nothing conAltIntExpr assembleExprConAlt alts def
      where
        assembleExprConAlt : (String, Int, NamedConAlt, String) -> Core ()
        assembleExprConAlt (labelStart, _, (MkNConAlt name conInfo _ args expr), labelEnd) =
            assembleCaseWithScope labelStart labelEnd
              (assembleConCaseExpr returnType name conInfo idrisObjectVariableIndex args expr)

    assembleStringConstructorSwitch : InferredType -> FC -> Int -> List NamedConAlt -> Maybe NamedCExp -> Core ()
    assembleStringConstructorSwitch returnType fc idrisObjectVariableIndex alts def = do
        constantExprVariableSuffixIndex <- newDynamicVariableIndex
        let constantExprVariableName = "constructorCaseExpr" ++ show constantExprVariableSuffixIndex
        constantExprVariableIndex <- getVariableIndex constantExprVariableName
        storeVar inferredStringType inferredStringType constantExprVariableIndex
        hashCodePositionVariableSuffixIndex <- newDynamicVariableIndex
        let hashCodePositionVariableName = "hashCodePosition" ++ show hashCodePositionVariableSuffixIndex
        hashCodePositionVariableIndex <- getVariableIndex hashCodePositionVariableName
        hashPositionAndAlts <- traverse (conAltHashCodeExpr fc) $
            zip [0 .. the Int $ cast $ length $ drop 1 alts] alts
        let positionAndAltsByHash = multiValueMap fst snd hashPositionAndAlts
        hashCodeSwitchCases <- getHashCodeCasesWithLabels positionAndAltsByHash
        let labels = fst <$> hashCodeSwitchCases
        let exprs = second <$> hashCodeSwitchCases
        switchEndLabel <- newLabel
        createLabel switchEndLabel
        traverse_ createLabel (toList labels)
        let constantType = inferredStringType
        constantClass <- getHashCodeSwitchClass fc constantType
        iconst (-1)
        storeVar IInt IInt hashCodePositionVariableIndex
        loadVar !getVariableTypes constantType constantType constantExprVariableIndex
        invokeMethod InvokeVirtual constantClass "hashCode" "()I" False
        lookupSwitch switchEndLabel labels exprs
        traverse_
            (assembleHashCodeSwitchCases fc constantClass constantExprVariableIndex hashCodePositionVariableIndex
                switchEndLabel)
            (toList hashCodeSwitchCases)
        scope <- getScope !getCurrentScopeIndex
        let lineNumberStart = fst $ lineNumbers scope
        labelStart switchEndLabel
        addLineNumber lineNumberStart switchEndLabel
        assembleExpr False IInt (NmLocal fc $ UN $ Basic hashCodePositionVariableName)
        assembleConstructorSwitch returnType fc idrisObjectVariableIndex (hashPositionSwitchAlts hashPositionAndAlts) def
      where
        conAltHashCodeExpr : FC -> (Int, NamedConAlt) -> Core (Int, Int, NamedConAlt)
        conAltHashCodeExpr fc positionAndAlt@(position, MkNConAlt name _ _ _ _) =
            case hashCode (Str $ getIdrisConstructorClassName !getProgramName (jvmSimpleName name)) of
                Just hashCodeValue => pure (hashCodeValue, position, snd positionAndAlt)
                Nothing => asmCrash ("Constructor " ++ show name ++ " cannot be compiled to 'Switch'.")

        hashPositionSwitchAlts : List (Int, Int, NamedConAlt) -> List NamedConAlt
        hashPositionSwitchAlts exprPositionAlts = reverse $ go [] exprPositionAlts where
            go : List NamedConAlt -> List (Int, Int, NamedConAlt) -> List NamedConAlt
            go acc [] = acc
            go acc ((_, position, (MkNConAlt name conInfo _ args expr)) :: alts) =
                go (MkNConAlt name conInfo (Just position) args expr :: acc) alts

        assembleHashCodeSwitchCases : FC -> String -> Int -> Int -> String
                                    -> (String, Int, List (Int, NamedConAlt)) -> Core ()
        assembleHashCodeSwitchCases fc _ _ _ _ (_, _, []) = throw $ GenericMsg fc "Empty cases"
        assembleHashCodeSwitchCases fc constantClass constantExprVariableIndex hashCodePositionVariableIndex
            switchEndLabel (label, _, positionAndAlts) = go label positionAndAlts where

                switchBody : String -> String -> Int -> NamedConAlt -> Core ()
                switchBody label nextLabel position (MkNConAlt name _ _ _ _) = do
                  scope <- getScope !getCurrentScopeIndex
                  let lineNumberStart = fst $ lineNumbers scope
                  labelStart label
                  addLineNumber lineNumberStart label
                  loadVar !getVariableTypes inferredStringType inferredStringType constantExprVariableIndex
                  ldc $ StringConst $ getIdrisConstructorClassName !getProgramName (jvmSimpleName name)
                  invokeMethod InvokeVirtual constantClass "equals" "(Ljava/lang/Object;)Z" False
                  ifeq nextLabel
                  iconst position
                  storeVar IInt IInt hashCodePositionVariableIndex
                  goto switchEndLabel

                go : String -> List (Int, NamedConAlt) -> Core ()
                go _ [] = pure ()
                go label ((position, alt) :: []) = switchBody label switchEndLabel position alt
                go label ((position, alt) :: positionAndAlts) = do
                    nextLabel <- newLabel
                    switchBody label nextLabel position alt
                    go nextLabel positionAndAlts

    asmJavaLambda : FC -> InferredType -> NamedCExp -> NamedCExp -> NamedCExp -> Core ()
    asmJavaLambda fc returnType functionType javaInterfaceType lambda = do
        assembleExpr False inferredLambdaType lambda
        lambdaType <- getJavaLambdaType fc [functionType, javaInterfaceType, lambda]
        let samType =
          if isIoAction then {parameterTypes $= dropWorldType} lambdaType.methodType else lambdaType.methodType
        let lambdaImplementationType = updateImplementationType samType.returnType lambdaType.implementationType
        let invokeDynamicType = MkInferredFunctionType lambdaType.javaInterface [inferredLambdaType]
        let invokeDynamicDescriptor = getMethodDescriptor invokeDynamicType
        let implementationParameterTypes = lambdaImplementationType.parameterTypes
        let implementationMethodType = MkInferredFunctionType lambdaImplementationType.returnType
              (inferredLambdaType :: implementationParameterTypes)
        let implementationMethodDescriptor = getMethodDescriptor implementationMethodType
        let instantiatedMethodDescriptor = getMethodDescriptor lambdaImplementationType
        lambdaClassMethodName <- getLambdaImplementationMethodName "lambda"
        let lambdaMethodName = methodName lambdaClassMethodName
        let lambdaClassName = className lambdaClassMethodName
        asmInvokeDynamic lambdaClassName lambdaMethodName lambdaType.methodName invokeDynamicDescriptor
            (getMethodDescriptor samType) implementationMethodDescriptor instantiatedMethodDescriptor
        asmCast lambdaType.javaInterface returnType
        let accessModifiers = [Public, Static, Synthetic]
        createMethod accessModifiers "" lambdaClassName lambdaMethodName implementationMethodDescriptor
          Nothing Nothing [] []
        methodCodeStart
        aload 0
        let arity = (cast {to=Int} $ length implementationParameterTypes) + 1
        typesByIndex <- coreLift $ Map.fromList $ zip [0 .. arity - 1]
          (inferredLambdaType :: implementationParameterTypes)
        applyParameters typesByIndex 1 lambdaImplementationType.returnType implementationParameterTypes
        maxStackAndLocal (-1) (-1)
        methodCodeEnd
      where
        isIoAction : Bool
        isIoAction = Optimizer.isIoAction functionType

        dropWorldType : List InferredType -> List InferredType
        dropWorldType [] = []
        dropWorldType parameterTypes@(_ :: _) = init parameterTypes

        updateImplementationType : InferredType -> InferredFunctionType -> InferredFunctionType
        updateImplementationType IVoid functionType =
          if isIoAction
            then {returnType := IVoid, parameterTypes $= dropWorldType} functionType
            else {returnType := IVoid} functionType
        updateImplementationType _ functionType =
          if isIoAction then {parameterTypes $= dropWorldType} functionType else functionType

        applyParameter : Map Int InferredType -> (isIoApplication: Bool) -> Int -> InferredType -> Core ()
        applyParameter typesByIndex isIoApplication index parameterType = do
          loadArgument
          invokeMethod InvokeInterface "java/util/function/Function" "apply" "(Ljava/lang/Object;)Ljava/lang/Object;" True
         where
          loadArgument : Core ()
          loadArgument =
            if isIoApplication
              then do
                iconst 0
                invokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False
              else loadVar typesByIndex parameterType inferredObjectType index

        applyParameters : Map Int InferredType -> Int -> InferredType -> List InferredType -> Core ()
        applyParameters typesByIndex index returnType [] = do
          when isIoAction $ applyParameter typesByIndex True index inferredObjectType
          asmCast inferredObjectType returnType
          when (returnType == IVoid) pop
          asmReturn returnType
        applyParameters typesByIndex index returnType (ty :: rest) = do
          applyParameter typesByIndex False index ty
          when (rest /= [] || isIoAction) $ asmCast inferredObjectType inferredLambdaType
          applyParameters typesByIndex (index + 1) returnType rest

    jvmExtPrim : FC -> InferredType -> ExtPrim -> List NamedCExp -> Core ()
    jvmExtPrim fc returnType JvmInstanceMethodCall [ret, NmApp _ _ [functionNamePrimVal], fargs, world] =
      jvmExtPrim fc returnType JvmInstanceMethodCall [ret, functionNamePrimVal, fargs, world]
    jvmExtPrim _ returnType JvmInstanceMethodCall [ret, NmPrimVal fc (Str fn), fargs, world] = do
        (obj :: instanceMethodArgs) <- getFArgs fargs
            | [] => asmCrash ("JVM instance method must have at least one argument " ++ fn)
        argTypes <- traverse tySpec (map fst instanceMethodArgs)
        methodReturnType <- tySpec ret
        let (cname, mnameWithDot) = break (== '.') fn
        traverse_ assembleParameter $ zip (snd obj :: map snd instanceMethodArgs) (iref cname [] :: argTypes)
        let (_, mname) = break (/= '.') mnameWithDot
        instanceType <- tySpec $ fst obj
        let isInterfaceInvocation = isInterfaceInvocation instanceType
        let invocationType = if isInterfaceInvocation then InvokeInterface else InvokeVirtual
        let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType methodReturnType argTypes
        invokeMethod invocationType cname mname methodDescriptor isInterfaceInvocation
        asmCast methodReturnType returnType
    jvmExtPrim fc returnType JvmSuper [clazz, fargs, world] = do
      rootMethodName <- getRootMethodName
      if endsWith (methodName rootMethodName) "$ltinit$gt"
        then
          do
            IRef typeName _ _ <- tySpec clazz
              | _ => asmCrash ("super constructor should be called with a reference type but got " ++ show clazz)
            let functionNamePrimVal = NmPrimVal fc (Str (typeName ++ "." ++ "<super>"))
            jvmExtPrim fc returnType JvmStaticMethodCall [NmErased fc, functionNamePrimVal, fargs, world]
        else aconstnull
    jvmExtPrim fc returnType JvmStaticMethodCall [ret, NmApp _ _ [functionNamePrimVal], fargs, world] =
      jvmExtPrim fc returnType JvmStaticMethodCall [ret, functionNamePrimVal, fargs, world]
    jvmExtPrim _ returnType JvmStaticMethodCall [ret, NmPrimVal fc (Str fn), fargs, _] = do
        args <- getFArgs fargs
        argTypes <- traverse tySpec (map fst args)
        let (cname, mnameWithDot) = break (== '.') fn
        let (_, mname) = break (/= '.') mnameWithDot
        let isConstructor = mname == "<init>"
        when isConstructor $ do
            new cname
            dup
        let isSuper = mname == "<super>"
        when isSuper $ aload 0
        traverse_ assembleParameter $ zip (map snd args) argTypes
        methodReturnType <- if isSuper then pure IVoid else tySpec ret
        let descriptorReturnType = if isConstructor then IVoid else methodReturnType
        let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType descriptorReturnType argTypes
        let invocationType = if isConstructor || isSuper then InvokeSpecial else InvokeStatic
        let mname = if isSuper then "<init>" else mname
        invokeMethod invocationType cname mname methodDescriptor False
        asmCast methodReturnType returnType
    jvmExtPrim _ returnType SetInstanceField [ret, NmPrimVal fc (Str fn), fargs, _] = do
        (obj :: value :: []) <- getFArgs fargs
            | _ => asmCrash ("Setting an instance field should have two arguments for " ++ fn)
        fieldType <- tySpec (fst value)
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False (iref cname []) (snd obj)
        assembleExpr False fieldType (snd value)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#' && c /= '=') fnameWithDot
        field PutField cname fieldName (getJvmTypeDescriptor fieldType)
        aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType SetStaticField [ret, NmPrimVal fc (Str fn), fargs, _] = do
        (value :: []) <- getFArgs fargs
            | _ => asmCrash ("Setting a static field should have one argument for " ++ fn)
        fieldType <- tySpec (fst value)
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False fieldType (snd value)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#' && c /= '=') fnameWithDot
        field PutStatic cname fieldName (getJvmTypeDescriptor fieldType)
        aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType GetInstanceField [ret, NmPrimVal fc (Str fn), fargs, _] = do
        (obj :: []) <- getFArgs fargs
            | _ => asmCrash ("Getting an instance field should have one argument for " ++ fn)
        fieldType <- tySpec ret
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False (iref cname []) (snd obj)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#') fnameWithDot
        field GetField cname fieldName (getJvmTypeDescriptor fieldType)
        asmCast fieldType returnType
    jvmExtPrim _ returnType GetStaticField [ret, NmPrimVal fc (Str fn), fargs, _] = do
        fieldType <- tySpec ret
        let (cname, fnameWithDot) = break (== '.') fn
        let (_, fieldName) = break (\c => c /= '.' && c /= '#') fnameWithDot
        field GetStatic cname fieldName (getJvmTypeDescriptor fieldType)
        asmCast fieldType returnType
    jvmExtPrim _ returnType NewArray [_, size, val, _] = do
        assembleExpr False IInt size
        assembleExpr False IUnknown val
        invokeMethod InvokeStatic arraysClass "create" "(ILjava/lang/Object;)Ljava/util/ArrayList;" False
        asmCast arrayListType returnType
    jvmExtPrim _ returnType ArrayGet [_, arr, pos, _] = do
        assembleExpr False arrayListType arr
        assembleExpr False IInt pos
        invokeMethod InvokeVirtual arrayListClass "get" "(I)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType ArraySet [_, arr, pos, val, _] = do
        assembleExpr False arrayListType arr
        assembleExpr False IInt pos
        assembleExpr False IUnknown val
        invokeMethod InvokeVirtual arrayListClass "set" "(ILjava/lang/Object;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType JvmNewArray [tyExpr, size, _] = do
        assembleExpr False IInt size
        elemTy <- tySpec tyExpr
        assembleArray elemTy
        asmCast (IArray elemTy) returnType
    jvmExtPrim _ returnType JvmSetArray [tyExpr, index, val, arr, _] = do
        elemTy <- tySpec tyExpr
        assembleExpr False (IArray elemTy) arr
        assembleExpr False IInt index
        assembleExpr False elemTy val
        storeArray elemTy
        aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType JvmGetArray [tyExpr, index, arr, _] = do
        elemTy <- tySpec tyExpr
        assembleExpr False (IArray elemTy) arr
        assembleExpr False IInt index
        loadArray elemTy
        asmCast elemTy returnType
    jvmExtPrim _ returnType JvmArrayLength [tyExpr, arr] = do
        elemTy <- tySpec tyExpr
        assembleExpr False (IArray elemTy) arr
        arraylength
        asmCast IInt returnType
    jvmExtPrim _ returnType NewIORef [_, val, _] = do
        new refClass
        dup
        assembleExpr False IUnknown val
        invokeMethod InvokeSpecial refClass "<init>" "(Ljava/lang/Object;)V" False
        asmCast refType returnType
    jvmExtPrim _ returnType ReadIORef [_, ref, _] = do
        assembleExpr False refType ref
        invokeMethod InvokeVirtual refClass "getValue" "()Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType WriteIORef [_, ref, val, _] = do
        assembleExpr False refType ref
        assembleExpr False IUnknown val
        invokeMethod InvokeVirtual refClass "setValue" "(Ljava/lang/Object;)V" False
        aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType SysOS [] = do
        field GetStatic idrisSystemClass "OS_NAME" "Ljava/lang/String;"
        asmCast inferredStringType returnType
    jvmExtPrim _ returnType SysCodegen [] = do
        ldc $ StringConst "\"jvm\""
        asmCast inferredStringType returnType
    jvmExtPrim _ returnType VoidElim _ = do
        ldc $ StringConst "Error: Executed 'void'"
        invokeMethod InvokeStatic runtimeClass "crash" "(Ljava/lang/String;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType JvmClassLiteral [ty] = do
        assembleClassLiteral !(tySpec ty)
        asmCast (IRef "java/lang/Class" Class []) returnType
    jvmExtPrim _ returnType JvmInstanceOf [_, obj, ty] = do
        assembleExpr False IUnknown obj
        typeName <- getJvmReferenceTypeName !(tySpec ty)
        instanceOf typeName
        asmCast IBool returnType
    jvmExtPrim _ returnType JvmRefEq [_, _, firstObj, secondObj] =
      assembleExprBinaryBoolOp returnType IUnknown ifacmpne firstObj secondObj
    jvmExtPrim fc returnType JavaLambda [functionType, javaInterfaceType, lambda] =
      asmJavaLambda fc returnType functionType javaInterfaceType lambda
    jvmExtPrim _ returnType MakeFuture [_, action] = do
        assembleExpr False delayedType action
        invokeMethod InvokeStatic runtimeClass "fork" "(Lio/github/mmhelloworld/idrisjvm/runtime/Delayed;)Ljava/util/concurrent/ForkJoinTask;" False
        asmCast inferredForkJoinTaskType returnType
    jvmExtPrim _ returnType (Unknown name) _ = asmCrash $ "Can't compile unknown external directive " ++ show name
    jvmExtPrim fc _ prim args = throw $ GenericMsg fc $ "Unsupported external function " ++ show prim ++ "(" ++
        (show $ showNamedCExp 0 <$> args) ++ ")"

initializeFunctionState : {auto stateRef: Ref AsmState AsmState} -> Core ()
initializeFunctionState = do
  lineNumberLabels <- coreLift $ Map.newTreeMap {key=Int} {value=String}
  updateState $ {
      scopeCounter := 0,
      currentScopeIndex := 0,
      lambdaCounter := 0,
      labelCounter := 1,
      lineNumberLabels := lineNumberLabels }
  updateCurrentFunction $ { dynamicVariableCounter := 0 }

assembleDefinition : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState}
                   -> Name -> FC -> Core ()
assembleDefinition idrisName fc = do
    jname <- getRootMethodName
    resetScope
    loadFunction jname
    function <- getCurrentFunction
    let functionType = inferredFunctionType function
    let arity = length $ parameterTypes functionType
    let jvmClassAndMethodName = function.name
    let declaringClassName = className jvmClassAndMethodName
    let methodName = methodName jvmClassAndMethodName
    let methodReturnType = returnType functionType
    initializeFunctionState
    let optimizedExpr = optimizedBody function
    when (shouldDebugFunction jname) $
      logAsm $ "Assembling " ++ declaringClassName ++ "." ++ methodName ++
        ": " ++ show functionType ++ "\n" ++
        showNamedCExp 0 optimizedExpr
    let fileName = fst $ getSourceLocationFromFc fc
    let descriptor = getMethodDescriptor functionType
    -- Cache only top level nil arity functions. Don't cache extracted function results.
    let isField = arity == 0 && not (extractedFunctionLabel `isInfixOf` methodName)
    let classInitOrMethodName = if isField then "<clinit>" else methodName
    when isField $ do
        createField [Public, Static, Final] fileName declaringClassName methodName
            "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;" Nothing Nothing []
        fieldEnd
    createMethod [Public, Static] fileName declaringClassName classInitOrMethodName descriptor Nothing Nothing [] []
    if (not isField)
        then do
            methodCodeStart
            createLabel methodStartLabel
            createLabel methodEndLabel
            labelStart methodStartLabel
            withScope $ do
                scopeIndex <- getCurrentScopeIndex
                scope <- getScope scopeIndex
                let (lineNumberStart, lineNumberEnd) = lineNumbers scope
                addLineNumber lineNumberStart methodStartLabel
                updateScopeStartLabel scopeIndex methodStartLabel
                updateScopeEndLabel scopeIndex methodEndLabel
                assembleExpr True methodReturnType optimizedExpr
                labelStart methodEndLabel
            addLocalVariables 0
            maxStackAndLocal (-1) (-1)
            methodCodeEnd
        else do
            withScope $ assembleExpr False delayedType optimizedExpr
            field PutStatic declaringClassName methodName "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"

createMainMethod : {auto stateRef: Ref AsmState AsmState} -> String -> Jname -> String -> Core ()
createMainMethod programName mainFunctionName javaMainClassName = do
    function <- getFunction mainFunctionName
    logAsm ("generating main function in " ++ javaMainClassName)
    createMethod [Public, Static] "Main.idr" javaMainClassName "main" "([Ljava/lang/String;)V" Nothing Nothing [] []
    methodCodeStart
    ldc $ StringConst programName
    aload 0
    invokeMethod InvokeStatic runtimeClass "setProgramArgs" "(Ljava/lang/String;[Ljava/lang/String;)V" False
    let idrisMainClassMethodName = function.name
    let idrisMainClassName = className idrisMainClassMethodName
    field GetStatic idrisMainClassName (methodName idrisMainClassMethodName) "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
    invokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate" "()Ljava/lang/Object;" False
    return
    maxStackAndLocal (-1) (-1)
    methodCodeEnd

assemble : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState} -> Name -> FC -> Core ()
assemble name fc = do
  assembleDefinition name fc
  scopes <- coreLift $ ArrayList.new {elemTy=Scope}
  updateCurrentFunction $ { scopes := (subtyping scopes), optimizedBody := emptyFunction }

getNameStrFcDef : String -> (Name, FC, NamedDef) -> (String, FC, NamedDef)
getNameStrFcDef programName (name, fc, def) =
  let jname = jvmName programName name
  in (getSimpleName jname, fc, def)

isForeignDef : NamedDef -> Bool
isForeignDef (MkNmForeign _ _ _) = True
isForeignDef _ = False

isFunctionDef : NamedDef -> Bool
isFunctionDef (MkNmFun _ _) = True
isFunctionDef _ = False

exportConstructor : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState}
                  -> SortedMap Namespace (List String) -> Map Int InferredType
                  -> InferredType -> Int -> Jname -> Name -> InferredFunctionType -> Core ()
exportConstructor typeExports jvmArgumentTypesByIndex jvmReturnType arity jvmIdrisName idrisName idrisFunctionType = do
  function <- getCurrentFunction
  initializeFunctionState
  let optimizedExpr = optimizedBody function
  let internalJname = function.name
  when (shouldDebugFunction internalJname) $ logAsm $ "Assembling " ++ (className internalJname) ++ "." ++
    (methodName internalJname) ++ "\n" ++ showNamedCExp 0 optimizedExpr
  createLabel methodStartLabel
  createLabel methodEndLabel
  labelStart methodStartLabel
  withScope $ do
    scopeIndex <- getCurrentScopeIndex
    scope <- getScope scopeIndex
    let (lineNumberStart, lineNumberEnd) = lineNumbers scope
    addLineNumber lineNumberStart methodStartLabel
    updateScopeStartLabel scopeIndex methodStartLabel
    updateScopeEndLabel scopeIndex methodEndLabel
    assembleExpr False IVoid (optimizedBody function)
    loadArguments typeExports jvmArgumentTypesByIndex idrisName arity (parameterTypes idrisFunctionType)
    let idrisMethodDescriptor = getMethodDescriptor idrisFunctionType
    programName <- getProgramName
    invokeMethod InvokeStatic
      (className jvmIdrisName) (methodName jvmIdrisName) idrisMethodDescriptor False
    invokeMethod InvokeStatic (programName ++ "/PrimIO") "unsafePerformIO" "(Ljava/lang/Object;)Ljava/lang/Object;" False
    asmCast (returnType idrisFunctionType) jvmReturnType
    asmReturn jvmReturnType
    labelStart methodEndLabel
  maxStackAndLocal (-1) (-1)
  methodCodeEnd

exportFunction : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState}
               -> SortedMap Namespace (List String) -> MethodExport -> Core ()
exportFunction typeExports (MkMethodExport jvmFunctionName idrisName type shouldPerformIO encloser
  modifiers annotations parameterAnnotations) = do
    let jvmClassName = encloser.name
    let fileName = fst $ getSourceLocationFromFc emptyFC
    let MkInferredFunctionType jvmReturnType jvmArgumentTypes = type
    let arity = length jvmArgumentTypes
    let arityInt = the Int $ cast $ length jvmArgumentTypes
    jvmArgumentTypesByIndex <- coreLift $ Map.fromList $ zip [0 .. (arityInt - 1)] jvmArgumentTypes
    let isInstance = not $ elem Static modifiers
    jvmArgumentTypesForSignature <- adjustArgumentsForInstanceMember idrisName isInstance jvmArgumentTypes
    let functionType = MkInferredFunctionType jvmReturnType jvmArgumentTypesForSignature
    let asmAnnotations = asmAnnotation <$> annotations
    let asmParameterAnnotations = (\annotations => asmAnnotation <$> annotations) <$> parameterAnnotations
    let descriptor = getMethodDescriptor functionType
    let signature = Just $ getMethodSignature functionType
    createMethod modifiers fileName jvmClassName jvmFunctionName descriptor signature Nothing asmAnnotations asmParameterAnnotations
    methodCodeStart
    programName <- getProgramName
    let jvmIdrisName = jvmName programName idrisName
    (_, MkNmFun idrisFunctionArgs _) <- getFcAndDefinition (getSimpleName jvmIdrisName)
      | _ => asmCrash ("Unknown idris function " ++ show idrisName)
    let idrisFunctionArity = length idrisFunctionArgs
    let idrisArgumentTypes = replicate idrisFunctionArity inferredObjectType
    let idrisFunctionType = MkInferredFunctionType inferredObjectType idrisArgumentTypes
    let isField = idrisFunctionArity == 0
    let isConstructor = jvmFunctionName == "<init>"
    if isConstructor
      then exportConstructor typeExports jvmArgumentTypesByIndex jvmReturnType arityInt jvmIdrisName idrisName idrisFunctionType
      else if not isField then do
        loadArguments typeExports jvmArgumentTypesByIndex idrisName arityInt (parameterTypes idrisFunctionType)
        let idrisMethodDescriptor = getMethodDescriptor idrisFunctionType
        programName <- getProgramName
        invokeMethod InvokeStatic (className jvmIdrisName) (methodName jvmIdrisName) idrisMethodDescriptor False
        when shouldPerformIO $ invokeMethod InvokeStatic (programName ++ "/PrimIO") "unsafePerformIO" "(Ljava/lang/Object;)Ljava/lang/Object;" False
        toJava idrisName typeExports jvmReturnType (returnType idrisFunctionType)
        asmReturn jvmReturnType
        maxStackAndLocal (-1) (-1)
        methodCodeEnd
      else do
        field GetStatic (className jvmIdrisName) (methodName jvmIdrisName) "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
        invokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate" "()Ljava/lang/Object;" False
        toJava idrisName typeExports jvmReturnType (returnType idrisFunctionType)
        asmReturn jvmReturnType
        maxStackAndLocal (-1) (-1)
        methodCodeEnd

generateAccessors : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> (accessorCreator: FieldExport -> IO ()) -> IO ()
generateAccessors descriptorsByEncloser classExport accessorCreator = do
  let className = classExport.name
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  traverse_ accessorCreator fields

generateGetters : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateGetters descriptorsByEncloser classExport = generateAccessors descriptorsByEncloser classExport (createGetter classExport)

generateSetters : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateSetters descriptorsByEncloser classExport = generateAccessors descriptorsByEncloser classExport (createSetter classExport)

generateConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> List FieldExport -> List Annotation -> List (List Annotation) -> IO ()
generateConstructor descriptorsByEncloser classExport fields annotations parameterAnnotations = do
  let fieldTypes = FieldExport.type <$> fields
  let descriptor = getMethodDescriptor $ MkInferredFunctionType IVoid fieldTypes
  let signature = Just $ getMethodSignature $ MkInferredFunctionType IVoid fieldTypes
  let classType = iref classExport.name []
  let arity = the Int $ cast $ length fields
  jvmArgumentTypesByIndex <- Map.fromList $ zip [0 .. arity] (classType :: fieldTypes)
  let asmAnnotations = asmAnnotation <$> annotations
  let asmParameterAnnotations = (\annotations => asmAnnotation <$> annotations) <$> parameterAnnotations
  asmState <- AsmState.fromJavaName (Jqualified classExport.name "<init>")
  runAsm asmState $ \stateRef => do
    createMethod [Public] "generated.idr" classExport.name "<init>" descriptor signature Nothing asmAnnotations
      asmParameterAnnotations
    methodCodeStart
    createLabel methodStartLabel
    createLabel methodEndLabel
    labelStart methodStartLabel
    aload 0
    extendsTypeName <- getJvmReferenceTypeName classExport.extends
    invokeMethod InvokeSpecial extendsTypeName "<init>" "()V" False
    assignFields jvmArgumentTypesByIndex fields
    return
    labelStart methodEndLabel
    localVariable "this" (getJvmTypeDescriptor classType) Nothing methodStartLabel methodEndLabel 0
    traverse_ (uncurry addLocalVariable) $ zip [1 .. arity] fields
    maxStackAndLocal (-1) (-1)
    methodCodeEnd
 where
  assignField : {auto stateRef: Ref AsmState AsmState} -> Map Int InferredType -> Int -> FieldExport -> Core ()
  assignField jvmArgumentTypesByIndex varIndex fieldExport = do
    let fieldType = fieldExport.type
    aload 0
    loadVar jvmArgumentTypesByIndex fieldType fieldType varIndex
    field PutField classExport.name fieldExport.name (getJvmTypeDescriptor fieldType)

  assignFields : {auto stateRef: Ref AsmState AsmState} -> Map Int InferredType -> List FieldExport -> Core ()
  assignFields jvmArgumentTypesByIndex fieldExports = do
    let arity = the Int $ cast $ length fieldExports
    let varIndexAndExports = zip [1 .. arity] fieldExports
    traverse_ (uncurry $ assignField jvmArgumentTypesByIndex) varIndexAndExports

  addLocalVariable : {auto stateRef: Ref AsmState AsmState} -> Int -> FieldExport -> Core ()
  addLocalVariable index field = do
    let fieldType = field.type
    localVariable field.name (getJvmTypeDescriptor fieldType) Nothing methodStartLabel methodEndLabel index

getMatchingAnnotationProperty : String -> List AnnotationProperty -> Maybe AnnotationValue
getMatchingAnnotationProperty name props = snd <$> find (\(currentName, value) => name == currentName) props

generateRequiredArgsConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> List AnnotationProperty -> IO ()
generateRequiredArgsConstructor descriptorsByEncloser classExport props = do
  let allFields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  let requiredFields@(_ :: _) = filter isRequiredField allFields
        | [] => pure ()
  let annotations = getAnnotationValues $ fromMaybe (AnnArray []) $ getMatchingAnnotationProperty "annotations" props
  let parameterAnnotations = getParameterAnnotationValues $ fromMaybe (AnnArray []) $ getMatchingAnnotationProperty "parameterAnnotations" props
  generateConstructor descriptorsByEncloser classExport requiredFields annotations parameterAnnotations

getExcludedAnnotationValues : List AnnotationProperty -> Maybe AnnotationValue
getExcludedAnnotationValues props = snd <$> (find (\(name, _) => name == "exclude") props)

generateAllArgsConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateAllArgsConstructor descriptorsByEncloser classExport = do
  let Just (MkAnnotation _ props) = findAllArgsConstructor classExport
      | _ => pure ()
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  let excludedAnnotationValues = getExcludedAnnotationValues props
  let excludedFields = getStringAnnotationValues $ fromMaybe (AnnArray []) excludedAnnotationValues
  let constructorFields = filter (\fieldExport => not $ elem fieldExport.name excludedFields) fields
  let annotations = getAnnotationValues $ fromMaybe (AnnArray []) $ getMatchingAnnotationProperty "annotations" props
  let parameterAnnotations = getParameterAnnotationValues $ fromMaybe (AnnArray []) $ getMatchingAnnotationProperty "parameterAnnotations" props
  generateConstructor descriptorsByEncloser classExport constructorFields annotations parameterAnnotations

generateNoArgsConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateNoArgsConstructor descriptorsByEncloser classExport = do
  let Just (MkAnnotation _ props) = findNoArgsConstructor classExport
        | _ => pure ()
  let annotations = getAnnotationValues $ fromMaybe (AnnArray []) $ (snd {b=AnnotationValue} <$> find (\(name, _) => name == "annotations") props)
  asmState <- AsmState.fromJavaName (Jqualified classExport.name "<init>")
  runAsm asmState $ \asmStateRef => do
    createMethod [Public] "generated.idr" classExport.name "<init>" "()V" Nothing Nothing [] []
    methodCodeStart
    aload 0
    extendsTypeName <- getJvmReferenceTypeName classExport.extends
    invokeMethod InvokeSpecial extendsTypeName "<init>" "()V" False
    return
    maxStackAndLocal (-1) (-1)
    methodCodeEnd

generateHashCode : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateHashCode descriptorsByEncloser classExport = do
  let fields = filter (not . isTransientField) $ getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  asmState <- AsmState.fromJavaName (Jqualified classExport.name "hashCode")
  runAsm asmState $ \asmStateRef => do
    createMethod [Public] "generated.idr" classExport.name "hashCode" "()I" Nothing Nothing [] []
    methodCodeStart
    let fieldsCount = the Int $ cast $ length fields
    iconst fieldsCount
    anewarray "java/lang/Object"
    traverse_ (uncurry loadField) $ zip [0 .. fieldsCount - 1] fields
    invokeMethod InvokeStatic "java/util/Objects" "hash" "([Ljava/lang/Object;)I" False
    ireturn
    maxStackAndLocal (-1) (-1)
    methodCodeEnd
 where
  loadField : {auto stateRef: Ref AsmState AsmState} -> Int -> FieldExport -> Core ()
  loadField index fieldExport = do
    dup
    iconst index
    aload 0
    let fieldType = fieldExport.type
    field GetField classExport.name fieldExport.name (getJvmTypeDescriptor fieldType)
    asmCast fieldType inferredObjectType
    aastore

generateEquals : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateEquals descriptorsByEncloser classExport = do
  let fields = filter (not . isTransientField) $ getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  asmState <- AsmState.fromJavaName (Jqualified classExport.name "equals")
  runAsm asmState $ \stateRef => do
    createMethod [Public] "generated.idr" classExport.name "equals" "(Ljava/lang/Object;)Z" Nothing Nothing [] []
    methodCodeStart
    aload 0
    aload 1
    refEqLabel <- newLabel
    createLabel refEqLabel
    ifacmpne refEqLabel
    iconst 1
    ireturn
    labelStart refEqLabel
    aload 1
    let className = classExport.name
    instanceOf className
    instanceOfLabel <- newLabel
    createLabel instanceOfLabel
    ifne instanceOfLabel
    iconst 0
    ireturn
    labelStart instanceOfLabel
    aload 1
    checkcast className
    astore 2
    let fieldsCount = the Int $ cast $ length fields
    equalsLabel <- newLabel
    createLabel equalsLabel
    equalsFields equalsLabel fields
    iconst 1
    methodEndLabel <- newLabel
    createLabel methodEndLabel
    goto methodEndLabel
    labelStart equalsLabel
    iconst 0
    labelStart methodEndLabel
    ireturn
    maxStackAndLocal (-1) (-1)
    methodCodeEnd
 where
  equalsFields : {auto stateRef: Ref AsmState AsmState} -> String -> List FieldExport -> Core ()
  equalsFields equalsLabel [] = pure ()
  equalsFields equalsLabel (fieldExport :: rest) = do
    let fieldType = fieldExport.type
    let className = classExport.name
    aload 0
    field GetField className fieldExport.name (getJvmTypeDescriptor fieldType)
    asmCast fieldType inferredObjectType
    aload 2
    field GetField className fieldExport.name (getJvmTypeDescriptor fieldType)
    asmCast fieldType inferredObjectType
    invokeMethod InvokeStatic "java/util/Objects" "equals" "(Ljava/lang/Object;Ljava/lang/Object;)Z" False
    ifeq equalsLabel
    equalsFields equalsLabel rest

generateToString : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateToString descriptorsByEncloser classExport = do
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  asmState <- AsmState.fromJavaName (Jqualified classExport.name "toString")
  runAsm asmState $ \stateRef => do
    createMethod [Public] "generated.idr" classExport.name "toString" "()Ljava/lang/String;" Nothing Nothing [] []
    methodCodeStart
    new "java/lang/StringBuilder"
    dup
    invokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
    ldc $ StringConst classExport.name
    invokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
    let hasFields = not $ isNil fields
    when hasFields $ do
      appendFields "{" fields
      iconst 125 -- '}'
      invokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(C)Ljava/lang/StringBuilder;" False
    invokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
    areturn
    maxStackAndLocal (-1) (-1)
    methodCodeEnd
 where
  getAppendParamType : InferredType -> InferredType
  getAppendParamType IChar = IChar
  getAppendParamType IBool = IBool
  getAppendParamType (IArray IChar) = IArray IChar
  getAppendParamType IDouble = IDouble
  getAppendParamType IFloat = IFloat
  getAppendParamType IInt = IInt
  getAppendParamType IByte = IInt
  getAppendParamType IShort = IInt
  getAppendParamType ILong = ILong
  getAppendParamType ty =
    if (ty == iref "java/lang/CharSequence" [] || ty == inferredStringType ||
      ty == iref "java/lang/StringBuffer" []) then ty
    else inferredObjectType

  appendFields : {auto stateRef: Ref AsmState AsmState} -> String -> List FieldExport -> Core ()
  appendFields _ [] = pure ()
  appendFields prefixChar (fieldExport :: rest) = do
    let fieldName = fieldExport.name
    let fieldType = fieldExport.type
    let className = classExport.name
    let isStringField = fieldExport.type == inferredStringType
    ldc $ StringConst (prefixChar ++ fieldName ++ "=" ++ if isStringField then "'" else "")
    invokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
    aload 0
    field GetField className fieldName (getJvmTypeDescriptor fieldType)
    let appendParamType = getAppendParamType fieldType
    asmCast fieldType appendParamType
    let stringBuilderType = iref "java/lang/StringBuilder" []
    let appendDescriptor = getMethodDescriptor $ MkInferredFunctionType stringBuilderType [appendParamType]
    invokeMethod InvokeVirtual "java/lang/StringBuilder" "append" appendDescriptor False
    when isStringField $ do
      iconst 39 -- single quote
      invokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(C)Ljava/lang/StringBuilder;" False
    appendFields ", " rest

generateDataClass : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> IO ()
generateDataClass descriptorsByEncloser classExport = do
  generateGetters descriptorsByEncloser classExport
  generateSetters descriptorsByEncloser classExport
  generateRequiredArgsConstructor descriptorsByEncloser classExport []
  generateHashCode descriptorsByEncloser classExport
  generateEquals descriptorsByEncloser classExport
  generateToString descriptorsByEncloser classExport

exportMemberIo : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
               -> SortedMap Namespace (List String)
               -> SortedMap ClassExport (List ExportDescriptor) -> ExportDescriptor -> IO ()
exportMemberIo typeExports descriptorsByEncloser (MkMethodExportDescriptor desc) =
  if desc.name == "<init>"
    then do
      let idrisName = desc.idrisName
      programName <- AsmGlobalState.getProgramName
      fcDef <- AsmGlobalState.getFcAndDefinition (getSimpleName (jvmName programName idrisName))
      case snd <$> nullableToMaybe fcDef of
        Just (MkNmFun args expr) => do
            let jname = jvmName programName desc.idrisName
            let dottedClassName = replace (className jname) '/' '.'
            let constructorJavaName = methodName jname ++ "<init>"
            let constructorIdrisName = NS (mkNamespace desc.encloser.name) (UN $ Basic constructorJavaName)
            let constructorJavaQualifiedName = Jqualified desc.encloser.name constructorJavaName
            asmState <- AsmState.fromJavaName constructorJavaQualifiedName
            ignore $ runAsm asmState $ \stateRef => do
              Just superCallExpr <- getSuperCallExpr expr
                | Nothing => asmCrash ("Constructor export for " ++ show idrisName ++ " should call 'super'")
              let constructorDef = MkNmFun args superCallExpr
              initialFunctionType <- getInitialFunctionType constructorIdrisName constructorDef
              inferFunctionType (Just initialFunctionType) constructorDef
              inferDef
              resetScope
              loadFunction constructorJavaQualifiedName
              exportFunction typeExports desc
              scopes <- coreLift $ ArrayList.new {elemTy=Scope}
              updateCurrentFunction $ { scopes := (subtyping scopes), optimizedBody := emptyFunction }
        _ => pure ()
    else do
      asmState <- AsmState.fromJavaName (Jqualified desc.encloser.name desc.name)
      ignore $ runAsm asmState $ \stateRef => exportFunction typeExports desc
exportMemberIo typeExports descriptorsByEncloser (MkFieldExportDescriptor desc) = exportField desc
exportMemberIo _ descriptorsByEncloser (MkClassExportDescriptor classExport) = do
  exportClass classExport
  let hasDataAnnotation = isJust (findClassAnnotation "Data" classExport)
  generateAllArgsConstructor descriptorsByEncloser classExport
  generateNoArgsConstructor descriptorsByEncloser classExport
  when (not hasDataAnnotation) $
    generateRequiredArgsConstructor descriptorsByEncloser classExport (maybe [] getAnnotationProperties $ findRequiredArgsConstructor classExport)
  when hasDataAnnotation $ ignore $ generateDataClass descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "Getter" classExport)) $ generateGetters descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "Setter" classExport)) $ generateSetters descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "EqualsAndHashCode" classExport)) $ do
      generateEquals descriptorsByEncloser classExport
      generateHashCode descriptorsByEncloser classExport
exportMemberIo _ _ _ = pure ()

groupByEncloser : List ExportDescriptor -> SortedMap ClassExport (List ExportDescriptor)
groupByEncloser descriptors =
  let (classExports, methodFieldExports) = partitionExports ([], []) descriptors
      classExportsByName = SortedMap.fromList $ (\classExport => (classExport.name, classExport)) <$> classExports
  in pairEncloserDescriptor classExportsByName empty methodFieldExports
  where
    partitionExports : (List ClassExport, List ExportDescriptor) -> List ExportDescriptor ->
                         (List ClassExport, List ExportDescriptor)
    partitionExports acc [] = acc
    partitionExports (classExports, methodFieldExports) (desc@(MkMethodExportDescriptor _) :: rest) =
      partitionExports (classExports, desc :: methodFieldExports) rest
    partitionExports (classExports, methodFieldExports) (desc@(MkFieldExportDescriptor _) :: rest) =
      partitionExports (classExports, desc :: methodFieldExports) rest
    partitionExports (classExports, methodFieldExports) ((MkClassExportDescriptor desc) :: rest) =
      partitionExports (desc :: classExports, methodFieldExports) rest
    partitionExports exports (_ :: rest) = partitionExports exports rest

    updateExportDescriptors : ClassExport -> ExportDescriptor -> SortedMap ClassExport (List ExportDescriptor) ->
                                SortedMap ClassExport (List ExportDescriptor)
    updateExportDescriptors classExport desc descriptorsByEncloser =
       mergeWith (++) descriptorsByEncloser (singleton classExport [desc])

    pairEncloserDescriptor : SortedMap String ClassExport -> SortedMap ClassExport (List ExportDescriptor) ->
                               List ExportDescriptor -> SortedMap ClassExport (List ExportDescriptor)
    pairEncloserDescriptor classExports acc [] = acc
    pairEncloserDescriptor classExports acc (desc@(MkMethodExportDescriptor methodExport) :: rest) =
      let encloser = methodExport.encloser
          classExport = fromMaybe encloser (SortedMap.lookup encloser.name classExports)
      in pairEncloserDescriptor classExports (updateExportDescriptors classExport desc acc) rest
    pairEncloserDescriptor classExports acc (desc@(MkFieldExportDescriptor fieldExport) :: rest) =
      let encloser = fieldExport.encloser
          classExport = fromMaybe encloser (SortedMap.lookup encloser.name classExports)
      in pairEncloserDescriptor classExports (updateExportDescriptors classExport desc acc) rest
    pairEncloserDescriptor classExports acc (_ :: rest) = pairEncloserDescriptor classExports acc rest

exportTypeIo : String -> IO ()
exportTypeIo name = do
  asmState <- AsmState.fromJavaName (Jqualified name "")
  ignore $ runAsm asmState $ \stateRef => exportType name

exportTypes : SortedMap Namespace (List String) -> IO ()
exportTypes typeExports = traverse_ exportTypeIo $ concat $ values typeExports

exportDefs : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
           -> LazyList (Name, String) -> IO ()
exportDefs nameAndDescriptors = do
  (typeExports, descriptors) <- parseExportDescriptors nameAndDescriptors
  let descriptorsByEncloser = groupByEncloser descriptors
  exportTypes typeExports
  traverse_ (exportMemberIo typeExports descriptorsByEncloser) descriptors

export
getExport : NoMangleMap -> Name -> Maybe (Name, String)
getExport noMangleMap name = (\descriptor => (name, descriptor)) <$> isNoMangle noMangleMap name

-- Register a stub Function record for every spec in the plan.  The body of
-- the stub is never read; the type is what we need so that, when the
-- inferDef-rewritten original later `invokestatic`s the spec, the assembler's
-- findFunctionType returns the spec's primitive-typed signature.  Each spec's
-- entry is later overwritten with the real Function record by assembleSpec
-- before the spec's own body is emitted.
preRegisterSpecs : SpecialisationPlan -> Core ()
preRegisterSpecs plan = traverse_ register (snd =<< SortedMap.toList plan)
  where
    register : SpecialisedSignature -> Core ()
    register (MkSpecialisedSig specName _ specType) = coreLift $ do
      programName <- AsmGlobalState.getProgramName
      let jname = jvmName programName specName
      scopes <- ArrayList.new {elemTy=Scope}
      let stub = MkFunction jname specType (believe_me scopes) 0 (NmCrash emptyFC "spec stub")
      AsmGlobalState.addFunction jname stub

-- Emit every typed callback interface the emitted bytecode can reference
-- (higher-order specialisation): the slots of every function spec in the
-- plan, plus every declaration-driven callback signature — emission
-- creates a typed lambda at a decl-driven slot even when the site never
-- earns a spec (the typed value is assignable to the natural Function
-- slot), so the interface class must exist regardless of the plan.  One
-- interface per distinct signature program-wide.
preRegisterCallbackInterfaces : (programName : String) -> SpecialisationPlan
                             -> SortedMap String (List CallbackSlot) -> Core ()
preRegisterCallbackInterfaces programName plan cbSigs = do
    let planSpecs = the (List SpecialisedSignature) (concatMap snd (SortedMap.toList plan))
    let planSlotTypes = concatMap (\(MkSpecialisedSig _ _ t) => t.returnType :: t.parameterTypes) planSpecs
    let planSigs = mapMaybe parseCallbackIfaceType planSlotTypes
    let declSigs = mapMaybe concreteSig (concatMap snd (SortedMap.toList cbSigs))
    let named = map (\sig => (mkCallbackIfaceName programName sig, sig)) (planSigs ++ declSigs)
    traverse_ createCallbackIface (nubBy (\a, b => fst a == fst b) named)
  where
    concreteSig : CallbackSlot -> Maybe InferredFunctionType
    concreteSig (CallbackConcrete sig) = Just sig
    concreteSig _ = Nothing

    createCallbackIface : (String, InferredFunctionType) -> Core ()
    createCallbackIface (ifaceName, sig) = do
      asmState <- coreLift $ AsmState.fromJavaName (Jqualified ifaceName "<clinit>")
      stateRef <- newRef AsmState asmState
      createIdrisFunctionInterface ifaceName (getMethodDescriptor sig)

-- Eagerly emit the specialised constructor classes the plan discovered.  Each
-- spec class has typed fields (per-slot JVM descriptors from `fieldTypes`)
-- and per-slot typed accessors — see `Assembler.createIdrisConstructorClassTyped`.
-- A `(programName, simpleClassName)` is tracked in `AsmGlobalState.constructors`
-- so we don't re-emit the class on every constructor call site.
--
-- Two passes: ALL family-instantiation interfaces first, then the spec
-- classes.  Determined entries `implements` their own instantiation
-- interface; UNDER-DETERMINED entries (tconClassName "", tconFamilyBase
-- set) `implements` EVERY active instantiation of their TCon — such a
-- cell is a semantic member of SOME instantiation and must pass that
-- instantiation's checkcasts (Phase-2d construction sites,
-- `$idrisTailRec` reassignment).  The plan has converged when this runs,
-- so "every active" is final.
preRegisterConstructorSpecs : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
                           -> ConSpecialisationPlan -> Core ()
preRegisterConstructorSpecs plan = do
    let entries = snd =<< SortedMap.toList plan
    let actives = nub $ mapMaybe activeIface entries
    traverse_ createIface actives
    traverse_ (registerClass (activesByBase entries)) entries
  where
    activeIface : SpecialisedConstructor -> Maybe String
    activeIface sc = if sc.tconClassName /= "" then Just sc.tconClassName else Nothing

    activesByBase : List SpecialisedConstructor -> SortedMap String (List String)
    activesByBase = foldl add SortedMap.empty
      where
        add : SortedMap String (List String) -> SpecialisedConstructor
           -> SortedMap String (List String)
        add acc sc =
          if sc.tconClassName == "" || sc.tconFamilyBase == ""
            then acc
            else let existing = fromMaybe [] (SortedMap.lookup sc.tconFamilyBase acc)
                 in if elem sc.tconClassName existing
                      then acc
                      else SortedMap.insert sc.tconFamilyBase (sc.tconClassName :: existing) acc

    createIface : String -> Core ()
    createIface tconClassName = do
      tconAsm <- coreLift $ AsmState.fromJavaName (Jqualified tconClassName "<clinit>")
      tconRef <- newRef AsmState tconAsm
      createIdrisTypeConstructorInterface tconClassName

    registerClass : SortedMap String (List String) -> SpecialisedConstructor -> Core ()
    registerClass byBase (MkSpecialisedCon _ _ tag _ fieldTypes specClassName tconClassName tconFamilyBase) = do
      hasIt <- coreLift $ AsmGlobalState.hasConstructor specClassName
      when (not hasIt) $ do
        coreLift $ AsmGlobalState.addConstructor specClassName
        -- A spec class needs an AsmState because `createIdrisConstructorClassTyped`
        -- is a method on the `Assembler` instance bundled into the state.  The
        -- Jname's "class" component is the spec class name; the "method"
        -- component is irrelevant for class emission.
        asmState <- coreLift $ AsmState.fromJavaName (Jqualified specClassName "<init>")
        stateRef <- newRef AsmState asmState
        let descriptors = getJvmTypeDescriptor <$> fieldTypes
        let tconIfaces = if tconClassName /= ""
                           then [tconClassName]
                           else if tconFamilyBase /= ""
                                  then sort (fromMaybe [] (SortedMap.lookup tconFamilyBase byBase))
                                  else []
        createIdrisConstructorClassTypedWithIfaces specClassName (isNothing tag) descriptors tconIfaces

-- Emit a single specialised method using the existing assembly pipeline.
-- The spec has its own AsmState (so it can register a fresh Function with the
-- spec's primitive signature) but lands in the same Java class as the original.
assembleSpec : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
            -> SpecialisationPlan -> ConSpecialisationPlan -> SortedSet Name
            -> SortedMap String (List String)
            -> SortedMap String (List CallbackSlot)
            -> SpecialisedSignature -> Core ()
assembleSpec plan conPlan natLive natIfaces cbSigs (MkSpecialisedSig specName def specType) = do
  asmState <- coreLift $ AsmState.fromIdrisName specName
  stateRef <- newRef AsmState asmState
  setSpecialisationPlan {stateRef} plan
  setConSpecialisationPlan {stateRef} conPlan
  setNaturalConsLive {stateRef} natLive
  setNaturalToTConIfaces {stateRef} natIfaces
  setCallbackSlotSigs {stateRef} cbSigs
  inferFunctionType {stateRef} (Just specType) def
  inferDef {stateRef}
  assemble {stateRef} specName emptyFC

assembleNameFcStateRefs : {auto c : Ref Ctxt Defs}
                        -> {auto s : Ref Syn SyntaxInfo}
                        -> SpecialisationPlan
                        -> ConSpecialisationPlan
                        -> SortedSet Name
                        -> SortedMap String (List String)
                        -> SortedMap String NamedDef
                        -> LazyList (Name, FC, Ref AsmState AsmState) -> Core ()
assembleNameFcStateRefs _ _ _ _ _ [] = pure ()
assembleNameFcStateRefs plan conPlan natLive natIfaces defs ((name, fc, stateRef) :: rest) = do
  setSpecialisationPlan {stateRef} plan
  setConSpecialisationPlan {stateRef} conPlan
  setNaturalConsLive {stateRef} natLive
  setNaturalToTConIfaces {stateRef} natIfaces
  -- Mirror assembleSpec for the natural method: re-run inferFunctionType +
  -- inferDef so the body is rewritten with the FINAL plan right before it's
  -- lowered to bytecode.  buildSpecialisationPlan's iteration also rewrites
  -- the body, but only as a side effect of fixed-point convergence — doing
  -- it explicitly here makes the assembly pipeline symmetric with
  -- assembleSpec and immune to any future change in the iteration order.
  case SortedMap.lookup (jvmSimpleName name) defs of
    Just def => do
      initialFunctionType <- inferredFunctionType <$> getCurrentFunction {stateRef}
      inferFunctionType {stateRef} (Just initialFunctionType) def
      inferDef {stateRef}
    Nothing => pure ()
  assemble {stateRef=stateRef} name fc
  cbSigs <- getCallbackSlotSigs {stateRef}
  traverse_ (assembleSpec plan conPlan natLive natIfaces cbSigs) (fromMaybe [] $ SortedMap.lookup name plan)
  assembleNameFcStateRefs plan conPlan natLive natIfaces defs rest

getNamedDefsByName : String -> LazyList (Name, FC, Ref AsmState AsmState) -> Core (SortedMap String NamedDef)
getNamedDefsByName programName nameFcStateRefs = go SortedMap.empty nameFcStateRefs where
  go : SortedMap String NamedDef -> LazyList (Name, FC, Ref AsmState AsmState) -> Core (SortedMap String NamedDef)
  go acc [] = pure acc
  go acc ((name, _, ref) :: rest) = do
    let javaKey = getSimpleName (jvmName programName name)
    (_, def) <- getFcAndDefinition javaKey
    let mapKey = jvmSimpleName name
    let newAcc = SortedMap.insert mapKey def acc
    go newAcc rest

-- Walk the call tree in post-order from main, iterating to a fixed point, to
-- build a plan of functions that benefit from a primitive-typed specialisation.
--
-- Phase 1 (seed): functions whose Idris-declared type already has a primitive
--   parameter are added directly (e.g. showPrec$showPrec_Show_Int has `int`).
-- Phase 2 (propagate): for each seeded function, every direct call site whose
--   argument is a known primitive but whose callee param is Object gets a
--   derived specialisation (e.g. primNumShow → primNumShow$sp(Function,Object,int)).
-- Both phases repeat until the plan stops growing.
buildSpecialisationPlan
  :  {auto c : Ref Ctxt Defs}
  -> {auto s : Ref Syn SyntaxInfo}
  -> (programName: String)
  -> SortedMap String NamedDef
  -> SortedMap String (Ref AsmState AsmState)
  -> List Name
  -> Core (SpecialisationPlan, ConSpecialisationPlan)
buildSpecialisationPlan programName defs stateRefs reachable = iterate (SortedMap.empty, SortedMap.empty)
  where
    alreadySpecFor : List SpecialisedSignature -> InferredFunctionType -> Bool
    alreadySpecFor existing (MkInferredFunctionType retType params) = any (\s => s.type.parameterTypes == params && s.type.returnType == retType) existing

    mkSpecName : Int -> Name -> Name
    mkSpecName specIndex name =
      let jname = jvmName programName name
          fnName = methodName jname
          specFnName = fnName ++ "$sp" ++ show specIndex
      in getIdrisName (Jqualified (className jname) specFnName)

    mkSpecSig : NamedDef -> Name -> InferredFunctionType -> Int -> SpecialisedSignature
    mkSpecSig def name type idx = MkSpecialisedSig (mkSpecName idx name) def type

    naturalType : Name -> Core (Maybe InferredFunctionType)
    naturalType callee = do
      mFn <- findFunction (jvmName programName callee)
      pure (inferredFunctionType <$> mFn)

    -- A spec parameter must be at least as specific as the corresponding
    -- natural parameter.  Two narrowings are accepted:
    --
    --   1. `Object → primitive` — always sound; primitives can't be subtyped.
    --
    --   2. `Object → IRef SpecClass` where `SpecClass` corresponds to a
    --      constructor whose `ConInfo` is `RECORD` or `UNIT` (single-
    --      constructor data type).  Sound because:
    --        - At the call site, `inferExprCon` returns the spec class only
    --          when it matched the construction's argument types, so the
    --          runtime value is provably that spec class.
    --        - Inside the spec body, the pattern match has only one alt
    --          (the unique constructor), so there's no sibling-class cast
    --          to fail.
    --        - Field types within a single-constructor record can be
    --          recursive in `T` itself (e.g. `data Tree a = MkT a (Tree a)`)
    --          and the recursion stays within the same spec class.
    --
    -- Multi-constructor sum types (`List`, `Maybe`, `Either`) are NOT
    -- handled by this narrowing — those need the Phase 2 TCon-family
    -- interface hierarchy.  See the monomorphization design doc.
    isRecordCon : ConInfo -> Bool
    isRecordCon RECORD = True
    isRecordCon UNIT   = True
    isRecordCon _      = False

    isRecordSpecRef : ConSpecialisationPlan -> InferredType -> Bool
    isRecordSpecRef plan (IRef cls _ _) =
      cls /= "java/lang/Object"
      && cls /= idrisObjectClass
      && any (\(_, entries) => any (\sc => sc.specClassName == cls && isRecordCon sc.conInfo) entries)
             (SortedMap.toList plan)
    isRecordSpecRef _ _ = False

    -- True when `ty` is `IRef X` for some `X` that's a TCon-family interface
    -- in `conPlan` (registered by Phase 2's `deriveTConClassName`).  Such
    -- narrowings are safe for sum types: every runtime instance of the
    -- family implements the interface (siblings are taught to via
    -- `computeNaturalToTConIfaces` + `createIdrisConstructorClassWithIfaces`),
    -- so a function-spec parameter typed as the interface can checkcast
    -- without sibling-class confusion.
    isTConIfaceRef : ConSpecialisationPlan -> InferredType -> Bool
    isTConIfaceRef plan (IRef cls _ _) =
      cls /= "java/lang/Object"
      && cls /= idrisObjectClass
      && any (\(_, entries) => any (\sc => sc.tconClassName == cls) entries)
             (SortedMap.toList plan)
    isTConIfaceRef _ _ = False

    -- Original v1 check: only `Object → primitive` is at-least-as-specific.
    -- Used for the "pure primitive narrowing" path so that type-case
    -- function bodies (like `strangeId : {a:Type} -> a -> a`) don't get
    -- a spec — their bodies have inferenced bytecode for the natural Type-
    -- discriminated branches that doesn't match a spec'd primitive slot.
    isAtLeastAsSpecificStrict : InferredType -> InferredType -> Bool
    isAtLeastAsSpecificStrict spec natural =
      spec == natural
      || (isObjectType natural && isPrimitive spec)
      -- Higher-order specialisation: a natural `Function` slot may narrow
      -- to a typed callback interface.  Sound because the interface
      -- extends `Function` (with a default boxed-apply bridge), and the
      -- slot is only logged as the interface for literal lambdas that
      -- emission constructs against it.
      || (natural == inferredLambdaType && isJust (parseCallbackIfaceType spec))

    -- Extended check (Phase 1.1 + Phase 2): also allow `Object → IRef
    -- RECORD-DCon`, `Object → IRef TConInterface`, and treat
    -- `idrisObjectType` as a no-op narrowing.  Used only when the
    -- candidate spec narrows at least one slot to either a RECORD/UNIT
    -- spec class or a TCon-family interface — those are the signals the
    -- spec carries a real benefit (typed-accessor pattern matching or
    -- sibling-safe family dispatch inside the body).
    isAtLeastAsSpecificExtended : ConSpecialisationPlan -> InferredType -> InferredType -> Bool
    isAtLeastAsSpecificExtended conPlan spec natural =
      spec == natural
      || (isObjectType natural
          && (isPrimitive spec
              || spec == idrisObjectType
              || isRecordSpecRef conPlan spec
              || isTConIfaceRef conPlan spec))
      || (natural == inferredLambdaType && isJust (parseCallbackIfaceType spec))

    hasRecordNarrowing : ConSpecialisationPlan -> InferredFunctionType -> InferredFunctionType -> Bool
    hasRecordNarrowing conPlan spec natural =
      any (\(s, n) => isObjectType n && (isRecordSpecRef conPlan s || isTConIfaceRef conPlan s))
          (zip (spec.returnType :: spec.parameterTypes)
               (natural.returnType :: natural.parameterTypes))

    isUsefulRefinement : ConSpecialisationPlan -> InferredFunctionType -> InferredFunctionType -> Bool
    isUsefulRefinement conPlan spec natural =
      length spec.parameterTypes == length natural.parameterTypes
      && spec /= natural
      && (let strict = isAtLeastAsSpecificStrict spec.returnType natural.returnType
                       && all (uncurry isAtLeastAsSpecificStrict)
                              (zip spec.parameterTypes natural.parameterTypes)
              extended = hasRecordNarrowing conPlan spec natural
                       && isAtLeastAsSpecificExtended conPlan spec.returnType natural.returnType
                       && all (uncurry (isAtLeastAsSpecificExtended conPlan))
                              (zip spec.parameterTypes natural.parameterTypes)
          in strict || extended)

    processSite : SpecialisationPlan -> ConSpecialisationPlan -> Name -> InferredFunctionType -> Core (SpecialisationPlan, Bool)
    processSite plan conPlan callee fnType =
      case SortedMap.lookup (jvmSimpleName callee) defs of
        Just def@(MkNmFun compiledParams _) => do
              -- A specialised signature only earns its emission when it is
              -- strictly narrower than the callee's natural signature.  A
              -- duplicate (`spec == natural`) or a widening (`spec` has
              -- `Object` where natural has a concrete type) does not — callers
              -- can either invokestatic the original directly or insert a
              -- cast at the call site.
              mNatural <- naturalType callee
              let isUseful = case mNatural of
                    Just natural => isUsefulRefinement conPlan fnType natural
                    Nothing => True
              if not isUseful
                then pure (plan, False)
                else
                  let existing = fromMaybe [] $ SortedMap.lookup callee plan
                  in if alreadySpecFor existing fnType
                       then pure (plan, False)
                       else
                         let sig     = mkSpecSig def callee fnType (cast $ length existing)
                             updated = SortedMap.insert callee (existing ++ [sig]) plan
                         in pure (updated, True)
        _ => pure (plan, False)

    foldSites : SpecialisationPlan -> ConSpecialisationPlan -> List (FC, Name, InferredFunctionType) -> Core (SpecialisationPlan, Bool)
    foldSites plan _ [] = pure (plan, False)
    foldSites plan conPlan ((_, callee, fnType) :: rest) = do
      (plan',  ch)  <- processSite plan conPlan callee fnType
      (plan'', ch') <- foldSites plan' conPlan rest
      pure (plan'', ch || ch')

    -- A constructor spec is *useful* under the same rule as a function spec:
    -- at least one slot must narrow `Object → primitive` and the spec must
    -- differ from the all-Object natural.
    isUsefulConRefinement : List InferredType -> Bool
    isUsefulConRefinement spec =
      not (null spec)
      && all (\t => isAtLeastAsSpecificStrict t inferredObjectType) spec
      && any isPrimitive spec

    -- For Idris's special-case data shapes (CONS/NIL, JUST/NOTHING,
    -- ZERO/SUCC, etc.), `ConInfo` already pins down the family.  Map it
    -- to a synthetic TCon name that all sibling DCons share — this lets
    -- user-defined `data Opt a = MkN | MkS a` and `Maybe a` both fold into
    -- the same JVM TCon-interface family at runtime (they're structurally
    -- equivalent anyway).
    syntheticTConName : ConInfo -> Maybe String
    syntheticTConName NIL     = Just "List"
    syntheticTConName CONS    = Just "List"
    syntheticTConName NOTHING = Just "Maybe"
    syntheticTConName JUST    = Just "Maybe"
    syntheticTConName ZERO    = Just "Nat"
    syntheticTConName SUCC    = Just "Nat"
    syntheticTConName _       = Nothing

    -- Single-constructor (`RECORD`/`UNIT`) data types don't need a TCon
    -- family interface: Phase 1's `findConSpecByClass` works directly with
    -- the DCon spec class — adding a TCon interface for the lone DCon
    -- would just confuse `inferExprCon` into returning the TCon type at
    -- construction sites, which forces function specs to use the
    -- interface signature when the more specific DCon signature would
    -- enable typed accessors in the spec's pattern match.
    isSingleConCon : ConInfo -> Bool
    isSingleConCon RECORD = True
    isSingleConCon UNIT   = True
    isSingleConCon _      = False

    isBareFor : Nat -> ConSlotRole -> Bool
    isBareFor k (SlotBare ks) = elem k ks
    isBareFor _ _ = False

    -- A TCon's family interfaces may be derived only when its FULL set of
    -- DCons cooperates (pure in Ctxt, so stable across plan iterations):
    --   * every DCon's telescope walks (all-Local return spine, no
    --     indexed shapes — a nullary `VNil : V 0` disqualifies the
    --     family rather than silently joining instantiations its index
    --     excludes), with equal spine lengths;
    --   * every NON-NULLARY DCon has, for every type-param position, at
    --     least one bare non-erased slot witnessing it.
    -- Without the second condition a data-carrying sibling could never
    -- determine its instantiation (`data E a b = L a | R b`: L never
    -- sees b, R never sees a) and its spec instances would be stranded
    -- outside every instantiation interface — so NO DCon of such a TCon
    -- gets a family.  Returns the TCon's type-param count when eligible.
    tconFamilyEligible : Name -> Core (Maybe Nat)
    tconFamilyEligible tcon = do
      dcons <- findDConsForTCon tcon
      if isNil dcons
        then pure Nothing
        else do
          mRoles <- traverse findConSlotRoles dcons
          let Just allRoles = sequence mRoles
            | Nothing => pure Nothing
          case allRoles of
            [] => pure Nothing
            ((n, _) :: _) =>
              if all (\(count, _) => count == n) allRoles && all (dconOk n) allRoles
                then pure (Just n)
                else pure Nothing
      where
        dconOk : Nat -> (Nat, List ConSlotRole) -> Bool
        dconOk Z _ = True
        dconOk _ (_, []) = True
        dconOk n (_, roles) = all (\k => any (isBareFor k) roles) [0 .. minus n 1]

    -- Derive `(tconFamilyBase, tconClassName)` for a DCon spec entry.
    --
    -- 1. Synthetic `ConInfo` shapes (CONS/NIL/JUST/NOTHING/...): keep the
    --    DCon-slot-suffix scheme — the intrinsic `_builtin/*` Names have
    --    no Ctxt telescope to derive type params from, and each
    --    synthetic family has exactly one data-carrying shape, so the
    --    slot suffix IS the instantiation key there.  `tconFamilyBase`
    --    stays "" (synthetic entries are never under-determined).
    -- 2. Ordinary DATACONs: keyed by the parent TCon's TYPE-ARGUMENT
    --    instantiation — `<TConNatural>$F` + one char per type param
    --    derived from primitive observations at bare-param slots
    --    (`Run$F` for parameterless `Run`; `T$F$I` for `T Int`).  ALL
    --    data-carrying siblings of one instantiation share the
    --    interface, which is what makes family-typed tail recursion
    --    across siblings sound (previously `Done$I`/`Step$I$L` got
    --    disjoint `Run$I`/`Run$I$L` and crashed with a CCE).  The `$F`
    --    marker avoids colliding with the TCon's own natural class
    --    (type-case programs construct TCon values via `assembleCon`
    --    with `tag = Nothing`) — and with same-named DCons (`data Run =
    --    Run Int | Stop`).
    --
    -- A param char comes only from a PRIMITIVE observation (an Object
    -- observation of a bare-`a` slot does not pin `a`); conflicting
    -- primitive observations (believe_me) also refuse.  Specs that fail
    -- to pin every param are UNDER-DETERMINED: `("base", "")` — they
    -- behave as lone classes for routing but `implements` every active
    -- instantiation of the base at emission (see
    -- `preRegisterConstructorSpecs`).
    deriveTConFamily : Name -> ConInfo -> String -> List InferredType -> Core (String, String)
    deriveTConFamily dconName conInfo specClassName paramTypes =
      if isSingleConCon conInfo then pure ("", "") else do
        programName <- getProgramName
        let dconNatural = getIdrisConstructorClassName programName (jvmSimpleName dconName)
        case syntheticTConName conInfo of
          Just synthetic =>
            let suffix = substr (length dconNatural) (length specClassName) specClassName
                segments = forget (split (== '/') dconNatural)
            in case reverse segments of
              (_ :: revPath) =>
                pure ("", concat (intersperse "/" (reverse (synthetic :: revPath))) ++ suffix)
              [] => pure ("", "")
          Nothing => do
            Just tcon <- findTConForDCon dconName
              | Nothing => pure ("", "")
            Just nParams <- tconFamilyEligible tcon
              | Nothing => pure ("", "")
            let base = getIdrisConstructorClassName programName (jvmSimpleName tcon) ++ "$F"
            Just (_, roles) <- findConSlotRoles dconName
              | Nothing => pure ("", "")
            if length roles /= length paramTypes
              then pure (base, "")
              else case paramChars nParams roles of
                Nothing => pure (base, "")
                Just chars => pure (base, base ++ concat (map ("$" ++) chars))
      where
        primCharAt : Nat -> (ConSlotRole, InferredType) -> Maybe String
        primCharAt k (role, ty) =
          if isPrimitive ty && isBareFor k role
            then Just (specConDescriptorChar ty)
            else Nothing

        charFor : List (ConSlotRole, InferredType) -> Nat -> Maybe String
        charFor slots k =
          case nub (mapMaybe (primCharAt k) slots) of
            [c] => Just c
            _ => Nothing

        paramChars : Nat -> List ConSlotRole -> Maybe (List String)
        paramChars Z _ = Just []
        paramChars n roles =
          traverse (charFor (zip roles paramTypes)) [0 .. minus n 1]

    -- Refine reference slots that are RECURSIVE in the DCon's own type
    -- family to the family interface (`Node$I$L$L`'s subtree slots become
    -- `Tree$I$L$L` instead of Object) — the emitted field, `<init>` and
    -- accessor descriptors then carry the family type, and
    -- `$idrisTailRec` reassignments of the extracted slot skip a
    -- per-iteration checkcast.  Only Object slots are refined, only when
    -- the spec has a family interface, and only when the recursion flags
    -- align 1:1 with `paramTypes` (arity/erasure mismatch → conservative
    -- skip).
    --
    -- Synthetic `ConInfo` shapes are NEVER refined.  Their constructors
    -- are rewritten to shared intrinsic Names (`_builtin/CONS` etc., see
    -- Compiler.Opts.Constructor.tryIntrinsic), so one spec class serves
    -- every source type of that shape — and the shapes don't guarantee
    -- recursion: `calcListy` (TTImp.ProcessData) assigns CONS to ANY
    -- 2-unerased-arg constructor, explicitly including non-recursive
    -- pairs ("Note they don't have to be recursive!").  Typing
    -- `CONS$I$L`'s second slot as `List$I$L` would checkcast-crash the
    -- first `(5, "five")` pair.  There is also no Ctxt entry for the
    -- intrinsic Names to consult.
    --
    -- For ordinary DATACONs the Ctxt telescope walk
    -- (`findRecursiveConSlots`) decides recursion.  `tconClassName /= ""`
    -- means the TCon passed `tconFamilyEligible` AND this spec fully
    -- determined its instantiation — every well-typed runtime value of a
    -- refined slot then implements the slot's interface: fully-determined
    -- sibling specs of the same instantiation share the identical
    -- interface name (instantiation-keyed naming), under-determined
    -- sibling specs `implements` every active instantiation
    -- (`preRegisterConstructorSpecs`), and live natural siblings do too
    -- (`computeNaturalToTConIfaces`).
    computeFieldTypes : Name -> ConInfo -> String -> List InferredType -> Core (List InferredType)
    computeFieldTypes name conInfo tconClassName paramTypes =
      if tconClassName == "" || isJust (syntheticTConName conInfo)
        then pure paramTypes
        else do
          Just flags <- findRecursiveConSlots name
            | Nothing => pure paramTypes
          if length flags /= length paramTypes
            then pure paramTypes
            else pure $ zipWith refine flags paramTypes
      where
        refine : Bool -> InferredType -> InferredType
        refine True ty = if ty == inferredObjectType then IRef tconClassName Interface [] else ty
        refine False ty = ty

    processConSite : ConSpecialisationPlan
                  -> Name -> ConInfo -> Maybe Int -> List InferredType
                  -> Core (ConSpecialisationPlan, Bool)
    processConSite conPlan name conInfo tag paramTypes = do
      if not (isUsefulConRefinement paramTypes)
        then pure (conPlan, False)
        else do
          programName <- getProgramName
          let baseClass = getIdrisConstructorClassName programName (jvmSimpleName name)
          let specName = mkConSpecClassName baseClass paramTypes
          let existing = fromMaybe [] $ SortedMap.lookup name conPlan
          if any (\sc => sc.paramTypes == paramTypes) existing
             then pure (conPlan, False)
             else do
                   (tconFamilyBase, tconClassName) <- deriveTConFamily name conInfo specName paramTypes
                   fieldTypes <- computeFieldTypes name conInfo tconClassName paramTypes
                   let entry   = MkSpecialisedCon name conInfo tag paramTypes fieldTypes specName
                                   tconClassName tconFamilyBase
                   let updated = SortedMap.insert name (existing ++ [entry]) conPlan
                   pure (updated, True)

    foldConSites : ConSpecialisationPlan
                -> List (FC, Name, ConInfo, Maybe Int, List InferredType)
                -> Core (ConSpecialisationPlan, Bool)
    foldConSites conPlan [] = pure (conPlan, False)
    foldConSites conPlan ((_, name, conInfo, tag, paramTypes) :: rest) = do
      (conPlan',  ch)  <- processConSite conPlan name conInfo tag paramTypes
      (conPlan'', ch') <- foldConSites conPlan' rest
      pure (conPlan'', ch || ch')

    -- Threaded plan: (function specs, constructor specs).  Constructor specs
    -- are discovered from `conSiteLog` alongside the existing `callSiteLog`
    -- discovery; the fixed-point loop converges over both simultaneously.
    Plans : Type
    Plans = (SpecialisationPlan, ConSpecialisationPlan)

    step : Plans -> Name -> Core (Plans, Bool)
    step (plan, conPlan) name = do
      let nameStr = jvmSimpleName name
      let Just asmStateRef = SortedMap.lookup nameStr stateRefs
            | Nothing => pure ((plan, conPlan), False)
      let Just def = SortedMap.lookup nameStr defs
            | Nothing => pure ((plan, conPlan), False)
      let initialFunctionType = inferredFunctionType !getCurrentFunction
      -- Make both plans visible to inferDef so its rewriteSpecCalls
      -- post-pass can rewire NmRef[orig] → NmRef[spec] using the latest plan,
      -- and so emission of the function body can look up constructor specs.
      setSpecialisationPlan {stateRef = asmStateRef} plan
      setConSpecialisationPlan {stateRef = asmStateRef} conPlan
      inferFunctionType (Just initialFunctionType) def
      inferDef
      let functionType = inferredFunctionType !getCurrentFunction
      -- Function-spec discovery from the natural's call sites.  Previously
      -- gated on `hasPrimitiveType functionType` to avoid over-spec'ing
      -- polymorphic functions, but with `inferExprCon` returning concrete
      -- class IRefs (spec or natural), the call-site types now carry
      -- enough information for `isUsefulRefinement` to filter on its own —
      -- a refinement is "useful" only when at least one slot narrows
      -- `Object → primitive` or `Object → IRef ConcreteClass`.  Pure
      -- polymorphic call sites (Object→Object) are rejected there.
      naturalSites <- getCallSiteLog {stateRef = asmStateRef}
      (plan1, ch1) <- foldSites plan conPlan naturalSites
      naturalConSites <- getConSiteLog {stateRef = asmStateRef}
      (conPlan1, ch1c) <- foldConSites conPlan naturalConSites
      -- For each existing function spec, re-run inference with the spec's
      -- parameter types and drain BOTH logs.
      let allSpecs = fromMaybe [] $ SortedMap.lookup name plan1
      case def of
        (MkNmFun paramNames body) => do
          (p, cp, ch) <- foldl
            (\acc, (MkSpecialisedSig specFnName def fnType) => do
                (p, cp, ch) <- acc
                let specParams = fnType.parameterTypes
                let arity = length paramNames
                let paddedParams = if length specParams < arity then padParams arity specParams else specParams
                let initialFunctionType = MkInferredFunctionType fnType.returnType paddedParams
                asmState <- coreLift $ AsmState.fromIdrisName specFnName
                specRef <- newRef AsmState asmState
                setSpecialisationPlan {stateRef = specRef} p
                setConSpecialisationPlan {stateRef = specRef} cp
                setCallbackSlotSigs {stateRef = specRef} !(getCallbackSlotSigs {stateRef = asmStateRef})
                inferFunctionType {stateRef = specRef} (Just initialFunctionType) def
                inferDef {stateRef = specRef}
                sites <- getCallSiteLog {stateRef = specRef}
                (p', ch') <- foldSites p cp sites
                conSites <- getConSiteLog {stateRef = specRef}
                (cp', chc') <- foldConSites cp conSites
                pure (p', cp', ch || ch' || chc'))
            (pure (plan1, conPlan1, ch1 || ch1c))
            allSpecs
          pure ((p, cp), ch)
        _ => pure ((plan1, conPlan1), ch1 || ch1c)

    scan : Plans -> List Name -> Core (Plans, Bool)
    scan plans [] = pure (plans, False)
    scan plans (name :: names) = do
      (plans',  ch)  <- step plans name
      (plans'', ch') <- scan plans' names
      pure (plans'', ch || ch')

    iterate' : List Name -> Plans -> Core Plans
    iterate' names plans = do
      (plans', changed) <- scan plans names
      if changed then iterate' names plans' else pure plans'

    iterate : Plans -> Core Plans
    iterate plans = iterate' reachable plans

-- Aggregate every construction site observed across the natural and
-- specialised bodies of all reachable functions after the spec plan has
-- converged.  A constructor `name` is added to the returned set when at
-- least one observed site has `paramTypes` that don't match any spec in
-- `conPlan[name]`.  The natural class is therefore still emitted at
-- runtime for that constructor, so `assembleConCaseExpr` must NOT narrow
-- pattern-match discriminants of that constructor via `checkcast` to a
-- spec class — the runtime instance could be either.
--
-- Conversely, when a constructor is absent from this set, every runtime
-- instance is a spec instance and the typed-accessor path is safe.
--
-- The result feeds back into inference (`inferConCaseExpr` narrows bound
-- variables only when the natural class is dead), and narrowing decisions
-- change which `paramTypes` get logged at downstream construction sites —
-- so the set is computed as a fixpoint: each pass drains inference with
-- the previous pass's candidate set installed on every state ref, and the
-- results are accumulated by UNION until stable.  Union (rather than
-- replacement) is required both for termination and for conservatism: the
-- step function is not monotone — turning narrowing off can flip a
-- primitive slot back to Object and accidentally *satisfy* a ref-slot
-- spec match that a previous pass rejected.  Once a constructor is
-- observed natural-live under any candidate set, it stays live.
computeNaturalConsLive : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
                     -> SortedMap String NamedDef
                     -> SortedMap String (Ref AsmState AsmState)
                     -> SpecialisationPlan
                     -> ConSpecialisationPlan
                     -> List Name -> Core (SortedSet Name)
computeNaturalConsLive defs stateRefs plan conPlan reachable = loop SortedSet.empty
  where
    slotMatch : InferredType -> InferredType -> Bool
    slotMatch spec arg =
      if isPrimitive spec then spec == arg
      else not (isPrimitive arg)

    slotsMatch : List InferredType -> List InferredType -> Bool
    slotsMatch xs ys =
      length xs == length ys
      && all (uncurry slotMatch) (zip xs ys)

    needsNatural : (Name, List InferredType) -> Maybe Name
    needsNatural (name, paramTypes) =
      let specs = fromMaybe [] $ SortedMap.lookup name conPlan
      in if any (\sc => slotsMatch sc.paramTypes paramTypes) specs
         then Nothing
         else Just name

    drain : {auto stateRef : Ref AsmState AsmState} -> InferredFunctionType
         -> NamedDef -> Core (List (Name, List InferredType))
    drain initialFunctionType def = do
      inferFunctionType (Just initialFunctionType) def
      inferDef
      log <- getConSiteLog
      pure $ map (\(_, n, _, _, ts) => (n, ts)) log

    gatherForSpec : SortedSet Name -> NamedDef -> SortedMap String (List CallbackSlot)
                 -> SpecialisedSignature
                 -> Core (List (Name, List InferredType))
    gatherForSpec current def cbSigs (MkSpecialisedSig specName specDef specType) = do
      let specParams = specType.parameterTypes
      let arity = case def of
                    MkNmFun ps _ => length ps
                    _ => length specParams
      let paddedParams = if length specParams < arity
                           then padParams arity specParams
                           else specParams
      let specInitial = MkInferredFunctionType specType.returnType paddedParams
      asmState <- coreLift $ AsmState.fromIdrisName specName
      specRef <- newRef AsmState asmState
      setSpecialisationPlan {stateRef = specRef} plan
      setConSpecialisationPlan {stateRef = specRef} conPlan
      setNaturalConsLive {stateRef = specRef} current
      setCallbackSlotSigs {stateRef = specRef} cbSigs
      drain {stateRef = specRef} specInitial specDef

    gatherFor : SortedSet Name -> Name -> Core (List (Name, List InferredType))
    gatherFor current name = do
      let nameStr = jvmSimpleName name
      let Just asmStateRef = SortedMap.lookup nameStr stateRefs
            | Nothing => pure []
      let Just def = SortedMap.lookup nameStr defs
            | Nothing => pure []
      setSpecialisationPlan {stateRef = asmStateRef} plan
      setConSpecialisationPlan {stateRef = asmStateRef} conPlan
      setNaturalConsLive {stateRef = asmStateRef} current
      initialFunctionType <- inferredFunctionType <$> getCurrentFunction {stateRef = asmStateRef}
      naturalSites <- drain {stateRef = asmStateRef} initialFunctionType def
      let specs = fromMaybe [] $ SortedMap.lookup name plan
      cbSigs <- getCallbackSlotSigs {stateRef = asmStateRef}
      specSites <- traverse (gatherForSpec current def cbSigs) specs
      pure $ naturalSites ++ concat specSites

    onePass : SortedSet Name -> Core (SortedSet Name)
    onePass current = do
      perName <- traverse (gatherFor current) reachable
      pure $ SortedSet.fromList $ mapMaybe needsNatural (concat perName)

    loop : SortedSet Name -> Core (SortedSet Name)
    loop current = do
      next <- onePass current
      let merged = SortedSet.union current next
      if SortedSet.toList merged == SortedSet.toList current
        then pure merged
        else loop merged

-- For each spec entry with a TCon-family interface, find every sibling
-- DCon's natural class and register that natural class to implement the
-- TCon interface.  Without this, a function spec parameter typed as the
-- family (e.g. `Maybe$I`) would refuse callers that pass the sibling
-- natural class (`NOTHING`), reducing routing to natural-only — or worse,
-- crash with a ClassCastException when a natural instance reaches a slot
-- the inference typed at the family.
--
-- Sibling matching mirrors BOTH discovery paths of `deriveTConClassName`:
-- the well-known `ConInfo` shapes (CONS/NIL, JUST/NOTHING, ZERO/SUCC)
-- share a synthetic family, and ordinary DCons (e.g. `Left`/`Right`,
-- which are plain DATACONs) resolve their parent TCon via
-- `findTConForDCon`.  Matching only the synthetic shapes left natural
-- `Right` without `implements Either$I` while `Either$I`-typed slots
-- existed — the prelude build then died with
-- `Right cannot be cast to Either$I`.
--
-- Inputs walked:
--   * `conPlan` — spec entries with their `tconClassName` and originating
--     `conInfo`.
--   * `conSiteLog` aggregates — surfaces sibling `Name`s actually
--     constructed in the program (so we register only siblings that exist
--     at runtime, avoiding spurious interface declarations).
computeNaturalToTConIfaces : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
                          -> String -> ConSpecialisationPlan
                          -> SortedMap String NamedDef
                          -> SortedMap String (Ref AsmState AsmState)
                          -> SpecialisationPlan
                          -> List Name
                          -> Core (SortedMap String (List String))
computeNaturalToTConIfaces programName conPlan defs stateRefs plan reachable = do
    perName <- traverse gatherSites reachable
    let sites = concat perName
    -- Family resolution hits the context (`findTConForDCon`), so collapse
    -- the site list to one entry per constructor before tagging.
    let uniqueSites = map snd $ SortedMap.toList $ SortedMap.fromList $
                        map (\site => (jvmSimpleName (fst site), site)) sites
    specFamilies <- traverse tagSpecFamily allSpecs
    siteFamilies <- traverse tagSiteFamily uniqueSites
    pure $ foldl (recordSibling specFamilies) SortedMap.empty siteFamilies
  where
    allSpecs : List SpecialisedConstructor
    allSpecs = concat (snd <$> SortedMap.toList conPlan)

    syntheticFamily : ConInfo -> Maybe String
    syntheticFamily NIL     = Just "List"
    syntheticFamily CONS    = Just "List"
    syntheticFamily NOTHING = Just "Maybe"
    syntheticFamily JUST    = Just "Maybe"
    syntheticFamily ZERO    = Just "Nat"
    syntheticFamily SUCC    = Just "Nat"
    syntheticFamily _       = Nothing

    Site : Type
    Site = (Name, ConInfo)

    -- A family key identifies one TCon hierarchy.  Well-known shapes
    -- share a synthetic key regardless of parent TCon (`CONS` matches
    -- `CONS` across `List$I$L`/`List$L$L` AND `NIL`); everything else is
    -- keyed by the parent TCon from the context, so `Left`/`Right` both
    -- map to `Either` and natural siblings of `Either$<sig>` get found.
    familyKey : Name -> ConInfo -> Core (Maybe String)
    familyKey name conInfo =
      case syntheticFamily conInfo of
        Just synthetic => pure $ Just ("synthetic:" ++ synthetic)
        Nothing => do
          mTCon <- findTConForDCon name
          pure $ (\tcon => "tcon:" ++ show tcon) <$> mTCon

    tagSpecFamily : SpecialisedConstructor -> Core (SpecialisedConstructor, Maybe String)
    tagSpecFamily sc@(MkSpecialisedCon dconName conInfo _ _ _ _ _ _) = do
      family <- familyKey dconName conInfo
      pure (sc, family)

    tagSiteFamily : Site -> Core (Name, Maybe String)
    tagSiteFamily (name, conInfo) = do
      family <- familyKey name conInfo
      pure (name, family)

    drainSites : {auto stateRef : Ref AsmState AsmState} -> InferredFunctionType
              -> NamedDef -> Core (List Site)
    drainSites initialFunctionType def = do
      inferFunctionType (Just initialFunctionType) def
      inferDef
      log <- getConSiteLog
      pure $ map (\(_, n, ci, _, _) => (n, ci)) log

    gatherSites : Name -> Core (List Site)
    gatherSites name = do
      let nameStr = jvmSimpleName name
      let Just asmStateRef = SortedMap.lookup nameStr stateRefs
            | Nothing => pure []
      let Just def = SortedMap.lookup nameStr defs
            | Nothing => pure []
      setConSpecialisationPlan {stateRef = asmStateRef} conPlan
      initialFunctionType <- inferredFunctionType <$> getCurrentFunction {stateRef = asmStateRef}
      drainSites {stateRef = asmStateRef} initialFunctionType def

    addUnique : String -> List String -> List String
    addUnique s xs = if elem s xs then xs else xs ++ [s]

    -- For each same-family spec, register its `tconClassName` on the
    -- site's natural class.  Multiple instantiations share a family key
    -- (e.g. `List$I$L` and `List$L$L` both ask `NIL` to implement them),
    -- so we accumulate one entry per matching spec instead of choosing
    -- one canonical interface per family.
    recordSibling : List (SpecialisedConstructor, Maybe String)
                 -> SortedMap String (List String) -> (Name, Maybe String)
                 -> SortedMap String (List String)
    recordSibling specs acc (name, Just siteFamily) =
      let cls = getIdrisConstructorClassName programName (jvmSimpleName name)
          existing = fromMaybe [] (SortedMap.lookup cls acc)
          matching = filter (\(sc, family) => sc.tconClassName /= "" && family == Just siteFamily) specs
          updated = foldl (\xs, (sc, _) => addUnique sc.tconClassName xs) existing matching
      in if isNil matching then acc else SortedMap.insert cls updated acc
    recordSibling _ acc (_, Nothing) = acc

-- Declaration-driven callback-slot classification for every function def:
-- jvmSimpleName -> one CallbackSlot per unerased parameter slot (see
-- getTermCallbackSigs).  Only functions with at least one callback slot
-- get an entry.
computeCallbackSlotSigs : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
                       -> LazyList (Name, FC, NamedDef)
                       -> Core (SortedMap String (List CallbackSlot))
computeCallbackSlotSigs nameFcDefs = go SortedMap.empty nameFcDefs
  where
    go : SortedMap String (List CallbackSlot) -> LazyList (Name, FC, NamedDef)
       -> Core (SortedMap String (List CallbackSlot))
    go acc [] = pure acc
    go acc ((name, _, MkNmFun _ _) :: rest) = do
      sigs <- getTermCallbackSigs name
      if any isCallbackSlot sigs
        then go (SortedMap.insert (jvmSimpleName name) sigs acc) rest
        else go acc rest
    go acc (_ :: rest) = go acc rest

||| Compile a TT expression to JVM bytecode
compileToJvmBytecode : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> String -> String -> ClosedTerm -> Core ()
compileToJvmBytecode outputDirectory outputFile term = do
    noMangleMapRef <- initNoMangle ["jvm"] (const True)
    noMangleMap <- get NoMangleMap
    cdata <- getCompileDataWith ["jvm"] False Cases term
    directives <- getDirectives Jvm
    let ndefs = namedDefs cdata
    let idrisMainBody = forget (mainExpr cdata)
    let programName = if outputFile == "" then "repl" else outputFile
    let mainFunctionName = idrisMainFunctionName programName
    let allDefs = the (LazyList _) $ (mainFunctionName, emptyFC, MkNmFun [] idrisMainBody) :: fromList ndefs
    let nameFcDefs = filter (isForeignDef . snd . snd) allDefs ++ optimize programName allDefs
    let nameStrFcDefs = getNameStrFcDef programName <$> nameFcDefs
    fcAndDefinitionsByName <- coreLift $ Map.fromLazyList nameStrFcDefs
    coreLift $ initAsmGlobalState programName fcAndDefinitionsByName
    nameFcStateRefs <- inferFunctionTypes nameFcDefs
    namedDefsByName <- getNamedDefsByName programName nameFcStateRefs
    let exportedNames = mapMaybe (\(n, _, _) => n <$ isNoMangle noMangleMap n) allDefs
    let rootNames = mainFunctionName :: toList exportedNames
    let callForest  = buildFunctionForestMain rootNames namedDefsByName
    let reachable   = concatMap postOrder callForest
    let reachableSet = SortedSet.fromList reachable
    let stateRefsMap = SortedMap.fromList $ mapMaybe (\(n, _, r) => Just (jvmSimpleName n, r)) (toList nameFcStateRefs)
    -- Declaration-driven callback signatures (higher-order specialisation)
    -- are a pure function of the Ctxt; compute them once and install on
    -- every per-def state before ANY inferDef runs, so all inference
    -- passes (plan iteration, naturalConsLive fixpoint, final assembly)
    -- see the same configuration.
    callbackSigs <- computeCallbackSlotSigs nameFcDefs
    traverse_ (\(_, r) => setCallbackSlotSigs {stateRef = r} callbackSigs) (SortedMap.toList stateRefsMap)
    (plan, conPlan) <- buildSpecialisationPlan programName namedDefsByName stateRefsMap reachable
    natLive <- computeNaturalConsLive namedDefsByName stateRefsMap plan conPlan reachable
    natIfaces <- computeNaturalToTConIfaces programName conPlan namedDefsByName stateRefsMap plan reachable
    preRegisterSpecs plan
    preRegisterConstructorSpecs conPlan
    preRegisterCallbackInterfaces programName plan callbackSigs
    let reachableNameFcStateRefs = filter (\(n, _, _) => SortedSet.contains n reachableSet) nameFcStateRefs
    assembleNameFcStateRefs plan conPlan natLive natIfaces namedDefsByName reachableNameFcStateRefs
    coreLift $ do
        exportDefs $ mapMaybe (getExport noMangleMap . fst) allDefs
        mainAsmState <- AsmState.fromIdrisName mainFunctionName
        let mainFunctionJname = jvmName programName mainFunctionName
        let javaMainClassName = programName ++ "/JvmMain"
        ignore $ runAsm mainAsmState $ \stateRef => createMainMethod programName mainFunctionJname javaMainClassName
        classCodeEnd outputDirectory outputFile (programName ++ ".JvmMain")
  where
    inferFunctionTypes : LazyList (Name, FC, NamedDef) -> Core (LazyList (Name, FC, Ref AsmState AsmState))
    inferFunctionTypes nameFcDefs = go [] nameFcDefs where
      go : LazyList (Name, FC, (Ref AsmState AsmState)) -> LazyList (Name, FC, NamedDef) -> Core (LazyList (Name, FC, (Ref AsmState AsmState)))
      go acc [] = pure acc
      go acc ((name, fc, def) :: rest) = do
        asmState <- coreLift $ AsmState.fromIdrisName name
        asmStateRef <- newRef AsmState asmState
        initialFunctionType <- if isFunctionDef def then Just <$> getInitialFunctionType name def else pure Nothing
        inferFunctionType initialFunctionType def
        go ((name, fc, asmStateRef) :: acc) rest

||| JVM bytecode implementation of the `compileExpr` interface.
compileExprJvm : Ref Ctxt Defs
               -> Ref Syn SyntaxInfo -> (tmpDir : String) -> (outDir: String) -> ClosedTerm
               -> (outputFile : String) -> Core (Maybe String)
compileExprJvm _ _ tmpDir outDir term outputFile
    = do let outputDirectory = if outputFile == "" then "" else outDir
         when (outputDirectory /= "") $ ignore $ coreLift $ mkdirAll outputDirectory
         compileToJvmBytecode outputDirectory outputFile term
         pure $ Just outputDirectory

||| JVM bytecode implementation of the `executeExpr` interface.
||| This implementation simply runs the usual compiler, saving it to a temp file, then interpreting it.
executeExprJvm : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> (execDir : String) -> ClosedTerm -> Core ()
executeExprJvm c s execDir term = ignore $ compileExprJvm c s execDir "" term ""

||| Codegen wrapper for JVM implementation.
export
codegenJvm : Codegen
codegenJvm = MkCG compileExprJvm executeExprJvm Nothing Nothing
