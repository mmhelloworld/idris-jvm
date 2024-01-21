module Compiler.Jvm.Codegen

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.NoMangle

import Core.Context
import Core.Directory
import Core.Name
import Core.Options
import Core.TT

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect

import Debug.Trace

import Libraries.Data.NameMap
import Libraries.Data.SortedMap
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

%hide System.FFI.runtimeClass
%hide Compiler.Jvm.Asm.assemble
%hide Core.Context.Context.Constructor.arity

addScopeLocalVariables : Scope -> Asm ()
addScopeLocalVariables scope = do
    let scopeIndex = index scope
    let (lineNumberStart, lineNumberEnd) = lineNumbers scope
    let (labelStart, labelEnd) = labels scope
    nameAndIndices <- LiftIo $ Map.toList $ variableIndices scope
    go labelStart labelEnd nameAndIndices
  where
    go : String -> String -> List (String, Int) -> Asm ()
    go _ _ [] = Pure ()
    go labelStart labelEnd ((name, varIndex) :: rest) = do
        variableType <- getVariableTypeAtScope (index scope) name
        LocalVariable name (getJvmTypeDescriptor variableType) Nothing labelStart labelEnd varIndex
        go labelStart labelEnd rest

addLocalVariables : Int -> Asm ()
addLocalVariables scopeIndex = do
    scope <- getScope scopeIndex
    addScopeLocalVariables scope
    traverse_ addLocalVariables $ childIndices scope

enterScope : Asm ()
enterScope = do
    scopeIndex <- newScopeIndex
    updateCurrentScopeIndex scopeIndex

exitScope : Int -> Asm ()
exitScope = updateCurrentScopeIndex

withScope : Lazy (Asm ()) -> Asm ()
withScope op = do
    scopeIndex <- getCurrentScopeIndex
    enterScope
    op
    exitScope scopeIndex

defaultValue : InferredType -> Asm ()
defaultValue IBool = Iconst 0
defaultValue IByte = Iconst 0
defaultValue IChar = Iconst 0
defaultValue IShort = Iconst 0
defaultValue IInt = Iconst 0
defaultValue ILong = Ldc $ Int64Const 0
defaultValue IFloat = Fconst 0
defaultValue IDouble = Dconst 0
defaultValue _ = Aconstnull

assembleArray : (elemTy: InferredType) -> Asm ()
assembleArray IBool = Anewbooleanarray
assembleArray IByte = Anewbytearray
assembleArray IChar = Anewchararray
assembleArray IShort = Anewshortarray
assembleArray IInt = Anewintarray
assembleArray ILong = Anewlongarray
assembleArray IFloat = Anewfloatarray
assembleArray IDouble = Anewdoublearray
assembleArray (IRef ty _ _) = Anewarray ty
assembleArray (IArray ty) = Anewarray (getJvmTypeDescriptor ty)
assembleArray (IFunction (MkJavaLambdaType (IRef ty _ _) _ _ _)) = Anewarray ty
assembleArray _ = Anewarray "java/lang/Object"

storeArray : (elemTy: InferredType) -> Asm ()
storeArray IBool = Bastore
storeArray IByte = Bastore
storeArray IChar = Castore
storeArray IShort = Sastore
storeArray IInt = Iastore
storeArray ILong = Lastore
storeArray IFloat = Fastore
storeArray IDouble = Dastore
storeArray _ = Aastore

loadArray : (elemTy: InferredType) -> Asm ()
loadArray IBool = Baload
loadArray IByte = Baload
loadArray IChar = Caload
loadArray IShort = Saload
loadArray IInt = Iaload
loadArray ILong = Laload
loadArray IFloat = Faload
loadArray IDouble = Daload
loadArray _ = Aaload

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

constantAltIntExpr : FC -> NamedConstAlt -> Asm (String, Int, NamedConstAlt)
constantAltIntExpr fc alt@(MkNConstAlt constant _) = do
        constExpr <- getIntConstantValue fc constant
        label <- newLabel
        Pure (label, constExpr, alt)

%foreign jvm' "java/lang/Long" "hashCode" "long" "int"
int64HashCode : Int64 -> Int

%foreign jvm' "java/lang/Long" "hashCode" "long" "int"
bits64HashCode : Bits64 -> Int

hashCode : TT.Constant -> Maybe Int
hashCode (BI value) = Just $ Object.hashCode value
hashCode (I64 value) = Just $ int64HashCode value
hashCode (B64 value) = Just $ bits64HashCode value
hashCode (Str value) = Just $ Object.hashCode value
hashCode x = Nothing

getHashCodeSwitchClass : FC -> InferredType -> Asm String
getHashCodeSwitchClass fc (IRef "java/lang/String" _ _) = Pure stringClass
getHashCodeSwitchClass fc (IRef "java/math/BigInteger" _ _) = Pure bigIntegerClass
getHashCodeSwitchClass fc ILong = Pure "java/lang/Long"
getHashCodeSwitchClass fc constantType = asmCrash ("Constant type " ++ show constantType ++ " cannot be compiled to 'Switch'.")

assembleHashCodeSwitchConstant : FC -> TT.Constant -> Asm ()
assembleHashCodeSwitchConstant _ (BI value) = newBigInteger $ show value
assembleHashCodeSwitchConstant _ (I64 value) = Ldc $ Int64Const value
assembleHashCodeSwitchConstant _ (B64 value) = Ldc $ Bits64Const value
assembleHashCodeSwitchConstant _ (Str value) = Ldc $ StringConst value
assembleHashCodeSwitchConstant fc constant =
    asmCrash $ "Constant " ++ show constant ++ " cannot be compiled to 'switch'"

conAltIntExpr : NamedConAlt -> Asm (String, Int, NamedConAlt)
conAltIntExpr alt@(MkNConAlt name conInfo tag _ expr) = do
    label <- newLabel
    intValue <- case conInfo of
      NOTHING => Pure 0
      NIL => Pure 0
      JUST => Pure 1
      CONS => Pure 1
      _ => maybe (asmCrash $ "Missing constructor tag " ++ show name) Pure tag
    Pure (label, intValue, alt)

conAltStringExpr : NamedConAlt -> Asm (String, String, NamedConAlt)
conAltStringExpr alt@(MkNConAlt name _ _ _ expr) = do
    label <- newLabel
    Pure (label, jvmSimpleName name, alt)

createDefaultLabel : Asm String
createDefaultLabel = do
    label <- newLabel
    CreateLabel label
    Pure label

getSwitchCasesWithEndLabel : List (String, Int, a) -> List String -> List (String, Int, a, String)
getSwitchCasesWithEndLabel switchCases labelStarts = go $ zip switchCases (drop 1 labelStarts ++ [methodEndLabel])
    where
        go : List ((String, Int, a), String) -> List (String, Int, a, String)
        go (((labelStart, constExpr, body), labelEnd) :: xs) = (labelStart, constExpr, body, labelEnd) :: go xs
        go [] = []

labelHashCodeAlt : (Int, a) -> Asm (String, Int, a)
labelHashCodeAlt (hash, expressions) = Pure (!newLabel, hash, expressions)

getHashCodeCasesWithLabels : SortedMap Int (List (Int, a)) ->
    Asm (List (String, Int, List (Int, a)))
getHashCodeCasesWithLabels positionAndAltsByHash = traverse labelHashCodeAlt $ SortedMap.toList positionAndAltsByHash

toUnsignedInt : Int -> Asm ()
toUnsignedInt bits = do
  Iconst bits
  InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False

assembleInt : (isTailCall: Bool) -> InferredType -> Int -> Asm ()
assembleInt isTailCall returnType value = do
    Iconst value
    asmCast IInt returnType
    when isTailCall $ asmReturn returnType

assembleInt64 : (isTailCall: Bool) -> InferredType -> Int64 -> Asm ()
assembleInt64 isTailCall returnType value = do
    Ldc $ Int64Const value
    asmCast ILong returnType
    when isTailCall $ asmReturn returnType

assembleBits64 : (isTailCall: Bool) -> InferredType -> Bits64 -> Asm ()
assembleBits64 isTailCall returnType value = do
    Ldc $ Bits64Const value
    asmCast ILong returnType
    when isTailCall $ asmReturn returnType

isInterfaceInvocation : InferredType -> Bool
isInterfaceInvocation (IRef _ Interface _) = True
isInterfaceInvocation _ = False

assembleNil : (isTailCall: Bool) -> InferredType -> Asm ()
assembleNil isTailCall returnType = do
    Field GetStatic idrisNilClass "INSTANCE" "Lio/github/mmhelloworld/idrisjvm/runtime/IdrisList$Nil;"
    asmCast idrisObjectType returnType
    when isTailCall $ asmReturn returnType

assembleNothing : (isTailCall: Bool) -> InferredType -> Asm ()
assembleNothing isTailCall returnType = do
    Field GetStatic idrisNothingClass "INSTANCE" "Lio/github/mmhelloworld/idrisjvm/runtime/Maybe$Nothing;"
    asmCast idrisObjectType returnType
    when isTailCall $ asmReturn returnType

getDynamicVariableIndex : (variablePrefix: String) -> Asm Int
getDynamicVariableIndex variablePrefix = do
    suffixIndex <- newDynamicVariableIndex
    let variableName = variablePrefix ++ show suffixIndex
    getVariableIndex variableName

assembleIdentityLambda : (isTailCall : Bool) -> Asm ()
assembleIdentityLambda isTailCall = do
  Field GetStatic functionsClass "IDENTITY" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleIdentity1Lambda : (isTailCall : Bool) -> Asm ()
assembleIdentity1Lambda isTailCall = do
  Field GetStatic functionsClass "IDENTITY_1" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleIdentity2Lambda : (isTailCall : Bool) -> Asm ()
assembleIdentity2Lambda isTailCall = do
  Field GetStatic functionsClass "IDENTITY_2" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleConstantLambda : (isTailCall : Bool) -> Asm ()
assembleConstantLambda isTailCall = do
  Field GetStatic functionsClass "CONSTANT" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

assembleConstant1Lambda : (isTailCall : Bool) -> Asm ()
assembleConstant1Lambda isTailCall = do
  Field GetStatic functionsClass "CONSTANT_1" (getJvmTypeDescriptor inferredLambdaType)
  when isTailCall $ asmReturn inferredLambdaType

getLambdaTypeByArity: (arity: Nat) -> LambdaType
getLambdaTypeByArity 2 = Function2Lambda
getLambdaTypeByArity 3 = Function3Lambda
getLambdaTypeByArity 4 = Function4Lambda
getLambdaTypeByArity 5 = Function5Lambda
getLambdaTypeByArity _ = FunctionLambda

assembleClassLiteral : InferredType -> Asm ()
assembleClassLiteral IByte   = Field GetStatic "java/lang/Byte" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IChar   = Field GetStatic "java/lang/Character" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IShort  = Field GetStatic "java/lang/Short" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IBool   = Field GetStatic "java/lang/Boolean" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IDouble = Field GetStatic "java/lang/Double" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IFloat  = Field GetStatic "java/lang/Float" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IInt    = Field GetStatic "java/lang/Integer" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral ILong   = Field GetStatic "java/lang/Long" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral IVoid   = Field GetStatic "java/lang/Void" "TYPE"  "Ljava/lang/Class;"
assembleClassLiteral type    = Ldc $ TypeConst $ getJvmTypeDescriptor type

intToBigInteger : Asm ()
intToBigInteger = do
  I2l
  InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False

mutual
    assembleExpr : (isTailCall: Bool) -> InferredType -> NamedCExp -> Asm ()
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
        CreateLabel valueScopeStartLabel
        targetExprScopeStartLabel <- newLabel
        CreateLabel targetExprScopeStartLabel
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
            LabelStart valueScopeStartLabel
            addLineNumber lineNumberStart valueScopeStartLabel
            assembleExpr False variableType value
            storeVar variableType variableType variableIndex

        withScope $ do
            targetExprScopeIndex <- getCurrentScopeIndex
            scope <- getScope targetExprScopeIndex
            let (lineNumberStart, lineNumberEnd) = lineNumbers scope
            updateScopeStartLabel targetExprScopeIndex targetExprScopeStartLabel
            LabelStart targetExprScopeStartLabel
            addLineNumber lineNumberStart targetExprScopeStartLabel
            updateScopeEndLabel targetExprScopeIndex methodEndLabel
            assembleExpr isTailCall returnType expr

    -- Tail recursion. Store arguments and recur to the beginning of the method
    assembleExpr _ returnType app@(NmApp fc (NmRef _ (UN (Basic "$idrisTailRec"))) args) =
        case length args of
            Z => Goto methodStartLabel
            (S lastArgIndex) => do
                jname <- idrisName <$> getCurrentFunction
                parameterTypes <- getFunctionParameterTypes jname
                let argsWithTypes = zip args parameterTypes
                variableTypes <- getVariableTypes
                let argIndices = [0 .. the Int $ cast lastArgIndex]
                targetVariableIndices <- traverse (storeParameter variableTypes) $ zip argIndices argsWithTypes
                traverse_ (assign variableTypes) $ zip targetVariableIndices $ zip argIndices parameterTypes
                Goto methodStartLabel
              where
                assign : Map Int InferredType -> (Int, Int, InferredType) -> Asm ()
                assign types (targetVariableIndex, argIndex, ty) =
                    when (targetVariableIndex /= argIndex) $ do
                        loadVar types ty ty targetVariableIndex
                        storeVar ty ty argIndex

    assembleExpr isTailCall returnType (NmApp _ (NmRef _ idrisName) []) =
        assembleNmAppNilArity isTailCall returnType idrisName
    assembleExpr isTailCall returnType (NmApp _ (NmRef _ idrisName) args) =
      if isSuperCall idrisName args then do Aconstnull; when isTailCall $ asmReturn returnType
      else do
        let jname = jvmName idrisName
        functionType <- case !(findFunctionType jname) of
            Just ty => Pure ty
            Nothing => Pure $ MkInferredFunctionType inferredObjectType $ replicate (length args) inferredObjectType
        let paramTypes = parameterTypes functionType
        if paramTypes == []
            then assembleNmAppNilArity isTailCall returnType idrisName
            else do
                let argsWithTypes = zip args paramTypes
                traverse_ assembleParameter argsWithTypes
                let methodReturnType = InferredFunctionType.returnType functionType
                let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType methodReturnType paramTypes
                let functionName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
                InvokeMethod InvokeStatic (className functionName) (methodName functionName) methodDescriptor False
                asmCast methodReturnType returnType
                when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType (NmApp _ lambdaVariable [arg]) = do
        assembleExpr False inferredLambdaType lambdaVariable
        assembleExpr False IUnknown arg
        InvokeMethod InvokeInterface "java/util/function/Function" "apply" "(Ljava/lang/Object;)Ljava/lang/Object;" True
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType expr@(NmCon _ _ NOTHING _ []) = assembleNothing isTailCall returnType
    assembleExpr isTailCall returnType expr@(NmCon fc _ NOTHING _ _) = Throw fc "Invalid NOTHING constructor"
    assembleExpr isTailCall returnType expr@(NmCon _ _ JUST _ [value]) = assembleJust isTailCall returnType value
    assembleExpr isTailCall returnType expr@(NmCon fc _ JUST _ _) = Throw fc "Invalid JUST constructor"

    assembleExpr isTailCall returnType expr@(NmCon _ _ NIL _ []) = assembleNil isTailCall returnType
    assembleExpr isTailCall returnType expr@(NmCon fc _ NIL _ _) = Throw fc "Invalid NIL constructor"
    assembleExpr isTailCall returnType expr@(NmCon _ _ CONS _ [head, tail]) =
      assembleCons isTailCall returnType head tail
    assembleExpr isTailCall returnType expr@(NmCon fc _ CONS _ _) = Throw fc "Invalid CONS constructor"

    assembleExpr isTailCall returnType expr@(NmCon fc name conInfo tag args) =
        assembleCon isTailCall returnType fc name tag args

    assembleExpr isTailCall returnType (NmOp fc fn args) = do
        assembleExprOp returnType fc fn args
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmExtPrim fc p args) = do
        jvmExtPrim fc returnType (toPrim p) args
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmForce _ _ expr) = do
        assembleExpr False delayedType expr
        InvokeMethod InvokeStatic runtimeClass "force" "(Ljava/lang/Object;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType (NmConCase fc sc [] Nothing) = do
        defaultValue returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmConCase fc sc [] (Just expr)) = do
        ignore $ assembleConstructorSwitchExpr sc
        assembleExpr isTailCall returnType expr
    assembleExpr _ returnType (NmConCase fc sc [MkNConAlt name _ _ args expr] Nothing) = do
        idrisObjectVariableIndex <- assembleConstructorSwitchExpr sc
        assembleConCaseExpr returnType idrisObjectVariableIndex args expr
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
        Ldc $ StringConst value
        asmCast inferredStringType returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmPrimVal fc (Ch value)) = do
        Iconst $ cast value
        asmCast IChar returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmPrimVal fc (Db value)) = do
        Ldc $ DoubleConst value
        asmCast IDouble returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmPrimVal fc _) = do
        Iconst 0
        asmCast IInt returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall IInt (NmErased fc) = do Iconst 0; when isTailCall $ asmReturn IInt
    assembleExpr isTailCall IChar (NmErased fc) = do Iconst 0; when isTailCall $ asmReturn IChar
    assembleExpr isTailCall IDouble (NmErased fc) =  do Ldc $ DoubleConst 0; when isTailCall $ asmReturn IDouble
    assembleExpr isTailCall returnType (NmErased fc) = do Aconstnull; when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmCrash fc msg) = do
        Ldc $ StringConst msg
        InvokeMethod InvokeStatic runtimeClass "crash" "(Ljava/lang/String;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType
    assembleExpr _ _ expr = Throw (getFC expr) $ "Cannot compile " ++ show expr ++ " yet"

    castInt : InferredType -> Asm() -> NamedCExp -> Asm ()
    castInt returnType conversionOp expr = jassembleCast returnType IInt IInt conversionOp expr

    jassembleCast : InferredType -> InferredType -> InferredType -> Asm() -> NamedCExp -> Asm ()
    jassembleCast returnType from to conversionOp expr = do
        assembleExpr False from expr
        conversionOp
        asmCast to returnType

    assembleNmAppNilArity : (isTailCall : Bool) -> InferredType -> Name -> Asm ()
    assembleNmAppNilArity isTailCall returnType idrisName = do
        let jname = jvmName idrisName
        let functionName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
        Field GetStatic (className functionName) (methodName functionName)
            "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
        InvokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate"
            "()Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    unsignedIntToBigInteger : Asm ()
    unsignedIntToBigInteger = do
        InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
        InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False

    unsignedIntToString : Asm ()
    unsignedIntToString = InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedString" "(I)Ljava/lang/String;" False

    bigIntegerToInt : Asm () -> Asm ()
    bigIntegerToInt op = do
      InvokeMethod InvokeVirtual "java/math/BigInteger" "intValue" "()I" False
      op

    assembleCon : (isTailCall: Bool) -> InferredType -> FC -> Name -> (tag : Maybe Int) -> List NamedCExp -> Asm ()
    assembleCon isTailCall returnType fc name tag args = do
        let fileName = fst $ getSourceLocationFromFc fc
        let constructorClassName = getIdrisConstructorClassName (jvmSimpleName name)
        let constructorType = maybe inferredStringType (const IInt) tag
        New constructorClassName
        Dup
        maybe (Ldc . StringConst $ constructorClassName) Iconst tag
        let constructorParameterCountNat = length args
        let constructorParameterCount = the Int $ cast constructorParameterCountNat
        let constructorTypes = constructorType :: replicate constructorParameterCountNat inferredObjectType
        let argsWithTypes = zip args $ drop 1 constructorTypes
        traverse_ assembleParameter argsWithTypes
        let descriptor = getMethodDescriptor $ MkInferredFunctionType IVoid constructorTypes
        globalState <- getGlobalState
        hasConstructor <- LiftIo $ AsmGlobalState.hasConstructor globalState constructorClassName
        when (not hasConstructor) $ do
            LiftIo $ AsmGlobalState.addConstructor globalState constructorClassName
            CreateIdrisConstructorClass constructorClassName (isNothing tag) constructorParameterCount
        InvokeMethod InvokeSpecial constructorClassName "<init>" descriptor False
        asmCast idrisObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleCons : (isTailCall: Bool) -> InferredType -> NamedCExp -> NamedCExp -> Asm ()
    assembleCons isTailCall returnType head tail = do
        New idrisConsClass
        Dup
        assembleExpr False inferredObjectType head
        assembleExpr False inferredObjectType tail
        InvokeMethod InvokeSpecial idrisConsClass "<init>" "(Ljava/lang/Object;Ljava/lang/Object;)V" False
        asmCast idrisObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleJust : (isTailCall: Bool) -> InferredType -> NamedCExp -> Asm ()
    assembleJust isTailCall returnType value = do
        New idrisJustClass
        Dup
        assembleExpr False inferredObjectType value
        InvokeMethod InvokeSpecial idrisJustClass "<init>" "(Ljava/lang/Object;)V" False
        asmCast idrisObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleConstructorSwitchExpr : NamedCExp -> Asm Int
    assembleConstructorSwitchExpr (NmLocal _ loc) = getVariableIndex $ jvmSimpleName loc
    assembleConstructorSwitchExpr sc = do
        idrisObjectVariableIndex <- getVariableIndex $ "constructorSwitchValue" ++ show !newDynamicVariableIndex
        assembleExpr False idrisObjectType sc
        storeVar idrisObjectType idrisObjectType idrisObjectVariableIndex
        Pure idrisObjectVariableIndex

    assembleExprBinaryOp : InferredType -> InferredType -> Asm () -> NamedCExp -> NamedCExp -> Asm ()
    assembleExprBinaryOp returnType exprType operator expr1 expr2 = do
        assembleExpr False exprType expr1
        assembleExpr False exprType expr2
        operator
        asmCast exprType returnType

    assembleExprBinaryBoolOp : InferredType -> InferredType -> (String -> Asm ()) ->
        NamedCExp -> NamedCExp -> Asm ()
    assembleExprBinaryBoolOp returnType exprType operator expr1 expr2 = do
        assembleExpr False exprType expr1
        assembleExpr False exprType expr2
        ifLabel <- newLabel
        CreateLabel ifLabel
        elseLabel <- newLabel
        CreateLabel elseLabel
        endLabel <- newLabel
        CreateLabel endLabel
        operator elseLabel
        LabelStart ifLabel
        Iconst 1
        Goto endLabel
        LabelStart elseLabel
        Iconst 0
        LabelStart endLabel
        asmCast IInt returnType

    assembleExprComparableBinaryBoolOp : InferredType -> String -> (String -> Asm ()) ->
        NamedCExp -> NamedCExp -> Asm ()
    assembleExprComparableBinaryBoolOp returnType className operator expr1 expr2 = do
        let exprType = IRef className Class []
        assembleExpr False exprType expr1
        assembleExpr False exprType expr2
        ifLabel <- newLabel
        CreateLabel ifLabel
        elseLabel <- newLabel
        CreateLabel elseLabel
        endLabel <- newLabel
        CreateLabel endLabel
        InvokeMethod InvokeVirtual className "compareTo" ("(L" ++ className ++ ";)I") False
        operator elseLabel
        LabelStart ifLabel
        Iconst 1
        Goto endLabel
        LabelStart elseLabel
        Iconst 0
        LabelStart endLabel
        asmCast IInt returnType

    assembleExprUnaryOp : InferredType -> InferredType -> Asm () -> NamedCExp -> Asm ()
    assembleExprUnaryOp returnType exprType operator expr = do
        assembleExpr False exprType expr
        operator
        asmCast exprType returnType

    assembleStrCons : InferredType -> (char: NamedCExp) -> (str: NamedCExp) -> Asm ()
    assembleStrCons returnType char str = do
        New "java/lang/StringBuilder"
        Dup
        InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
        assembleExpr False IChar char
        InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(C)Ljava/lang/StringBuilder;" False
        assembleExpr False inferredStringType str
        InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append"
            "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
        InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
        asmCast inferredStringType returnType

    assembleStrReverse : InferredType -> NamedCExp -> Asm ()
    assembleStrReverse returnType str = do
        New "java/lang/StringBuilder"
        Dup
        assembleExpr False inferredStringType str
        InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "(Ljava/lang/String;)V" False
        InvokeMethod InvokeVirtual "java/lang/StringBuilder" "reverse" "()Ljava/lang/StringBuilder;" False
        InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
        asmCast inferredStringType returnType

    compareUnsignedLong : (String -> Asm ()) -> String -> Asm ()
    compareUnsignedLong op label = do longCompareUnsigned; op label

    compareUnsignedInt : (String -> Asm ()) -> String -> Asm ()
    compareUnsignedInt op label = do integerCompareUnsigned; op label

    compareSignedLong : (String -> Asm ()) -> String -> Asm ()
    compareSignedLong op label = do Lcmp; op label

    assembleCast : InferredType -> FC -> PrimType -> PrimType -> NamedCExp -> Asm ()
    assembleCast returnType fc from to x =
      jassembleCast returnType (getInferredType from) (getInferredType to) (getCastAsmOp from to) x

    getCastAsmOp : PrimType -> PrimType -> Asm ()
    getCastAsmOp IntegerType Bits8Type = do
        Iconst 8
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(Ljava/math/BigInteger;I)I" False
    getCastAsmOp IntegerType Bits16Type = do
        Iconst 16
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(Ljava/math/BigInteger;I)I" False
    getCastAsmOp IntegerType Bits32Type = do
        Iconst 32
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(Ljava/math/BigInteger;I)I" False
    getCastAsmOp IntegerType Bits64Type = do
        Iconst 64
        InvokeMethod InvokeStatic conversionClass "toUnsignedLong" "(Ljava/math/BigInteger;I)J" False
    getCastAsmOp IntegerType Int64Type = InvokeMethod InvokeVirtual "java/math/BigInteger" "longValue" "()J" False
    getCastAsmOp IntegerType Int16Type = bigIntegerToInt I2s
    getCastAsmOp IntegerType Int32Type = bigIntegerToInt (Pure ())
    getCastAsmOp IntegerType Int8Type = bigIntegerToInt I2b
    getCastAsmOp IntegerType IntType = bigIntegerToInt (Pure ())
    getCastAsmOp IntegerType CharType =
      bigIntegerToInt (InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False)
    getCastAsmOp IntegerType DoubleType = InvokeMethod InvokeVirtual "java/math/BigInteger" "doubleValue" "()D" False
    getCastAsmOp IntegerType StringType = InvokeMethod InvokeVirtual "java/math/BigInteger" "toString" "()Ljava/lang/String;" False

    getCastAsmOp Int8Type Bits64Type = I2l
    getCastAsmOp Int8Type IntegerType = intToBigInteger
    getCastAsmOp Int8Type Int64Type = I2l
    getCastAsmOp Int8Type DoubleType = I2d
    getCastAsmOp Int8Type CharType = InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

    getCastAsmOp Int16Type Int8Type = I2b
    getCastAsmOp Int16Type IntegerType = intToBigInteger
    getCastAsmOp Int16Type Bits64Type = I2l
    getCastAsmOp Int16Type Int64Type = I2l
    getCastAsmOp Int16Type DoubleType = I2d
    getCastAsmOp Int16Type CharType = InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

    getCastAsmOp Int32Type Int8Type = I2b
    getCastAsmOp Int32Type Int16Type = I2s
    getCastAsmOp Int32Type Int64Type = I2l
    getCastAsmOp Int32Type Bits64Type = I2l
    getCastAsmOp Int32Type Bits16Type = toUnsignedInt 16
    getCastAsmOp Int32Type Bits8Type = toUnsignedInt 8
    getCastAsmOp Int32Type IntegerType = intToBigInteger
    getCastAsmOp Int32Type DoubleType = I2d
    getCastAsmOp Int32Type CharType = InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

    getCastAsmOp IntType Int8Type = I2b
    getCastAsmOp IntType Int16Type = I2s
    getCastAsmOp IntType Int64Type = I2l
    getCastAsmOp IntType Bits64Type = I2l
    getCastAsmOp IntType Bits16Type = toUnsignedInt 16
    getCastAsmOp IntType Bits8Type = toUnsignedInt 8
    getCastAsmOp IntType IntegerType = intToBigInteger
    getCastAsmOp IntType DoubleType = I2d
    getCastAsmOp IntType CharType = InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False

    getCastAsmOp DoubleType StringType =
      InvokeMethod InvokeStatic "java/lang/Double" "toString" "(D)Ljava/lang/String;" False
    getCastAsmOp DoubleType IntegerType = do
        InvokeMethod InvokeStatic "java/math/BigDecimal" "valueOf" "(D)Ljava/math/BigDecimal;" False
        InvokeMethod InvokeVirtual "java/math/BigDecimal" "toBigInteger" "()Ljava/math/BigInteger;" False
    getCastAsmOp DoubleType Bits8Type = do D2i; toUnsignedInt 8
    getCastAsmOp DoubleType Bits16Type = do D2i; toUnsignedInt 16
    getCastAsmOp DoubleType Bits32Type = do D2l; L2i
    getCastAsmOp DoubleType Bits64Type = InvokeMethod InvokeStatic conversionClass "toLong" "(D)J" False
    getCastAsmOp DoubleType IntType = do D2l; L2i
    getCastAsmOp DoubleType Int8Type = do D2i; I2b
    getCastAsmOp DoubleType Int16Type = do D2i; I2s
    getCastAsmOp DoubleType Int32Type = do D2l; L2i
    getCastAsmOp DoubleType Int64Type = InvokeMethod InvokeStatic conversionClass "toLong" "(D)J" False
    getCastAsmOp DoubleType _ = D2i

    getCastAsmOp CharType IntegerType = do
        I2l
        InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
    getCastAsmOp CharType Bits64Type = I2l
    getCastAsmOp CharType Int64Type = I2l
    getCastAsmOp CharType DoubleType = I2d
    getCastAsmOp CharType StringType =
      InvokeMethod InvokeStatic "java/lang/Character" "toString" "(C)Ljava/lang/String;" False
    getCastAsmOp CharType _ = Pure ()

    getCastAsmOp Bits8Type Bits16Type = do
        Iconst 16
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp Bits8Type Bits32Type = do
        I2l
        Iconst 32
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Bits8Type Bits64Type = do
        Iconst 64
        InvokeMethod InvokeStatic conversionClass "toUnsignedLong" "(II)J" False
    getCastAsmOp Bits8Type IntegerType = unsignedIntToBigInteger
    getCastAsmOp Bits8Type Int8Type = I2b
    getCastAsmOp Bits8Type Int16Type = I2s
    getCastAsmOp Bits8Type Int64Type = I2l
    getCastAsmOp Bits8Type CharType = InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
    getCastAsmOp Bits8Type StringType = unsignedIntToString
    getCastAsmOp Bits8Type DoubleType = I2d
    getCastAsmOp Bits8Type _ = Pure ()

    getCastAsmOp Bits16Type Bits8Type = do
        Iconst 8
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp Bits16Type Bits32Type = do
        I2l
        Iconst 32
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Bits16Type IntType = Pure ()

    getCastAsmOp Bits16Type Bits64Type = do
        Iconst 64
        InvokeMethod InvokeStatic conversionClass "toUnsignedLong" "(II)J" False
    getCastAsmOp Bits16Type IntegerType = unsignedIntToBigInteger
    getCastAsmOp Bits16Type Int8Type = I2b
    getCastAsmOp Bits16Type Int16Type = I2s
    getCastAsmOp Bits16Type Int64Type = I2l
    getCastAsmOp Bits16Type CharType = InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
    getCastAsmOp Bits16Type DoubleType = I2d
    getCastAsmOp Bits16Type StringType = unsignedIntToString

    getCastAsmOp Bits32Type Bits8Type = do
        Iconst 8
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp Bits32Type Bits16Type = do
        Iconst 16
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp Bits32Type IntType = Pure ()
    getCastAsmOp Bits32Type Bits64Type =
        InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
    getCastAsmOp Bits32Type IntegerType = unsignedIntToBigInteger
    getCastAsmOp Bits32Type Int8Type = I2b
    getCastAsmOp Bits32Type Int16Type = I2s
    getCastAsmOp Bits32Type Int64Type = InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
    getCastAsmOp Bits32Type DoubleType = do
      InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
      L2d
    getCastAsmOp Bits32Type CharType = InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
    getCastAsmOp Bits32Type StringType = unsignedIntToString

    getCastAsmOp Bits64Type Bits8Type = do
        Iconst 8
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Bits64Type Bits16Type = do
        Iconst 16
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Bits64Type Bits32Type = do
        Iconst 32
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Bits64Type IntegerType =
        InvokeMethod InvokeStatic conversionClass "toUnsignedBigInteger" "(J)Ljava/math/BigInteger;" False
    getCastAsmOp Bits64Type Int64Type = Pure ()
    getCastAsmOp Bits64Type Int8Type = L2i
    getCastAsmOp Bits64Type Int16Type = L2i
    getCastAsmOp Bits64Type Int32Type = L2i
    getCastAsmOp Bits64Type IntType = L2i
    getCastAsmOp Bits64Type DoubleType = InvokeMethod InvokeStatic conversionClass "unsignedLongToDouble" "(J)D" False
    getCastAsmOp Bits64Type CharType = do
      L2i
      InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
    getCastAsmOp Bits64Type StringType =
        InvokeMethod InvokeStatic "java/lang/Long" "toUnsignedString" "(J)Ljava/lang/String;" False

    getCastAsmOp Int64Type IntegerType =
        InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
    getCastAsmOp Int64Type Bits8Type = do
        Iconst 8
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Int64Type Bits16Type = do
        Iconst 16
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Int64Type Bits32Type = do
        Iconst 32
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp Int64Type Bits64Type = Pure ()
    getCastAsmOp Int64Type Int8Type = L2i
    getCastAsmOp Int64Type Int16Type = L2i
    getCastAsmOp Int64Type Int32Type = L2i
    getCastAsmOp Int64Type IntType = L2i
    getCastAsmOp Int64Type DoubleType = L2d
    getCastAsmOp Int64Type CharType = do L2i; InvokeMethod InvokeStatic conversionClass "toChar" "(I)C" False
    getCastAsmOp Int64Type StringType =
        InvokeMethod InvokeStatic "java/lang/Long" "toString" "(J)Ljava/lang/String;" False

    getCastAsmOp StringType IntegerType =
      InvokeMethod InvokeStatic conversionClass "toInteger" "(Ljava/lang/String;)Ljava/math/BigInteger;" False
    getCastAsmOp StringType Bits8Type = do
      InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
      Iconst 8
      InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp StringType Bits16Type = do
      InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
      Iconst 16
      InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp StringType Bits32Type = do
      InvokeMethod InvokeStatic "java/lang/Long" "parseLong" "(Ljava/lang/String;)J" False
      L2i
    getCastAsmOp StringType Bits64Type =
      InvokeMethod InvokeStatic conversionClass "toLong" "(Ljava/lang/String;)J" False
    getCastAsmOp StringType Int8Type = do
      InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
      I2b
    getCastAsmOp StringType Int16Type = do
      InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
      I2s
    getCastAsmOp StringType Int32Type = do
      InvokeMethod InvokeStatic "java/lang/Long" "parseLong" "(Ljava/lang/String;)J" False
      L2i
    getCastAsmOp StringType IntType = InvokeMethod InvokeStatic conversionClass "toInt" "(Ljava/lang/String;)I" False
    getCastAsmOp StringType Int64Type =
      InvokeMethod InvokeStatic conversionClass "toLong" "(Ljava/lang/String;)J" False
    getCastAsmOp StringType DoubleType =
      InvokeMethod InvokeStatic "java/lang/Double" "parseDouble" "(Ljava/lang/String;)D" False
    getCastAsmOp StringType CharType = do
      Iconst 0
      InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(Ljava/lang/String;I)C" False
    getCastAsmOp StringType _ =
        InvokeMethod InvokeStatic conversionClass "toInt" "(Ljava/lang/String;)I" False

    getCastAsmOp _ Bits8Type = do
        Iconst 8
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp _ Bits16Type = do
        Iconst 16
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(II)I" False
    getCastAsmOp _ Bits32Type = do
        I2l
        Iconst 32
        InvokeMethod InvokeStatic conversionClass "toUnsignedInt" "(JI)I" False
    getCastAsmOp _ Bits64Type =
        InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedLong" "(I)J" False
    getCastAsmOp _ Int64Type = I2l
    getCastAsmOp _ IntegerType = do
        I2l
        InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
    getCastAsmOp _ DoubleType = I2d
    getCastAsmOp _ StringType =
        InvokeMethod InvokeStatic "java/lang/Integer" "toString" "(I)Ljava/lang/String;" False
    getCastAsmOp _ _ = Pure ()

    assembleExprOp : InferredType -> FC -> PrimFn arity -> Vect arity NamedCExp -> Asm ()
    assembleExprOp returnType fc (Neg Bits64Type) [x] = assembleExprUnaryOp returnType ILong Lneg x
    assembleExprOp returnType fc (ShiftR Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong (do L2i; Lushr) x y
    assembleExprOp returnType fc (BAnd Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong Land x y
    assembleExprOp returnType fc (BOr Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong Lor x y
    assembleExprOp returnType fc (BXOr Bits64Type) [x, y] = assembleExprBinaryOp returnType ILong Lxor x y

    assembleExprOp returnType fc (Neg Int64Type) [x] = assembleExprUnaryOp returnType ILong Lneg x
    assembleExprOp returnType fc (ShiftR Int64Type) [x, y] = assembleExprBinaryOp returnType ILong (do L2i; Lshr) x y
    assembleExprOp returnType fc (BAnd Int64Type) [x, y] = assembleExprBinaryOp returnType ILong Land x y
    assembleExprOp returnType fc (BOr Int64Type) [x, y] = assembleExprBinaryOp returnType ILong Lor x y
    assembleExprOp returnType fc (BXOr Int64Type) [x, y] = assembleExprBinaryOp returnType ILong Lxor x y

    assembleExprOp returnType fc (Neg IntegerType) [x] =
        let op = InvokeMethod InvokeVirtual "java/math/BigInteger" "negate" "()Ljava/math/BigInteger;" False
        in assembleExprUnaryOp returnType inferredBigIntegerType op x
    assembleExprOp returnType fc (ShiftR IntegerType) [x, y] = do
        let op = do
            InvokeMethod InvokeVirtual "java/math/BigInteger" "intValueExact" "()I" False
            InvokeMethod InvokeVirtual "java/math/BigInteger" "shiftRight" "(I)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y
    assembleExprOp returnType fc (BAnd IntegerType) [x, y] = do
        let op = InvokeMethod InvokeVirtual "java/math/BigInteger" "and"
                    "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y
    assembleExprOp returnType fc (BOr IntegerType) [x, y] = do
        let op = InvokeMethod InvokeVirtual "java/math/BigInteger" "or"
                    "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y
    assembleExprOp returnType fc (BXOr IntegerType) [x, y] = do
        let op = InvokeMethod InvokeVirtual "java/math/BigInteger" "xor"
                    "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
        assembleExprBinaryOp returnType inferredBigIntegerType op x y

    assembleExprOp returnType fc (Add DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble Dadd x y
    assembleExprOp returnType fc (Sub DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble Dsub x y
    assembleExprOp returnType fc (Mul DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble Dmul x y
    assembleExprOp returnType fc (Div DoubleType) [x, y] = assembleExprBinaryOp returnType IDouble Ddiv x y
    assembleExprOp returnType fc (Neg DoubleType) [x] = assembleExprUnaryOp returnType IDouble Dneg x

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
    assembleExprOp returnType fc (Neg ty) [x] = assembleExprUnaryOp returnType IInt Ineg x

    assembleExprOp returnType fc (ShiftL ty) [x, y] =
      assembleExprBinaryOp returnType (getInferredType ty) (shl (jIntKind ty)) x y
    assembleExprOp returnType fc (ShiftR Bits32Type) [x, y] = assembleExprBinaryOp returnType IInt Iushr x y
    assembleExprOp returnType fc (ShiftR ty) [x, y] = assembleExprBinaryOp returnType IInt Ishr x y
    assembleExprOp returnType fc (BAnd ty) [x, y] = assembleExprBinaryOp returnType IInt Iand x y
    assembleExprOp returnType fc (BOr ty) [x, y] = assembleExprBinaryOp returnType IInt Ior x y
    assembleExprOp returnType fc (BXOr ty) [x, y] = assembleExprBinaryOp returnType IInt Ixor x y

    assembleExprOp returnType fc (LT DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do Dcmpg; Ifge label) x y
    assembleExprOp returnType fc (LTE DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do Dcmpg; Ifgt label) x y
    assembleExprOp returnType fc (EQ DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do Dcmpl; Ifne label) x y
    assembleExprOp returnType fc (GTE DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do Dcmpl; Iflt label) x y
    assembleExprOp returnType fc (GT DoubleType) [x, y] =
        assembleExprBinaryBoolOp returnType IDouble (\label => do Dcmpl; Ifle label) x y

    assembleExprOp returnType fc (LT IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass Ifge x y
    assembleExprOp returnType fc (LTE IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass Ifgt x y
    assembleExprOp returnType fc (EQ IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass Ifne x y
    assembleExprOp returnType fc (GTE IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass Iflt x y
    assembleExprOp returnType fc (GT IntegerType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType bigIntegerClass Ifle x y

    assembleExprOp returnType fc (LT StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass Ifge x y
    assembleExprOp returnType fc (LTE StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass Ifgt x y
    assembleExprOp returnType fc (EQ StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass Ifne x y
    assembleExprOp returnType fc (GTE StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass Iflt x y
    assembleExprOp returnType fc (GT StringType) [x, y] =
        assembleExprComparableBinaryBoolOp returnType stringClass Ifle x y

    assembleExprOp returnType fc (LT Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong Ifge) x y
    assembleExprOp returnType fc (LTE Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong Ifgt) x y
    assembleExprOp returnType fc (EQ Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong Ifne) x y
    assembleExprOp returnType fc (GTE Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong Iflt) x y
    assembleExprOp returnType fc (GT Bits64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareUnsignedLong Ifle) x y

    assembleExprOp returnType fc (LT Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong Ifge) x y
    assembleExprOp returnType fc (LTE Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong Ifgt) x y
    assembleExprOp returnType fc (EQ Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong Ifne) x y
    assembleExprOp returnType fc (GTE Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong Iflt) x y
    assembleExprOp returnType fc (GT Int64Type) [x, y] =
        assembleExprBinaryBoolOp returnType ILong (compareSignedLong Ifle) x y

    assembleExprOp returnType fc (LT Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt Ifge) x y
    assembleExprOp returnType fc (LTE Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt Ifgt) x y
    assembleExprOp returnType fc (EQ Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt Ifne) x y
    assembleExprOp returnType fc (GTE Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt Iflt) x y
    assembleExprOp returnType fc (GT Bits32Type) [x, y] =
        assembleExprBinaryBoolOp returnType IInt (compareUnsignedInt Ifle) x y

    assembleExprOp returnType fc (LT ty) [x, y] = assembleExprBinaryBoolOp returnType IInt Ificmpge x y
    assembleExprOp returnType fc (LTE ty) [x, y] = assembleExprBinaryBoolOp returnType IInt Ificmpgt x y
    assembleExprOp returnType fc (EQ ty) [x, y] = assembleExprBinaryBoolOp returnType IInt Ificmpne x y
    assembleExprOp returnType fc (GTE ty) [x, y] = assembleExprBinaryBoolOp returnType IInt Ificmplt x y
    assembleExprOp returnType fc (GT ty) [x, y] = assembleExprBinaryBoolOp returnType IInt Ificmple x y

    assembleExprOp returnType fc StrLength [x] = do
        assembleExpr False inferredStringType x
        InvokeMethod InvokeVirtual "java/lang/String" "length" "()I" False
        asmCast IInt returnType

    assembleExprOp returnType fc StrHead [x] = do
        assembleExpr False inferredStringType x
        Iconst 0
        InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
        asmCast IChar returnType

    assembleExprOp returnType fc StrTail [x] = do
        assembleExpr False inferredStringType x
        Iconst 1
        InvokeMethod InvokeVirtual "java/lang/String" "substring" "(I)Ljava/lang/String;" False
        asmCast inferredStringType returnType

    assembleExprOp returnType fc StrIndex [x, i] = do
        assembleExpr False inferredStringType x
        assembleExpr False IInt i
        InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
        asmCast IChar returnType

    assembleExprOp returnType fc StrCons [x, y] = assembleStrCons returnType x y

    assembleExprOp returnType fc StrAppend [x, y] =
        let op = InvokeMethod InvokeVirtual "java/lang/String" "concat" "(Ljava/lang/String;)Ljava/lang/String;" False
        in assembleExprBinaryOp returnType inferredStringType op x y

    assembleExprOp returnType fc StrReverse [x] = assembleStrReverse returnType x

    assembleExprOp returnType fc StrSubstr [offset, len, str] =do
        assembleExpr False IInt offset
        assembleExpr False IInt len
        assembleExpr False inferredStringType str
        InvokeMethod InvokeStatic (getRuntimeClass "Strings") "substring"
            "(IILjava/lang/String;)Ljava/lang/String;" False
        asmCast inferredStringType returnType

    -- `e` is Euler's number, which approximates to: 2.718281828459045
    assembleExprOp returnType fc DoubleExp [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "exp" "(D)D" False -- Base is `e`. Same as: `pow(e, x)
    assembleExprOp returnType fc DoubleLog [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "log" "(D)D" False -- Base is `e`.
    assembleExprOp returnType fc DoublePow [x, y] =
        let op = InvokeMethod InvokeStatic "java/lang/Math" "pow" "(DD)D" False
        in assembleExprBinaryOp returnType IDouble op x y
    assembleExprOp returnType fc DoubleSin [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "sin" "(D)D" False
    assembleExprOp returnType fc DoubleCos [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "cos" "(D)D" False
    assembleExprOp returnType fc DoubleTan [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "tan" "(D)D" False
    assembleExprOp returnType fc DoubleASin [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "asin" "(D)D" False
    assembleExprOp returnType fc DoubleACos [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "acos" "(D)D" False
    assembleExprOp returnType fc DoubleATan [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "atan" "(D)D" False
    assembleExprOp returnType fc DoubleSqrt [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "sqrt" "(D)D" False
    assembleExprOp returnType fc DoubleFloor [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "floor" "(D)D" False
    assembleExprOp returnType fc DoubleCeiling [x] = assembleExprUnaryOp returnType IDouble op x where
        op : Asm ()
        op = InvokeMethod InvokeStatic "java/lang/Math" "ceil" "(D)D" False

    assembleExprOp returnType fc (Cast from to) [arg] = assembleCast returnType fc from to arg

    assembleExprOp returnType fc BelieveMe [_,_,x] = do
        assembleExpr False IUnknown x
        asmCast IUnknown returnType

    assembleExprOp returnType fc Crash [_, msg] = do
        assembleExpr False inferredStringType msg
        InvokeMethod InvokeStatic runtimeClass "crash" "(Ljava/lang/String;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType

    assembleExprOp returnType fc op _ = Throw fc ("Unsupported operator " ++ show op)

    assembleParameter : (NamedCExp, InferredType) -> Asm ()
    assembleParameter (param, ty) = assembleExpr False ty param

    storeParameter : Map Int InferredType -> (Int, NamedCExp, InferredType) -> Asm Int
    storeParameter variableTypes (var, (NmLocal _ loc), ty) = do
        let valueVariableName = jvmSimpleName loc
        valueVariableIndex <- getVariableIndex valueVariableName
        if var /= valueVariableIndex
            then do
                valueVariableType <- getVariableType valueVariableName
                loadVar variableTypes valueVariableType ty valueVariableIndex
                targetVariableIndex <- getDynamicVariableIndex "tailRecArg"
                storeVar ty ty targetVariableIndex
                Pure targetVariableIndex
            else Pure var
    storeParameter _ (var, param, ty) = do
        assembleExpr False ty param
        targetVariableIndex <- getDynamicVariableIndex "tailRecArg"
        storeVar ty ty targetVariableIndex
        Pure targetVariableIndex

    createMethodReference : (isTailCall: Bool) -> (arity: Nat) -> Name -> Asm ()
    createMethodReference isTailCall arity name = do
        let jname = jvmName name
        functionType <- case !(findFunctionType jname) of
            Just ty => Pure ty
            Nothing => Pure $ MkInferredFunctionType inferredObjectType $ replicate arity inferredObjectType
        let methodReturnType = InferredFunctionType.returnType functionType
        let paramTypes = parameterTypes functionType
        let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType methodReturnType paramTypes
        let functionName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
        let functionInterface = getFunctionInterface arity
        let invokeDynamicDescriptor = getMethodDescriptor $ MkInferredFunctionType functionInterface []
        invokeDynamic (className functionName) (methodName functionName) "apply" invokeDynamicDescriptor
           (getSamDesc (getLambdaTypeByArity arity)) methodDescriptor methodDescriptor
        when (arity > 1) $ do
          let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType inferredLambdaType [functionInterface]
          InvokeMethod InvokeStatic functionsClass "curry" methodDescriptor False
        when isTailCall $ asmReturn inferredLambdaType

    assembleSubMethodWithScope1 : (isTailCall: Bool) -> InferredType -> (parameterName : Maybe Name) ->
      NamedCExp -> Asm ()
    assembleSubMethodWithScope1 isTailCall returnType parameterName body = do
        parentScope <- getScope !getCurrentScopeIndex
        withScope $ assembleSubMethod isTailCall returnType Nothing parameterName parentScope body

    assembleMethodReference : (isTailCall: Bool) -> InferredType -> (isMethodReference : Bool) -> (arity: Nat) ->
      (functionName: Name) -> (parameterName : Maybe Name) -> NamedCExp -> Asm ()
    assembleMethodReference isTailCall returnType isMethodReference arity functionName parameterName body =
      if isMethodReference
        then createMethodReference isTailCall arity functionName
        else assembleSubMethodWithScope1 isTailCall returnType parameterName body

    assembleSubMethodWithScope : (isTailCall: Bool) -> InferredType -> (parameterValue: Maybe NamedCExp) ->
        (parameterName : Maybe Name) -> NamedCExp -> Asm ()
    assembleSubMethodWithScope isTailCall returnType (Just value) (Just name) body = do
        parentScope <- getScope !getCurrentScopeIndex
        let shouldGenerateVariable = name == extractedMethodArgumentName
        parameterValueVariable <-
            if shouldGenerateVariable
                then Pure $ jvmSimpleName name ++ show !newDynamicVariableIndex
                else Pure $ jvmSimpleName name
        let parameterValueVariableName = UN $ Basic parameterValueVariable
        withScope $ assembleSubMethod isTailCall returnType (Just (assembleValue parentScope parameterValueVariable))
            (Just parameterValueVariableName) parentScope
            (substituteVariableSubMethodBody (NmLocal (getFC body) parameterValueVariableName) body)
      where
          assembleValue : Scope -> String -> Asm ()
          assembleValue enclosingScope variableName = do
            lambdaScopeIndex <- getCurrentScopeIndex
            updateCurrentScopeIndex (index enclosingScope)
            assembleExpr False !(getVariableType variableName) value
            updateCurrentScopeIndex lambdaScopeIndex

    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmLam _ p4 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1,
        NmLocal _ arg2, NmLocal _ arg3, NmLocal _ arg4]))))) = assembleMethodReference
            isTailCall returnType
            (maybe False ((==) arg0) p0 && p1 == arg1 && p2 == arg2 && p3 == arg3 && p4 == arg4)
            5 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2,
        NmLocal _ arg3])))) = assembleMethodReference isTailCall returnType
          (maybe False ((==) arg0) p0 && p1 == arg1 && p2 == arg2 && p3 == arg3) 4 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmLam _ p2 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2]))) =
        assembleMethodReference isTailCall returnType (maybe False ((==) arg0) p0 && p1 == arg1 && p2 == arg2)
          3 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0
      body@(NmLam _ p1 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1])) =
        assembleMethodReference isTailCall returnType (maybe False ((==) arg0) p0 && p1 == arg1)
          2 name p0 body
    assembleSubMethodWithScope isTailCall returnType _ p0 body@(NmApp _ (NmRef _ name) [NmLocal _ arg0]) =
      assembleMethodReference isTailCall returnType (maybe False ((==) arg0) p0) 1 name p0 body

    assembleSubMethodWithScope isTailCall returnType _ parameterName body@(NmLam _ c (NmLam _ a (NmLocal _ b))) =
      let hasParameter = isJust parameterName
      in if hasParameter && c == b
          then assembleConstant1Lambda isTailCall
          else if hasParameter && a == b
            then assembleIdentity2Lambda isTailCall
            else assembleSubMethodWithScope1 isTailCall returnType parameterName body
    assembleSubMethodWithScope isTailCall returnType _ parameterName body@(NmLam _ a (NmLocal _ b)) =
      if maybe False ((==) b) parameterName
        then assembleConstantLambda isTailCall
        else if isJust parameterName && a == b
          then assembleIdentity1Lambda isTailCall
          else assembleSubMethodWithScope1 isTailCall returnType parameterName body
    assembleSubMethodWithScope isTailCall returnType _ parameterName body@(NmLocal _ b) =
      if maybe False ((==) b) parameterName
        then assembleIdentityLambda isTailCall
        else assembleSubMethodWithScope1 isTailCall returnType parameterName body
    assembleSubMethodWithScope isTailCall returnType _ parameterName body =
      assembleSubMethodWithScope1 isTailCall returnType parameterName body

    assembleSubMethod : (isTailCall: Bool) -> InferredType -> (parameterValueExpr: (Maybe (Asm ()))) ->
        (parameterName: Maybe Name) -> Scope -> NamedCExp -> Asm ()
    assembleSubMethod isTailCall lambdaReturnType parameterValueExpr parameterName declaringScope expr = do
            scope <- getScope !getCurrentScopeIndex
            maybe (Pure ()) (setScopeCounter . succ) (parentIndex scope)
            let lambdaBodyReturnType = returnType scope
            let lambdaType = getLambdaTypeByParameter parameterName
            when (lambdaType == DelayedLambda) $ do
                New "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed"
                Dup
            let lambdaInterfaceType = getLambdaInterfaceType lambdaType
            parameterType <- the (Asm (Maybe InferredType)) $ traverse getVariableType (jvmSimpleName <$> parameterName)
            variableTypes <- LiftIo $ Map.values {key=Int} !(loadClosures declaringScope scope)
            maybe (Pure ()) id parameterValueExpr
            let invokeDynamicDescriptor = getMethodDescriptor $ MkInferredFunctionType lambdaInterfaceType variableTypes
            let isExtracted = isJust parameterValueExpr
            let implementationMethodReturnType =
                if isExtracted then lambdaBodyReturnType else getLambdaImplementationMethodReturnType lambdaType
            let implementationMethodDescriptor =
                getMethodDescriptor $
                    MkInferredFunctionType implementationMethodReturnType (variableTypes ++ toList parameterType)
            let methodPrefix = if isExtracted then "extr" else "lambda"
            lambdaClassMethodName <- getLambdaImplementationMethodName methodPrefix
            let lambdaMethodName = methodName lambdaClassMethodName
            let lambdaClassName = className lambdaClassMethodName
            let interfaceMethodName = getLambdaInterfaceMethodName lambdaType
            let indy = the (Asm ()) $ do
                let instantiatedMethodDescriptor = getMethodDescriptor $
                    MkInferredFunctionType implementationMethodReturnType $ toList parameterType
                invokeDynamic lambdaClassName lambdaMethodName interfaceMethodName invokeDynamicDescriptor
                    (getSamDesc lambdaType) implementationMethodDescriptor instantiatedMethodDescriptor
                when (lambdaType == DelayedLambda) $
                    InvokeMethod InvokeSpecial "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "<init>"
                        "(Lio/github/mmhelloworld/idrisjvm/runtime/Delayed;)V" False
            let staticCall = do
                 InvokeMethod InvokeStatic lambdaClassName lambdaMethodName implementationMethodDescriptor False
                 asmCast lambdaBodyReturnType lambdaReturnType
            maybe indy (const staticCall) parameterValueExpr
            when isTailCall $ if isExtracted then asmReturn lambdaReturnType else asmReturn lambdaInterfaceType
            let oldLineNumberLabels = lineNumberLabels !GetState
            newLineNumberLabels <- LiftIo $ Map.newTreeMap {key=Int} {value=String}
            updateState $ { lineNumberLabels := newLineNumberLabels }
            let accessModifiers = if isExtracted then [Public, Static] else [Public, Static, Synthetic]
            CreateMethod accessModifiers "" lambdaClassName lambdaMethodName implementationMethodDescriptor
                Nothing Nothing [] []
            MethodCodeStart
            let labelStart = methodStartLabel
            let labelEnd = methodEndLabel
            addLambdaStartLabel scope labelStart
            maybe (Pure ()) (\parentScopeIndex => updateScopeStartLabel parentScopeIndex labelStart) (parentIndex scope)
            let lambdaReturnType = if isExtracted then lambdaBodyReturnType else inferredObjectType
            assembleExpr True lambdaReturnType expr
            addLambdaEndLabel scope labelEnd
            maybe (Pure ()) (\parentScopeIndex => updateScopeEndLabel parentScopeIndex labelEnd) (parentIndex scope)
            addLocalVariables $ fromMaybe (index scope) (parentIndex scope)
            MaxStackAndLocal (-1) (-1)
            MethodCodeEnd
            updateState $ { lineNumberLabels := oldLineNumberLabels }
        where
            addLambdaStartLabel : Scope -> String -> Asm ()
            addLambdaStartLabel scope label = do
                let scopeIndex = index scope
                let lineNumberStart = fst $ lineNumbers scope
                CreateLabel label
                LabelStart label
                addLineNumber lineNumberStart label
                updateScopeStartLabel scopeIndex label

            addLambdaEndLabel : Scope -> String -> Asm ()
            addLambdaEndLabel scope label = do
                let scopeIndex = index scope
                let lineNumberEnd = snd $ lineNumbers scope
                CreateLabel label
                LabelStart label
                updateScopeEndLabel scopeIndex label

            readSourceTargetType : Maybe (Entry InferredType InferredType) -> IO (InferredType, InferredType)
            readSourceTargetType Nothing = pure (IUnknown, IUnknown)
            readSourceTargetType (Just entry) = Entry.toTuple {k=InferredType} {v=InferredType} entry

            loadVariables : Map Int InferredType -> Map Int (Entry InferredType InferredType) -> List Int -> Asm ()
            loadVariables _ _ [] = Pure ()
            loadVariables declaringScopeVariableTypes types (var :: vars) = do
                sourceTargetTypeEntry <- LiftIo $ Map.get types var
                (sourceType, targetType) <- LiftIo $ readSourceTargetType $ nullableToMaybe sourceTargetTypeEntry
                loadVar declaringScopeVariableTypes sourceType targetType var
                loadVariables declaringScopeVariableTypes types vars

            loadClosures : Scope -> Scope -> Asm (Map Int InferredType)
            loadClosures declaringScope currentScope = case parentIndex currentScope of
                    Just parentScopeIndex => do
                        parentScope <- getScope parentScopeIndex
                        variableNames <- LiftIo $ Map.keys {value=Int} $ variableIndices parentScope
                        variableNameAndIndex <- traverse getVariableNameAndIndex variableNames
                        typesByIndex <- getIndexAndType variableNameAndIndex
                        declaringScopeVariableTypes <- getVariableTypesAtScope (index declaringScope)
                        indices <- LiftIo $ Map.keys {value=Entry InferredType InferredType} typesByIndex
                        loadVariables declaringScopeVariableTypes typesByIndex indices
                        LiftIo $ Map.getValue2 {k=Int} {v1=InferredType} {v2=InferredType} typesByIndex
                    Nothing => LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
                where
                    getVariableNameAndIndex : String -> Asm (String, Int)
                    getVariableNameAndIndex name = do
                        variableIndex <- getVariableIndexAtScope (index declaringScope) name
                        Pure (name, variableIndex)

                    getIndexAndType : List (String, Int) -> Asm (Map Int (Entry InferredType InferredType))
                    getIndexAndType nameAndIndices = do
                        typesByIndexMap <- LiftIo $ Map.newTreeMap {key=Int} {value=Entry InferredType InferredType}
                        go typesByIndexMap
                        Pure typesByIndexMap
                      where
                        go : Map Int (Entry InferredType InferredType) -> Asm ()
                        go typesByIndexMap = go1 nameAndIndices where
                            go1 : List (String, Int) -> Asm ()
                            go1 [] = Pure ()
                            go1 ((name, varIndex) :: rest) = do
                                targetType <- getVariableType name
                                sourceType <- getVariableTypeAtScope (index declaringScope) name
                                entry <- LiftIo $ Entry.new sourceType targetType
                                _ <- LiftIo $ Map.put typesByIndexMap varIndex entry
                                go1 rest

    assembleMissingDefault :InferredType -> FC -> String -> Asm ()
    assembleMissingDefault returnType fc defaultLabel = do
        LabelStart defaultLabel
        defaultValue returnType
        asmReturn returnType

    assembleConstantSwitch : (returnType: InferredType) -> (switchExprType: InferredType) -> FC ->
        NamedCExp -> List NamedConstAlt -> Maybe NamedCExp -> Asm ()
    assembleConstantSwitch _ _ fc _ [] _ = Throw fc "Empty cases"

    assembleConstantSwitch returnType IInt fc sc alts def = do
        assembleExpr False IInt sc
        switchCases <- getCasesWithLabels alts
        let labels = fst <$> switchCases
        let exprs = second <$> switchCases
        traverse_ CreateLabel labels
        defaultLabel <- createDefaultLabel
        LookupSwitch defaultLabel labels exprs
        let switchCasesWithEndLabel = getSwitchCasesWithEndLabel switchCases labels
        traverse_ assembleExprConstAlt switchCasesWithEndLabel
        maybe (assembleMissingDefault returnType fc defaultLabel) (assembleDefault defaultLabel) def
      where
        getCasesWithLabels : List NamedConstAlt -> Asm (List (String, Int, NamedConstAlt))
        getCasesWithLabels alts = do
            caseExpressionsWithLabels <- traverse (constantAltIntExpr fc) alts
            Pure $ sortBy (comparing second) caseExpressionsWithLabels

        assembleCaseWithScope : String -> String -> NamedCExp -> Asm ()
        assembleCaseWithScope labelStart labelEnd expr = withScope $ do
            scopeIndex <- getCurrentScopeIndex
            scope <- getScope scopeIndex
            let (lineNumberStart, lineNumberEnd) = lineNumbers scope
            LabelStart labelStart
            updateScopeStartLabel scopeIndex labelStart
            addLineNumber lineNumberStart labelStart
            updateScopeEndLabel scopeIndex labelEnd
            assembleExpr True returnType expr

        assembleDefault : String -> NamedCExp -> Asm ()
        assembleDefault defaultLabel expr = assembleCaseWithScope defaultLabel methodEndLabel expr

        assembleExprConstAlt : (String, Int, NamedConstAlt, String) -> Asm ()
        assembleExprConstAlt (labelStart, _, (MkNConstAlt _ expr), labelEnd) =
            assembleCaseWithScope labelStart labelEnd expr

    assembleConstantSwitch returnType constantType fc sc alts def = do
            hashPositionAndAlts <- traverse (constantAltHashCodeExpr fc) $
                zip [0 .. the Int $ cast $ length $ drop 1 alts] alts
            let positionAndAltsByHash = multiValueMap fst snd hashPositionAndAlts
            hashCodeSwitchCases <- getHashCodeCasesWithLabels positionAndAltsByHash
            let labels = fst <$> hashCodeSwitchCases
            let exprs = second <$> hashCodeSwitchCases
            switchEndLabel <- newLabel
            CreateLabel switchEndLabel
            traverse_ CreateLabel labels
            assembleExpr False constantType sc
            constantExprVariableSuffixIndex <- newDynamicVariableIndex
            let constantExprVariableName = "constantCaseExpr" ++ show constantExprVariableSuffixIndex
            constantExprVariableIndex <- getVariableIndex constantExprVariableName
            hashCodePositionVariableSuffixIndex <- newDynamicVariableIndex
            let hashCodePositionVariableName = "hashCodePosition" ++ show hashCodePositionVariableSuffixIndex
            hashCodePositionVariableIndex <- getVariableIndex hashCodePositionVariableName
            storeVar constantType constantType constantExprVariableIndex
            constantClass <- getHashCodeSwitchClass fc constantType
            Iconst (-1)
            storeVar IInt IInt hashCodePositionVariableIndex
            loadVar !getVariableTypes constantType constantType constantExprVariableIndex
            let isLong = constantClass == "java/lang/Long"
            let invocationType = if isLong then InvokeStatic else InvokeVirtual
            let signature = if isLong then "(J)I" else "()I"
            InvokeMethod invocationType constantClass "hashCode" signature False
            LookupSwitch switchEndLabel labels exprs
            traverse_
                (assembleHashCodeSwitchCases fc constantClass constantExprVariableIndex hashCodePositionVariableIndex
                    switchEndLabel)
                hashCodeSwitchCases
            scope <- getScope !getCurrentScopeIndex
            let lineNumberStart = fst $ lineNumbers scope
            LabelStart switchEndLabel
            addLineNumber lineNumberStart switchEndLabel
            assembleConstantSwitch returnType IInt fc (NmLocal fc $ UN $ Basic hashCodePositionVariableName)
                (hashPositionSwitchAlts hashPositionAndAlts) def
        where
            constantAltHashCodeExpr : FC -> (Int, NamedConstAlt) -> Asm (Int, Int, NamedConstAlt)
            constantAltHashCodeExpr fc positionAndAlt@(position, MkNConstAlt constant _) = case hashCode constant of
                Just hashCodeValue => Pure (hashCodeValue, position, snd positionAndAlt)
                Nothing => asmCrash ("Constant " ++ show constant ++ " cannot be compiled to 'Switch'.")

            hashPositionSwitchAlts : List (Int, Int, NamedConstAlt) -> List NamedConstAlt
            hashPositionSwitchAlts exprPositionAlts = reverse $ go [] exprPositionAlts where
                go : List NamedConstAlt -> List (Int, Int, NamedConstAlt) -> List NamedConstAlt
                go acc [] = acc
                go acc ((_, position, (MkNConstAlt _ expr)) :: alts) =
                    go (MkNConstAlt (I position) expr :: acc) alts

            assembleHashCodeSwitchCases : FC -> String -> Int -> Int -> String ->
                (String, Int, List (Int, NamedConstAlt)) -> Asm ()
            assembleHashCodeSwitchCases fc _ _ _ _ (_, _, []) = Throw fc "Empty cases"
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

                    compareConstant : String -> Asm ()
                    compareConstant "java/lang/Long" = Lcmp
                    compareConstant "java/lang/String" =
                      InvokeMethod InvokeVirtual stringClass "equals" "(Ljava/lang/Object;)Z" False
                    compareConstant "java/math/BigInteger" =
                      InvokeMethod InvokeVirtual bigIntegerClass "equals" "(Ljava/lang/Object;)Z" False
                    compareConstant clazz = asmCrash ("Unknown constant class " ++ clazz ++ " for switch")

                    switchBody : String -> String -> Int -> NamedConstAlt -> Asm ()
                    switchBody label nextLabel position (MkNConstAlt constant _) = do
                      scope <- getScope !getCurrentScopeIndex
                      let lineNumberStart = fst $ lineNumbers scope
                      LabelStart label
                      addLineNumber lineNumberStart label
                      loadVar !getVariableTypes constantType constantType constantExprVariableIndex
                      assembleHashCodeSwitchConstant fc constant
                      compareConstant constantClass
                      let condition = if isComparator constantClass then Ifne else Ifeq
                      condition nextLabel
                      Iconst position
                      storeVar IInt IInt hashCodePositionVariableIndex
                      Goto switchEndLabel

                    go : String -> List (Int, NamedConstAlt) -> Asm ()
                    go _ [] = Pure ()
                    go label ((position, alt) :: []) = switchBody label switchEndLabel position alt
                    go label ((position, alt) :: positionAndAlts) = do
                        nextLabel <- newLabel
                        switchBody label nextLabel position alt
                        go nextLabel positionAndAlts

    assembleConCase : InferredType -> FC -> (sc : NamedCExp) -> List NamedConAlt -> Maybe NamedCExp -> Asm ()
    assembleConCase returnType fc sc alts def = do
        idrisObjectVariableIndex <- assembleConstructorSwitchExpr sc
        let hasTypeCase = any isTypeCase alts
        let constructorType = if hasTypeCase then "Ljava/lang/String;" else "I"
        variableTypes <- getVariableTypes
        optTy <- LiftIo $ Map.get variableTypes idrisObjectVariableIndex
        let idrisObjectVariableType = fromMaybe IUnknown $ nullableToMaybe optTy
        loadVar variableTypes idrisObjectVariableType idrisObjectType idrisObjectVariableIndex
        when (idrisObjectVariableType /= idrisObjectType) $ do
            storeVar idrisObjectType idrisObjectType idrisObjectVariableIndex
            loadVar !getVariableTypes idrisObjectType idrisObjectType idrisObjectVariableIndex
        let constructorGetter = if hasTypeCase then "getStringConstructorId" else "getConstructorId"
        InvokeMethod InvokeInterface idrisObjectClass constructorGetter ("()" ++ constructorType) True
        if hasTypeCase
            then assembleStringConstructorSwitch returnType fc idrisObjectVariableIndex alts def
            else assembleConstructorSwitch returnType fc idrisObjectVariableIndex alts def

    assembleConCaseExpr : InferredType -> Int -> List Name -> NamedCExp -> Asm ()
    assembleConCaseExpr returnType idrisObjectVariableIndex args expr = do
            variableTypes <- getVariableTypes
            optTy <- LiftIo $ Map.get variableTypes idrisObjectVariableIndex
            let idrisObjectVariableType = fromMaybe IUnknown $ nullableToMaybe optTy
            bindArg idrisObjectVariableType variableTypes 0 args
            assembleExpr True returnType expr
        where
            bindArg : InferredType -> Map Int InferredType -> Int -> List Name -> Asm ()
            bindArg _ _ _ [] = Pure ()
            bindArg idrisObjectVariableType variableTypes index (var :: vars) = do
                let variableName = jvmSimpleName var
                when (used variableName expr) $ do
                    loadVar variableTypes idrisObjectVariableType idrisObjectType idrisObjectVariableIndex
                    Iconst index
                    InvokeMethod InvokeInterface idrisObjectClass "getProperty" "(I)Ljava/lang/Object;" True
                    variableIndex <- getVariableIndex variableName
                    storeVar inferredObjectType !(getVariableType variableName) variableIndex
                bindArg idrisObjectVariableType variableTypes (index + 1) vars

    assembleConstructorSwitch : InferredType -> FC -> Int -> List NamedConAlt -> Maybe NamedCExp -> Asm ()
    assembleConstructorSwitch returnType fc idrisObjectVariableIndex alts def = do
            switchCases <- getCasesWithLabels alts
            let labels = fst <$> switchCases
            let switchCasesWithEndLabel = getSwitchCasesWithEndLabel switchCases labels
            let exprs = caseExpression <$> switchCases
            traverse_ CreateLabel labels
            defaultLabel <- createDefaultLabel
            LookupSwitch defaultLabel labels exprs
            traverse_ assembleExprConAlt switchCasesWithEndLabel
            maybe (assembleMissingDefault returnType fc defaultLabel) (assembleDefault defaultLabel) def
        where
            caseExpression : (String, Int, NamedConAlt) -> Int
            caseExpression (_, expr, _) = expr

            getCasesWithLabels : List NamedConAlt -> Asm (List (String, Int, NamedConAlt))
            getCasesWithLabels alts = do
                caseExpressionsWithLabels <- traverse conAltIntExpr alts
                Pure $ sortBy (comparing caseExpression) caseExpressionsWithLabels

            assembleDefault : String -> NamedCExp -> Asm ()
            assembleDefault labelStart expr = withScope $ do
                scopeIndex <- getCurrentScopeIndex
                scope <- getScope scopeIndex
                let (lineNumberStart, lineNumberEnd) = lineNumbers scope
                LabelStart labelStart
                addLineNumber lineNumberStart labelStart
                updateScopeStartLabel scopeIndex labelStart
                updateScopeEndLabel scopeIndex methodEndLabel
                assembleExpr True returnType expr

            assembleCaseWithScope : String -> String -> List Name -> NamedCExp -> Asm ()
            assembleCaseWithScope labelStart labelEnd args expr = withScope $ do
                scopeIndex <- getCurrentScopeIndex
                scope <- getScope scopeIndex
                let (lineNumberStart, lineNumberEnd) = lineNumbers scope
                LabelStart labelStart
                addLineNumber lineNumberStart labelStart
                updateScopeStartLabel scopeIndex labelStart
                updateScopeEndLabel scopeIndex labelEnd
                assembleConCaseExpr returnType idrisObjectVariableIndex args expr

            assembleExprConAlt : (String, Int, NamedConAlt, String) -> Asm ()
            assembleExprConAlt (labelStart, _, (MkNConAlt _ _ _ args expr), labelEnd) =
                assembleCaseWithScope labelStart labelEnd args expr

    assembleStringConstructorSwitch : InferredType -> FC -> Int -> List NamedConAlt -> Maybe NamedCExp -> Asm ()
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
        CreateLabel switchEndLabel
        traverse_ CreateLabel labels
        let constantType = inferredStringType
        constantClass <- getHashCodeSwitchClass fc constantType
        Iconst (-1)
        storeVar IInt IInt hashCodePositionVariableIndex
        loadVar !getVariableTypes constantType constantType constantExprVariableIndex
        InvokeMethod InvokeVirtual constantClass "hashCode" "()I" False
        LookupSwitch switchEndLabel labels exprs
        traverse_
            (assembleHashCodeSwitchCases fc constantClass constantExprVariableIndex hashCodePositionVariableIndex
                switchEndLabel)
            hashCodeSwitchCases
        scope <- getScope !getCurrentScopeIndex
        let lineNumberStart = fst $ lineNumbers scope
        LabelStart switchEndLabel
        addLineNumber lineNumberStart switchEndLabel
        assembleExpr False IInt (NmLocal fc $ UN $ Basic hashCodePositionVariableName)
        assembleConstructorSwitch returnType fc idrisObjectVariableIndex
            (hashPositionSwitchAlts hashPositionAndAlts) def
      where
        conAltHashCodeExpr : FC -> (Int, NamedConAlt) -> Asm (Int, Int, NamedConAlt)
        conAltHashCodeExpr fc positionAndAlt@(position, MkNConAlt name _ _ _ _) =
            case hashCode (Str $ getIdrisConstructorClassName (jvmSimpleName name)) of
                Just hashCodeValue => Pure (hashCodeValue, position, snd positionAndAlt)
                Nothing => asmCrash ("Constructor " ++ show name ++ " cannot be compiled to 'Switch'.")

        hashPositionSwitchAlts : List (Int, Int, NamedConAlt) -> List NamedConAlt
        hashPositionSwitchAlts exprPositionAlts = reverse $ go [] exprPositionAlts where
            go : List NamedConAlt -> List (Int, Int, NamedConAlt) -> List NamedConAlt
            go acc [] = acc
            go acc ((_, position, (MkNConAlt name conInfo _ args expr)) :: alts) =
                go (MkNConAlt name conInfo (Just position) args expr :: acc) alts

        assembleHashCodeSwitchCases : FC -> String -> Int -> Int -> String ->
            (String, Int, List (Int, NamedConAlt)) -> Asm ()
        assembleHashCodeSwitchCases fc _ _ _ _ (_, _, []) = Throw fc "Empty cases"
        assembleHashCodeSwitchCases fc constantClass constantExprVariableIndex hashCodePositionVariableIndex
            switchEndLabel (label, _, positionAndAlts) = go label positionAndAlts where

                switchBody : String -> String -> Int -> NamedConAlt -> Asm ()
                switchBody label nextLabel position (MkNConAlt name _ _ _ _) = do
                  scope <- getScope !getCurrentScopeIndex
                  let lineNumberStart = fst $ lineNumbers scope
                  LabelStart label
                  addLineNumber lineNumberStart label
                  loadVar !getVariableTypes inferredStringType inferredStringType constantExprVariableIndex
                  Ldc $ StringConst $ getIdrisConstructorClassName (jvmSimpleName name)
                  InvokeMethod InvokeVirtual constantClass "equals" "(Ljava/lang/Object;)Z" False
                  Ifeq nextLabel
                  Iconst position
                  storeVar IInt IInt hashCodePositionVariableIndex
                  Goto switchEndLabel

                go : String -> List (Int, NamedConAlt) -> Asm ()
                go _ [] = Pure ()
                go label ((position, alt) :: []) = switchBody label switchEndLabel position alt
                go label ((position, alt) :: positionAndAlts) = do
                    nextLabel <- newLabel
                    switchBody label nextLabel position alt
                    go nextLabel positionAndAlts

    asmJavaLambda : FC -> InferredType -> NamedCExp -> NamedCExp -> NamedCExp -> Asm ()
    asmJavaLambda fc returnType functionType javaInterfaceType lambda = do
        assembleExpr False inferredLambdaType lambda
        lambdaType <- getJavaLambdaType fc [functionType, javaInterfaceType, lambda]
        let samType =
          if isIoAction then {parameterTypes $= dropWorldType} lambdaType.methodType else lambdaType.methodType
        let lambdaImplementationType = lambdaType.implementationType
        let lambdaImplementationType = updateImplementationType samType.returnType lambdaImplementationType
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
        invokeDynamic lambdaClassName lambdaMethodName lambdaType.methodName invokeDynamicDescriptor
            (getMethodDescriptor samType) implementationMethodDescriptor instantiatedMethodDescriptor
        asmCast lambdaType.javaInterface returnType
        let accessModifiers = [Public, Static, Synthetic]
        CreateMethod accessModifiers "" lambdaClassName lambdaMethodName implementationMethodDescriptor
          Nothing Nothing [] []
        MethodCodeStart
        Aload 0
        let arity = (cast {to=Int} $ length implementationParameterTypes) + 1
        typesByIndex <- LiftIo $ Map.fromList $ zip [0 .. arity - 1]
          (inferredLambdaType :: implementationParameterTypes)
        applyParameters typesByIndex 1 lambdaImplementationType.returnType implementationParameterTypes
        MaxStackAndLocal (-1) (-1)
        MethodCodeEnd
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

        applyParameter : Map Int InferredType -> (isIoApplication: Bool) -> Int -> InferredType -> Asm ()
        applyParameter typesByIndex isIoApplication index parameterType = do
          loadArgument
          InvokeMethod InvokeInterface "java/util/function/Function" "apply" "(Ljava/lang/Object;)Ljava/lang/Object;" True
          InvokeMethod InvokeStatic runtimeClass "unwrap" "(Ljava/lang/Object;)Ljava/lang/Object;" False
         where
          loadArgument : Asm ()
          loadArgument =
            if isIoApplication
              then do
                Iconst 0
                InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False
              else loadVar typesByIndex parameterType inferredObjectType index

        applyParameters : Map Int InferredType -> Int -> InferredType -> List InferredType -> Asm ()
        applyParameters typesByIndex index returnType [] = do
          when isIoAction $ applyParameter typesByIndex True index inferredObjectType
          asmCast inferredObjectType returnType
          when (returnType == IVoid) Pop
          asmReturn returnType
        applyParameters typesByIndex index returnType (ty :: rest) = do
          applyParameter typesByIndex False index ty
          when (rest /= [] || isIoAction) $ asmCast inferredObjectType inferredLambdaType
          applyParameters typesByIndex (index + 1) returnType rest

    jvmExtPrim : FC -> InferredType -> ExtPrim -> List NamedCExp -> Asm ()
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
        InvokeMethod invocationType cname mname methodDescriptor isInterfaceInvocation
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
        else Aconstnull
    jvmExtPrim fc returnType JvmStaticMethodCall [ret, NmApp _ _ [functionNamePrimVal], fargs, world] =
      jvmExtPrim fc returnType JvmStaticMethodCall [ret, functionNamePrimVal, fargs, world]
    jvmExtPrim _ returnType JvmStaticMethodCall [ret, NmPrimVal fc (Str fn), fargs, world] = do
        args <- getFArgs fargs
        argTypes <- traverse tySpec (map fst args)
        let (cname, mnameWithDot) = break (== '.') fn
        let (_, mname) = break (/= '.') mnameWithDot
        let isConstructor = mname == "<init>"
        when isConstructor $ do
            New cname
            Dup
        let isSuper = mname == "<super>"
        when isSuper $ Aload 0
        traverse_ assembleParameter $ zip (map snd args) argTypes
        methodReturnType <- if isSuper then Pure IVoid else tySpec ret
        let descriptorReturnType = if isConstructor then IVoid else methodReturnType
        let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType descriptorReturnType argTypes
        let invocationType = if isConstructor || isSuper then InvokeSpecial else InvokeStatic
        let mname = if isSuper then "<init>" else mname
        InvokeMethod invocationType cname mname methodDescriptor False
        asmCast methodReturnType returnType
    jvmExtPrim _ returnType SetInstanceField [ret, NmPrimVal fc (Str fn), fargs, world] = do
        (obj :: value :: []) <- getFArgs fargs
            | _ => asmCrash ("Setting an instance field should have two arguments for " ++ fn)
        fieldType <- tySpec (fst value)
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False (iref cname []) (snd obj)
        assembleExpr False fieldType (snd value)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#' && c /= '=') fnameWithDot
        Field PutField cname fieldName (getJvmTypeDescriptor fieldType)
        Aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType SetStaticField [ret, NmPrimVal fc (Str fn), fargs, world] = do
        (value :: []) <- getFArgs fargs
            | _ => asmCrash ("Setting a static field should have one argument for " ++ fn)
        fieldType <- tySpec (fst value)
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False fieldType (snd value)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#' && c /= '=') fnameWithDot
        Field PutStatic cname fieldName (getJvmTypeDescriptor fieldType)
        Aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType GetInstanceField [ret, NmPrimVal fc (Str fn), fargs, world] = do
        (obj :: []) <- getFArgs fargs
            | _ => asmCrash ("Getting an instance field should have one argument for " ++ fn)
        fieldType <- tySpec ret
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False (iref cname []) (snd obj)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#') fnameWithDot
        Field GetField cname fieldName (getJvmTypeDescriptor fieldType)
        asmCast fieldType returnType
    jvmExtPrim _ returnType GetStaticField [ret, NmPrimVal fc (Str fn), fargs, world] = do
        fieldType <- tySpec ret
        let (cname, fnameWithDot) = break (== '.') fn
        let (_, fieldName) = break (\c => c /= '.' && c /= '#') fnameWithDot
        Field GetStatic cname fieldName (getJvmTypeDescriptor fieldType)
        asmCast fieldType returnType
    jvmExtPrim _ returnType NewArray [_, size, val, world] = do
        assembleExpr False IInt size
        assembleExpr False IUnknown val
        InvokeMethod InvokeStatic arraysClass "create" "(ILjava/lang/Object;)Ljava/util/ArrayList;" False
        asmCast arrayListType returnType
    jvmExtPrim _ returnType ArrayGet [_, arr, pos, world] = do
        assembleExpr False arrayListType arr
        assembleExpr False IInt pos
        InvokeMethod InvokeVirtual arrayListClass "get" "(I)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType ArraySet [_, arr, pos, val, world] = do
        assembleExpr False arrayListType arr
        assembleExpr False IInt pos
        assembleExpr False IUnknown val
        InvokeMethod InvokeVirtual arrayListClass "set" "(ILjava/lang/Object;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType JvmNewArray [tyExpr, size, world] = do
        assembleExpr False IInt size
        elemTy <- tySpec tyExpr
        assembleArray elemTy
        asmCast (IArray elemTy) returnType
    jvmExtPrim _ returnType JvmSetArray [tyExpr, index, val, arr, world] = do
        elemTy <- tySpec tyExpr
        assembleExpr False (IArray elemTy) arr
        assembleExpr False IInt index
        assembleExpr False elemTy val
        storeArray elemTy
        Aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType JvmGetArray [tyExpr, index, arr, world] = do
        elemTy <- tySpec tyExpr
        assembleExpr False (IArray elemTy) arr
        assembleExpr False IInt index
        loadArray elemTy
        asmCast elemTy returnType
    jvmExtPrim _ returnType JvmArrayLength [tyExpr, arr] = do
        elemTy <- tySpec tyExpr
        assembleExpr False (IArray elemTy) arr
        Arraylength
        asmCast IInt returnType
    jvmExtPrim _ returnType NewIORef [_, val, world] = do
        New refClass
        Dup
        assembleExpr False IUnknown val
        InvokeMethod InvokeSpecial refClass "<init>" "(Ljava/lang/Object;)V" False
        asmCast refType returnType
    jvmExtPrim _ returnType ReadIORef [_, ref, world] = do
        assembleExpr False refType ref
        InvokeMethod InvokeVirtual refClass "getValue" "()Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType WriteIORef [_, ref, val, world] = do
        assembleExpr False refType ref
        assembleExpr False IUnknown val
        InvokeMethod InvokeVirtual refClass "setValue" "(Ljava/lang/Object;)V" False
        Aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType SysOS [] = do
        Field GetStatic idrisSystemClass "OS_NAME" "Ljava/lang/String;"
        asmCast inferredStringType returnType
    jvmExtPrim _ returnType SysCodegen [] = do
        Ldc $ StringConst "\"jvm\""
        asmCast inferredStringType returnType
    jvmExtPrim _ returnType VoidElim _ = do
        Ldc $ StringConst "Error: Executed 'void'"
        InvokeMethod InvokeStatic runtimeClass "crash" "(Ljava/lang/String;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType JvmClassLiteral [ty] = do
        assembleClassLiteral !(tySpec ty)
        asmCast (IRef "java/lang/Class" Class []) returnType
    jvmExtPrim _ returnType JvmInstanceOf [_, obj, ty] = do
        assembleExpr False IUnknown obj
        typeName <- getJvmReferenceTypeName !(tySpec ty)
        InstanceOf typeName
        asmCast IBool returnType
    jvmExtPrim _ returnType JvmRefEq [_, _, firstObj, secondObj] =
      assembleExprBinaryBoolOp returnType IUnknown Ifacmpne firstObj secondObj
    jvmExtPrim fc returnType JavaLambda [functionType, javaInterfaceType, lambda] =
      asmJavaLambda fc returnType functionType javaInterfaceType lambda
    jvmExtPrim _ returnType MakeFuture [_, action] = do
        assembleExpr False delayedType action
        InvokeMethod InvokeStatic runtimeClass "fork" "(Lio/github/mmhelloworld/idrisjvm/runtime/Delayed;)Ljava/util/concurrent/ForkJoinTask;" False
        asmCast inferredForkJoinTaskType returnType
    jvmExtPrim _ returnType (Unknown name) _ = asmCrash $ "Can't compile unknown external directive " ++ show name
    jvmExtPrim fc _ prim args = Throw fc $ "Unsupported external function " ++ show prim ++ "(" ++
        (show $ showNamedCExp 0 <$> args) ++ ")"

initializeFunctionState : Asm ()
initializeFunctionState = do
  lineNumberLabels <- LiftIo $ Map.newTreeMap {key=Int} {value=String}
  updateState $ {
      scopeCounter := 0,
      currentScopeIndex := 0,
      lambdaCounter := 0,
      labelCounter := 1,
      lineNumberLabels := lineNumberLabels }
  updateCurrentFunction $ { dynamicVariableCounter := 0 }

assembleDefinition : Name -> FC -> Asm ()
assembleDefinition idrisName fc = do
    let jname = jvmName idrisName
    resetScope
    loadFunction jname
    function <- getCurrentFunction
    let functionType = inferredFunctionType function
    let arity = length $ parameterTypes functionType
    let jvmClassAndMethodName = jvmClassMethodName function
    let declaringClassName = className jvmClassAndMethodName
    let methodName = methodName jvmClassAndMethodName
    let methodReturnType = returnType functionType
    initializeFunctionState
    let optimizedExpr = optimizedBody function
    when (shouldDebugFunction jname) $ logAsm $ "Assembling " ++ declaringClassName ++ "." ++ methodName ++ "\n" ++
      showNamedCExp 0 optimizedExpr
    let fileName = fst $ getSourceLocationFromFc fc
    let descriptor = getMethodDescriptor functionType
    -- Cache only top level nil arity functions. Don't cache extracted function results.
    let isField = arity == 0 && not (extractedFunctionLabel `isInfixOf` methodName)
    let classInitOrMethodName = if isField then "<clinit>" else methodName
    when isField $ do
        CreateField [Public, Static, Final] fileName declaringClassName methodName
            "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;" Nothing Nothing []
        FieldEnd
    CreateMethod [Public, Static] fileName declaringClassName classInitOrMethodName descriptor Nothing Nothing [] []
    if (not isField)
        then do
            MethodCodeStart
            CreateLabel methodStartLabel
            CreateLabel methodEndLabel
            LabelStart methodStartLabel
            withScope $ do
                scopeIndex <- getCurrentScopeIndex
                scope <- getScope scopeIndex
                let (lineNumberStart, lineNumberEnd) = lineNumbers scope
                addLineNumber lineNumberStart methodStartLabel
                updateScopeStartLabel scopeIndex methodStartLabel
                updateScopeEndLabel scopeIndex methodEndLabel
                assembleExpr True methodReturnType optimizedExpr
                LabelStart methodEndLabel
            addLocalVariables 0
            MaxStackAndLocal (-1) (-1)
            MethodCodeEnd
        else do
            withScope $ assembleExpr False delayedType optimizedExpr
            Field PutStatic declaringClassName methodName "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"

createMainMethod : String -> Jname -> Asm ()
createMainMethod programName mainFunctionName = do
    function <- getFunction mainFunctionName
    let idrisMainClassMethodName = jvmClassMethodName function
    let mainClassName = className idrisMainClassMethodName
    CreateMethod [Public, Static] "Main.idr" mainClassName "main" "([Ljava/lang/String;)V" Nothing Nothing [] []
    MethodCodeStart
    Ldc $ StringConst programName
    Aload 0
    InvokeMethod InvokeStatic runtimeClass "setProgramArgs" "(Ljava/lang/String;[Ljava/lang/String;)V" False
    Field GetStatic mainClassName (methodName idrisMainClassMethodName) "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
    InvokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate"
        "()Ljava/lang/Object;" False
    Return
    MaxStackAndLocal (-1) (-1)
    MethodCodeEnd

groupByClassName : String -> List Name -> List (List Name)
groupByClassName programName names = unsafePerformIO $ do
    namesByClassName <- Map.newTreeMap {key=String} {value=List Name}
    go1 namesByClassName names
    Map.values {key=String} namesByClassName
  where
    go1 : Map String (List Name) -> List Name -> IO ()
    go1 namesByClassName values = go2 values where
        go2 : List Name -> IO ()
        go2 [] = pure ()
        go2 (name :: names) = do
            let jvmClassName = className $ getJvmClassMethodName programName name
            existingNamesOpt <- Map.get namesByClassName jvmClassName
            let newNames = maybe [name] ((::) name) $ nullableToMaybe existingNamesOpt
            _ <- Map.put {key=String} {value=List Name} namesByClassName jvmClassName newNames
            go2 names

assemble : AsmGlobalState -> Map String (FC, NamedDef) -> Name -> IO ()
assemble globalState fcAndDefinitionsByName name = do
    fcDef <- Map.get {value=(FC, NamedDef)} fcAndDefinitionsByName (jvmSimpleName name)
    case nullableToMaybe fcDef of
        Just (fc, def) => do
            programName <- AsmGlobalState.getProgramName globalState
            asmState <- createAsmState globalState name
            ignore $ asm asmState $ do
                inferDef programName name fc def
                assembleDefinition name fc
                scopes <- LiftIo $ ArrayList.new {elemTy=Scope}
                updateCurrentFunction $ { scopes := (subtyping scopes), optimizedBody := emptyFunction }
        Nothing => pure ()

assembleAsync : AsmGlobalState -> Map String (FC, NamedDef) -> List (List Name) -> IO ()
assembleAsync _ _ [] = pure ()
assembleAsync globalState fcAndDefinitionsByName (xs :: xss) = do
    threadIds <- traverse forkAssemble xs
    waitForFuturesToComplete threadIds
    assembleAsync globalState fcAndDefinitionsByName xss
  where
    forkAssemble : Name -> IO ThreadID
    forkAssemble name = fork $ assemble globalState fcAndDefinitionsByName name

getNameStrFcDef : (Name, FC, NamedDef) -> (String, FC, NamedDef)
getNameStrFcDef (name, fc, def) = (jvmSimpleName name, fc, def)

getNameStrDef : (String, FC, NamedDef) -> (String, NamedDef)
getNameStrDef (name, fc, def) = (name, def)

isForeignDef : (Name, FC, NamedDef) -> Bool
isForeignDef (_, _, MkNmForeign _ _ _) = True
isForeignDef _ = False

exportConstructor : Map Int InferredType -> InferredType -> Int -> Jname -> Name -> InferredFunctionType -> Asm ()
exportConstructor jvmArgumentTypesByIndex jvmReturnType arity jvmIdrisName idrisName idrisFunctionType = do
  function <- getCurrentFunction
  initializeFunctionState
  let optimizedExpr = optimizedBody function
  let internalJname = function.idrisName
  when (shouldDebugFunction internalJname) $ logAsm $ "Assembling " ++ (className internalJname) ++ "." ++
    (methodName internalJname) ++ "\n" ++ showNamedCExp 0 optimizedExpr
  CreateLabel methodStartLabel
  CreateLabel methodEndLabel
  LabelStart methodStartLabel
  withScope $ do
    scopeIndex <- getCurrentScopeIndex
    scope <- getScope scopeIndex
    let (lineNumberStart, lineNumberEnd) = lineNumbers scope
    addLineNumber lineNumberStart methodStartLabel
    updateScopeStartLabel scopeIndex methodStartLabel
    updateScopeEndLabel scopeIndex methodEndLabel
    assembleExpr False IVoid (optimizedBody function)
    loadArguments jvmArgumentTypesByIndex idrisName arity (parameterTypes idrisFunctionType)
    let idrisMethodDescriptor = getMethodDescriptor idrisFunctionType
    programName <- getProgramName
    let qualifiedJvmIdrisName = getIdrisFunctionName programName (className jvmIdrisName) (methodName jvmIdrisName)
    InvokeMethod InvokeStatic
      (className qualifiedJvmIdrisName) (methodName qualifiedJvmIdrisName) idrisMethodDescriptor False
    InvokeMethod InvokeStatic (programName ++ "/PrimIO") "unsafePerformIO" "(Ljava/lang/Object;)Ljava/lang/Object;" False
    asmCast (returnType idrisFunctionType) jvmReturnType
    asmReturn jvmReturnType
    LabelStart methodEndLabel
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

exportFunction : MethodExport -> Asm ()
exportFunction (MkMethodExport jvmFunctionName idrisName type shouldPerformIO encloser modifiers annotations parameterAnnotations) = do
    let jvmClassName = encloser.name
    let fileName = fst $ getSourceLocationFromFc emptyFC
    let MkInferredFunctionType jvmReturnType jvmArgumentTypes = type
    let arity = length jvmArgumentTypes
    let arityInt = the Int $ cast $ length jvmArgumentTypes
    jvmArgumentTypesByIndex <- LiftIo $ Map.fromList $ zip [0 .. (arityInt - 1)] jvmArgumentTypes
    let isInstance = not $ elem Static modifiers
    jvmArgumentTypesForSignature <- adjustArgumentsForInstanceMember idrisName isInstance jvmArgumentTypes
    let functionType = MkInferredFunctionType jvmReturnType jvmArgumentTypesForSignature
    let exportedFieldName = jvmFunctionName
    let simpleIdrisName = dropAllNS idrisName
    let asmAnnotations = asmAnnotation <$> annotations
    let asmParameterAnnotations = (\annotations => asmAnnotation <$> annotations) <$> parameterAnnotations
    let descriptor = getMethodDescriptor functionType
    let signature = Just $ getMethodSignature functionType
    CreateMethod modifiers fileName jvmClassName jvmFunctionName descriptor signature Nothing asmAnnotations
      asmParameterAnnotations
    MethodCodeStart
    (_, MkNmFun idrisFunctionArgs idrisFunctionBody) <- getFcAndDefinition (jvmSimpleName idrisName)
      | _ => asmCrash ("Unknown idris function " ++ show idrisName)
    let idrisFunctionArity = length idrisFunctionArgs
    let idrisArgumentTypes = replicate idrisFunctionArity inferredObjectType
    let idrisFunctionType = MkInferredFunctionType inferredObjectType idrisArgumentTypes
    let jvmIdrisName = jvmName idrisName
    let isField = idrisFunctionArity == 0
    let isConstructor = jvmFunctionName == "<init>"
    if isConstructor
      then exportConstructor jvmArgumentTypesByIndex jvmReturnType arityInt jvmIdrisName idrisName idrisFunctionType
      else if not isField then do
        loadArguments jvmArgumentTypesByIndex idrisName arityInt (parameterTypes idrisFunctionType)
        let idrisMethodDescriptor = getMethodDescriptor idrisFunctionType
        programName <- getProgramName
        let qualifiedJvmIdrisName = getIdrisFunctionName programName (className jvmIdrisName)
                                      (methodName jvmIdrisName)
        InvokeMethod InvokeStatic
          (className qualifiedJvmIdrisName) (methodName qualifiedJvmIdrisName) idrisMethodDescriptor False
        when shouldPerformIO $
          InvokeMethod InvokeStatic (programName ++ "/PrimIO") "unsafePerformIO"
            "(Ljava/lang/Object;)Ljava/lang/Object;" False
        asmCast (returnType idrisFunctionType) jvmReturnType
        asmReturn jvmReturnType
        MaxStackAndLocal (-1) (-1)
        MethodCodeEnd
      else do
        programName <- getProgramName
        let qualifiedJvmIdrisName = getIdrisFunctionName programName (className jvmIdrisName)
                                      (methodName jvmIdrisName)
        Field GetStatic (className qualifiedJvmIdrisName) (methodName qualifiedJvmIdrisName)
            "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
        InvokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate"
            "()Ljava/lang/Object;" False
        asmCast inferredObjectType jvmReturnType
        asmReturn jvmReturnType
        MaxStackAndLocal (-1) (-1)
        MethodCodeEnd

exportField : FieldExport -> Asm ()
exportField (MkFieldExport fieldName type encloser modifiers annotations) = do
  let jvmClassName = encloser.name
  let asmAnnotations = asmAnnotation <$> annotations
  CreateField modifiers "Unknown.idr" jvmClassName fieldName (getJvmTypeDescriptor type) Nothing Nothing asmAnnotations
  FieldEnd

exportClass : ClassExport -> Asm ()
exportClass (MkClassExport name idrisName extends implements modifiers annotations) = do
  CreateClass [ComputeMaxs, ComputeFrames]
  let annotations = filter (not . isIdrisJvmAnnotation) annotations
  let signature = getSignature extends ++ concat (getSignature <$> implements)
  extendsTypeName <- getJvmReferenceTypeName extends
  implementsTypeNames <- traverse getJvmReferenceTypeName implements
  let asmAnnotations = asmAnnotation <$> annotations
  ClassCodeStart 52 modifiers name (Just signature) extendsTypeName implementsTypeNames asmAnnotations

generateAccessors : SortedMap ClassExport (List ExportDescriptor) -> ClassExport ->
                      (accessorCreator: FieldExport -> Asm ()) -> Asm ()
generateAccessors descriptorsByEncloser classExport accessorCreator = do
  let className = classExport.name
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  traverse_ accessorCreator fields

generateGetters : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateGetters descriptorsByEncloser classExport =
  generateAccessors descriptorsByEncloser classExport (createGetter classExport)

generateSetters : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateSetters descriptorsByEncloser classExport =
  generateAccessors descriptorsByEncloser classExport (createSetter classExport)

generateConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport ->
                        List FieldExport -> Asm ()
generateConstructor descriptorsByEncloser classExport fields = do
  let fieldTypes = FieldExport.type <$> fields
  let descriptor = getMethodDescriptor $ MkInferredFunctionType IVoid fieldTypes
  let signature = Just $ getMethodSignature $ MkInferredFunctionType IVoid fieldTypes
  let classType = iref classExport.name []
  extendsTypeName <- getJvmReferenceTypeName classExport.extends
  let arity = the Int $ cast $ length fields
  jvmArgumentTypesByIndex <- LiftIo $ Map.fromList $ zip [0 .. arity] (classType :: fieldTypes)
  CreateMethod [Public] "generated.idr" classExport.name "<init>" descriptor signature Nothing [] []
  MethodCodeStart
  CreateLabel methodStartLabel
  CreateLabel methodEndLabel
  LabelStart methodStartLabel
  Aload 0
  InvokeMethod InvokeSpecial extendsTypeName "<init>" "()V" False
  assignFields jvmArgumentTypesByIndex fields
  Return
  LabelStart methodEndLabel
  LocalVariable "this" (getJvmTypeDescriptor classType) Nothing methodStartLabel methodEndLabel 0
  traverse_ (uncurry addLocalVariable) $ zip [1 .. arity] fields
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd
 where
  assignField : Map Int InferredType -> Int -> FieldExport -> Asm ()
  assignField jvmArgumentTypesByIndex varIndex field = do
    let fieldType = field.type
    Aload 0
    loadVar jvmArgumentTypesByIndex fieldType fieldType varIndex
    Field PutField classExport.name field.name (getJvmTypeDescriptor fieldType)

  assignFields : Map Int InferredType -> List FieldExport -> Asm ()
  assignFields jvmArgumentTypesByIndex fieldExports = do
    let arity = the Int $ cast $ length fieldExports
    let varIndexAndExports = zip [1 .. arity] fieldExports
    traverse_ (uncurry $ assignField jvmArgumentTypesByIndex) varIndexAndExports

  addLocalVariable : Int -> FieldExport -> Asm ()
  addLocalVariable index field = do
    let fieldType = field.type
    LocalVariable field.name (getJvmTypeDescriptor fieldType) Nothing methodStartLabel methodEndLabel index

generateRequiredArgsConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateRequiredArgsConstructor descriptorsByEncloser classExport = do
  let allFields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  let requiredFields = filter isRequiredField allFields
  when (not $ isNil requiredFields) $ generateConstructor descriptorsByEncloser classExport requiredFields

generateAllArgsConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateAllArgsConstructor descriptorsByEncloser classExport = do
  let Just (MkAnnotation _ props) = findAllArgsConstructor classExport
        | _ => Pure ()
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  let excludedFields = getStringAnnotationValues $ snd $ fromMaybe ("exclude", AnnArray []) $
                        (find (\(name, value) => name == "exclude") props)
  let constructorFields = filter (\fieldExport => not $ elem fieldExport.name excludedFields) fields
  generateConstructor descriptorsByEncloser classExport constructorFields

generateNoArgsConstructor : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateNoArgsConstructor descriptorsByEncloser classExport = do
  CreateMethod [Public] "generated.idr" classExport.name "<init>" "()V" Nothing Nothing [] []
  MethodCodeStart
  Aload 0
  extendsTypeName <- getJvmReferenceTypeName classExport.extends
  InvokeMethod InvokeSpecial extendsTypeName "<init>" "()V" False
  Return
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

generateHashCode : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateHashCode descriptorsByEncloser classExport = do
  let fields = filter (not . isTransientField) $ getFields $
                 fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  CreateMethod [Public] "generated.idr" classExport.name "hashCode" "()I" Nothing Nothing [] []
  MethodCodeStart
  let fieldsCount = the Int $ cast $ length fields
  Iconst fieldsCount
  Anewarray "java/lang/Object"
  traverse_ (uncurry loadField) $ zip [0 .. fieldsCount - 1] fields
  InvokeMethod InvokeStatic "java/util/Objects" "hash" "([Ljava/lang/Object;)I" False
  Ireturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd
 where
  loadField : Int -> FieldExport -> Asm ()
  loadField index field = do
    Dup
    Iconst index
    Aload 0
    let fieldType = field.type
    Field GetField classExport.name field.name (getJvmTypeDescriptor fieldType)
    asmCast fieldType inferredObjectType
    Aastore

generateEquals : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateEquals descriptorsByEncloser classExport = do
  let fields = filter (not . isTransientField) $ getFields $
                 fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  CreateMethod [Public] "generated.idr" classExport.name "equals" "(Ljava/lang/Object;)Z" Nothing Nothing [] []
  MethodCodeStart
  Aload 0
  Aload 1
  refEqLabel <- newLabel
  CreateLabel refEqLabel
  Ifacmpne refEqLabel
  Iconst 1
  Ireturn
  LabelStart refEqLabel
  Aload 1
  let className = classExport.name
  InstanceOf className
  instanceOfLabel <- newLabel
  CreateLabel instanceOfLabel
  Ifne instanceOfLabel
  Iconst 0
  Ireturn
  LabelStart instanceOfLabel
  Aload 1
  checkcast className
  Astore 2
  let fieldsCount = the Int $ cast $ length fields
  equalsLabel <- newLabel
  CreateLabel equalsLabel
  equalsFields equalsLabel fields
  Iconst 1
  methodEndLabel <- newLabel
  CreateLabel methodEndLabel
  Goto methodEndLabel
  LabelStart equalsLabel
  Iconst 0
  LabelStart methodEndLabel
  Ireturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd
 where
  equalsFields : String -> List FieldExport -> Asm ()
  equalsFields equalsLabel [] = Pure ()
  equalsFields equalsLabel (field :: rest) = do
    let fieldType = field.type
    let className = classExport.name
    Aload 0
    Field GetField className field.name (getJvmTypeDescriptor fieldType)
    asmCast fieldType inferredObjectType
    Aload 2
    Field GetField className field.name (getJvmTypeDescriptor fieldType)
    asmCast fieldType inferredObjectType
    InvokeMethod InvokeStatic "java/util/Objects" "equals" "(Ljava/lang/Object;Ljava/lang/Object;)Z" False
    Ifeq equalsLabel
    equalsFields equalsLabel rest

generateToString : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateToString descriptorsByEncloser classExport = do
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  CreateMethod [Public] "generated.idr" classExport.name "toString" "()Ljava/lang/String;" Nothing Nothing [] []
  MethodCodeStart
  New "java/lang/StringBuilder"
  Dup
  InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
  Ldc $ StringConst classExport.name
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
  let hasFields = not $ isNil fields
  when hasFields $ do
    appendFields "{" fields
    Iconst 125 -- '}'
    InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(C)Ljava/lang/StringBuilder;" False
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
  Areturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd
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

  appendFields : String -> List FieldExport -> Asm ()
  appendFields _ [] = Pure ()
  appendFields prefixChar (field :: rest) = do
    let fieldName = field.name
    let fieldType = field.type
    let className = classExport.name
    let isStringField = field.type == inferredStringType
    Ldc $ StringConst (prefixChar ++ fieldName ++ "=" ++ if isStringField then "'" else "")
    InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
    Aload 0
    Field GetField className fieldName (getJvmTypeDescriptor fieldType)
    let appendParamType = getAppendParamType fieldType
    asmCast fieldType appendParamType
    let stringBuilderType = iref "java/lang/StringBuilder" []
    let appendDescriptor = getMethodDescriptor $ MkInferredFunctionType stringBuilderType [appendParamType]
    InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" appendDescriptor False
    when isStringField $ do
      Iconst 39 -- single quote
      InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(C)Ljava/lang/StringBuilder;" False
    appendFields ", " rest

generateDataClass : SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Asm ()
generateDataClass descriptorsByEncloser classExport = do
  generateGetters descriptorsByEncloser classExport
  generateSetters descriptorsByEncloser classExport
  generateRequiredArgsConstructor descriptorsByEncloser classExport
  generateHashCode descriptorsByEncloser classExport
  generateEquals descriptorsByEncloser classExport
  generateToString descriptorsByEncloser classExport

exportMemberIo : AsmGlobalState -> SortedMap ClassExport (List ExportDescriptor) -> ExportDescriptor -> IO ()
exportMemberIo globalState descriptorsByEncloser (MkMethodExportDescriptor desc) =
  if desc.name == "<init>"
    then do
      let idrisName = desc.idrisName
      fcDef <- getFcAndDefinition globalState (jvmSimpleName idrisName)
      case nullableToMaybe fcDef of
        Just (fc, MkNmFun args expr) => do
            let jname = jvmName desc.idrisName
            let dottedClassName = replace (className jname) '/' '.'
            let constructorIdrisName = NS (mkNamespace desc.encloser.name) (UN $ Basic (methodName jname ++ "<init>"))
            programName <- AsmGlobalState.getProgramName globalState
            asmState <- createAsmStateJavaName globalState desc.encloser.name
            ignore $ asm asmState $ do
              Just superCallExpr <- getSuperCallExpr expr
                | Nothing => asmCrash ("Constructor export for " ++ show idrisName ++ " should call 'super'")
              inferDef programName constructorIdrisName fc (MkNmFun args superCallExpr)
              resetScope
              loadFunction $ jvmName constructorIdrisName
              exportFunction desc
              scopes <- LiftIo $ ArrayList.new {elemTy=Scope}
              updateCurrentFunction $ { scopes := (subtyping scopes), optimizedBody := emptyFunction }
        _ => pure ()
    else do
      asmState <- createAsmStateJavaName globalState desc.encloser.name
      ignore $ asm asmState $ exportFunction desc
exportMemberIo globalState descriptorsByEncloser (MkFieldExportDescriptor desc) = do
  asmState <- createAsmStateJavaName globalState desc.encloser.name
  ignore $ asm asmState $ exportField desc
exportMemberIo globalState descriptorsByEncloser (MkClassExportDescriptor classExport) = do
  asmState <- createAsmStateJavaName globalState classExport.name
  ignore $ asm asmState $ exportClass classExport
  let hasDataAnnotation = isJust (findClassAnnotation "Data" classExport)
  ignore $ asm asmState $ generateAllArgsConstructor descriptorsByEncloser classExport
  when (isJust (findNoArgsConstructor classExport)) $
    ignore $ asm asmState $ generateNoArgsConstructor descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findRequiredArgsConstructor classExport)) $
    ignore $ asm asmState $ generateRequiredArgsConstructor descriptorsByEncloser classExport
  when hasDataAnnotation $ ignore $ asm asmState $ generateDataClass descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "Getter" classExport)) $
    ignore $ asm asmState $ generateGetters descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "Setter" classExport)) $
    ignore $ asm asmState $ generateSetters descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "EqualsAndHashCode" classExport)) $ do
      ignore $ asm asmState $ generateEquals descriptorsByEncloser classExport
      ignore $ asm asmState $ generateHashCode descriptorsByEncloser classExport
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
    partitionExports (classExports, methodFieldExports) (desc@(MkImportDescriptor _ _) :: rest) =
      partitionExports (classExports, methodFieldExports) rest
    partitionExports (classExports, methodFieldExports) ((MkClassExportDescriptor desc) :: rest) =
      partitionExports (desc :: classExports, methodFieldExports) rest

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

export
exportDefs : AsmGlobalState -> List (Name, String) -> IO ()
exportDefs globalState nameAndDescriptors = do
  descriptors <- parseExportDescriptors globalState nameAndDescriptors
  let descriptorsByEncloser = groupByEncloser descriptors
  traverse_ (exportMemberIo globalState descriptorsByEncloser) descriptors

export
getExport : NoMangleMap -> Name -> Maybe (Name, String)
getExport noMangleMap name = (\descriptor => (name, descriptor)) <$> isNoMangle noMangleMap name

||| Compile a TT expression to JVM bytecode
compileToJvmBytecode : Ref Ctxt Defs -> String -> String -> ClosedTerm -> Core ()
compileToJvmBytecode c outputDirectory outputFile term = do
    noMangleMapRef <- initNoMangle ["jvm"] (const True)
    noMangleMap <- get NoMangleMap
    cdata <- getCompileDataWith ["jvm"] False Cases term
    directives <- getDirectives Jvm
    let ndefs = namedDefs cdata
    let idrisMainBody = forget (mainExpr cdata)
    let programName = if outputFile == "" then "repl" else outputFile
    let mainFunctionName = idrisMainFunctionName programName
    let allDefs = (mainFunctionName, emptyFC, MkNmFun [] idrisMainBody) :: ndefs
    let nameFcDefs = optimize programName allDefs ++ filter isForeignDef allDefs
    let nameStrFcDefs = getNameStrFcDef <$> nameFcDefs
    fcAndDefinitionsByName <- coreLift $ Map.fromList nameStrFcDefs
    let nameStrDefs = getNameStrDef <$> nameStrFcDefs
    definitionsByName <- coreLift $ Map.fromList nameStrDefs
    globalState <- coreLift $ newAsmGlobalState programName fcAndDefinitionsByName
    let names = fst <$> nameFcDefs
    let namesByClassName = groupByClassName programName names
    coreLift $ do
        assembleAsync globalState fcAndDefinitionsByName (transpose namesByClassName)
        exportDefs globalState $ mapMaybe (getExport noMangleMap) (fst <$> allDefs)
        mainAsmState <- createAsmState globalState mainFunctionName
        let mainFunctionJname = jvmName mainFunctionName
        _ <- runAsm mainAsmState $ createMainMethod programName mainFunctionJname
        classCodeEnd globalState outputDirectory outputFile (className mainFunctionJname)

||| JVM bytecode implementation of the `compileExpr` interface.
compileExprJvm : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> (tmpDir : String) -> (outDir: String) -> ClosedTerm ->
                    (outputFile : String) -> Core (Maybe String)
compileExprJvm c _ tmpDir outDir term outputFile
    = do let outputDirectory = if outputFile == "" then "" else outDir
         when (outputDirectory /= "") $ ignore $ coreLift $ mkdirAll outputDirectory
         compileToJvmBytecode c outputDirectory outputFile term
         pure $ Just outputDirectory

||| JVM bytecode implementation of the `executeExpr` interface.
||| This implementation simply runs the usual compiler, saving it to a temp file, then interpreting it.
executeExprJvm : Ref Ctxt Defs -> Ref Syn SyntaxInfo -> (execDir : String) -> ClosedTerm -> Core ()
executeExprJvm c s execDir term = ignore $ compileExprJvm c s execDir "" term ""

||| Codegen wrapper for JVM implementation.
export
codegenJvm : Codegen
codegenJvm = MkCG compileExprJvm executeExprJvm Nothing Nothing
