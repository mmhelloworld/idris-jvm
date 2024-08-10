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
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect

import Debug.Trace

import Core.Env

import Idris.Pretty.Annotations
import Idris.Syntax
import Idris.Resugar
import Idris.Doc.String

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

%hide Core.Context.Context.Constructor.arity
%hide Core.Name.Scoped.Scope
%hide System.FFI.runtimeClass

getType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> Name -> Core (Maybe (Term []))
getType name = do
  defs <- get Ctxt
  Just gdef <- lookupCtxtExact name (gamma defs)
    | Nothing => pure Nothing
  ty <- normaliseHoles defs [] gdef.type
  Just <$> toFullNames ty

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

conAltStringExpr : {auto stateRef: Ref AsmState AsmState} -> NamedConAlt -> Core (String, String, NamedConAlt)
conAltStringExpr alt@(MkNConAlt name _ _ _ expr) = do
    label <- newLabel
    pure (label, jvmSimpleName name, alt)

createDefaultLabel : {auto stateRef: Ref AsmState AsmState} -> Core String
createDefaultLabel = do
    label <- newLabel
    createLabel label
    pure label

getSwitchCasesWithEndLabel : List (String, Int, a) -> List String -> List (String, Int, a, String)
getSwitchCasesWithEndLabel switchCases labelStarts = go $ zip switchCases (drop 1 labelStarts ++ [methodEndLabel])
    where
        go : List ((String, Int, a), String) -> List (String, Int, a, String)
        go (((labelStart, constExpr, body), labelEnd) :: xs) = (labelStart, constExpr, body, labelEnd) :: go xs
        go [] = []

labelHashCodeAlt : {auto stateRef: Ref AsmState AsmState} -> (Int, a) -> Core (String, Int, a)
labelHashCodeAlt (hash, expressions) = pure (!newLabel, hash, expressions)

getHashCodeCasesWithLabels : {auto stateRef: Ref AsmState AsmState} -> SortedMap Int (List (Int, a)) -> Core (List (String, Int, List (Int, a)))
getHashCodeCasesWithLabels positionAndAltsByHash = traverse labelHashCodeAlt $ SortedMap.toList positionAndAltsByHash

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

assembleNil : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> Core ()
assembleNil isTailCall returnType = do
    field GetStatic idrisNilClass "INSTANCE" "Lio/github/mmhelloworld/idrisjvm/runtime/IdrisList$Nil;"
    asmCast idrisObjectType returnType
    when isTailCall $ asmReturn returnType

assembleNothing : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> Core ()
assembleNothing isTailCall returnType = do
    field GetStatic idrisNothingClass "INSTANCE" "Lio/github/mmhelloworld/idrisjvm/runtime/Maybe$Nothing;"
    asmCast idrisObjectType returnType
    when isTailCall $ asmReturn returnType

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

mutual
    assembleExpr : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> NamedCExp -> Core ()
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
                jname <- idrisName <$> getCurrentFunction
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
        let jname = jvmName idrisName
        functionType <- case !(findFunctionType jname) of
            Just ty => pure ty
            Nothing => pure $ MkInferredFunctionType inferredObjectType $ replicate (length args) inferredObjectType
        let paramTypes = parameterTypes functionType
        if paramTypes == []
            then assembleNmAppNilArity isTailCall returnType idrisName
            else do
                let argsWithTypes = zip args paramTypes
                traverse_ assembleParameter argsWithTypes
                let methodReturnType = InferredFunctionType.returnType functionType
                let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType methodReturnType paramTypes
                let functionName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
                invokeMethod InvokeStatic (className functionName) (methodName functionName) methodDescriptor False
                asmCast methodReturnType returnType
                when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType (NmApp _ lambdaVariable [arg]) = do
        assembleExpr False inferredLambdaType lambdaVariable
        assembleExpr False IUnknown arg
        invokeMethod InvokeInterface "java/util/function/Function" "apply" "(Ljava/lang/Object;)Ljava/lang/Object;" True
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleExpr isTailCall returnType expr@(NmCon _ _ NOTHING _ []) = assembleNothing isTailCall returnType
    assembleExpr isTailCall returnType expr@(NmCon fc _ NOTHING _ _) =
      throw $ GenericMsg fc "Invalid NOTHING constructor"
    assembleExpr isTailCall returnType expr@(NmCon _ _ JUST _ [value]) = assembleJust isTailCall returnType value
    assembleExpr isTailCall returnType expr@(NmCon fc _ JUST _ _) = throw $ GenericMsg fc "Invalid JUST constructor"

    assembleExpr isTailCall returnType expr@(NmCon _ _ NIL _ []) = assembleNil isTailCall returnType
    assembleExpr isTailCall returnType expr@(NmCon fc _ NIL _ _) = throw $ GenericMsg fc "Invalid NIL constructor"
    assembleExpr isTailCall returnType expr@(NmCon _ _ CONS _ [head, tail]) =
      assembleCons isTailCall returnType head tail
    assembleExpr isTailCall returnType expr@(NmCon fc _ CONS _ _) = throw $ GenericMsg fc "Invalid CONS constructor"

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
        invokeMethod InvokeStatic runtimeClass "force" "(Ljava/lang/Object;)Ljava/lang/Object;" False
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
    assembleExpr isTailCall returnType (NmPrimVal fc _) = do
        iconst 0
        asmCast IInt returnType
        when isTailCall $ asmReturn returnType
    assembleExpr isTailCall IInt (NmErased fc) = do iconst 0; when isTailCall $ asmReturn IInt
    assembleExpr isTailCall IChar (NmErased fc) = do iconst 0; when isTailCall $ asmReturn IChar
    assembleExpr isTailCall IDouble (NmErased fc) =  do ldc $ DoubleConst 0; when isTailCall $ asmReturn IDouble
    assembleExpr isTailCall returnType (NmErased fc) = do aconstnull; when isTailCall $ asmReturn returnType
    assembleExpr isTailCall returnType (NmCrash fc msg) = do
        ldc $ StringConst msg
        invokeMethod InvokeStatic runtimeClass "crash" "(Ljava/lang/String;)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType
    assembleExpr _ _ expr = throw $ GenericMsg (getFC expr) $ "Cannot compile " ++ show expr ++ " yet"

    castInt : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Core () -> NamedCExp -> Core ()
    castInt returnType conversionOp expr = jassembleCast returnType IInt IInt conversionOp expr

    jassembleCast : {auto stateRef: Ref AsmState AsmState} -> InferredType -> InferredType -> InferredType -> Core ()
                  -> NamedCExp -> Core ()
    jassembleCast returnType from to conversionOp expr = do
        assembleExpr False from expr
        conversionOp
        asmCast to returnType

    assembleNmAppNilArity : {auto stateRef: Ref AsmState AsmState} -> (isTailCall : Bool) -> InferredType -> Name -> Core ()
    assembleNmAppNilArity isTailCall returnType idrisName = do
        let jname = jvmName idrisName
        let functionName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
        field GetStatic (className functionName) (methodName functionName)
            "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
        invokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate"
            "()Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
        when isTailCall $ asmReturn returnType

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

    assembleCon : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> FC -> Name -> (tag : Maybe Int) -> List NamedCExp -> Core ()
    assembleCon isTailCall returnType fc name tag args = do
        let fileName = fst $ getSourceLocationFromFc fc
        let constructorClassName = getIdrisConstructorClassName (jvmSimpleName name)
        let constructorType = maybe inferredStringType (const IInt) tag
        new constructorClassName
        dup
        maybe (ldc . StringConst $ constructorClassName) iconst tag
        let constructorParameterCountNat = length args
        let constructorParameterCount = the Int $ cast constructorParameterCountNat
        let constructorTypes = constructorType :: replicate constructorParameterCountNat inferredObjectType
        let argsWithTypes = zip args $ drop 1 constructorTypes
        traverse_ assembleParameter argsWithTypes
        let descriptor = getMethodDescriptor $ MkInferredFunctionType IVoid constructorTypes
        globalState <- getGlobalState
        hasConstructor <- coreLift $ AsmGlobalState.hasConstructor globalState constructorClassName
        when (not hasConstructor) $ do
            coreLift $ AsmGlobalState.addConstructor globalState constructorClassName
            createIdrisConstructorClass constructorClassName (isNothing tag) constructorParameterCount
        invokeMethod InvokeSpecial constructorClassName "<init>" descriptor False
        asmCast idrisObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleCons : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> NamedCExp -> NamedCExp -> Core ()
    assembleCons isTailCall returnType head tail = do
        new idrisConsClass
        dup
        assembleExpr False inferredObjectType head
        assembleExpr False inferredObjectType tail
        invokeMethod InvokeSpecial idrisConsClass "<init>" "(Ljava/lang/Object;Ljava/lang/Object;)V" False
        asmCast idrisObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleJust : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType -> NamedCExp -> Core ()
    assembleJust isTailCall returnType value = do
        new idrisJustClass
        dup
        assembleExpr False inferredObjectType value
        invokeMethod InvokeSpecial idrisJustClass "<init>" "(Ljava/lang/Object;)V" False
        asmCast idrisObjectType returnType
        when isTailCall $ asmReturn returnType

    assembleConstructorSwitchExpr : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core Int
    assembleConstructorSwitchExpr (NmLocal _ loc) = getVariableIndex $ jvmSimpleName loc
    assembleConstructorSwitchExpr sc = do
        idrisObjectVariableIndex <- getVariableIndex $ "constructorSwitchValue" ++ show !newDynamicVariableIndex
        assembleExpr False idrisObjectType sc
        storeVar idrisObjectType idrisObjectType idrisObjectVariableIndex
        pure idrisObjectVariableIndex

    assembleExprBinaryOp : {auto stateRef: Ref AsmState AsmState} -> InferredType -> InferredType -> Core ()
                         -> NamedCExp -> NamedCExp -> Core ()
    assembleExprBinaryOp returnType exprType operator expr1 expr2 = do
        assembleExpr False exprType expr1
        assembleExpr False exprType expr2
        operator
        asmCast exprType returnType

    assembleExprBinaryBoolOp : {auto stateRef: Ref AsmState AsmState} -> InferredType -> InferredType
                             -> (String -> Core ()) -> NamedCExp -> NamedCExp -> Core ()
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

    assembleExprComparableBinaryBoolOp : {auto stateRef: Ref AsmState AsmState} -> InferredType -> String -> (String -> Core ()) ->
        NamedCExp -> NamedCExp -> Core ()
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

    assembleExprUnaryOp : {auto stateRef: Ref AsmState AsmState} -> InferredType -> InferredType -> Core () -> NamedCExp -> Core ()
    assembleExprUnaryOp returnType exprType operator expr = do
        assembleExpr False exprType expr
        operator
        asmCast exprType returnType

    assembleStrCons : {auto stateRef: Ref AsmState AsmState} -> InferredType -> (char: NamedCExp) -> (str: NamedCExp) -> Core ()
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

    assembleStrReverse : {auto stateRef: Ref AsmState AsmState} -> InferredType -> NamedCExp -> Core ()
    assembleStrReverse returnType str = do
        new "java/lang/StringBuilder"
        dup
        assembleExpr False inferredStringType str
        invokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "(Ljava/lang/String;)V" False
        invokeMethod InvokeVirtual "java/lang/StringBuilder" "reverse" "()Ljava/lang/StringBuilder;" False
        invokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
        asmCast inferredStringType returnType

    compareUnsignedLong : {auto stateRef: Ref AsmState AsmState} -> (String -> Core ()) -> String -> Core ()
    compareUnsignedLong op label = do longCompareUnsigned; op label

    compareUnsignedInt : {auto stateRef: Ref AsmState AsmState} -> (String -> Core ()) -> String -> Core ()
    compareUnsignedInt op label = do integerCompareUnsigned; op label

    compareSignedLong : {auto stateRef: Ref AsmState AsmState} -> (String -> Core ()) -> String -> Core ()
    compareSignedLong op label = do lcmp; op label

    assembleCast : {auto stateRef: Ref AsmState AsmState} -> InferredType -> FC -> PrimType -> PrimType -> NamedCExp -> Core ()
    assembleCast returnType fc from to x =
      jassembleCast returnType (getInferredType from) (getInferredType to) (getCastAsmOp from to) x

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

    assembleExprOp : {auto stateRef: Ref AsmState AsmState} -> InferredType -> FC -> PrimFn arity -> Vect arity NamedCExp -> Core ()
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

    assembleParameter : {auto stateRef: Ref AsmState AsmState} -> (NamedCExp, InferredType) -> Core ()
    assembleParameter (param, ty) = assembleExpr False ty param

    storeParameter : {auto stateRef: Ref AsmState AsmState} -> Map Int InferredType
                   -> (Int, NamedCExp, InferredType) -> Core Int
    storeParameter variableTypes (var, (NmLocal _ loc), ty) = do
        let valueVariableName = jvmSimpleName loc
        valueVariableIndex <- getVariableIndex valueVariableName
        if var /= valueVariableIndex
            then do
                valueVariableType <- getVariableType valueVariableName
                loadVar variableTypes valueVariableType ty valueVariableIndex
                targetVariableIndex <- getDynamicVariableIndex "tailRecArg"
                storeVar ty ty targetVariableIndex
                pure targetVariableIndex
            else pure var
    storeParameter _ (var, param, ty) = do
        assembleExpr False ty param
        targetVariableIndex <- getDynamicVariableIndex "tailRecArg"
        storeVar ty ty targetVariableIndex
        pure targetVariableIndex

    createMethodReference : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> (arity: Nat) -> Name -> Core ()
    createMethodReference isTailCall arity name = do
        let jname = jvmName name
        functionType <- case !(findFunctionType jname) of
            Just ty => pure ty
            Nothing => pure $ MkInferredFunctionType inferredObjectType $ replicate arity inferredObjectType
        let methodReturnType = InferredFunctionType.returnType functionType
        let paramTypes = parameterTypes functionType
        let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType methodReturnType paramTypes
        let functionName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
        let functionInterface = getFunctionInterface arity
        let invokeDynamicDescriptor = getMethodDescriptor $ MkInferredFunctionType functionInterface []
        asmInvokeDynamic (className functionName) (methodName functionName) "apply" invokeDynamicDescriptor
           (getSamDesc (getLambdaTypeByArity arity)) methodDescriptor methodDescriptor
        when (arity > 1) $ do
          let methodDescriptor = getMethodDescriptor $ MkInferredFunctionType inferredLambdaType [functionInterface]
          invokeMethod InvokeStatic functionsClass "curry" methodDescriptor False
        when isTailCall $ asmReturn inferredLambdaType

    assembleSubMethodWithScope1 : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType
                                -> (parameterName : Maybe Name) -> NamedCExp -> Core ()
    assembleSubMethodWithScope1 isTailCall returnType parameterName body = do
        parentScope <- getScope !getCurrentScopeIndex
        withScope $ assembleSubMethod isTailCall returnType Nothing parameterName parentScope body

    assembleMethodReference : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType
                            -> (isMethodReference : Bool) -> (arity: Nat) -> (functionName: Name)
                            -> (parameterName : Maybe Name) -> NamedCExp -> Core ()
    assembleMethodReference isTailCall returnType isMethodReference arity functionName parameterName body =
      if isMethodReference
        then createMethodReference isTailCall arity functionName
        else assembleSubMethodWithScope1 isTailCall returnType parameterName body

    assembleSubMethodWithScope : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType
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

    assembleSubMethod : {auto stateRef: Ref AsmState AsmState} -> (isTailCall: Bool) -> InferredType
                      -> (parameterValueExpr: (Maybe (Core ()))) -> (parameterName: Maybe Name) -> Scope
                      -> NamedCExp -> Core ()
    assembleSubMethod isTailCall lambdaReturnType parameterValueExpr parameterName declaringScope expr = do
            scope <- getScope !getCurrentScopeIndex
            maybe (pure ()) (setScopeCounter . succ) (parentIndex scope)
            let lambdaBodyReturnType = returnType scope
            let lambdaType = getLambdaTypeByParameter parameterName
            when (lambdaType == DelayedLambda) $ do
                new "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed"
                dup
            let lambdaInterfaceType = getLambdaInterfaceType lambdaType
            parameterType <- traverseOpt getVariableType (jvmSimpleName <$> parameterName)
            variableTypes <- coreLift $ Map.values {key=Int} !(loadClosures declaringScope scope)
            maybe (pure ()) id parameterValueExpr
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
            let indy = the (Core ()) $ do
                let instantiatedMethodDescriptor = getMethodDescriptor $
                    MkInferredFunctionType implementationMethodReturnType $ toList parameterType
                asmInvokeDynamic lambdaClassName lambdaMethodName interfaceMethodName invokeDynamicDescriptor
                    (getSamDesc lambdaType) implementationMethodDescriptor instantiatedMethodDescriptor
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
            let lambdaReturnType = if isExtracted then lambdaBodyReturnType else inferredObjectType
            assembleExpr True lambdaReturnType expr
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

    assembleMissingDefault : {auto stateRef: Ref AsmState AsmState} ->InferredType -> FC -> String -> Core ()
    assembleMissingDefault returnType fc defaultLabel = do
        labelStart defaultLabel
        defaultValue returnType
        asmReturn returnType

    assembleConstantSwitch : {auto stateRef: Ref AsmState AsmState} -> (returnType: InferredType)
                           -> (switchExprType: InferredType) -> FC -> NamedCExp -> List NamedConstAlt
                           -> Maybe NamedCExp -> Core ()
    assembleConstantSwitch _ _ fc _ [] _ = throw $ GenericMsg fc "Empty cases"

    assembleConstantSwitch returnType IInt fc sc alts def = do
        assembleExpr False IInt sc
        switchCases <- getCasesWithLabels alts
        let labels = fst <$> switchCases
        let exprs = second <$> switchCases
        traverse_ createLabel labels
        defaultLabel <- createDefaultLabel
        lookupSwitch defaultLabel labels exprs
        let switchCasesWithEndLabel = getSwitchCasesWithEndLabel switchCases labels
        traverse_ assembleExprConstAlt switchCasesWithEndLabel
        maybe (assembleMissingDefault returnType fc defaultLabel) (assembleDefault defaultLabel) def
      where
        getCasesWithLabels : List NamedConstAlt -> Core (List (String, Int, NamedConstAlt))
        getCasesWithLabels alts = do
            caseExpressionsWithLabels <- traverse (constantAltIntExpr fc) alts
            pure $ sortBy (comparing second) caseExpressionsWithLabels

        assembleCaseWithScope : String -> String -> NamedCExp -> Core ()
        assembleCaseWithScope lblStart lblEnd expr = withScope $ do
            scopeIndex <- getCurrentScopeIndex
            scope <- getScope scopeIndex
            let (lineNumberStart, lineNumberEnd) = lineNumbers scope
            labelStart lblStart
            updateScopeStartLabel scopeIndex lblStart
            addLineNumber lineNumberStart lblStart
            updateScopeEndLabel scopeIndex lblEnd
            assembleExpr True returnType expr

        assembleDefault : String -> NamedCExp -> Core ()
        assembleDefault defaultLabel expr = assembleCaseWithScope defaultLabel methodEndLabel expr

        assembleExprConstAlt : (String, Int, NamedConstAlt, String) -> Core ()
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
            createLabel switchEndLabel
            traverse_ createLabel labels
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
                hashCodeSwitchCases
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

    assembleConCase : {auto stateRef: Ref AsmState AsmState} -> InferredType -> FC -> (sc : NamedCExp) -> List NamedConAlt -> Maybe NamedCExp -> Core ()
    assembleConCase returnType fc sc alts def = do
        idrisObjectVariableIndex <- assembleConstructorSwitchExpr sc
        let hasTypeCase = any isTypeCase alts
        let constructorType = if hasTypeCase then "Ljava/lang/String;" else "I"
        variableTypes <- getVariableTypes
        optTy <- coreLift $ Map.get variableTypes idrisObjectVariableIndex
        let idrisObjectVariableType = fromMaybe IUnknown $ nullableToMaybe optTy
        loadVar variableTypes idrisObjectVariableType idrisObjectType idrisObjectVariableIndex
        when (idrisObjectVariableType /= idrisObjectType) $ do
            storeVar idrisObjectType idrisObjectType idrisObjectVariableIndex
            loadVar !getVariableTypes idrisObjectType idrisObjectType idrisObjectVariableIndex
        let constructorGetter = if hasTypeCase then "getStringConstructorId" else "getConstructorId"
        invokeMethod InvokeInterface idrisObjectClass constructorGetter ("()" ++ constructorType) True
        if hasTypeCase
            then assembleStringConstructorSwitch returnType fc idrisObjectVariableIndex alts def
            else assembleConstructorSwitch returnType fc idrisObjectVariableIndex alts def

    assembleConCaseExpr : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Int -> List Name -> NamedCExp -> Core ()
    assembleConCaseExpr returnType idrisObjectVariableIndex args expr = do
            variableTypes <- getVariableTypes
            optTy <- coreLift $ Map.get variableTypes idrisObjectVariableIndex
            let idrisObjectVariableType = fromMaybe IUnknown $ nullableToMaybe optTy
            bindArg idrisObjectVariableType variableTypes 0 args
            assembleExpr True returnType expr
        where
            bindArg : InferredType -> Map Int InferredType -> Int -> List Name -> Core ()
            bindArg _ _ _ [] = pure ()
            bindArg idrisObjectVariableType variableTypes index (var :: vars) = do
                let variableName = jvmSimpleName var
                when (used variableName expr) $ do
                    loadVar variableTypes idrisObjectVariableType idrisObjectType idrisObjectVariableIndex
                    iconst index
                    invokeMethod InvokeInterface idrisObjectClass "getProperty" "(I)Ljava/lang/Object;" True
                    variableIndex <- getVariableIndex variableName
                    storeVar inferredObjectType !(getVariableType variableName) variableIndex
                bindArg idrisObjectVariableType variableTypes (index + 1) vars

    assembleConstructorSwitch : {auto stateRef: Ref AsmState AsmState} -> InferredType -> FC -> Int -> List NamedConAlt -> Maybe NamedCExp -> Core ()
    assembleConstructorSwitch returnType fc idrisObjectVariableIndex alts def = do
            switchCases <- getCasesWithLabels alts
            let labels = fst <$> switchCases
            let switchCasesWithEndLabel = getSwitchCasesWithEndLabel switchCases labels
            let exprs = caseExpression <$> switchCases
            traverse_ createLabel labels
            defaultLabel <- createDefaultLabel
            lookupSwitch defaultLabel labels exprs
            traverse_ assembleExprConAlt switchCasesWithEndLabel
            maybe (assembleMissingDefault returnType fc defaultLabel) (assembleDefault defaultLabel) def
        where
            caseExpression : (String, Int, NamedConAlt) -> Int
            caseExpression (_, expr, _) = expr

            getCasesWithLabels : List NamedConAlt -> Core (List (String, Int, NamedConAlt))
            getCasesWithLabels alts = do
                caseExpressionsWithLabels <- traverse conAltIntExpr alts
                pure $ sortBy (comparing caseExpression) caseExpressionsWithLabels

            assembleDefault : String -> NamedCExp -> Core ()
            assembleDefault lblStart expr = withScope $ do
                scopeIndex <- getCurrentScopeIndex
                scope <- getScope scopeIndex
                let (lineNumberStart, lineNumberEnd) = lineNumbers scope
                labelStart lblStart
                addLineNumber lineNumberStart lblStart
                updateScopeStartLabel scopeIndex lblStart
                updateScopeEndLabel scopeIndex methodEndLabel
                assembleExpr True returnType expr

            assembleCaseWithScope : String -> String -> List Name -> NamedCExp -> Core ()
            assembleCaseWithScope lblStart lblEnd args expr = withScope $ do
                scopeIndex <- getCurrentScopeIndex
                scope <- getScope scopeIndex
                let (lineNumberStart, lineNumberEnd) = lineNumbers scope
                labelStart lblStart
                addLineNumber lineNumberStart lblStart
                updateScopeStartLabel scopeIndex lblStart
                updateScopeEndLabel scopeIndex lblEnd
                assembleConCaseExpr returnType idrisObjectVariableIndex args expr

            assembleExprConAlt : (String, Int, NamedConAlt, String) -> Core ()
            assembleExprConAlt (labelStart, _, (MkNConAlt _ _ _ args expr), labelEnd) =
                assembleCaseWithScope labelStart labelEnd args expr

    assembleStringConstructorSwitch : {auto stateRef: Ref AsmState AsmState} -> InferredType -> FC -> Int -> List NamedConAlt -> Maybe NamedCExp -> Core ()
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
        traverse_ createLabel labels
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
            hashCodeSwitchCases
        scope <- getScope !getCurrentScopeIndex
        let lineNumberStart = fst $ lineNumbers scope
        labelStart switchEndLabel
        addLineNumber lineNumberStart switchEndLabel
        assembleExpr False IInt (NmLocal fc $ UN $ Basic hashCodePositionVariableName)
        assembleConstructorSwitch returnType fc idrisObjectVariableIndex
            (hashPositionSwitchAlts hashPositionAndAlts) def
      where
        conAltHashCodeExpr : FC -> (Int, NamedConAlt) -> Core (Int, Int, NamedConAlt)
        conAltHashCodeExpr fc positionAndAlt@(position, MkNConAlt name _ _ _ _) =
            case hashCode (Str $ getIdrisConstructorClassName (jvmSimpleName name)) of
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
                  ldc $ StringConst $ getIdrisConstructorClassName (jvmSimpleName name)
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

    asmJavaLambda : {auto stateRef: Ref AsmState AsmState} -> FC -> InferredType -> NamedCExp -> NamedCExp -> NamedCExp -> Core ()
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
          invokeMethod InvokeStatic runtimeClass "unwrap" "(Ljava/lang/Object;)Ljava/lang/Object;" False
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

    jvmExtPrim : {auto stateRef: Ref AsmState AsmState} -> FC -> InferredType -> ExtPrim -> List NamedCExp -> Core ()
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
    jvmExtPrim _ returnType JvmStaticMethodCall [ret, NmPrimVal fc (Str fn), fargs, world] = do
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
    jvmExtPrim _ returnType SetInstanceField [ret, NmPrimVal fc (Str fn), fargs, world] = do
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
    jvmExtPrim _ returnType SetStaticField [ret, NmPrimVal fc (Str fn), fargs, world] = do
        (value :: []) <- getFArgs fargs
            | _ => asmCrash ("Setting a static field should have one argument for " ++ fn)
        fieldType <- tySpec (fst value)
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False fieldType (snd value)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#' && c /= '=') fnameWithDot
        field PutStatic cname fieldName (getJvmTypeDescriptor fieldType)
        aconstnull
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType GetInstanceField [ret, NmPrimVal fc (Str fn), fargs, world] = do
        (obj :: []) <- getFArgs fargs
            | _ => asmCrash ("Getting an instance field should have one argument for " ++ fn)
        fieldType <- tySpec ret
        let (cname, fnameWithDot) = break (== '.') fn
        assembleExpr False (iref cname []) (snd obj)
        let (_, fieldName) = break (\c => c /= '.' && c /= '#') fnameWithDot
        field GetField cname fieldName (getJvmTypeDescriptor fieldType)
        asmCast fieldType returnType
    jvmExtPrim _ returnType GetStaticField [ret, NmPrimVal fc (Str fn), fargs, world] = do
        fieldType <- tySpec ret
        let (cname, fnameWithDot) = break (== '.') fn
        let (_, fieldName) = break (\c => c /= '.' && c /= '#') fnameWithDot
        field GetStatic cname fieldName (getJvmTypeDescriptor fieldType)
        asmCast fieldType returnType
    jvmExtPrim _ returnType NewArray [_, size, val, world] = do
        assembleExpr False IInt size
        assembleExpr False IUnknown val
        invokeMethod InvokeStatic arraysClass "create" "(ILjava/lang/Object;)Ljava/util/ArrayList;" False
        asmCast arrayListType returnType
    jvmExtPrim _ returnType ArrayGet [_, arr, pos, world] = do
        assembleExpr False arrayListType arr
        assembleExpr False IInt pos
        invokeMethod InvokeVirtual arrayListClass "get" "(I)Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType ArraySet [_, arr, pos, val, world] = do
        assembleExpr False arrayListType arr
        assembleExpr False IInt pos
        assembleExpr False IUnknown val
        invokeMethod InvokeVirtual arrayListClass "set" "(ILjava/lang/Object;)Ljava/lang/Object;" False
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
        aconstnull
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
        arraylength
        asmCast IInt returnType
    jvmExtPrim _ returnType NewIORef [_, val, world] = do
        new refClass
        dup
        assembleExpr False IUnknown val
        invokeMethod InvokeSpecial refClass "<init>" "(Ljava/lang/Object;)V" False
        asmCast refType returnType
    jvmExtPrim _ returnType ReadIORef [_, ref, world] = do
        assembleExpr False refType ref
        invokeMethod InvokeVirtual refClass "getValue" "()Ljava/lang/Object;" False
        asmCast inferredObjectType returnType
    jvmExtPrim _ returnType WriteIORef [_, ref, val, world] = do
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

assembleDefinition : {auto stateRef: Ref AsmState AsmState} -> Name -> FC -> Core ()
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

createMainMethod : {auto stateRef: Ref AsmState AsmState} -> String -> Jname -> Core ()
createMainMethod programName mainFunctionName = do
    function <- getFunction mainFunctionName
    let idrisMainClassMethodName = jvmClassMethodName function
    let mainClassName = className idrisMainClassMethodName
    createMethod [Public, Static] "Main.idr" mainClassName "main" "([Ljava/lang/String;)V" Nothing Nothing [] []
    methodCodeStart
    ldc $ StringConst programName
    aload 0
    invokeMethod InvokeStatic runtimeClass "setProgramArgs" "(Ljava/lang/String;[Ljava/lang/String;)V" False
    field GetStatic mainClassName (methodName idrisMainClassMethodName) "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
    invokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate"
        "()Ljava/lang/Object;" False
    return
    maxStackAndLocal (-1) (-1)
    methodCodeEnd

assemble : AsmGlobalState -> Map String (FC, NamedDef) -> Name -> IO ()
assemble globalState fcAndDefinitionsByName name = do
    fcDef <- Map.get {value=(FC, NamedDef)} fcAndDefinitionsByName (jvmSimpleName name)
    case nullableToMaybe fcDef of
        Just (fc, def) => do
            programName <- AsmGlobalState.getProgramName globalState
            asmState <- createAsmState globalState name
            ignore $ runAsm asmState $ \stateRef => do
                inferDef programName name fc def
                assembleDefinition name fc
                scopes <- coreLift $ ArrayList.new {elemTy=Scope}
                updateCurrentFunction $ { scopes := (subtyping scopes), optimizedBody := emptyFunction }
        Nothing => pure ()

getNameStrFcDef : (Name, FC, NamedDef) -> (String, FC, NamedDef)
getNameStrFcDef (name, fc, def) = (jvmSimpleName name, fc, def)

getNameStrDef : (String, FC, NamedDef) -> (String, NamedDef)
getNameStrDef (name, fc, def) = (name, def)

isForeignDef : (Name, FC, NamedDef) -> Bool
isForeignDef (_, _, MkNmForeign _ _ _) = True
isForeignDef _ = False

exportConstructor : {auto stateRef: Ref AsmState AsmState} -> SortedMap Namespace (List String) -> Map Int InferredType
                  -> InferredType -> Int -> Jname -> Name -> InferredFunctionType -> Core ()
exportConstructor typeExports jvmArgumentTypesByIndex jvmReturnType arity jvmIdrisName idrisName idrisFunctionType = do
  function <- getCurrentFunction
  initializeFunctionState
  let optimizedExpr = optimizedBody function
  let internalJname = function.idrisName
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
    let qualifiedJvmIdrisName = getIdrisFunctionName programName (className jvmIdrisName) (methodName jvmIdrisName)
    invokeMethod InvokeStatic
      (className qualifiedJvmIdrisName) (methodName qualifiedJvmIdrisName) idrisMethodDescriptor False
    invokeMethod InvokeStatic (programName ++ "/PrimIO") "unsafePerformIO" "(Ljava/lang/Object;)Ljava/lang/Object;" False
    asmCast (returnType idrisFunctionType) jvmReturnType
    asmReturn jvmReturnType
    labelStart methodEndLabel
  maxStackAndLocal (-1) (-1)
  methodCodeEnd

exportFunction : {auto stateRef: Ref AsmState AsmState} -> SortedMap Namespace (List String) -> MethodExport -> Core ()
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
    let exportedFieldName = jvmFunctionName
    let simpleIdrisName = dropAllNS idrisName
    let asmAnnotations = asmAnnotation <$> annotations
    let asmParameterAnnotations = (\annotations => asmAnnotation <$> annotations) <$> parameterAnnotations
    let descriptor = getMethodDescriptor functionType
    let signature = Just $ getMethodSignature functionType
    createMethod modifiers fileName jvmClassName jvmFunctionName descriptor signature Nothing asmAnnotations
      asmParameterAnnotations
    methodCodeStart
    (_, MkNmFun idrisFunctionArgs _) <- getFcAndDefinition (jvmSimpleName idrisName)
      | _ => asmCrash ("Unknown idris function " ++ show idrisName)
    let idrisFunctionArity = length idrisFunctionArgs
    let idrisArgumentTypes = replicate idrisFunctionArity inferredObjectType
    let idrisFunctionType = MkInferredFunctionType inferredObjectType idrisArgumentTypes
    let jvmIdrisName = jvmName idrisName
    let isField = idrisFunctionArity == 0
    let isConstructor = jvmFunctionName == "<init>"
    if isConstructor
      then exportConstructor typeExports jvmArgumentTypesByIndex jvmReturnType arityInt jvmIdrisName idrisName idrisFunctionType
      else if not isField then do
        loadArguments typeExports jvmArgumentTypesByIndex idrisName arityInt (parameterTypes idrisFunctionType)
        let idrisMethodDescriptor = getMethodDescriptor idrisFunctionType
        programName <- getProgramName
        let qualifiedJvmIdrisName = getIdrisFunctionName programName (className jvmIdrisName)
                                      (methodName jvmIdrisName)
        invokeMethod InvokeStatic
          (className qualifiedJvmIdrisName) (methodName qualifiedJvmIdrisName) idrisMethodDescriptor False
        when shouldPerformIO $
          invokeMethod InvokeStatic (programName ++ "/PrimIO") "unsafePerformIO"
            "(Ljava/lang/Object;)Ljava/lang/Object;" False
        toJava idrisName typeExports jvmReturnType (returnType idrisFunctionType)
        asmReturn jvmReturnType
        maxStackAndLocal (-1) (-1)
        methodCodeEnd
      else do
        programName <- getProgramName
        let qualifiedJvmIdrisName = getIdrisFunctionName programName (className jvmIdrisName)
                                      (methodName jvmIdrisName)
        field GetStatic (className qualifiedJvmIdrisName) (methodName qualifiedJvmIdrisName)
            "Lio/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed;"
        invokeMethod InvokeVirtual "io/github/mmhelloworld/idrisjvm/runtime/MemoizedDelayed" "evaluate"
            "()Ljava/lang/Object;" False
        toJava idrisName typeExports jvmReturnType (returnType idrisFunctionType)
        asmReturn jvmReturnType
        maxStackAndLocal (-1) (-1)
        methodCodeEnd

generateAccessors : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor)
                  -> ClassExport -> (accessorCreator: FieldExport -> Core ()) -> Core ()
generateAccessors descriptorsByEncloser classExport accessorCreator = do
  let className = classExport.name
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  traverse_ accessorCreator fields

generateGetters : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateGetters descriptorsByEncloser classExport =
  generateAccessors descriptorsByEncloser classExport (createGetter classExport)

generateSetters : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateSetters descriptorsByEncloser classExport =
  generateAccessors descriptorsByEncloser classExport (createSetter classExport)

generateConstructor : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor)
                    -> ClassExport -> List FieldExport -> List Annotation -> List (List Annotation) -> Core ()
generateConstructor descriptorsByEncloser classExport fields annotations parameterAnnotations = do
  let fieldTypes = FieldExport.type <$> fields
  let descriptor = getMethodDescriptor $ MkInferredFunctionType IVoid fieldTypes
  let signature = Just $ getMethodSignature $ MkInferredFunctionType IVoid fieldTypes
  let classType = iref classExport.name []
  extendsTypeName <- getJvmReferenceTypeName classExport.extends
  let arity = the Int $ cast $ length fields
  jvmArgumentTypesByIndex <- coreLift $ Map.fromList $ zip [0 .. arity] (classType :: fieldTypes)
  let asmAnnotations = asmAnnotation <$> annotations
  let asmParameterAnnotations = (\annotations => asmAnnotation <$> annotations) <$> parameterAnnotations
  createMethod [Public] "generated.idr" classExport.name "<init>" descriptor signature Nothing asmAnnotations
    asmParameterAnnotations
  methodCodeStart
  createLabel methodStartLabel
  createLabel methodEndLabel
  labelStart methodStartLabel
  aload 0
  invokeMethod InvokeSpecial extendsTypeName "<init>" "()V" False
  assignFields jvmArgumentTypesByIndex fields
  return
  labelStart methodEndLabel
  localVariable "this" (getJvmTypeDescriptor classType) Nothing methodStartLabel methodEndLabel 0
  traverse_ (uncurry addLocalVariable) $ zip [1 .. arity] fields
  maxStackAndLocal (-1) (-1)
  methodCodeEnd
 where
  assignField : Map Int InferredType -> Int -> FieldExport -> Core ()
  assignField jvmArgumentTypesByIndex varIndex fieldExport = do
    let fieldType = fieldExport.type
    aload 0
    loadVar jvmArgumentTypesByIndex fieldType fieldType varIndex
    field PutField classExport.name fieldExport.name (getJvmTypeDescriptor fieldType)

  assignFields : Map Int InferredType -> List FieldExport -> Core ()
  assignFields jvmArgumentTypesByIndex fieldExports = do
    let arity = the Int $ cast $ length fieldExports
    let varIndexAndExports = zip [1 .. arity] fieldExports
    traverse_ (uncurry $ assignField jvmArgumentTypesByIndex) varIndexAndExports

  addLocalVariable : Int -> FieldExport -> Core ()
  addLocalVariable index field = do
    let fieldType = field.type
    localVariable field.name (getJvmTypeDescriptor fieldType) Nothing methodStartLabel methodEndLabel index

getMatchingAnnotationProperty : String -> List AnnotationProperty -> Maybe AnnotationValue
getMatchingAnnotationProperty name props = snd <$> find (\(currentName, value) => name == currentName) props

generateRequiredArgsConstructor : {auto stateRef: Ref AsmState AsmState}
                                -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport
                                -> List AnnotationProperty -> Core ()
generateRequiredArgsConstructor descriptorsByEncloser classExport props = do
  let allFields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  let requiredFields@(_ :: _) = filter isRequiredField allFields
        | [] => pure ()
  let annotations = getAnnotationValues $ fromMaybe (AnnArray []) $ getMatchingAnnotationProperty "annotations" props
  let parameterAnnotations = getParameterAnnotationValues $ fromMaybe (AnnArray []) $
                               getMatchingAnnotationProperty "parameterAnnotations" props
  generateConstructor descriptorsByEncloser classExport requiredFields annotations parameterAnnotations

generateAllArgsConstructor : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateAllArgsConstructor descriptorsByEncloser classExport = do
  let Just (MkAnnotation _ props) = findAllArgsConstructor classExport
        | _ => pure ()
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
  let excludedFields = getStringAnnotationValues $ snd $ fromMaybe ("exclude", AnnArray []) $
                        (find (\(name, value) => name == "exclude") props)
  let constructorFields = filter (\fieldExport => not $ elem fieldExport.name excludedFields) fields
  let annotations = getAnnotationValues $ fromMaybe (AnnArray []) $ getMatchingAnnotationProperty "annotations" props
  let parameterAnnotations = getParameterAnnotationValues $ fromMaybe (AnnArray []) $
                               getMatchingAnnotationProperty "parameterAnnotations" props
  generateConstructor descriptorsByEncloser classExport constructorFields annotations parameterAnnotations

generateNoArgsConstructor : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateNoArgsConstructor descriptorsByEncloser classExport = do
  let Just (MkAnnotation _ props) = findNoArgsConstructor classExport
        | _ => pure ()
  let annotations = getAnnotationValues $ snd $ fromMaybe ("annotations", AnnArray []) $
                        (find (\(name, value) => name == "annotations") props)
  createMethod [Public] "generated.idr" classExport.name "<init>" "()V" Nothing Nothing [] []
  methodCodeStart
  aload 0
  extendsTypeName <- getJvmReferenceTypeName classExport.extends
  invokeMethod InvokeSpecial extendsTypeName "<init>" "()V" False
  return
  maxStackAndLocal (-1) (-1)
  methodCodeEnd

generateHashCode : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateHashCode descriptorsByEncloser classExport = do
  let fields = filter (not . isTransientField) $ getFields $
                 fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
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
  loadField : Int -> FieldExport -> Core ()
  loadField index fieldExport = do
    dup
    iconst index
    aload 0
    let fieldType = fieldExport.type
    field GetField classExport.name fieldExport.name (getJvmTypeDescriptor fieldType)
    asmCast fieldType inferredObjectType
    aastore

generateEquals : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateEquals descriptorsByEncloser classExport = do
  let fields = filter (not . isTransientField) $ getFields $
                 fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
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
  equalsFields : String -> List FieldExport -> Core ()
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

generateToString : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateToString descriptorsByEncloser classExport = do
  let fields = getFields $ fromMaybe [] $ SortedMap.lookup classExport descriptorsByEncloser
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

  appendFields : String -> List FieldExport -> Core ()
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

generateDataClass : {auto stateRef: Ref AsmState AsmState} -> SortedMap ClassExport (List ExportDescriptor) -> ClassExport -> Core ()
generateDataClass descriptorsByEncloser classExport = do
  generateGetters descriptorsByEncloser classExport
  generateSetters descriptorsByEncloser classExport
  generateRequiredArgsConstructor descriptorsByEncloser classExport []
  generateHashCode descriptorsByEncloser classExport
  generateEquals descriptorsByEncloser classExport
  generateToString descriptorsByEncloser classExport

exportMemberIo : AsmGlobalState -> SortedMap Namespace (List String) ->
                   SortedMap ClassExport (List ExportDescriptor) -> ExportDescriptor -> IO ()
exportMemberIo globalState typeExports descriptorsByEncloser (MkMethodExportDescriptor desc) =
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
            ignore $ runAsm asmState $ \stateRef => do
              Just superCallExpr <- getSuperCallExpr expr
                | Nothing => asmCrash ("Constructor export for " ++ show idrisName ++ " should call 'super'")
              inferDef programName constructorIdrisName fc (MkNmFun args superCallExpr)
              resetScope
              loadFunction $ jvmName constructorIdrisName
              exportFunction typeExports desc
              scopes <- coreLift $ ArrayList.new {elemTy=Scope}
              updateCurrentFunction $ { scopes := (subtyping scopes), optimizedBody := emptyFunction }
        _ => pure ()
    else do
      asmState <- createAsmStateJavaName globalState desc.encloser.name
      ignore $ runAsm asmState $ \stateRef => exportFunction typeExports desc
exportMemberIo globalState typeExports descriptorsByEncloser (MkFieldExportDescriptor desc) = do
  asmState <- createAsmStateJavaName globalState desc.encloser.name
  ignore $ runAsm asmState $ \stateRef => exportField desc
exportMemberIo globalState _ descriptorsByEncloser (MkClassExportDescriptor classExport) = do
  asmState <- createAsmStateJavaName globalState classExport.name
  ignore $ runAsm asmState $ \stateRef => exportClass classExport
  let hasDataAnnotation = isJust (findClassAnnotation "Data" classExport)
  ignore $ runAsm asmState $ \stateRef => generateAllArgsConstructor descriptorsByEncloser classExport
  ignore $ runAsm asmState $ \stateRef => generateNoArgsConstructor descriptorsByEncloser classExport
  when (not hasDataAnnotation) $
    ignore $ runAsm asmState $ \stateRef =>
      generateRequiredArgsConstructor descriptorsByEncloser classExport
        (maybe [] getAnnotationProperties $ findRequiredArgsConstructor classExport)
  when hasDataAnnotation $ ignore $ runAsm asmState $ \stateRef => generateDataClass descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "Getter" classExport)) $
    ignore $ runAsm asmState $ \stateRef => generateGetters descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "Setter" classExport)) $
    ignore $ runAsm asmState $ \stateRef => generateSetters descriptorsByEncloser classExport
  when (not hasDataAnnotation && isJust (findClassAnnotation "EqualsAndHashCode" classExport)) $ do
      ignore $ runAsm asmState $ \stateRef => generateEquals descriptorsByEncloser classExport
      ignore $ runAsm asmState $ \stateRef => generateHashCode descriptorsByEncloser classExport
exportMemberIo _ _ _ _ = pure ()

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

exportTypeIo : AsmGlobalState -> String -> IO ()
exportTypeIo globalState name = do
  asmState <- createAsmStateJavaName globalState name
  ignore $ runAsm asmState $ \stateRef => exportType name

exportTypes : AsmGlobalState -> SortedMap Namespace (List String) -> IO ()
exportTypes globalState typeExports = traverse_ (exportTypeIo globalState) $ concat $ values typeExports

exportDefs : AsmGlobalState -> List (Name, String) -> IO ()
exportDefs globalState nameAndDescriptors = do
  (typeExports, descriptors) <- parseExportDescriptors globalState nameAndDescriptors
  let descriptorsByEncloser = groupByEncloser descriptors
  exportTypes globalState typeExports
  traverse_ (exportMemberIo globalState typeExports descriptorsByEncloser) descriptors

export
getExport : NoMangleMap -> Name -> Maybe (Name, String)
getExport noMangleMap name = (\descriptor => (name, descriptor)) <$> isNoMangle noMangleMap name

showType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> Name -> Core ()
showType name = do
  Just ty <- getType name
    | Nothing => pure ()
  coreLift $ printLn $ show name ++ ": " ++ show ty

||| Compile a TT expression to JVM bytecode
compileToJvmBytecode : {auto c : Ref Ctxt Defs}
                     -> {auto s : Ref Syn SyntaxInfo} -> String -> String -> ClosedTerm -> Core ()
compileToJvmBytecode outputDirectory outputFile term = do
    noMangleMapRef <- initNoMangle ["jvm"] (const True)
    noMangleMap <- get NoMangleMap
    cdata <- getCompileDataWith ["jvm"] False Cases term
    directives <- getDirectives Jvm
    let ndefs = namedDefs cdata
    let idrisMainBody = forget (mainExpr cdata)
    let programName = if outputFile == "" then "repl" else outputFile
    let mainFunctionName = idrisMainFunctionName programName
    let allDefs = (mainFunctionName, emptyFC, MkNmFun [] idrisMainBody) :: ndefs
    -- traverse_ (showType . fst) ndefs
    let nameFcDefs = optimize programName allDefs ++ filter isForeignDef allDefs
    let nameStrFcDefs = getNameStrFcDef <$> nameFcDefs
    fcAndDefinitionsByName <- coreLift $ Map.fromList nameStrFcDefs
    let nameStrDefs = getNameStrDef <$> nameStrFcDefs
    definitionsByName <- coreLift $ Map.fromList nameStrDefs
    globalState <- coreLift $ newAsmGlobalState programName fcAndDefinitionsByName
    let names = fst <$> nameFcDefs
    coreLift $ do
        traverse_ (assemble globalState fcAndDefinitionsByName) names
        exportDefs globalState $ mapMaybe (getExport noMangleMap) (fst <$> allDefs)
        mainAsmState <- createAsmState globalState mainFunctionName
        let mainFunctionJname = jvmName mainFunctionName
        _ <- runAsm mainAsmState $ \stateRef => createMainMethod programName mainFunctionJname
        classCodeEnd globalState outputDirectory outputFile (className mainFunctionJname)

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
