module Compiler.Jvm.Foreign

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.TT

import Libraries.Data.SortedMap
import Data.List
import Data.List1
import Data.Vect
import Data.Strings

import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.Asm
import Compiler.Jvm.ExtPrim
import Compiler.Jvm.ShowUtil

namespace ForeignType
    public export
    data ForeignType
        = AtomicForeignType InferredType
        | FunctionForeignType
            String -- (interfaceName: String)
            String -- (methodName: String)
            InferredFunctionType -- (interfaceMethodType: InferredFunctionType)
            InferredFunctionType -- (implementationMethodType: InferredFunctionType)

    public export
    getInferredType : ForeignType -> InferredType
    getInferredType (FunctionForeignType interfaceName _ _ _) = IRef interfaceName
    getInferredType (AtomicForeignType ty) = ty

export
Show ForeignType where
    show (AtomicForeignType ty) = show ty
    show (FunctionForeignType interfaceName methodName interfaceMethodType implementationMethodType) =
        interfaceName ++ "." ++ methodName ++ "#" ++ show interfaceMethodType ++ "#" ++ show implementationMethodType

namespace ForeignImplementationType
    public export
    data ForeignImplementationType
        = AtomicForeignImplementationType InferredType
        | FunctionForeignImplementationType InferredFunctionType

    export
    getInferredType : FC -> ForeignImplementationType -> Asm InferredType
    getInferredType fc (AtomicForeignImplementationType ty) = Pure ty
    getInferredType fc _ = Throw fc "Callback functions cannot be higher order"

    mutual
        parseCallbackType : FC -> List CFType -> CFType -> Asm ForeignImplementationType
        parseCallbackType fc arguments (CFFun CFWorld returnType) = parseCallbackType fc arguments returnType
        parseCallbackType fc arguments (CFFun nextArgument returnType) =
            parseCallbackType fc (nextArgument :: arguments) returnType
        parseCallbackType fc arguments returnType = do
                argumentForeignTypes <- traverse parseInferredType (List.reverse arguments)
                returnForeignType <- parseInferredType returnType
                Pure $ FunctionForeignImplementationType (MkInferredFunctionType returnForeignType argumentForeignTypes)
            where
                parseInferredType : CFType -> Asm InferredType
                parseInferredType ty = do
                    foreignType <- parse fc ty
                    inferredType <- getInferredType fc foreignType
                    Pure inferredType

        export
        parse : FC -> CFType -> Asm ForeignImplementationType
        parse _ CFUnit = Pure $ AtomicForeignImplementationType IVoid
        parse _ CFInt = Pure $ AtomicForeignImplementationType IInt
        parse _ CFUnsigned8 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFUnsigned16 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFUnsigned32 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFUnsigned64 = Pure $ AtomicForeignImplementationType ILong
        parse _ CFString = Pure $ AtomicForeignImplementationType inferredStringType
        parse _ CFDouble = Pure $ AtomicForeignImplementationType IDouble
        parse _ CFChar = Pure $ AtomicForeignImplementationType IChar
        parse _ CFWorld = Pure $ AtomicForeignImplementationType IInt
        parse fc (CFIORes returnType) = parse fc returnType
        parse fc (CFFun argument returnType) = parseCallbackType fc [argument] returnType
        parse _ _ = Pure $ AtomicForeignImplementationType inferredObjectType

export
Show ForeignImplementationType where
    show (AtomicForeignImplementationType ty) = show ty
    show (FunctionForeignImplementationType functionType) = show functionType

export
throwExplicitFunctionDescriptorRequired : FC -> Asm a
throwExplicitFunctionDescriptorRequired fc = Throw fc ("Explicit function descriptor must be provided while " ++
                                                        "passing idris functions to JVM functions")

export
parseForeignCallbackDeclarationType : FC -> (descriptorParts : List String) -> Asm InferredFunctionType
parseForeignCallbackDeclarationType fc [] = throwExplicitFunctionDescriptorRequired fc
parseForeignCallbackDeclarationType _ [returnDescriptor] = Pure $ MkInferredFunctionType (parse returnDescriptor) []
parseForeignCallbackDeclarationType _ (arg :: next) =
    let (argumentTypesReversed, returnType) = go [] arg next
    in Pure $ MkInferredFunctionType returnType (List.reverse argumentTypesReversed)
  where
    go : List InferredType -> String -> List String -> (List InferredType, InferredType)
    go acc descriptor (nextArgument :: rest) = go (parse descriptor :: acc) nextArgument rest
    go acc descriptor [] = (acc, parse descriptor)

export
getForeignCallbackDeclarationType : FC -> ForeignImplementationType -> Asm ForeignType
getForeignCallbackDeclarationType fc (AtomicForeignImplementationType ty) = Pure $ AtomicForeignType ty
getForeignCallbackDeclarationType fc _ = throwExplicitFunctionDescriptorRequired fc

{-
 - Callbacks are represented as JVM functional interface types. For example, a foreign descriptor
 - might have a callback like "jvm:foo:String java/util/function/ToIntFunction#applyAsInt#Object#Int Int".
 - This descriptor provides the underlying functional interface method type for the second argument with the
 - interface name, interface abstract method name, input and output types, all separated by "#".
 -}
export
parseForeignType : FC -> String -> ForeignImplementationType -> Asm ForeignType
parseForeignType fc descriptor implementationType = case toList $ Strings.split (== '#') descriptor of
    [] => Throw fc $ "Invalid descriptor: " ++ descriptor
    (interfaceName :: interfaceMethodName :: signatureParts) =>
        case implementationType of
            AtomicForeignImplementationType _ => Throw fc ("Cannot pass non function argument as a JVM function")
            FunctionForeignImplementationType implementationType => do
                declarationType <- parseForeignCallbackDeclarationType fc signatureParts
                Pure $ FunctionForeignType interfaceName interfaceMethodName declarationType implementationType
    [_] => case implementationType of
        FunctionForeignImplementationType _ => throwExplicitFunctionDescriptorRequired fc
        _ => Pure $ AtomicForeignType $ parse descriptor

export
parseForeignFunctionDescriptor : FC -> List String -> List ForeignImplementationType ->
    InferredType -> Asm (String, String, InferredType, List ForeignType)
parseForeignFunctionDescriptor fc (functionDescriptor :: className :: _) argumentTypes returnType =
    case Strings.break (== '(') functionDescriptor of
        (fn, "") => do
            argumentDeclarationTypes <- traverse (getForeignCallbackDeclarationType fc) argumentTypes
            Pure (className, fn, returnType, argumentDeclarationTypes)
        (fn, signature) => do
            let descriptorsWithIdrisTypes =
                List.zip
                    (toList $ Strings.split (== ' ') (assert_total $ strTail . fst $ break (== ')') signature))
                    (argumentTypes ++ [AtomicForeignImplementationType returnType])
            (argumentTypesReversed, returnType) <- go [] descriptorsWithIdrisTypes
            Pure (className, fn, returnType, List.reverse argumentTypesReversed)
  where
    go : List ForeignType -> List (String, ForeignImplementationType) -> Asm (List ForeignType, InferredType)
    go acc [] = Pure (acc, IUnknown)
    go acc ((returnTypeDesc, _) :: []) = Pure (acc, parse returnTypeDesc)
    go acc ((argument, ty) :: rest) = do
        foreignType <- parseForeignType fc argument ty
        go (foreignType :: acc) rest
parseForeignFunctionDescriptor fc descriptors _ _ = Throw fc $ "Invalid foreign descriptor: " ++ show descriptors

export
findJvmDescriptor : FC -> Name -> List String -> Asm (List String)
findJvmDescriptor fc name descriptors = case parseCC ["jvm"] descriptors of
    Just ("jvm", descriptorParts) => Pure descriptorParts
    _ => Throw fc $ "Cannot compile foreign function " ++ show name ++ " to JVM as JVM foreign descriptor is missing"

export
getArgumentIndices : (arity: Int) -> List String -> IO (Map String Int)
getArgumentIndices 0 _ = Map.newTreeMap {key=String} {value=Int}
getArgumentIndices argIndex args = Map.fromList $ List.zip args [0 .. argIndex - 1]

getPrimMethodName : String -> String
getPrimMethodName "" = "prim__jvmStatic"
getPrimMethodName name =
    assert_total $ if prim__strHead name == '.' then "prim__jvmInstance" else "prim__jvmStatic"

export
inferForeign : String -> Name -> FC -> List String -> List CFType -> CFType -> Asm ()
inferForeign programName idrisName fc foreignDescriptors argumentTypes returnType = do
    resetScope
    let jname = jvmName idrisName
    let jvmClassAndMethodName = getIdrisFunctionName programName (className jname) (methodName jname)
    jvmArgumentTypes <- traverse (parse fc) argumentTypes
    let arityNat = length jvmArgumentTypes
    let isNilArity = arityNat == 0
    jvmDescriptor <- findJvmDescriptor fc idrisName foreignDescriptors
    jvmReturnType <- getInferredType fc !(parse fc returnType)
    (foreignFunctionClassName, foreignFunctionName, jvmReturnType, jvmArgumentTypes) <-
        parseForeignFunctionDescriptor fc jvmDescriptor jvmArgumentTypes jvmReturnType
    let jvmArgumentTypes = getInferredType <$> jvmArgumentTypes -- TODO: Do not discard Java lambda type descriptor
    scopeIndex <- newScopeIndex
    let arity = the Int $ cast arityNat
    let argumentNames =
       if isNilArity then [] else (\argumentIndex => "arg" ++ show argumentIndex) <$> [0 .. arity - 1]
    let argumentTypesByName = SortedMap.fromList $ List.zip argumentNames jvmArgumentTypes
    let methodReturnType = if isNilArity then delayedType else inferredObjectType
    let inferredFunctionType = MkInferredFunctionType methodReturnType (replicate arityNat inferredObjectType)
    scopes <- LiftIo $ JList.new {a=Scope}
    let externalFunctionBody =
        NmExtPrim fc (NS (mkNamespace "") $ UN $ getPrimMethodName foreignFunctionName) [
           NmCon fc (UN $ createExtPrimTypeSpec jvmReturnType) DATACON Nothing [],
           NmPrimVal fc (Str $ foreignFunctionClassName ++ "." ++ foreignFunctionName),
           getJvmExtPrimArguments $ List.zip argumentTypes $ SortedMap.toList argumentTypesByName,
           NmPrimVal fc WorldVal]
    let functionBody = if isNilArity then NmDelay fc LLazy externalFunctionBody else externalFunctionBody
    let function = MkFunction jname inferredFunctionType scopes 0 jvmClassAndMethodName functionBody
    setCurrentFunction function
    LiftIo $ AsmGlobalState.addFunction !getGlobalState jname function
    let parameterTypes = parameterTypes inferredFunctionType
    argumentTypesByIndex <- LiftIo $
        if isNilArity
            then Map.newTreeMap {key=Int} {value=InferredType}
            else Map.fromList $ List.zip [0 .. arity - 1] parameterTypes
    argumentTypesByName <- LiftIo $ Map.fromList $ List.zip argumentNames parameterTypes
    argIndices <- LiftIo $ getArgumentIndices arity argumentNames
    let functionScope = MkScope scopeIndex Nothing argumentTypesByName argumentTypesByIndex argIndices argIndices
                            methodReturnType arity (0, 0) ("", "") []
    saveScope functionScope
    when isNilArity $ do
        let parentScopeIndex = scopeIndex
        scopeIndex <- newScopeIndex
        variableTypes <- LiftIo $ Map.newTreeMap {key=String} {value=InferredType}
        allVariableTypes <- LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
        variableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
        allVariableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
        let delayLambdaScope =
            MkScope scopeIndex (Just parentScopeIndex) variableTypes allVariableTypes
                variableIndices allVariableIndices IUnknown 0 (0, 0) ("", "") []
        saveScope delayLambdaScope
    updateScopeVariableTypes arityNat
  where
    getJvmExtPrimArguments : List (CFType, String, InferredType) -> NamedCExp
    getJvmExtPrimArguments [] = NmCon fc (UN "emptyForeignArg") DATACON (Just 0) []
    getJvmExtPrimArguments ((CFWorld, _, _) :: rest) = getJvmExtPrimArguments rest
    getJvmExtPrimArguments ((_, name, ty) :: rest) = NmCon fc (UN "foreignArg") DATACON (Just 1) [
        NmCon fc (UN $ createExtPrimTypeSpec ty) DATACON (Just 0) [],
        NmLocal fc (UN name),
        getJvmExtPrimArguments rest ]
