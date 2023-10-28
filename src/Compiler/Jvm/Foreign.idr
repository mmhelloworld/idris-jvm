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
import Data.String
import Debug.Trace
import Data.Zippable

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
    getInferredType (FunctionForeignType interfaceName _ _ _) = IRef interfaceName Interface
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
    getInferredType fc (FunctionForeignImplementationType ty) = Pure $ getFunctionInterface (length $ parameterTypes ty)

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
        parse _ CFInt8 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFInt16 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFInt32 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFInt64 = Pure $ AtomicForeignImplementationType ILong
        parse _ CFUnsigned8 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFUnsigned16 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFUnsigned32 = Pure $ AtomicForeignImplementationType IInt
        parse _ CFUnsigned64 = Pure $ AtomicForeignImplementationType ILong
        parse _ CFString = Pure $ AtomicForeignImplementationType inferredStringType
        parse _ CFDouble = Pure $ AtomicForeignImplementationType IDouble
        parse _ CFInteger = Pure $ AtomicForeignImplementationType inferredBigIntegerType
        parse _ CFChar = Pure $ AtomicForeignImplementationType IChar
        parse _ CFWorld = Pure $ AtomicForeignImplementationType IInt
        parse fc (CFIORes returnType) = parse fc returnType
        parse fc (CFStruct name fields) = Pure $ AtomicForeignImplementationType $ iref name
        parse fc (CFFun argument returnType) = parseCallbackType fc [argument] returnType
        parse fc (CFUser _ (CFStruct name _ :: _)) = case words name of
          [] => asmCrash ("Invalid Java lambda type at " ++ show fc)
          (javaInterfaceName :: _) =>
            Pure $ AtomicForeignImplementationType $ IRef javaInterfaceName Interface
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
parseForeignType fc descriptor implementationType = case toList $ String.split (== '#') descriptor of
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
parseForeignFunctionDescriptor fc (functionDescriptor :: descriptorParts) argumentTypes returnType =
    case String.break (== '(') functionDescriptor of
        (fn, "") => do
            argumentDeclarationTypes <- traverse (getForeignCallbackDeclarationType fc) argumentTypes
            className <- getClassName fn descriptorParts returnType argumentDeclarationTypes
            Pure (className, fn, returnType, argumentDeclarationTypes)
        (fn, signature) => do
            let descriptorsWithIdrisTypes =
                zip
                    (toList $ String.split (== ' ') (assert_total $ strTail . fst $ break (== ')') signature))
                    (argumentTypes ++ [AtomicForeignImplementationType returnType])
            (argumentDeclarationTypesReversed, returnType) <- go [] descriptorsWithIdrisTypes
            let argumentDeclarationTypes = List.reverse argumentDeclarationTypesReversed
            className <- getClassName fn descriptorParts returnType argumentDeclarationTypes
            Pure (className, fn, returnType, argumentDeclarationTypes)
  where

    getInstanceMemberClass : (errorMessage: Lazy String) -> List ForeignType -> Asm String
    getInstanceMemberClass errorMessage (AtomicForeignType (IRef className _) :: _) = Pure className
    getInstanceMemberClass errorMessage _ = Throw fc errorMessage

    getClassName : String -> List String -> InferredType -> List ForeignType -> Asm String
    getClassName memberName descriptorParts returnType argumentTypes =
      let arity = length argumentTypes
      in
        if startsWith memberName "." then
            getInstanceMemberClass
              ("Instance method " ++ memberName ++ " must have first argument to be of reference type")
              argumentTypes
        else if startsWith memberName "#=" && arity == 2 then
          getInstanceMemberClass
            ("Setter for instance field " ++ memberName ++ " must have first argument to be of reference type")
            argumentTypes
        else if startsWith memberName "#" && arity == 1 then
          getInstanceMemberClass
            ("Getter for instance field " ++ memberName ++ " must have first argument to be of reference type")
            argumentTypes
        else
          if memberName == "<init>"
            then
              case returnType of
                IRef className _ => Pure className
                _ => Throw fc ("Constructor must return a reference type")
            else
              case descriptorParts of
                (className :: _) => Pure className
                _ => Throw fc
                       ("Static member " ++ memberName ++ " must have an explicit class name in foreign descriptor")


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
getArgumentIndices argIndex args = Map.fromList $ zip args [0 .. argIndex - 1]

getPrimMethodName : (arity : Nat) -> String -> String
getPrimMethodName arity name =
  cond
    [
      (startsWith name ".", "prim__jvmInstance"),
      (startsWith name "#=", if arity == 2 then "prim__setInstanceField" else "prim__setStaticField"),
      (startsWith name "#", if arity == 1 then "prim__getInstanceField" else "prim__getStaticField")
    ]
    "prim__jvmStatic"

isValidArgumentType : CFType -> Bool
isValidArgumentType (CFUser (UN (Basic "Type")) _) = False
isValidArgumentType _ = True

getIdrisJvmParameters : FC -> List CFType -> Asm (List (Nat, Bool, ForeignImplementationType))
getIdrisJvmParameters fc idrisTypes = pure $ reverse !(go [] 0 idrisTypes) where
  go : List (Nat, Bool, ForeignImplementationType) -> Nat -> List CFType ->
         Asm (List (Nat, Bool, ForeignImplementationType))
  go acc _ [] = pure acc
  go acc index (idrisType :: rest) = do
    jvmType <- parse fc idrisType
    let isValid = isValidArgumentType idrisType
    go ((index, isValid, jvmType) :: acc) (index + 1) rest

getJvmType : (Nat, Bool, ForeignImplementationType) -> ForeignImplementationType
getJvmType (_, _, jvmType) = jvmType

shouldPassToForeign : (CFType, Nat, Bool, ForeignImplementationType) -> Bool
shouldPassToForeign (_, _, shouldPass, _) = shouldPass

getArgumentNameAndTypes : FC -> List InferredType -> List (Nat, Bool, ForeignImplementationType) ->
                          Asm (List (String, InferredType))
getArgumentNameAndTypes fc descriptorTypes params = reverse <$> go [] descriptorTypes params where
  go : List (String, InferredType) -> List InferredType -> List (Nat, Bool, ForeignImplementationType) ->
        Asm (List (String, InferredType))
  go acc [] _ = pure acc -- Ignore any additional arguments from Idris
  go acc _ [] = Throw fc "Foreign descriptor and Idris types do not match"
  go acc (descriptorType :: descriptorTypes) ((index, _, _) :: rest) =
    go (("arg" ++ show index, descriptorType) :: acc) descriptorTypes rest

export
inferForeign : String -> Name -> FC -> List String -> List CFType -> CFType -> Asm ()
inferForeign programName idrisName fc foreignDescriptors argumentTypes returnType = do
    resetScope
    let jname = jvmName idrisName
    let jvmClassAndMethodName = getIdrisFunctionName programName (className jname) (methodName jname)
    idrisJvmParameters <- getIdrisJvmParameters fc argumentTypes
    let validIdrisTypes = map fst $ filter shouldPassToForeign $ zip argumentTypes idrisJvmParameters
    let idrisArgumentTypes = getJvmType <$> idrisJvmParameters
    let jvmArguments = filter (fst . snd) idrisJvmParameters
    let jvmArgumentTypes = getJvmType <$> jvmArguments
    let arityNat = length argumentTypes
    let isNilArity = arityNat == 0
    jvmDescriptor <- findJvmDescriptor fc idrisName foreignDescriptors
    jvmReturnType <- getInferredType fc !(parse fc returnType)
    (foreignFunctionClassName, foreignFunctionName, jvmReturnType, jvmArgumentForeignTypesFromDescriptor) <-
        parseForeignFunctionDescriptor fc jvmDescriptor jvmArgumentTypes jvmReturnType

    -- TODO: Do not discard Java lambda type descriptor
    let jvmArgumentTypesFromDescriptor = getInferredType <$> jvmArgumentForeignTypesFromDescriptor

    scopeIndex <- newScopeIndex
    let arity = the Int $ cast arityNat
    let argumentNames =
       if isNilArity then [] else (\argumentIndex => "arg" ++ show argumentIndex) <$> [0 .. arity - 1]
    argumentNameAndTypes <- getArgumentNameAndTypes fc jvmArgumentTypesFromDescriptor jvmArguments
    let methodReturnType = if isNilArity then delayedType else inferredObjectType
    let inferredFunctionType = MkInferredFunctionType methodReturnType (replicate arityNat inferredObjectType)
    scopes <- LiftIo $ ArrayList.new {elemTy=Scope}
    let extPrimName = NS (mkNamespace "") $ UN $ Basic $
      getPrimMethodName (length argumentNameAndTypes) foreignFunctionName
    let externalFunctionBody =
        NmExtPrim fc extPrimName [
           NmCon fc (UN $ Basic $ createExtPrimTypeSpec jvmReturnType) DATACON Nothing [],
           NmPrimVal fc (Str $ foreignFunctionClassName ++ "." ++ foreignFunctionName),
           getJvmExtPrimArguments $ zip validIdrisTypes argumentNameAndTypes,
           NmPrimVal fc WorldVal]
    let functionBody = if isNilArity then NmDelay fc LLazy externalFunctionBody else externalFunctionBody
    let function = MkFunction jname inferredFunctionType (subtyping scopes) 0 jvmClassAndMethodName functionBody
    setCurrentFunction function
    LiftIo $ AsmGlobalState.addFunction !getGlobalState jname function
    let parameterTypes = parameterTypes inferredFunctionType
    argumentTypesByIndex <- LiftIo $
        if isNilArity
            then Map.newTreeMap {key=Int} {value=InferredType}
            else Map.fromList $ zip [0 .. arity - 1] parameterTypes
    argumentTypesByName <- LiftIo $ Map.fromList $ zip argumentNames parameterTypes
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
    getJvmExtPrimArguments [] = NmCon fc (UN $ Basic "emptyForeignArg") DATACON (Just 0) []
    getJvmExtPrimArguments ((CFWorld, _, _) :: rest) = getJvmExtPrimArguments rest
    getJvmExtPrimArguments ((_, name, ty) :: rest) = NmCon fc (UN $ Basic "foreignArg") DATACON (Just 1) [
        NmCon fc (UN . Basic $ createExtPrimTypeSpec ty) DATACON (Just 0) [],
        NmLocal fc (UN $ Basic name),
        getJvmExtPrimArguments rest ]
