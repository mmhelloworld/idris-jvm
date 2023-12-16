module Compiler.Jvm.Foreign

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.Reflect
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

getArity : Nat -> CFType -> Nat
getArity arity (CFFun argument _) = getArity (arity + 1) argument
getArity arity _ = arity

export
parse : FC -> CFType -> Asm InferredType
parse _ CFUnit = Pure IVoid
parse _ CFInt = Pure IInt
parse _ CFInt8 = Pure IByte
parse _ CFInt16 = Pure IShort
parse _ CFInt32 = Pure IInt
parse _ CFInt64 = Pure ILong
parse _ CFUnsigned8 = Pure IInt
parse _ CFUnsigned16 = Pure IInt
parse _ CFUnsigned32 = Pure IInt
parse _ CFUnsigned64 = Pure ILong
parse _ CFString = Pure inferredStringType
parse _ CFDouble = Pure IDouble
parse _ CFInteger = Pure inferredBigIntegerType
parse _ CFChar = Pure IChar
parse _ CFWorld = Pure IInt
parse fc (CFIORes returnType) = parse fc returnType
parse fc (CFStruct name fields) = Pure $ iref name []
parse fc (CFFun argument _) = Pure $ getFunctionInterface (getArity 1 argument)
parse fc (CFUser name (ty :: _)) =
  if name == builtin "Pair" then
    case ty of
      CFStruct name _ =>
        case words name of
          [] => asmCrash ("Invalid Java lambda type at " ++ show fc)
          (javaInterfaceName :: _) => Pure $ IRef javaInterfaceName Interface []
      _ => Pure inferredObjectType
  else if name == arrayName then Pure $ IArray !(parse fc ty)
  else Pure inferredObjectType
parse _ ty = Pure inferredObjectType

export
parseForeignFunctionDescriptor : FC -> List String -> List InferredType -> InferredType ->
  Asm (String, String, InferredType, List InferredType)
parseForeignFunctionDescriptor fc (functionDescriptor :: descriptorParts) argumentTypes returnType =
    case String.break (== '(') functionDescriptor of
        (fn, "") => do
            className <- getClassName fn descriptorParts returnType argumentTypes
            Pure (className, fn, returnType, argumentTypes)
        (fn, signature) => do
            let descriptors =
              toList $ String.split (== ' ') (assert_total $ strTail . fst $ break (== ')') signature)
            (argumentDeclarationTypesReversed, returnType) <- go [] descriptors
            let argumentDeclarationTypes = List.reverse argumentDeclarationTypesReversed
            className <- getClassName fn descriptorParts returnType argumentDeclarationTypes
            Pure (className, fn, returnType, argumentDeclarationTypes)
  where

    getInstanceMemberClass : (errorMessage: Lazy String) -> List InferredType -> Asm String
    getInstanceMemberClass errorMessage ((IRef className _ _) :: _) = Pure className
    getInstanceMemberClass errorMessage _ = Throw fc errorMessage

    getClassName : String -> List String -> InferredType -> List InferredType -> Asm String
    getClassName memberName descriptorParts returnType argumentTypes =
      let arity = length argumentTypes
      in
        if startsWith memberName "." then
            getInstanceMemberClass
              ("Instance method " ++ memberName ++ " must have first argument to be of reference type")
              argumentTypes
        else if startsWith memberName "#=" && arity >= 2 then
          getInstanceMemberClass
            ("Setter for instance field " ++ memberName ++ " must have first argument to be of reference type")
            argumentTypes
        else if startsWith memberName "#" && arity >= 1 then
          getInstanceMemberClass
            ("Getter for instance field " ++ memberName ++ " must have first argument to be of reference type")
            argumentTypes
        else
          if memberName == "<init>"
            then
              case returnType of
                IRef className _ _ => Pure className
                _ => Throw fc ("Constructor must return a reference type")
            else
              case descriptorParts of
                (className :: _) => Pure className
                _ => Throw fc
                       ("Static member " ++ memberName ++ " must have an explicit class name in foreign descriptor")


    go : List InferredType -> List String -> Asm (List InferredType, InferredType)
    go acc [] = Pure (acc, IUnknown)
    go acc (returnTypeDesc :: []) = Pure (acc, parse returnTypeDesc)
    go acc (argument :: rest) = do
        let foreignType = parse argument
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
      (startsWith name "#=", if arity >= 2 then "prim__setInstanceField" else "prim__setStaticField"),
      (startsWith name "#", if arity >= 1 then "prim__getInstanceField" else "prim__getStaticField")
    ]
    "prim__jvmStatic"

isValidArgumentType : CFType -> Bool
isValidArgumentType (CFUser (UN (Basic "Type")) _) = False
isValidArgumentType _ = True

getIdrisJvmParameters : FC -> List CFType -> Asm (List (Nat, Bool, InferredType))
getIdrisJvmParameters fc idrisTypes = pure $ reverse !(go [] 0 idrisTypes) where
  go : List (Nat, Bool, InferredType) -> Nat -> List CFType ->
         Asm (List (Nat, Bool, InferredType))
  go acc _ [] = pure acc
  go acc index (idrisType :: rest) = do
    jvmType <- parse fc idrisType
    let isValid = isValidArgumentType idrisType
    go ((index, isValid, jvmType) :: acc) (index + 1) rest

getJvmType : (Nat, Bool, InferredType) -> InferredType
getJvmType (_, _, jvmType) = jvmType

shouldPassToForeign : (CFType, Nat, Bool, InferredType) -> Bool
shouldPassToForeign (_, _, shouldPass, _) = shouldPass

getArgumentNameAndTypes : FC -> List InferredType -> List (Nat, Bool, InferredType) ->
                          Asm (List (String, InferredType))
getArgumentNameAndTypes fc descriptorTypes params = reverse <$> go [] descriptorTypes params where
  go : List (String, InferredType) -> List InferredType -> List (Nat, Bool, InferredType) ->
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
    jvmReturnType <- parse fc returnType
    (foreignFunctionClassName, foreignFunctionName, jvmReturnType, jvmArgumentTypesFromDescriptor) <-
        parseForeignFunctionDescriptor fc jvmDescriptor jvmArgumentTypes jvmReturnType

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
