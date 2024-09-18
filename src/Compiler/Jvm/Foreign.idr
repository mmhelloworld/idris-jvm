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

%hide Core.Name.Scoped.Scope

getArity : Nat -> CFType -> Nat
getArity arity (CFFun argument _) = getArity (arity + 1) argument
getArity arity _ = arity

export
parse : {auto stateRef: Ref AsmState AsmState} -> FC -> CFType -> Core InferredType
parse _ CFUnit = pure IVoid
parse _ CFInt = pure IInt
parse _ CFInt8 = pure IByte
parse _ CFInt16 = pure IShort
parse _ CFInt32 = pure IInt
parse _ CFInt64 = pure ILong
parse _ CFUnsigned8 = pure IInt
parse _ CFUnsigned16 = pure IInt
parse _ CFUnsigned32 = pure IInt
parse _ CFUnsigned64 = pure ILong
parse _ CFString = pure inferredStringType
parse _ CFDouble = pure IDouble
parse _ CFInteger = pure inferredBigIntegerType
parse _ CFChar = pure IChar
parse _ CFWorld = pure IInt
parse fc (CFIORes returnType) = parse fc returnType
parse fc (CFStruct name fields) = pure $ iref name []
parse fc (CFFun argument _) = pure $ getFunctionInterface (getArity 1 argument)
parse fc (CFUser name (ty :: _)) =
  if name == builtin "Pair" then
    case ty of
      CFStruct name _ =>
        case words name of
          [] => asmCrash ("Invalid Java lambda type at " ++ show fc)
          (javaInterfaceName :: _) => pure $ IRef javaInterfaceName Interface []
      _ => pure inferredObjectType
  else if name == arrayName then pure $ IArray !(parse fc ty)
  else pure inferredObjectType
parse _ ty = pure inferredObjectType

export
parseForeignFunctionDescriptor : {auto stateRef: Ref AsmState AsmState} -> FC -> List String -> List InferredType -> InferredType -> Core (String, String, InferredType, List InferredType)
parseForeignFunctionDescriptor fc (functionDescriptor :: descriptorParts) argumentTypes returnType =
    case String.break (== '(') functionDescriptor of
        (fn, "") => do
            className <- getClassName fn descriptorParts returnType argumentTypes
            pure (className, fn, returnType, argumentTypes)
        (fn, signature) => do
            let descriptors =
              toList $ String.split (== ' ') (assert_total $ strTail . fst $ break (== ')') signature)
            (argumentDeclarationTypesReversed, returnType) <- go [] descriptors
            let argumentDeclarationTypes = List.reverse argumentDeclarationTypesReversed
            className <- getClassName fn descriptorParts returnType argumentDeclarationTypes
            pure (className, fn, returnType, argumentDeclarationTypes)
  where

    getInstanceMemberClass : (errorMessage: Lazy String) -> List InferredType -> Core String
    getInstanceMemberClass errorMessage ((IRef className _ _) :: _) = pure className
    getInstanceMemberClass errorMessage _ = throw $ GenericMsg fc errorMessage

    getDescriptorClassName : String -> Core String
    getDescriptorClassName memberName =
      case descriptorParts of
        (className :: _) => pure className
        _ => throw $ GenericMsg fc
               ("Static member " ++ memberName ++ " must have an explicit class name in foreign descriptor")

    getClassName : String -> List String -> InferredType -> List InferredType -> Core String
    getClassName memberName descriptorParts returnType argumentTypes =
      let arity = length argumentTypes
      in
        if startsWith memberName "." then
            getInstanceMemberClass
              ("Instance method " ++ memberName ++ " must have first argument to be of reference type")
              argumentTypes
        else if startsWith memberName "#=" then
          if arity >= 2 then
            getInstanceMemberClass
              ("Setter for instance field " ++ memberName ++ " must have first argument to be of reference type")
              argumentTypes
          else getDescriptorClassName memberName
        else if startsWith memberName "#" then
          if arity >= 1 then
            getInstanceMemberClass
              ("Getter for instance field " ++ memberName ++ " must have first argument to be of reference type")
              argumentTypes
          else getDescriptorClassName memberName
        else
          if memberName == "<init>"
            then
              case returnType of
                IRef className _ _ => pure className
                _ => throw $ GenericMsg fc ("Constructor must return a reference type")
            else getDescriptorClassName memberName

    go : List InferredType -> List String -> Core (List InferredType, InferredType)
    go acc [] = pure (acc, IUnknown)
    go acc (returnTypeDesc :: []) = pure (acc, parse returnTypeDesc)
    go acc (argument :: rest) = do
        let foreignType = parse argument
        go (foreignType :: acc) rest
parseForeignFunctionDescriptor fc descriptors _ _ =
    throw $ GenericMsg fc $ "Invalid foreign descriptor: " ++ show descriptors

export
findJvmDescriptor : {auto stateRef: Ref AsmState AsmState} -> FC -> Name -> List String -> Core (List String)
findJvmDescriptor fc name descriptors = case parseCC ["jvm"] descriptors of
    Just ("jvm", descriptorParts) => pure descriptorParts
    _ => throw $ GenericMsg fc $ "Cannot compile foreign function " ++ show name ++
            " to JVM as JVM foreign descriptor is missing"

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

getIdrisJvmParameters : {auto stateRef: Ref AsmState AsmState} -> FC -> List CFType -> Core (List (Nat, Bool, InferredType))
getIdrisJvmParameters fc idrisTypes = pure $ reverse !(go [] 0 idrisTypes) where
  go : List (Nat, Bool, InferredType) -> Nat -> List CFType -> Core (List (Nat, Bool, InferredType))
  go acc _ [] = pure acc
  go acc index (idrisType :: rest) = do
    jvmType <- parse fc idrisType
    let isValid = isValidArgumentType idrisType
    go ((index, isValid, jvmType) :: acc) (index + 1) rest

getJvmType : (Nat, Bool, InferredType) -> InferredType
getJvmType (_, _, jvmType) = jvmType

shouldPassToForeign : (CFType, Nat, Bool, InferredType) -> Bool
shouldPassToForeign (_, _, shouldPass, _) = shouldPass

getArgumentNameAndTypes : {auto stateRef: Ref AsmState AsmState} -> FC -> List InferredType
                        -> List (Nat, Bool, InferredType) -> Core (List (String, InferredType))
getArgumentNameAndTypes fc descriptorTypes params = reverse <$> go [] descriptorTypes params where
  go : List (String, InferredType) -> List InferredType -> List (Nat, Bool, InferredType)
     -> Core (List (String, InferredType))
  go acc [] _ = pure acc -- Ignore any additional arguments from Idris
  go acc _ [] = throw $ GenericMsg fc "Foreign descriptor and Idris types do not match"
  go acc (descriptorType :: descriptorTypes) ((index, _, _) :: rest) =
    go (("arg" ++ show index, descriptorType) :: acc) descriptorTypes rest

export
isForeign : NamedCExp -> Bool
isForeign (NmDelay _ LLazy expr) = isForeign expr
isForeign (NmExtPrim _ (NS ns (UN (Basic name))) _) =
  ns == mkNamespace "" &&
    (name `elem` the (List String) ["prim__jvmInstance", "prim__setInstanceField", "prim__setStaticField",
      "prim__getInstanceField", "prim__getStaticField", "prim__jvmStatic"])
isForeign _ = False

export
inferForeign : {auto stateRef: Ref AsmState AsmState} -> Name -> FC -> List String -> List CFType -> CFType -> Core ()
inferForeign idrisName fc foreignDescriptors argumentTypes returnType = do
    resetScope
    let jname = jvmName idrisName
    let jvmClassAndMethodName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
    idrisJvmParameters <- getIdrisJvmParameters fc argumentTypes
    let validIdrisTypes = map fst $ filter shouldPassToForeign $ zip argumentTypes idrisJvmParameters
    let idrisArgumentTypes = getJvmType <$> idrisJvmParameters
    let jvmArguments = filter (fst . snd) idrisJvmParameters
    let jvmArgumentTypes = getJvmType <$> jvmArguments
    jvmDescriptor <- findJvmDescriptor fc idrisName foreignDescriptors
    jvmReturnType <- parse fc returnType
    (foreignFunctionClassName, foreignFunctionName, jvmReturnType, jvmArgumentTypesFromDescriptor) <-
        parseForeignFunctionDescriptor fc jvmDescriptor jvmArgumentTypes jvmReturnType

    scopeIndex <- newScopeIndex
    argumentNameAndTypes <- getArgumentNameAndTypes fc jvmArgumentTypesFromDescriptor jvmArguments
    let arityNat = length idrisArgumentTypes
    let isNilArity = arityNat == 0
    let methodReturnType = if isNilArity then delayedType else jvmReturnType
    let argumentTypes = snd <$> argumentNameAndTypes

    scopes <- coreLift $ ArrayList.new {elemTy=Scope}
    let extPrimName = NS (mkNamespace "") $ UN $ Basic $
      getPrimMethodName (length argumentNameAndTypes) foreignFunctionName
    let externalFunctionBody =
        NmExtPrim fc extPrimName [
           NmCon fc (UN $ Basic $ createExtPrimTypeSpec jvmReturnType) DATACON Nothing [],
           NmPrimVal fc (Str $ foreignFunctionClassName ++ "." ++ foreignFunctionName),
           getJvmExtPrimArguments $ zip validIdrisTypes argumentNameAndTypes,
           NmPrimVal fc WorldVal]
    let functionBody = if isNilArity then NmDelay fc LLazy externalFunctionBody else externalFunctionBody
    let idrisReturnType = if methodReturnType == IVoid then IInt else methodReturnType
    let inferredFunctionType = MkInferredFunctionType idrisReturnType idrisArgumentTypes
    let function = MkFunction jname inferredFunctionType (subtyping scopes) 0 jvmClassAndMethodName functionBody
    setCurrentFunction function
    coreLift $ addFunction !getGlobalState jname function
    let arity = the Int $ cast arityNat
    let argumentNames =
      if isNilArity then [] else (\argumentIndex => "arg" ++ show argumentIndex) <$> [0 .. arity - 1]
    argumentTypesByIndex <- coreLift $
        if isNilArity
            then Map.newTreeMap {key=Int} {value=InferredType}
            else Map.fromList $ zip [0 .. arity - 1] idrisArgumentTypes
    argumentTypesByName <- coreLift $ Map.fromList $ zip argumentNames idrisArgumentTypes
    argIndices <- coreLift $ getArgumentIndices arity argumentNames
    let functionScope = MkScope scopeIndex Nothing argumentTypesByName argumentTypesByIndex argIndices argIndices
                            idrisReturnType arity (0, 0) ("", "") []
    saveScope functionScope
    when isNilArity $ do
        let parentScopeIndex = scopeIndex
        scopeIndex <- newScopeIndex
        variableTypes <- coreLift $ Map.newTreeMap {key=String} {value=InferredType}
        allVariableTypes <- coreLift $ Map.newTreeMap {key=Int} {value=InferredType}
        variableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
        allVariableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
        let delayLambdaScope =
            MkScope scopeIndex (Just parentScopeIndex) variableTypes allVariableTypes
                variableIndices allVariableIndices IUnknown 0 (0, 0) ("", "") []
        saveScope delayLambdaScope
    updateScopeVariableTypes
  where
    getJvmExtPrimArguments : List (CFType, String, InferredType) -> NamedCExp
    getJvmExtPrimArguments [] = NmCon fc (UN $ Basic "emptyForeignArg") DATACON (Just 0) []
    getJvmExtPrimArguments ((CFWorld, _, _) :: rest) = getJvmExtPrimArguments rest
    getJvmExtPrimArguments ((_, name, ty) :: rest) = NmCon fc (UN $ Basic "foreignArg") DATACON (Just 1) [
        NmCon fc (UN . Basic $ createExtPrimTypeSpec ty) DATACON (Just 0) [],
        NmLocal fc (UN $ Basic name),
        getJvmExtPrimArguments rest ]
