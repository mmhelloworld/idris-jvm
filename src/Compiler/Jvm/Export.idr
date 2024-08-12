module Compiler.Jvm.Export

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.NoMangle
import Compiler.Jvm.Asm
import Compiler.Jvm.ExtPrim
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.Optimizer
import Compiler.Jvm.Variable
import Core.Context
import Core.Directory
import Core.Name
import Core.Name.Namespace
import Core.Options
import Core.TT
import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Data.Vect
import Debug.Trace
import Language.JSON
import Libraries.Data.SortedMap
import Libraries.Utils.Path

import System.FFI

mutual
  parseAnnotationTypeValue : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> String -> JSON -> Core AnnotationValue
  parseAnnotationTypeValue functionName annotationName "int" (JNumber value) = pure $ AnnInt $ cast value
  parseAnnotationTypeValue functionName annotationName "boolean" (JBoolean value) = pure $ AnnBoolean value
  parseAnnotationTypeValue functionName annotationName "char" (JString value) =
    pure $ AnnChar $ assert_total (prim__strHead value)
  parseAnnotationTypeValue functionName annotationName "double" (JNumber value) = pure $ AnnDouble value
  parseAnnotationTypeValue functionName annotationName "String" (JString value) = pure $ AnnString value
  parseAnnotationTypeValue functionName annotationName "class" (JString value) = pure $ AnnClass value
  parseAnnotationTypeValue functionName annotationName "enum" (JObject properties) = do
    let propertiesByName = SortedMap.fromList properties
    let Just (JString type) = lookup "type" properties
      | _ => asmCrash ("Expected 'string' enum type for annotation " ++ show annotationName ++ " in " ++
               show functionName)
    let Just (JString value) = lookup "value" properties
      | _ => asmCrash ("Expected 'string' enum value for annotation " ++ show annotationName ++ " in " ++
               show functionName)
    pure $ AnnEnum type value
  parseAnnotationTypeValue functionName annotationName "annotation" annotationJson =
    AnnAnnotation <$> parseAnnotation functionName annotationJson
  parseAnnotationTypeValue functionName annotationName type _ =
    asmCrash ("Unknown type " ++ show type ++ " for annotation " ++ annotationName ++ " in " ++ show functionName)

  parseAnnotationValue : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> JSON -> Core AnnotationValue
  parseAnnotationValue functionName annotationName (JNumber value) = pure $ AnnInt $ cast value
  parseAnnotationValue functionName annotationName (JString value) = pure $ AnnString value
  parseAnnotationValue functionName annotationName (JBoolean value) = pure $ AnnBoolean value
  parseAnnotationValue functionName annotationName annotationJson@(JObject properties) = do
    let propertiesByName = SortedMap.fromList properties
    let Just (JString type) = lookup "type" propertiesByName
      | _ => AnnAnnotation <$> parseAnnotation functionName annotationJson
    let Just value = SortedMap.lookup "value" propertiesByName
      | _ => asmCrash ("Missing 'string' value for annotation " ++ annotationName ++ " in " ++ show functionName)
    parseAnnotationTypeValue functionName annotationName type value
  parseAnnotationValue functionName annotationName (JArray valuesJson) =
    pure $ AnnArray !(traverse (parseAnnotationValue functionName annotationName) valuesJson)
  parseAnnotationValue functionName annotationName JNull = asmCrash ("Annotation property value cannot be null " ++
    " for annotation " ++ show annotationName ++ " in function " ++ show functionName)

  parseAnnotationProperty : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> String -> JSON -> Core AnnotationProperty
  parseAnnotationProperty functionName annotationName propertyName valueJson = do
    value <- parseAnnotationValue functionName annotationName valueJson
    pure (propertyName, value)

  parseAnnotation : {auto stateRef: Ref AsmState AsmState} -> Name -> JSON -> Core Annotation
  parseAnnotation functionName (JObject [(annotationName, (JObject propertyNameAndValues))]) = do
    properties <- traverse
      (\(propertyName, value) => parseAnnotationProperty functionName annotationName propertyName value)
      propertyNameAndValues
    pure $ MkAnnotation annotationName properties
  parseAnnotation functionName (JObject [(annotationName, simplifiedValue)]) =
    parseAnnotation functionName (JObject [(annotationName, (JObject [("value", simplifiedValue)]))])
  parseAnnotation functionName _ =
    asmCrash ("Expected a JSON object for parameter annotations in " ++ show functionName)

parseAnnotations : {auto stateRef: Ref AsmState AsmState} -> Name -> JSON -> Core (List Annotation)
parseAnnotations functionName (JArray annotations) = traverse (parseAnnotation functionName) annotations
parseAnnotations functionName _ = asmCrash ("Expected an array for parameter annotations in " ++ show functionName)

getNamespace : Name -> Namespace
getNamespace (NS n _) = n
getNamespace n = emptyNS

findByNamespace : SortedMap Namespace value -> Name -> Maybe value
findByNamespace functionImports name = go (sortBy comparingNamespaceLength parents) where

    parents : List Namespace
    parents = allParents $ getNamespace name

    comparingNamespaceLength : Namespace -> Namespace -> Ordering
    comparingNamespaceLength = comparing (negate . cast {to=Int} . String.length . show)

    go : List Namespace -> Maybe value
    go [] = Nothing
    go (ns :: rest) = maybe (go rest) Just $ SortedMap.lookup ns functionImports

record ExportArgument where
  constructor MkExportArgument
  type: InferredType
  annotations: List Annotation

parseArgument : {auto stateRef: Ref AsmState AsmState} -> Name -> List (String, JSON) -> Core ExportArgument
parseArgument functionName keyAndValues = do
  let valuesByKey = SortedMap.fromList keyAndValues
  let Just (JString typeStr) = lookup "type" valuesByKey
    | _ => asmCrash $ "Expected 'string' argument type for " ++ show functionName
  let annotationsJson = fromMaybe (JArray []) $ lookup "annotations" valuesByKey
  annotations <- parseAnnotations functionName annotationsJson
  pure $ MkExportArgument (parse typeStr) annotations

parseArgumentsJson : {auto stateRef: Ref AsmState AsmState} -> Name -> JSON -> Core (List ExportArgument)
parseArgumentsJson functionName (JArray arguments) = go arguments where
  go : List JSON -> Core (List ExportArgument)
  go [] = pure[]
  go ((JObject keyAndValues) :: rest) = do
    argument <- parseArgument functionName keyAndValues
    restArguments <- go rest
    pure (argument :: restArguments)
  go _ = asmCrash ("Expected an argument object for foreign export function: " ++ show functionName)
parseArgumentsJson functionName _ =
  asmCrash ("Expected an array of arguments for foreign export function: " ++ show functionName)

export
loadJavaVar : {auto stateRef: Ref AsmState AsmState} -> Name -> Map Int InferredType -> Int -> InferredType
            -> SortedMap Namespace (List String) -> InferredType -> Core ()
loadJavaVar functionName typesByIndex varIndex idrisType typeExports jvmType@(IRef name _ _) =
  case findByNamespace typeExports functionName of
    Just exportedTypeNames =>
      if elem name exportedTypeNames
        then do
          loadVar typesByIndex jvmType jvmType varIndex
          let descriptor = getMethodDescriptor $ MkInferredFunctionType idrisType []
          invokeMethod InvokeVirtual name "toIdris" descriptor False
        else
          loadVar typesByIndex jvmType idrisType varIndex
    Nothing => loadVar typesByIndex jvmType idrisType varIndex
loadJavaVar _ typesByIndex varIndex idrisType _ jvmType = loadVar typesByIndex jvmType idrisType varIndex

export
toJava : {auto stateRef: Ref AsmState AsmState} -> Name -> SortedMap Namespace (List String) -> InferredType -> InferredType -> Core ()
toJava functionName typeExports jvmType@(IRef name _ _) idrisType =
  case findByNamespace typeExports functionName of
    Just exportedTypeNames =>
      if elem name exportedTypeNames
        then do
          let descriptor = getMethodDescriptor $ MkInferredFunctionType jvmType [idrisType]
          invokeMethod InvokeStatic name "toJava" descriptor False
        else
          asmCast idrisType jvmType
    Nothing => asmCast idrisType jvmType
toJava _ _ jvmType idrisType = asmCast idrisType jvmType

export
loadArguments : {auto stateRef: Ref AsmState AsmState} -> SortedMap Namespace (List String) -> Map Int InferredType -> Name -> Int -> List InferredType -> Core ()
loadArguments typeExports typesByIndex functionName arity idrisTypes = go 0 idrisTypes
  where
    go : Int -> List InferredType -> Core ()
    go n [] =
      if n == arity
        then pure ()
        else asmCrash ("JVM and Idris types do not match in foreign export for " ++ show functionName)
    go varIndex (idrisType :: rest) = do
      Just jvmType <- coreLift $ nullableToMaybe <$> Map.get typesByIndex varIndex
        | Nothing => do -- World type only exists for Idris functions
           iconst 0 -- Load "world" for PrimIO functions
           invokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False
      loadJavaVar functionName typesByIndex varIndex idrisType typeExports jvmType
      go (varIndex + 1) rest

public export
record ClassExport where
  constructor MkClassExport
  name: String
  idrisName: Name
  extends: InferredType
  implements: List InferredType
  modifiers: List Access
  annotations: List Annotation

public export
Eq ClassExport where
  export1 == export2 = export1.name == export2.name

public export
Ord ClassExport where
  compare = comparing name

public export
record MethodExport where
  constructor MkMethodExport
  name: String
  idrisName: Name
  type: InferredFunctionType
  shouldPerformIO: Bool
  encloser: ClassExport
  modifiers: List Access
  annotations: List Annotation
  parameterAnnotations: List (List Annotation)

public export
record FieldExport where
  constructor MkFieldExport
  name: String
  type: InferredType
  encloser: ClassExport
  modifiers: List Access
  annotations: List Annotation

public export
data ExportDescriptor : Type where
  MkFieldExportDescriptor : FieldExport -> ExportDescriptor
  MkMethodExportDescriptor : MethodExport -> ExportDescriptor
  MkClassExportDescriptor : ClassExport -> ExportDescriptor
  MkImportDescriptor : Name -> SortedMap String String -> ExportDescriptor
  MkTypeExportDescriptor : Name -> List String -> ExportDescriptor

parseModifier : Name -> String -> Access
parseModifier _ "public" = Public
parseModifier _ "private" = Private
parseModifier _ "static" = Static
parseModifier _ "synthetic" = Synthetic
parseModifier _ "final" = Final
parseModifier _ "interface" = Interface
parseModifier _ "abstract" = Abstract
parseModifier _ "transient" = Transient
parseModifier name invalid = believe_me $ crash ("Invalid modifier " ++ invalid ++ " in export " ++ show name)

parseString : {auto stateRef: Ref AsmState AsmState} -> String -> JSON -> Core String
parseString _ (JString value) = pure value
parseString errorMessage _ = asmCrash errorMessage

getEncloser : ExportDescriptor -> Maybe ClassExport
getEncloser (MkMethodExportDescriptor methodExport) = Just methodExport.encloser
getEncloser (MkFieldExportDescriptor fieldExport) = Just fieldExport.encloser
getEncloser (MkClassExportDescriptor classExport) = Just classExport
getEncloser _ = Nothing

parseModifierJson : Name -> JSON -> Access
parseModifierJson name (JString value) = parseModifier name value
parseModifierJson name invalid = believe_me $ crash ("Invalid modifier " ++ show invalid ++ " in export " ++ show name)

parseModifiers : Name -> JSON -> List Access
parseModifiers name (JArray modifiers) = (parseModifierJson name) <$> modifiers
parseModifiers name invalid = believe_me $ crash ("Invalid modifiers " ++ show invalid ++ " in export " ++ show name)

parseClassFieldExport : {auto stateRef: Ref AsmState AsmState} -> Name -> ClassExport -> String -> JSON -> Core FieldExport
parseClassFieldExport idrisName encloser fieldName (JString type) =
  pure $ MkFieldExport fieldName (parse type) encloser [Private] []
parseClassFieldExport idrisName encloser fieldName (JObject desc) = do
  let modifiersJson = fromMaybe (JArray [JString "private"]) $ lookup "modifiers" desc
  let modifiers = parseModifiers idrisName modifiersJson
  let Just typeJson = lookup "type" desc
        | _ => asmCrash ("Missing type for " ++ fieldName ++ " in export " ++ show idrisName)
  type <- parseString ("Invalid type for " ++ fieldName ++ " in export " ++ show idrisName) typeJson
  let annotationsJson = fromMaybe (JArray []) $ lookup "annotations" desc
  annotations <- parseAnnotations idrisName annotationsJson
  pure $ MkFieldExport fieldName (parse type) encloser modifiers annotations
parseClassFieldExport idrisName encloser fieldName descriptor =
  asmCrash ("Expected a JSON string or object for field export in " ++ show idrisName ++
    " but found: " ++ show descriptor)

parseClassFieldExports : {auto stateRef: Ref AsmState AsmState} -> Name -> ClassExport -> SortedMap String JSON -> Core (List FieldExport)
parseClassFieldExports name encloser descriptor = case lookup "fields" descriptor of
  Nothing => pure[]
  Just (JObject nameAndValues) => traverse (uncurry $ parseClassFieldExport name encloser) nameAndValues
  Just descriptor => asmCrash ("Expected a JSON object for exported fields in " ++ show name ++
                         " but found: " ++ show descriptor)

getModifiersAndName : Name -> List Access -> List String -> (List Access, String)
getModifiersAndName name acc [] = believe_me $ crash ("Missing exported function name in " ++ show name)
getModifiersAndName _ acc (functionName :: []) = (acc, functionName)
getModifiersAndName name acc (modifier :: rest) = getModifiersAndName name (parseModifier name modifier :: acc) rest

parseClassExport : {auto stateRef: Ref AsmState AsmState} -> Name -> (parts : List String) -> SortedMap String JSON -> List Annotation -> Core (List ExportDescriptor)
parseClassExport name parts descriptor annotations = do
  let isInterface = "interface" `elem` parts
  extends <- if isInterface
              then pure"java/lang/Object"
              else case lookup "extends" descriptor of
                     Nothing => pure"java/lang/Object"
                     Just (JString superName) => pure superName
                     _ => asmCrash ("Invalid 'extends' for " ++ show name)
  let implementsKey = if isInterface then "extends" else "implements"
  implements <- case lookup implementsKey descriptor of
                  Nothing => pure[]
                  Just (JArray implementsJson) =>
                    traverse (parseString ("Expected a string value for '" ++ implementsKey ++ "' for " ++ show name))
                      implementsJson
                  _ => asmCrash ("Invalid '" ++ implementsKey ++ "' for " ++ show name ++
                        ". Expected an array of type names.")
  let (modifiers, jvmFunctionName) = getModifiersAndName name [] parts
  let classExport = MkClassExport jvmFunctionName name (parse extends) (parse <$> implements)
                      modifiers annotations
  let classExportDescriptor = MkClassExportDescriptor classExport
  fieldExportDescriptors <- parseClassFieldExports name classExport descriptor
  pure $ (classExportDescriptor :: (MkFieldExportDescriptor <$> fieldExportDescriptors))

getReferenceTypeName : {auto stateRef: Ref AsmState AsmState} -> String -> InferredType -> Core String
getReferenceTypeName _ (IRef name _ _) = pure name
getReferenceTypeName functionName _ = asmCrash ("Expected a reference type to export function " ++ functionName)

makePublicByDefault : List Access -> List Access
makePublicByDefault modifiers =
  let accessModifiers = the (List Access) [Public, Private, Protected]
  in if any (flip elem accessModifiers) modifiers
      then modifiers
      else (Public :: modifiers)

parseJvmReturnType : {auto stateRef: Ref AsmState AsmState} -> String -> SortedMap String JSON -> Core InferredType
parseJvmReturnType functionName descriptor = do
  typeString <- parseString ("Invalid return type for function " ++ functionName) $
    fromMaybe (JString "java/lang/Object") $ lookup "returnType" descriptor
  pure $ parse typeString

stripLastChar : String -> String
stripLastChar str = case length str of
  Z => str
  (S n) => substr 0 n str

parseMethodExport : {auto stateRef: Ref AsmState AsmState} -> Name -> (javaName: String) -> (nameParts: List String)
                  -> SortedMap String JSON -> List Annotation -> Core MethodExport
parseMethodExport idrisName javaName parts descriptor annotations = do
    let argumentsJson = fromMaybe (JArray []) $ lookup "arguments" descriptor
    arguments <- parseArgumentsJson idrisName argumentsJson
    let (jvmArgumentTypes, parameterAnnotations) =
          unzip $ (\(MkExportArgument type annotations) => (type, annotations)) <$> arguments
    let (modifiers, initialMethodName) = getModifiersAndName idrisName [] parts
    let shouldPerformIO = endsWith initialMethodName "!"
    let methodName = if shouldPerformIO then stripLastChar initialMethodName else initialMethodName
    jvmReturnType <- if methodName == "<init>" then pure IVoid else parseJvmReturnType javaName descriptor
    let functionType = MkInferredFunctionType jvmReturnType jvmArgumentTypes
    let adjustedModifiers = makePublicByDefault modifiers
    let isInstance = not $ elem Static modifiers
    let adjustedParameterAnnotations = if isInstance then drop 1 parameterAnnotations else parameterAnnotations
    enclosingTypeName <- if isInstance
      then case jvmArgumentTypes of
        [] => asmCrash ("Expected first argument to be a reference type for instance member in " ++ javaName)
        (enclosingType :: _) => getReferenceTypeName javaName enclosingType
      else case lookup "enclosingType" descriptor of
        Nothing => asmCrash ("Missing 'enclosingType' for " ++ javaName)
        Just enclosingTypeJson => parseString ("Invalid enclosing type for function " ++ javaName) enclosingTypeJson
    [MkClassExportDescriptor encloser] <- case words enclosingTypeName of
        [] => asmCrash ("Unable to determine enclosing type for " ++ javaName)
        enclosingTypeParts@(_ :: _) =>
          parseClassExport idrisName enclosingTypeParts SortedMap.empty []
      | _ => asmCrash ("Unexpected 'enclosingType' for " ++ show javaName)
    pure $ MkMethodExport methodName idrisName functionType shouldPerformIO encloser adjustedModifiers annotations
      adjustedParameterAnnotations

parseFieldExport : {auto stateRef: Ref AsmState AsmState} -> Name -> (nameParts: List String)
                   -> SortedMap String JSON -> List Annotation -> Core (List ExportDescriptor)
parseFieldExport name parts descriptor annotations = do
  let (modifiers, fieldName) = getModifiersAndName name [] parts
  Just enclosingTypeName <-
      traverseOpt (parseString ("Invalid 'enclosingType' for " ++ show name)) $ lookup "enclosingType" descriptor
    | Nothing => asmCrash ("Missing 'enclosingType' for " ++ show name)
  [MkClassExportDescriptor encloser] <- case words enclosingTypeName of
       [] => asmCrash ("Missing enclosing type for " ++ show name)
       enclosingTypeParts@(_ :: _) => parseClassExport name enclosingTypeParts SortedMap.empty []
     | _ => asmCrash ("Unexpected 'enclosingType' for " ++ show name)
  Just typeString <- traverseOpt (parseString ("Invalid type for field " ++ show name)) $ lookup "type" descriptor
      | Nothing => asmCrash ("Missing type for " ++ show name)
  let type = parse typeString
  pure[MkFieldExportDescriptor $ MkFieldExport fieldName type encloser modifiers annotations]

parseObjectExportDescriptor : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> List (String, JSON) -> Core (List ExportDescriptor)
parseObjectExportDescriptor idrisName javaName descriptorKeyAndValues = do
  let descriptor = SortedMap.fromList descriptorKeyAndValues
  let annotationsJson = fromMaybe (JArray []) $ lookup "annotations" descriptor
  annotations <- parseAnnotations idrisName annotationsJson
  case words javaName of
    [] => asmCrash ("Invalid export descriptor " ++ javaName)
    parts@(_ :: _) =>
      cond
        [
          ((isJust (lookup "returnType" descriptor) || elem "<init>" parts),
            do
              methodExport <- parseMethodExport idrisName javaName parts descriptor annotations
              pure[MkMethodExportDescriptor methodExport]),
          (isJust $ lookup "type" descriptor, parseFieldExport idrisName parts descriptor annotations)
        ]
        (parseClassExport idrisName parts descriptor annotations)

parseJsonExport : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> Core (List ExportDescriptor)
parseJsonExport functionName descriptor = case String.break (\c => c == '{') descriptor of
  ("", _) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
  (_, "") => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
  (name, signature) => do
    case JSON.parse signature of
      Just (JObject keyAndValues) => parseObjectExportDescriptor functionName name keyAndValues
      _ => asmCrash ("Invalid foreign export descriptor " ++ descriptor ++ " for " ++ show functionName)

parseMethodSimpleExport : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> Core MethodExport
parseMethodSimpleExport functionName descriptor = case String.break (\c => c == '.') descriptor of
  ("", instanceMethodNameAndSig) => case words instanceMethodNameAndSig of
    [] => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (_ :: []) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (_ :: _ :: []) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (javaName :: instanceTypeString :: types@(_ :: _)) => do
      let shouldPerformIO = endsWith javaName "!"
      let javaName = if shouldPerformIO then stripLastChar javaName else javaName
      let instanceType = parse instanceTypeString
      let functionType = MkInferredFunctionType (parse (last types)) (instanceType :: (parse <$> (init types)))
      className <- getReferenceTypeName ("Invalid instance type in export for " ++ show functionName) instanceType
      let encloser = MkClassExport className functionName inferredObjectType [] [Public] []
      pure $ MkMethodExport javaName functionName functionType shouldPerformIO encloser [Public] [] []
  (className, staticMethodNameAndArgs) => case words staticMethodNameAndArgs of
    [] => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (_ :: []) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (javaName :: types@(_ :: _)) => do
      let shouldPerformIO = endsWith javaName "!"
      let javaName = if shouldPerformIO then stripLastChar javaName else javaName
      let functionType = MkInferredFunctionType (parse (last types)) (parse <$> (init types))
      let encloser = MkClassExport className functionName inferredObjectType [] [Public] []
      pure $ MkMethodExport javaName functionName functionType shouldPerformIO encloser [Public, Static] [] []

parseFieldSimpleExport : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> Core FieldExport
parseFieldSimpleExport functionName descriptor = case String.break (\c => c == '#') descriptor of
  ("", instanceFieldNameAndSig) => case words instanceFieldNameAndSig of
    [] => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (_ :: []) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (_ :: _ :: []) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (javaName :: instanceType :: type :: _) => do
      className <- getReferenceTypeName ("Invalid instance type in export for " ++ show functionName)
                     (parse instanceType)
      let encloser = MkClassExport className functionName inferredObjectType [] [Public] []
      pure $ MkFieldExport javaName (parse type) encloser [Public] []
  (className, staticFieldNameAndType) => case words staticFieldNameAndType of
    [] => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (_ :: []) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (javaName :: type :: _) => do
      let encloser = MkClassExport className functionName inferredObjectType [] [Public] []
      pure $ MkFieldExport javaName (parse type) encloser [Public, Static] []

%foreign jvm' "java/lang/Character" "isWhitespace" "char" "boolean"
isWhitespace : Char -> Bool

parseImport : String -> Maybe (String, String)
parseImport line = case words line of
  (type :: []) =>
    let alias = (Prelude.reverse . fst . break (== '/') . Prelude.reverse) type
    in Just (alias, type)
  (type :: alias :: []) => Just (alias, type)
  _ => Nothing

parseImports : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> Core ExportDescriptor
parseImports functionName descriptor =
  case String.break isWhitespace descriptor of
    ("", _) => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    (_, "") => asmCrash ("Invalid foreign export descriptor for " ++ show functionName)
    ("export", exportsDescriptor) =>
      pure $ MkTypeExportDescriptor functionName (trim <$> (drop 1 $ lines exportsDescriptor))
    (_, importsDescriptor) =>
      pure $ MkImportDescriptor functionName $ SortedMap.fromList $ catMaybes $
        parseImport <$> (lines importsDescriptor)

parseExportDescriptor : {auto stateRef: Ref AsmState AsmState} -> Name -> String -> Core (List ExportDescriptor)
parseExportDescriptor functionName descriptor = cond
  [
    ("{" `isInfixOf` descriptor, parseJsonExport functionName descriptor),
    ("." `isInfixOf` descriptor, do
      methodExport <- parseMethodSimpleExport functionName descriptor
      pure[MkMethodExportDescriptor methodExport]),
    ("#" `isInfixOf` descriptor, do
      fieldExport <- parseFieldSimpleExport functionName descriptor
      pure $ [MkFieldExportDescriptor fieldExport])
  ]
  (pure[!(parseImports functionName descriptor)])

export
adjustArgumentsForInstanceMember : {auto stateRef: Ref AsmState AsmState} -> Name -> (isInstance: Bool) -> List InferredType -> Core (List InferredType)
adjustArgumentsForInstanceMember _ False argumentTypes = pure argumentTypes
adjustArgumentsForInstanceMember _ _ (_ :: jvmArgumentTypes) = pure jvmArgumentTypes
adjustArgumentsForInstanceMember idrisName _ _ =
  asmCrash ("Expected first argument to be a reference type for instance member in " ++ show idrisName)

export
createAccessorName : {auto stateRef: Ref AsmState AsmState} -> String -> String -> Core String
createAccessorName pfix fieldName = case strM fieldName of
   StrNil => asmCrash "field name cannot be empty"
   StrCons firstLetter rest => pure (pfix ++ strCons (toUpper firstLetter) rest)

export
createGetter : {auto stateRef: Ref AsmState AsmState} -> ClassExport -> FieldExport -> Core ()
createGetter classExport fieldExport = do
  let fieldName = fieldExport.name
  getterName <- createAccessorName "get" fieldName
  let fieldType = fieldExport.type
  let getterType = getMethodDescriptor $ MkInferredFunctionType fieldType []
  let isStatic = elem Static fieldExport.modifiers
  let getterModifiers = Public :: (if isStatic then [Static] else [])
  let className = classExport.name
  createMethod getterModifiers "generated.idr" className getterName getterType Nothing Nothing [] []
  methodCodeStart
  when (not isStatic) $ aload 0
  let instructionType = if isStatic then GetStatic else GetField
  field instructionType className fieldName (getJvmTypeDescriptor fieldType)
  asmReturn fieldType
  maxStackAndLocal (-1) (-1)
  methodCodeEnd

export
createSetter : {auto stateRef: Ref AsmState AsmState} -> ClassExport -> FieldExport -> Core ()
createSetter classExport fieldExport = do
  let fieldName = fieldExport.name
  setterName <- createAccessorName "set" fieldName
  let fieldType = fieldExport.type
  let isStatic = elem Static fieldExport.modifiers
  let setterType = MkInferredFunctionType IVoid [fieldType]
  let descriptor = getMethodDescriptor setterType
  let signature = Just $ getMethodSignature setterType
  let setterModifiers = Public :: (if isStatic then [Static] else [])
  let className = classExport.name
  createMethod setterModifiers "generated.idr" className setterName descriptor signature Nothing [] []
  methodCodeStart
  createLabel methodStartLabel
  createLabel methodEndLabel
  labelStart methodStartLabel
  when (not isStatic) $ aload 0
  let arity = the Int $ if isStatic then 1 else 2
  let parameterTypes = if isStatic then [fieldType] else [iref className [], fieldType]
  jvmArgumentTypesByIndex <- coreLift $ Map.fromList $ zip [0 .. arity - 1] parameterTypes
  let varIndex = the Int $ if isStatic then 0 else 1
  loadVar jvmArgumentTypesByIndex fieldType fieldType varIndex
  let instructionType = if isStatic then PutStatic else PutField
  field instructionType className fieldName (getJvmTypeDescriptor fieldType)
  return
  labelStart methodEndLabel
  let classDescriptor = getJvmTypeDescriptor $ iref classExport.name []
  localVariable "this" classDescriptor Nothing methodStartLabel methodEndLabel 0
  let signature = Just $ getSignature fieldType
  localVariable fieldName (getJvmTypeDescriptor fieldType) signature methodStartLabel methodEndLabel 1
  maxStackAndLocal (-1) (-1)
  methodCodeEnd

mutual
  getSuperCallExprList : {auto stateRef: Ref AsmState AsmState} -> List NamedCExp -> Core (Maybe NamedCExp)
  getSuperCallExprList [] = pure Nothing
  getSuperCallExprList (expr :: rest) = pure (!(getSuperCallExpr expr) <|> !(getSuperCallExprList rest))

  getSuperCallExprVect : {auto stateRef: Ref AsmState AsmState} -> Vect n NamedCExp -> Core (Maybe NamedCExp)
  getSuperCallExprVect [] = pure Nothing
  getSuperCallExprVect (expr :: rest) = pure (!(getSuperCallExpr expr) <|> !(getSuperCallExprVect rest))

  export
  getSuperCallExpr : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core (Maybe NamedCExp)
  getSuperCallExpr expr@(NmExtPrim _ (NS _ n) args) = if isSuper n then pure (Just expr) else getSuperCallExprList args
  getSuperCallExpr (NmLam _ _ expr) = getSuperCallExpr expr
  getSuperCallExpr (NmLet _ _ value expr) = pure (!(getSuperCallExpr value) <|> !(getSuperCallExpr expr))
  getSuperCallExpr (NmApp _ (NmRef _ name) args) = do
    (_, MkNmFun _ def) <- getFcAndDefinition (jvmSimpleName name)
      | _ => getSuperCallExprList args
    pure (!(getSuperCallExpr def) <|> !(getSuperCallExprList args))
  getSuperCallExpr (NmOp _ _ args) = getSuperCallExprVect args
  getSuperCallExpr (NmForce _ _ expr) = getSuperCallExpr expr
  getSuperCallExpr (NmDelay _ _ expr) = getSuperCallExpr expr
  getSuperCallExpr (NmConCase _ expr alts deflt) =
    getSuperCallCaseExpr (expr :: (getSuperCallConAltExpr <$> alts)) deflt
  getSuperCallExpr (NmConstCase _ expr alts deflt) =
    getSuperCallCaseExpr (expr :: (getSuperCallConstAltExpr <$> alts)) deflt
  getSuperCallExpr _ = pure Nothing

  getSuperCallConAltExpr : NamedConAlt -> NamedCExp
  getSuperCallConAltExpr (MkNConAlt _ _ _ _ expr) = expr

  getSuperCallConstAltExpr : NamedConstAlt -> NamedCExp
  getSuperCallConstAltExpr (MkNConstAlt _ expr) = expr

  getSuperCallCaseExpr : {auto stateRef: Ref AsmState AsmState} -> List NamedCExp -> Maybe NamedCExp -> Core (Maybe NamedCExp)
  getSuperCallCaseExpr alts Nothing = getSuperCallExprList alts
  getSuperCallCaseExpr alts (Just deflt) = pure (!(getSuperCallExprList alts) <|> !(getSuperCallExpr deflt))

substituteTypeName : SortedMap String String -> String -> String
substituteTypeName imports type = fromMaybe type $ SortedMap.lookup type imports

substituteType : SortedMap String String -> InferredType -> InferredType
substituteType imports ref@(IRef type refType typeParams) =
  let substitutedType = substituteTypeName imports type
      substitutedTypeParams = substituteType imports <$> typeParams
  in IRef substitutedType refType substitutedTypeParams
substituteType imports ref@(IArray (IRef type refType typeParams)) =
  maybe ref (\type => IArray $ IRef type refType typeParams) $ SortedMap.lookup type imports
substituteType imports ref@(IArray (IArray type)) = IArray (IArray $ substituteType imports type)
substituteType imports type = type

substituteFunctionType : SortedMap String String -> InferredFunctionType -> InferredFunctionType
substituteFunctionType imports (MkInferredFunctionType returnType argumentTypes) =
  let updatedReturnType = substituteType imports returnType
      updatedArgumentTypes = (substituteType imports) <$> argumentTypes
  in MkInferredFunctionType updatedReturnType updatedArgumentTypes

mutual
  substituteAnnotationValue : SortedMap String String -> AnnotationValue -> AnnotationValue
  substituteAnnotationValue imports (AnnAnnotation annotation) = AnnAnnotation (substituteAnnotation imports annotation)
  substituteAnnotationValue imports (AnnArray values) = AnnArray (substituteAnnotationValue imports <$> values)
  substituteAnnotationValue imports (AnnEnum type value) = AnnEnum (substituteTypeName imports type) value
  substituteAnnotationValue imports (AnnClass value) = AnnClass (substituteTypeName imports value)
  substituteAnnotationValue _ value = value

  substituteAnnotationProperty : SortedMap String String -> AnnotationProperty -> AnnotationProperty
  substituteAnnotationProperty imports (name, value) = (name, substituteAnnotationValue imports value)

  substituteAnnotation : SortedMap String String -> Annotation -> Annotation
  substituteAnnotation imports (MkAnnotation name props) =
    MkAnnotation (substituteTypeName imports name) (substituteAnnotationProperty imports <$> props)

substituteTypeExports : SortedMap Namespace (SortedMap String String) -> Name -> List String -> List String
substituteTypeExports functionImports name exports = case findByNamespace functionImports name of
  Nothing => exports
  Just imports => substituteTypeName imports <$> exports

substituteClassExport : SortedMap Namespace (SortedMap String String) -> ClassExport -> ClassExport
substituteClassExport functionImports desc = case findByNamespace functionImports desc.idrisName of
  Nothing => desc
  Just imports =>
    let
      updatedName = substituteTypeName imports desc.name
      updatedExtends = substituteType imports desc.extends
      updatedImplements = substituteType imports <$> desc.implements
      updatedAnnotations = substituteAnnotation imports <$> desc.annotations
    in MkClassExport updatedName desc.idrisName updatedExtends updatedImplements desc.modifiers updatedAnnotations

substituteImport : SortedMap Namespace (SortedMap String String) -> ExportDescriptor -> ExportDescriptor
substituteImport functionImports exportDesc@(MkMethodExportDescriptor desc) =
  case findByNamespace functionImports desc.idrisName of
    Nothing => exportDesc
    Just imports =>
      let
        updatedType = substituteFunctionType imports desc.type
        updatedEncloser = substituteClassExport functionImports desc.encloser
        updatedAnnotations = substituteAnnotation imports <$> desc.annotations
        updatedParameterAnnotations = (substituteAnnotation imports <$>) <$> desc.parameterAnnotations
      in MkMethodExportDescriptor $ MkMethodExport desc.name desc.idrisName updatedType desc.shouldPerformIO
           updatedEncloser desc.modifiers updatedAnnotations updatedParameterAnnotations
substituteImport functionImports exportDesc@(MkFieldExportDescriptor desc) =
  case findByNamespace functionImports desc.encloser.idrisName of
    Nothing => exportDesc
    Just imports =>
      let
        updatedType = substituteType imports desc.type
        updatedAnnotations = substituteAnnotation imports <$> desc.annotations
        updatedEncloser = substituteClassExport functionImports desc.encloser
      in MkFieldExportDescriptor $ MkFieldExport desc.name updatedType updatedEncloser desc.modifiers
           updatedAnnotations
substituteImport functionImports (MkClassExportDescriptor desc) =
  MkClassExportDescriptor $ substituteClassExport functionImports desc
substituteImport functionImports (MkTypeExportDescriptor name exports) =
  MkTypeExportDescriptor name $ substituteTypeExports functionImports name exports
substituteImport _ desc = desc

substituteImports : SortedMap Namespace (SortedMap String String) -> List ExportDescriptor -> List ExportDescriptor
substituteImports imports descriptors = (substituteImport imports) <$> descriptors

isTypeExportDescriptor : ExportDescriptor -> Bool
isTypeExportDescriptor (MkTypeExportDescriptor _ _) = True
isTypeExportDescriptor _ = False

getExportsMap : List ExportDescriptor -> SortedMap Namespace (List String)
getExportsMap descriptors = go SortedMap.empty (descriptors >>= toTypeDescriptor) where

  go : SortedMap Namespace (List String) -> List (Name, List String) -> SortedMap Namespace (List String)
  go exportsByNamespace ((name, exports) :: rest) =
    let newExportsByNamespace = SortedMap.merge exportsByNamespace (SortedMap.singleton (getNamespace name) exports)
    in go newExportsByNamespace rest
  go exportsByNamespace [] = exportsByNamespace

  toTypeDescriptor : ExportDescriptor -> List (Name, List String)
  toTypeDescriptor (MkTypeExportDescriptor name exports) = [(name, exports)]
  toTypeDescriptor _ = []

export
parseExportDescriptors : AsmGlobalState -> List (Name, String) ->
                           IO (SortedMap Namespace (List String), List ExportDescriptor)
parseExportDescriptors globalState descriptors = do
    (imports, exportDescriptors) <- go (SortedMap.empty, []) descriptors
    let substitutedDescriptors = substituteImports imports $ sortBy (comparing memberTypeOrder) exportDescriptors
    let (exports, others) = partition isTypeExportDescriptor substitutedDescriptors
    pure (getExportsMap exports, others)
  where
    memberTypeOrder : ExportDescriptor -> Nat
    memberTypeOrder (MkClassExportDescriptor _) = 0
    memberTypeOrder (MkFieldExportDescriptor fieldExport) = if Static `elem` fieldExport.modifiers then 1 else 2
    memberTypeOrder (MkMethodExportDescriptor _) = 3
    memberTypeOrder _ = 4

    go : (SortedMap Namespace (SortedMap String String), List ExportDescriptor) ->
            List (Name, String) -> IO (SortedMap Namespace (SortedMap String String), List ExportDescriptor)
    go acc [] = pure acc
    go (imports, descriptors) ((idrisName, descriptor) :: rest) = do
      asmState <- createAsmState globalState idrisName
      exportDescriptors <- runAsm asmState (\stateRef => parseExportDescriptor idrisName descriptor)
      case exportDescriptors of
        [MkImportDescriptor name currentImports] =>
          let newImports = SortedMap.merge imports (SortedMap.singleton (getNamespace name) currentImports)
          in go (newImports, descriptors) rest
        _ => go (imports, exportDescriptors ++ descriptors) rest

export
findClassAnnotation : String -> ClassExport -> Maybe Annotation
findClassAnnotation name classExport =
  find (\(MkAnnotation currentName _) => name == currentName) classExport.annotations

export
findAllArgsConstructor : ClassExport -> Maybe Annotation
findAllArgsConstructor = findClassAnnotation "AllArgsConstructor"

export
findRequiredArgsConstructor : ClassExport -> Maybe Annotation
findRequiredArgsConstructor = findClassAnnotation "RequiredArgsConstructor"

export
findNoArgsConstructor : ClassExport -> Maybe Annotation
findNoArgsConstructor = findClassAnnotation "NoArgsConstructor"

hasFieldModifier : Access -> FieldExport -> Bool
hasFieldModifier modifier fieldExport = modifier `elem` fieldExport.modifiers

export
isRequiredField : FieldExport -> Bool
isRequiredField = hasFieldModifier Final

export
isTransientField : FieldExport -> Bool
isTransientField = hasFieldModifier Transient

export
getFields : List ExportDescriptor -> List FieldExport
getFields descriptors = go [] descriptors where
  go : List FieldExport -> List ExportDescriptor -> List FieldExport
  go acc [] = acc
  go acc (MkFieldExportDescriptor fieldExport :: rest) =
    if hasFieldModifier Static fieldExport then go acc rest else go (fieldExport :: acc) rest
  go acc (_ :: rest) = go acc rest

knownAnnotations : List String
knownAnnotations = ["Data", "Getter", "Setter", "NoArgsConstructor", "RequiredArgsConstructor",
                      "AllArgsConstructor", "EqualsAndHashCode"]
export
isIdrisJvmAnnotation : Annotation -> Bool
isIdrisJvmAnnotation (MkAnnotation name _) = name `elem` knownAnnotations

export
exportClass : {auto stateRef: Ref AsmState AsmState} -> ClassExport -> Core ()
exportClass (MkClassExport name _ extends implements modifiers annotations) = do
  createClass [ComputeMaxs, ComputeFrames]
  let annotations = filter (not . isIdrisJvmAnnotation) annotations
  let signature = getSignature extends ++ concat (getSignature <$> implements)
  extendsTypeName <- getJvmReferenceTypeName extends
  implementsTypeNames <- traverse getJvmReferenceTypeName implements
  let asmAnnotations = asmAnnotation <$> annotations
  classCodeStart javaClassFileVersion modifiers name (Just signature) extendsTypeName implementsTypeNames asmAnnotations

export
exportField : {auto stateRef: Ref AsmState AsmState} -> FieldExport -> Core ()
exportField (MkFieldExport fieldName type encloser modifiers annotations) = do
  let jvmClassName = encloser.name
  let asmAnnotations = asmAnnotation <$> annotations
  createField modifiers "Unknown.idr" jvmClassName fieldName (getJvmTypeDescriptor type) Nothing Nothing asmAnnotations
  fieldEnd

export
exportType : {auto stateRef: Ref AsmState AsmState} -> String -> Core ()
exportType name = do
  exportClass (MkClassExport name (UN $ Basic name) inferredObjectType [] [Public] [])
  createField [Private, Final] "Unknown.idr" name "idrisValue" (getJvmTypeDescriptor inferredObjectType)
    Nothing Nothing []
  fieldEnd

  createMethod [Private] "Unknown.idr" name "<init>" "(Ljava/lang/Object;)V" Nothing Nothing [] []
  methodCodeStart
  aload 0
  invokeMethod InvokeSpecial "java/lang/Object" "<init>" "()V" False
  aload 0
  aload 1
  field PutField name "idrisValue" "Ljava/lang/Object;"
  return
  maxStackAndLocal (-1) (-1)
  methodCodeEnd

  let toJavaDescriptor = getMethodDescriptor $ MkInferredFunctionType (IRef name Class []) [inferredObjectType]
  createMethod [Public, Static] "Unknown.idr" name "toJava" toJavaDescriptor Nothing Nothing [] []
  methodCodeStart
  new name
  dup
  aload 0
  invokeMethod InvokeSpecial name "<init>" "(Ljava/lang/Object;)V" False
  areturn
  maxStackAndLocal (-1) (-1)
  methodCodeEnd

  createMethod [Public] "Unknown.idr" name "toIdris" "()Ljava/lang/Object;" Nothing Nothing [] []
  methodCodeStart
  aload 0
  field GetField name "idrisValue" "Ljava/lang/Object;"
  areturn
  maxStackAndLocal (-1) (-1)
  methodCodeEnd
