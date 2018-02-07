module IdrisJvm.JvmImport

import System
import IdrisJvm.FFI

%dynamic "libc"

%access public export

data InvType = JStatic JVM_NativeTy
             | JInstance
             | JConstructor
             | JStaticField JVM_NativeTy
             | JInstanceField
             | JStaticFieldSetter JVM_NativeTy
             | JInstanceFieldSetter

-- Type for Elaborator Reflection to generate JVM FFI calls
data EType = EBool
           | EChar
           | EBits8
           | EBits16
           | EBits32
           | EBits64
           | EFloat
           | EDouble
           | EInt
           | EThrowablePrimitive EType

           | EString
           | ENullableString
           | EThrowableString
           | EThrowableNullableString

           | EVoid

           | EArray EType
           | ENullableArray EType
           | EThrowableNullableArray EType

           | EClass String
           | ENullableClass String
           | EThrowableClass String
           | EThrowableNullableClass String

           | EInterface String
           | ENullableInterface String
           | EThrowableNullableInterface String

Eq EType where
  EBool == EBool = True
  EChar == EChar = True
  EBits8 == EBits8 = True
  EBits16 == EBits16 = True
  EBits32 == EBits32 = True
  EBits64 == EBits64 = True
  EFloat == EFloat = True
  EDouble == EDouble = True
  EInt == EInt = True
  (EThrowablePrimitive e1) == (EThrowablePrimitive e2) = e1 == e2

  EString == EString = True
  ENullableString == ENullableString = True
  EThrowableString == EThrowableString = True
  EThrowableNullableString == EThrowableNullableString = True

  EVoid == EVoid = True

  (EArray e1) == (EArray e2) = e1 == e2
  (ENullableArray e1) == (ENullableArray e2) = e1 == e2
  (EThrowableNullableArray e1) == (EThrowableNullableArray e2) = e1 == e2

  (EClass c1) == (EClass c2) = c1 == c2
  (ENullableClass c1) == (ENullableClass c2) = c1 == c2
  (EThrowableClass c1) == (EThrowableClass c2) = c1 == c2
  (EThrowableNullableClass c1) == (EThrowableNullableClass c2) = c1 == c2

  (EInterface i1) == (EInterface i2) = i1 == i2
  (ENullableInterface i1) == (ENullableInterface i2) = i1 == i2
  (EThrowableNullableInterface i1) == (EThrowableNullableInterface i2) = i1 == i2

  _ == _ = False

data JvmFfiDesc = MethodDesc InvType EType String (List EType)
                | ConstructorDesc EType (List EType)

data JSafety = JSafe -- both exception and null safe

             -- Just exception safe, useful for constructors and primitives as those can never return or be `null`.
             | JThrowableSafe

             -- Just null safe, useful for field getters as they cannot throw exceptions
             | JNullableSafe

             | JUnsafe

JvmFfiDescs : Type
JvmFfiDescs = List JvmFfiDesc


createArray : Nat -> EType -> EType
createArray Z ty = ty
createArray (S k) ty = EArray $ createArray k ty

createNullableArray : Nat -> EType -> EType
createNullableArray Z ty = ty
createNullableArray (S k) ty = ENullableArray $ createArray k ty

parseEType : String -> Either String EType
parseEType jtypeStr with (words jtypeStr)
  | ["int"] = pure EInt
  | ["char"] = pure EChar
  | ["byte"] = pure EBits8
  | ["short"] = pure EBits16
  | ["long"] = pure EBits64
  | ["float"] = pure EFloat
  | ["double"] = pure EDouble
  | ["boolean"] = pure EBool
  | ["void"] = pure EVoid

  -- This and some of the following ones are used to parse the types explicitly given by the user.
  -- We don't want the user to have to differentiate between a class and interface so here everything is realized as a
  -- class but later on refined correctly for a class or interface based on type provider information
  | ["?Array", dim, className] = createNullableArray (cast dim) <$> parseEType ("c " ++ className)

  | ("Array" :: dim :: "c" :: className :: []) = createArray (cast dim) <$> parseEType ("c " ++ className)
  | ("Array" :: dim :: "i" :: className :: []) = createArray (cast dim) <$> parseEType ("i " ++ className)
  | ("Array" :: dim :: ty :: []) = createArray (cast dim) <$> parseEType ty

  | ["c", "java/lang/String"] = pure EString
  | ["?java/lang/String"] = pure ENullableString

  | ["c", className] = pure $ EClass className
  | [className] = pure $ if strHead className == '?' then ENullableClass (strTail className) else EClass className

  | ["i", className] = pure $ EInterface className
  | _ = Left $ "Unable to parse Java type: " ++ jtypeStr

parseArgs : List String -> Either String (List EType)
parseArgs args = sequence $ parseEType <$> args

{-
 - Type providers can only run in IO, not in JVM_IO so JVM is launched as an external process setting up correct classpath that
 - includes the classes we want to import into Idris. The JVM program outputs the types of all the methods for the passed classes.
 - This function parses the output and constructs JvmFfiDesc which is then used by a function running in Idris elaborator to generate
 - appropriate FFI call site.
-}
public export
jvmImport : String -> List (String, List String) -> IO (Provider JvmFfiDescs)
jvmImport cmd importList = do
    let args =  unwords $ uncurry createCommandParam <$> importList
    let types = ".idrisjvmtypes"
    system $ cmd ++ " " ++ quoted types ++ " " ++ args
    Right str <- readFile types
        | Left err => pure (Error $ show err)
    let errOrffiDescs = sequence $ parseJvmOutput <$> lines str
    pure $ either Error Provide errOrffiDescs
  where

    quoted : String -> String
    quoted s = "\"" ++ s ++ "\""

    createCommandParam : String -> List String -> String
    createCommandParam clazz clazzItems = quoted $ clazz ++ " " ++ (unwords clazzItems)

    parseInvType : String -> Either String InvType
    parseInvType invStr with (words invStr)
      | ["s", "c", className] = pure $ JStatic (FFI.Class className)
      | ["s", "i", className] = pure $ JStatic (FFI.Interface className)
      | ["i"] = pure JInstance
      | ["sfg", "c", className] = pure $ JStaticField (FFI.Class className)
      | ["sfs", "c", className] = pure $ JStaticFieldSetter (FFI.Class className)
      | ["ifg"] = pure JInstanceField
      | ["ifs"] = pure JInstanceFieldSetter
      | _ = Left $ "Unable to parse invocation type: " ++ invStr

    parseJvmOutput : String -> Either String JvmFfiDesc
    parseJvmOutput desc = case filter (not . (== "")) $ Strings.split (== ',') desc of
      "c" :: returns :: args => do
        jtype <- parseEType returns
        args <- parseArgs args
        pure $ ConstructorDesc jtype args
      invTypeStr :: retStr :: mname :: args => do
        invType <- parseInvType invTypeStr
        jtype <- parseEType retStr
        args <- parseArgs args
        pure $ MethodDesc invType jtype mname args
      _ => Left $ "Unable to parse JVM type provider output: " ++ desc

public export
jdkImport : List (String, List String) -> IO (Provider JvmFfiDescs)
jdkImport importList = do
  idrisJvmHomeMaybe <- getEnv "IDRIS_JVM_HOME"
  homeMaybe <- getEnv "HOME"
  let Just idrisJvmHome = idrisJvmHomeMaybe <|> homeMaybe
  let typeProviderClass = "io.github.mmhelloworld.idrisjvm.runtime.ffi.TypeProvider"
  let cmd = "java -cp " ++ idrisJvmHome ++ "/idris-jvm-runtime.jar " ++ typeProviderClass
  jvmImport cmd importList

jvmFfiNamespace : List String
jvmFfiNamespace = ["FFI", "IdrisJvm"]

appMaybeRaw : Raw -> Raw
appMaybeRaw ty = RApp (Var (NS (UN "Maybe") ["Maybe", "Prelude"])) ty

appThrowableRaw : Raw -> Raw
appThrowableRaw = RApp (RApp
                            (Var (NS (UN "Either") ["Either", "Prelude"]))
                            (Var (NS (UN "Throwable") jvmFfiNamespace)))

appThrowableNullableRaw : Raw -> Raw
appThrowableNullableRaw = appThrowableRaw . appMaybeRaw

jcallStaticTTName : TTName
jcallStaticTTName = NS (UN "jcallStatic") jvmFfiNamespace

jcallInstanceTTName : TTName
jcallInstanceTTName = NS (UN "jcallInstance") jvmFfiNamespace

jcallNewTTName : TTName
jcallNewTTName = NS (UN "jcallNew") jvmFfiNamespace

jcallGetStaticFieldTTName : TTName
jcallGetStaticFieldTTName = NS (UN "jcallGetStaticField") jvmFfiNamespace

jcallGetInstanceFieldTTName : TTName
jcallGetInstanceFieldTTName = NS (UN "jcallGetInstanceField") jvmFfiNamespace

jcallSetStaticFieldTTName : TTName
jcallSetStaticFieldTTName = NS (UN "jcallSetStaticField") jvmFfiNamespace

jcallSetInstanceFieldTTName : TTName
jcallSetInstanceFieldTTName = NS (UN "jcallSetInstanceField") jvmFfiNamespace

boolTyElab : Raw
boolTyElab = Var (NS (UN "Bool") ["Bool", "Prelude"])

charTyElab : Raw
charTyElab = RConstant (AType (ATInt ITChar))

bits8TyElab : Raw
bits8TyElab = RConstant (AType (ATInt (ITFixed IT8)))

bits16TyElab : Raw
bits16TyElab = RConstant (AType (ATInt (ITFixed IT16)))

bits32TyElab : Raw
bits32TyElab = RConstant (AType (ATInt (ITFixed IT32)))

bits64TyElab : Raw
bits64TyElab = RConstant (AType (ATInt (ITFixed IT64)))

intTyElab : Raw
intTyElab = RConstant (AType (ATInt ITNative))

stringTyElab : Raw
stringTyElab = RConstant StrType

floatTyElab : Raw
floatTyElab = Var (NS (UN "Float") ["FFI", "IdrisJvm"])

doubleTyElab: Raw
doubleTyElab = RConstant (AType ATDouble)

voidTyElab: Raw
voidTyElab = Var (UN "Unit")

jvmNativeElab : Raw
jvmNativeElab = Var (NS (UN "JVM_Native") jvmFfiNamespace)

ffiJvmElab : Raw
ffiJvmElab = Var (NS (UN "FFI_JVM") jvmFfiNamespace)

classRaw : Raw
classRaw = Var (NS (UN "Class") jvmFfiNamespace)

interfaceElab : Raw
interfaceElab = Var (NS (UN "Interface") jvmFfiNamespace)

classNameElab : String -> Raw
classNameElab className = RConstant (Str className)

jvmIOElab : Raw
jvmIOElab = Var (NS (UN "JVM_IO") jvmFfiNamespace)

nilRaw : Raw
nilRaw = RApp (Var (NS (UN "Nil") ["List", "Prelude"])) RType

jtypeElab : EType -> Raw
jtypeElab EBool = boolTyElab
jtypeElab EBits8 = bits8TyElab
jtypeElab EBits16 = bits16TyElab
jtypeElab EBits32 = bits32TyElab
jtypeElab EBits64 = bits64TyElab
jtypeElab EChar = charTyElab
jtypeElab EInt = intTyElab
jtypeElab EFloat = floatTyElab
jtypeElab EDouble = doubleTyElab
jtypeElab (EThrowablePrimitive primitive) = appThrowableRaw $ jtypeElab primitive

jtypeElab EString = stringTyElab
jtypeElab ENullableString = appMaybeRaw $ jtypeElab EString
jtypeElab EThrowableString = appThrowableRaw $ jtypeElab EString
jtypeElab EThrowableNullableString = appThrowableNullableRaw $ jtypeElab EString

jtypeElab (EArray ety) = RApp (Var (NS (UN "JVM_Array") ["FFI", "IdrisJvm"])) (jtypeElab ety)
jtypeElab (ENullableArray ety) = appMaybeRaw $ jtypeElab (EArray ety)
jtypeElab (EThrowableNullableArray ety) = appThrowableNullableRaw $ jtypeElab (EArray ety)

jtypeElab EVoid = voidTyElab

jtypeElab (EClass className) = RApp jvmNativeElab (RApp classRaw (classNameElab className))
jtypeElab (ENullableClass className) = appMaybeRaw $ jtypeElab (EClass className)
jtypeElab (EThrowableClass className) = appThrowableRaw $ jtypeElab (EClass className)
jtypeElab (EThrowableNullableClass className) = appThrowableNullableRaw $ jtypeElab (EClass className)

jtypeElab (EInterface className) = RApp jvmNativeElab (RApp interfaceElab (classNameElab className))
jtypeElab (ENullableInterface className) = appMaybeRaw $ jtypeElab (EInterface className)
jtypeElab (EThrowableNullableInterface className) = appThrowableNullableRaw $ jtypeElab (EInterface className)

jvmIORaw : EType -> Raw
jvmIORaw ty = RApp jvmIOElab (jtypeElab ty)

createSafeType : JSafety -> EType -> EType
createSafeType JThrowableSafe EString = EThrowableString
createSafeType JNullableSafe EString = ENullableString
createSafeType _ EString = EThrowableNullableString

createSafeType JThrowableSafe (EClass name) = EThrowableClass name
createSafeType JNullableSafe (EClass name) = ENullableClass name
createSafeType _ (EClass name) = EThrowableNullableClass name

createSafeType JNullableSafe (EInterface name) = ENullableInterface name
createSafeType _ (EInterface name) = EThrowableNullableInterface name

createSafeType JNullableSafe (EArray ty) = ENullableArray ty
createSafeType _ (EArray ty) = EThrowableNullableArray ty

createSafeType JNullableSafe primitive = primitive
createSafeType _ primitive = EThrowablePrimitive primitive

jvmFunctionTyElab : JSafety -> List EType -> EType -> Elab Raw
jvmFunctionTyElab JUnsafe [] ret = pure $ jvmIORaw ret
jvmFunctionTyElab safety [] ret = pure $ jvmIORaw $ createSafeType safety ret
jvmFunctionTyElab safety (arg :: args) ret = do
  argName <- gensym "arg"
  let argTyElab = jtypeElab arg
  rest <- jvmFunctionTyElab safety args ret
  pure $ RBind argName (Pi argTyElab RType) rest

nativeTyClassName : JVM_NativeTy -> String
nativeTyClassName (Class cname) = cname
nativeTyClassName (Interface cname) = cname

searchFty : Raw -> Elab ()
searchFty f = do
  [fty] <- apply f [True]
  solve
  focus fty
  search

gen : JSafety -> JvmFfiDesc -> Elab ()
gen safety (MethodDesc (JStatic ty) ret fname args) = do
    functionTy <- jvmFunctionTyElab safety args ret
    let cls = RApp classRaw (classNameElab $ nativeTyClassName ty)
    let appInvokeStatic = RApp (Var jcallStaticTTName) cls
    let appFunctionName = RApp appInvokeStatic $ RConstant (Str fname)
    let f = RApp appFunctionName functionTy
    searchFty f

gen safety (MethodDesc JInstance ret fname args) = do
    functionTy <- jvmFunctionTyElab safety args ret
    let appFunctionName = RApp (Var jcallInstanceTTName) $ RConstant (Str fname)
    let f = RApp appFunctionName functionTy
    searchFty f

gen JSafe desc@(ConstructorDesc _ _) = gen JThrowableSafe desc

gen safety (ConstructorDesc ret args) = do
    functionTy <- jvmFunctionTyElab safety args ret
    let f = RApp (Var jcallNewTTName) functionTy
    searchFty f

gen safety (MethodDesc (JStaticField ty) ret fname _) = do
    functionTy <- jvmFunctionTyElab safety [] ret
    let cls = RApp classRaw (classNameElab $ nativeTyClassName ty)
    let appGetStaticField = RApp (Var jcallGetStaticFieldTTName) cls
    let appFunctionName = RApp appGetStaticField $ RConstant (Str fname)
    let f = RApp appFunctionName functionTy
    searchFty f

gen safety (MethodDesc JInstanceField ret fname args) = do
    functionTy <- jvmFunctionTyElab safety args ret
    let appGetInstanceField = RApp (Var jcallGetInstanceFieldTTName) $ RConstant (Str fname)
    let f = RApp appGetInstanceField functionTy
    searchFty f

gen safety (MethodDesc (JStaticFieldSetter ty) ret fname args) = do
    functionTy <- jvmFunctionTyElab safety args ret
    let cls = RApp classRaw (classNameElab $ nativeTyClassName ty)
    let appSetStaticField = RApp (Var jcallSetStaticFieldTTName) cls
    let appFunctionName = RApp appSetStaticField $ RConstant (Str fname)
    let f = RApp appFunctionName functionTy
    searchFty f

gen safety (MethodDesc JInstanceFieldSetter ret fname args) = do
    functionTy <- jvmFunctionTyElab safety args ret
    let appSetInstanceField = RApp (Var jcallSetInstanceFieldTTName) $ RConstant (Str fname)
    let f = RApp appSetInstanceField functionTy
    searchFty f

decl syntax javaimport [cmd] [items] = %provide (jimports: JvmFfiDescs) with jvmImport cmd items
decl syntax jdkimport [items] = %provide (jimports: JvmFfiDescs) with jdkImport items

term syntax java [className] [methodName] = (%runElab (j jimports JSafe className methodName))
term syntax [className] "<.>" [methodName] = (%runElab (j jimports JSafe className methodName))

term syntax javaUnsafe [className] [methodName] = (%runElab (j jimports JUnsafe className methodName))
term syntax [className] "<.!>" [methodName] = (%runElab (j jimports JUnsafe className methodName))

term syntax javaField [className] [fieldName] = (%runElab (jf jimports JNullableSafe className fieldName))
term syntax [className] "<.#>" [fieldName] = (%runElab (jf jimports JNullableSafe className fieldName))

term syntax javaFieldUnsafe [className] [fieldName] = (%runElab (jf jimports JUnsafe className fieldName))
term syntax [className] "<.#!>" [fieldName] = (%runElab (jf jimports JUnsafe className fieldName))

term syntax javaSetField [className] [fieldName] = (%runElab (jfs jimports JUnsafe className fieldName))
term syntax [className] "<.=>" [fieldName] = (%runElab (jfs jimports JUnsafe className fieldName))

j : JvmFfiDescs -> JSafety -> String -> String -> Elab ()
j ffiDescs safety ty fname = do
    (methodName, args) <- argTypes
    maybe err (gen safety . addNullableIfNullableProvided args) $
        findFfiDesc ty methodName ((map stripNullable) <$> args) ffiDescs
  where
      err : Elab ()
      err = fail [TextPart $ "Couldn't find JVM FFI function " ++ fname ++ " in " ++ ty ++
        ". Did you forget to import?"]

      argTypes : Elab (String, Maybe (List EType))
      argTypes = case filter (not . (== "")) $ Strings.split (== '(') fname of
        [methodName] => pure (methodName, Nothing)
        methodName :: rest :: _ => case Strings.split (== ')') rest of
          "" :: _ => pure (methodName, Just [])
          paramsStr :: _ =>
            either (\err => fail [TextPart err]) (\args => pure (methodName, Just args)) $ parseArgs $
              Strings.split (== ',') paramsStr

      stripNullable : EType -> EType
      stripNullable ENullableString = EString
      stripNullable (ENullableInterface ty) = EInterface ty
      stripNullable (ENullableClass ty) = EClass ty
      stripNullable (ENullableArray ty) = EArray ty
      stripNullable ty = ty

      makeTypeAccurate : EType -> EType -> EType
      makeTypeAccurate (ENullableClass _) (EClass ty) = ENullableClass ty
      makeTypeAccurate (ENullableClass _) (EInterface ty) = ENullableInterface ty
      makeTypeAccurate (EClass _)         (EInterface ty) = EInterface ty
      makeTypeAccurate (ENullableArray _) (EArray ty) = ENullableArray ty
      makeTypeAccurate ENullableString _ = ENullableString
      makeTypeAccurate _ ty = ty

      makeTypesAccurate : List EType -> JvmFfiDesc -> JvmFfiDesc
      makeTypesAccurate callsiteArgs (MethodDesc invType@(JStatic otherNativeTy) ret name declaredArgs)
        = MethodDesc invType ret name (uncurry makeTypeAccurate <$> zip callsiteArgs declaredArgs)
      
      makeTypesAccurate callsiteArgs (MethodDesc JInstance ret name declaredArgs@(EString :: _))
        = let correctedTypes = uncurry makeTypeAccurate <$> zip (EString :: callsiteArgs) declaredArgs
          in MethodDesc JInstance ret name correctedTypes
      
      makeTypesAccurate callsiteArgs (MethodDesc JInstance ret methodName declaredArgs@(EClass className :: _))
        = let nonNullableInstanceCallsiteArgs = EClass className :: callsiteArgs
              correctedTypes = uncurry makeTypeAccurate <$> zip nonNullableInstanceCallsiteArgs declaredArgs
          in MethodDesc JInstance ret methodName correctedTypes
      
      makeTypesAccurate callsiteArgs (MethodDesc JInstance ret methodName declaredArgs@(EInterface className :: _))
        = let nonNullableInstanceCallsiteArgs = EInterface className :: callsiteArgs
              correctedTypes = uncurry makeTypeAccurate <$> zip nonNullableInstanceCallsiteArgs declaredArgs
          in MethodDesc JInstance ret methodName correctedTypes
      
      makeTypesAccurate _ desc = desc

      addNullableIfNullableProvided : Maybe (List EType) -> JvmFfiDesc -> JvmFfiDesc
      addNullableIfNullableProvided (Just callsiteArgs) ffiDesc = makeTypesAccurate callsiteArgs ffiDesc
      addNullableIfNullableProvided Nothing ffiDesc = ffiDesc

      argsMatching : Maybe (List EType) -> List EType -> Bool
      argsMatching Nothing _ = True
      argsMatching (Just []) [] = True
      argsMatching (Just []) _ = False
      argsMatching (Just _) [] = False
      argsMatching (Just callsiteArgs) declaredArgs =
        equalLength && (all (uncurry g) $ zip callsiteArgs declaredArgs) where
            equalLength : Bool
            equalLength = length callsiteArgs == length declaredArgs

            g : EType -> EType -> Bool
            -- callsite type may not be accurate as we don't want user to provide class/interface information so it is
            -- always defaulted to Class and here it gets refined based on type provider information
            g (EClass callsiteTy) (EInterface declaredTy) = callsiteTy == declaredTy
            g (EClass callsiteTy) (EClass declaredTy) = callsiteTy == declaredTy

            g callsiteTy declaredTy = callsiteTy == declaredTy


      findFfiDesc : String -> String -> Maybe (List EType) -> List JvmFfiDesc -> Maybe JvmFfiDesc

      findFfiDesc className "<init>" args ffiDescs = find f ffiDescs where
        f : JvmFfiDesc -> Bool
        f (ConstructorDesc EString declaredArgs)
            = className == "java/lang/String" && argsMatching args declaredArgs
        f (ConstructorDesc (EClass otherClassName) declaredArgs)
            = className == otherClassName && argsMatching args declaredArgs
        f _ = False

      findFfiDesc className methodName args ffiDescs = find f ffiDescs where

        f : JvmFfiDesc -> Bool
        f (MethodDesc (JStatic otherNativeTy) ret otherMethodName declaredArgs)
          = className == nativeTyClassName otherNativeTy && methodName == otherMethodName
                && argsMatching args declaredArgs
        f (MethodDesc JInstance ret otherMethodName (EString :: declaredArgs)) =
          className == "java/lang/String" && methodName == otherMethodName && argsMatching args declaredArgs
        f (MethodDesc JInstance ret otherMethodName (EClass otherClassName :: declaredArgs)) =
          className == otherClassName && methodName == otherMethodName && argsMatching args declaredArgs
        f (MethodDesc JInstance ret otherMethodName (EInterface otherClassName :: declaredArgs)) =
          className == otherClassName && methodName == otherMethodName && argsMatching args declaredArgs
        f _ = False

jf : JvmFfiDescs -> JSafety -> String -> String -> Elab ()
jf ffiDescs safety ty fieldName = do
    maybe err (gen safety) $ findFfiDesc ty ffiDescs
  where
      err : Elab ()
      err = fail [TextPart $ "Couldn't find JVM FFI field " ++ fieldName ++ " in " ++ ty ++
        ". Did you forget to import?"]

      findFfiDesc : String -> List JvmFfiDesc -> Maybe JvmFfiDesc
      findFfiDesc className ffiDescs = find f ffiDescs where

        f : JvmFfiDesc -> Bool
        f (MethodDesc (JStaticField otherNativeTy) ret otherFieldName _)
          = className == nativeTyClassName otherNativeTy && fieldName == otherFieldName
        f (MethodDesc JInstanceField ret otherFieldName [EString]) =
          className == "java/lang/String" && fieldName == otherFieldName
        f (MethodDesc JInstanceField ret otherFieldName [EClass otherClassName]) =
          className == otherClassName && fieldName == otherFieldName
        f _ = False

jfs : JvmFfiDescs -> JSafety -> String -> String -> Elab ()
jfs ffiDescs safety ty fieldName = do
    maybe err (gen safety) $ findFfiDesc ty ffiDescs
  where
      err : Elab ()
      err = fail [TextPart $ "Couldn't find JVM FFI field " ++ fieldName ++ " in " ++ ty ++
        ". Did you forget to import?"]

      findFfiDesc : String -> List JvmFfiDesc -> Maybe JvmFfiDesc
      findFfiDesc className ffiDescs = find f ffiDescs where

        f : JvmFfiDesc -> Bool
        f (MethodDesc (JStaticFieldSetter otherNativeTy) ret otherFieldName _)
          = className == nativeTyClassName otherNativeTy && fieldName == otherFieldName
        f (MethodDesc JInstanceFieldSetter ret otherFieldName (EClass otherClassName :: _)) =
          className == otherClassName && fieldName == otherFieldName
        f _ = False