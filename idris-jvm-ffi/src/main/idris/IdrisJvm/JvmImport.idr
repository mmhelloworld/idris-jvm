module IdrisJvm.JvmImport

import System
import IdrisJvm.FFI

%dynamic "libc"

%access public export

data InvType = JStatic JVM_NativeTy | JInstance

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
           | EString
           | EVoid
           | EArray EType
           | EClass String
           | EInterface String

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
  EString == EString = True
  EVoid == EVoid = True
  (EArray e1) == (EArray e2) = e1 == e2
  (EClass c1) == (EClass c2) = c1 == c2
  (EInterface i1) == (EInterface i2) = i1 == i2
  _ == _ = False

data JvmFfiDesc = MkJvmFfiDesc InvType EType String (List EType)

JvmFfiDescs : Type
JvmFfiDescs = List JvmFfiDesc

createArray : Nat -> EType -> EType
createArray Z ty = ty
createArray (S k) ty = EArray $ createArray k ty

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
  | ("Array" :: dim :: "class" :: className :: []) = createArray (cast dim) <$> parseEType ("class " ++ className)
  | ("Array" :: dim :: "interface" :: className :: []) = createArray (cast dim) <$> parseEType ("interface " ++ className)
  | ["Array", dim, ty] = createArray (cast dim) <$> parseEType ty
  | ["class", "java/lang/String"] = pure EString
  | ["class", className] = pure $ EClass className
  | ["interface", className] = pure $ EInterface className
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
jvmImport : String -> List String -> IO (Provider JvmFfiDescs)
jvmImport cmd classNames = do
    let args = unwords $ (\item => "\"" ++ item ++ "\"") <$> classNames
    let types = "jvm.types"
    system $ cmd ++ " " ++ args ++ " > " ++ types
    Right str <- readFile types
    let errOrffiDescs = sequence $ parseJvmOutput <$> lines str
    pure $ either Error Provide errOrffiDescs
  where

    parseInvType : String -> Either String InvType
    parseInvType invStr with (words invStr)
      | ["static", "class", className] = pure $ JStatic (FFI.Class className)
      | ["static", "interface", className] = pure $ JStatic (FFI.Interface className)
      | ["instance"] = pure JInstance
      | _ = Left $ "Unable to parse invocation type: " ++ invStr

    parseJvmOutput : String -> Either String JvmFfiDesc
    parseJvmOutput desc = case filter (not . (== "")) $ Strings.split (== ',') desc of
      invTypeStr :: retStr :: mname :: args => do
        invType <- parseInvType invTypeStr
        jtype <- parseEType retStr
        args <- parseArgs args
        pure $ MkJvmFfiDesc invType jtype mname args
      _ => Left $ "Unable to parse JVM type provider output: " ++ desc


jvmFfiNamespace : List String
jvmFfiNamespace = ["FFI", "IdrisJvm"]

jcallStaticTTName : TTName
jcallStaticTTName = NS (UN "jcallStatic") jvmFfiNamespace

jcallInstanceTTName : TTName
jcallInstanceTTName = NS (UN "jcallInstance") jvmFfiNamespace

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
jtypeElab EString = stringTyElab
jtypeElab EDouble = doubleTyElab
jtypeElab (EArray ety) = RApp (Var (NS (UN "JVM_Array") ["FFI", "IdrisJvm"])) (jtypeElab ety)
jtypeElab EVoid = voidTyElab
jtypeElab (EClass className) = RApp jvmNativeElab (RApp classRaw (classNameElab className))
jtypeElab (EInterface className) = RApp jvmNativeElab (RApp interfaceElab (classNameElab className))

jvmIORaw : EType -> Raw
jvmIORaw ty = RApp jvmIOElab (jtypeElab ty)

jvmFunctionTyElab : List EType -> EType -> Elab Raw
jvmFunctionTyElab [] ret = pure $ jvmIORaw ret
jvmFunctionTyElab (arg :: args) ret = do
  argName <- gensym "arg"
  let argTyElab = jtypeElab arg
  rest <- jvmFunctionTyElab args ret
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

{-
 - Generates an FFI function call to invoke a Java static method with appropriate types for arguments and return type using
 - types stored in `ffiDescs` provided by the type provider function `jvmImport`
 -}
jstatic : JvmFfiDesc -> Elab ()
jstatic (MkJvmFfiDesc (JStatic ty) ret fname args) = do
  functionTy <- jvmFunctionTyElab args ret
  let cls = RApp classRaw (classNameElab $ nativeTyClassName ty)
  let appInvokeStatic = RApp (Var jcallStaticTTName) cls
  let appFunctionName = RApp appInvokeStatic $ RConstant (Str fname)
  let f = RApp appFunctionName functionTy
  searchFty f

jinstance : JvmFfiDesc -> Elab ()
jinstance (MkJvmFfiDesc _ ret fname args) = do
  functionTy <- jvmFunctionTyElab args ret
  let appFunctionName = RApp (Var jcallInstanceTTName) $ RConstant (Str fname)
  let f = RApp appFunctionName functionTy
  searchFty f

decl syntax javaimport [cmd] [items] = %provide (jimports: JvmFfiDescs) with jvmImport cmd items

term syntax java [className] [methodName] = (%runElab (j jimports className methodName))

public export
j : JvmFfiDescs -> String -> String -> Elab ()
j ffiDescs ty fname = do
    (methodName, args) <- argTypes
    maybe err gen $ findFfiDesc ty methodName args ffiDescs
  where
      err : Elab ()
      err = fail [TextPart $ "Couldn't find JVM FFI function " ++ fname ++ " in " ++ ty]

      gen : JvmFfiDesc -> Elab ()
      gen ffi@(MkJvmFfiDesc (JStatic _) _ _ _) = jstatic ffi
      gen ffi@(MkJvmFfiDesc JInstance _ _ _) = jinstance ffi

      argTypes : Elab (String, Maybe (List EType))
      argTypes = case filter (not . (== "")) $ Strings.split (== '(') fname of
        [methodName] => pure (methodName, Nothing)
        methodName :: rest :: _ => case Strings.split (== ')') rest of
          "" :: _ => pure (methodName, Just [])
          paramsStr :: _ =>
            either (\err => fail [TextPart err]) (\args => pure (methodName, Just args)) $ parseArgs $
              Strings.split (== ',') paramsStr

      findFfiDesc : String -> String -> Maybe (List EType) -> List JvmFfiDesc -> Maybe JvmFfiDesc
      findFfiDesc className methodName args ffiDescs = find f ffiDescs where

        argsMatching : List EType -> Bool
        argsMatching otherArgs = maybe True (== otherArgs) args

        f : JvmFfiDesc -> Bool
        f (MkJvmFfiDesc (JStatic otherNativeTy) ret otherMethodName args)
          = className == nativeTyClassName otherNativeTy && methodName == otherMethodName && argsMatching args
        f (MkJvmFfiDesc JInstance ret otherMethodName (EString :: args)) =
          className == "java/lang/String" && methodName == otherMethodName && argsMatching args
        f (MkJvmFfiDesc JInstance ret otherMethodName (EClass otherClassName :: args)) =
          className == otherClassName && methodName == otherMethodName && argsMatching args
        f (MkJvmFfiDesc JInstance ret otherMethodName (EInterface otherClassName :: args)) =
          className == otherClassName && methodName == otherMethodName && argsMatching args
        f _ = False