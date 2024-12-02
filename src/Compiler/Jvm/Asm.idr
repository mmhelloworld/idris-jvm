module Compiler.Jvm.Asm

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Core
import Core.Name
import Core.Reflect
import Core.TT

import Data.List
import Data.List.Lazy
import Data.List1
import Data.Maybe
import Data.String
import Libraries.Data.SortedSet
import Libraries.Data.String.Extra
import Data.Vect
import Debug.Trace

import Compiler.Jvm.Tuples
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.ShowUtil

import System
import System.FFI

%hide Core.Name.Scoped.Scope
%hide Debug.Trace.toString
%hide Core.TT.Primitive.Constant

public export
Assembler : Type
Assembler = Struct "io/github/mmhelloworld/idrisjvm/assembler/Assembler" []

public export
JAnnotation : Type
JAnnotation = Struct "io/github/mmhelloworld/idrisjvm/assembler/Annotation" []

data JAnnString : Type where [external]
data JAnnEnum : Type where [external]
data JAnnInt : Type where [external]
data JAnnBoolean : Type where [external]
data JAnnByte : Type where [external]
data JAnnChar : Type where [external]
data JAnnShort : Type where [external]
data JAnnLong : Type where [external]
data JAnnFloat : Type where [external]
data JAnnDouble : Type where [external]
data JAnnClass : Type where [external]
data JAnnArray : Type where [external]
data JAnnAnnotation : Type where [external]
data JAnnotationProperty : Type where [external]
data JAnnotationValue: Type where [external]
data JHandle : Type where [external]
data JBsmArg : Type where [external]
data JBsmArgGetType : Type where [external]
data JBsmArgHandle : Type where [external]

data JInteger : Type where [external]
data JDouble : Type where [external]
data JLong : Type where [external]

public export
interface Inherits child parent where
    constructor MkInherits

    export %inline
    subtyping : child -> parent
    subtyping = believe_me

public export
Inherits a a where

namespace Object

    export
    data Object : Type where [external]

    %foreign jvm' "java/lang/Object" ".toString" "java/lang/Object" "String"
    prim_toString : Object -> PrimIO String

    %foreign jvm' "java/lang/Object" ".hashCode" "java/lang/Object" "int"
    prim_hashCode : Object -> PrimIO Int

    export
    toString : a -> String
    toString obj = unsafePerformIO $ primIO $ prim_toString (believe_me obj)

    export
    hashCode : a -> Int
    hashCode obj = unsafePerformIO $ primIO $ prim_hashCode (believe_me obj)

public export
Inherits a Object where

public export
%foreign "jvm:nullValue(java/lang/Object),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
nullValue : a

public export
%foreign jvm' "java/util/Objects" "isNull" "java/lang/Object" "boolean"
isNull : Object -> Bool

public export
maybeToNullable : Maybe t -> t
maybeToNullable (Just t) = t
maybeToNullable Nothing = nullValue

public export
nullableToMaybe : a -> Maybe a
nullableToMaybe value = if isNull (believe_me value) then Nothing else Just value

namespace Iterable
    export
    data Iterable : Type -> Type where [external]

namespace Collection
    export
    data Collection : Type -> Type where [external]

    %foreign "jvm:.add(i:java/util/Collection java/lang/Object Bool),java/util/Collection"
    prim_add : Collection a -> a -> PrimIO Bool

    %foreign "jvm:.size(i:java/util/Collection int),java/util/Collection"
    prim_size : Collection a -> PrimIO Int

    export %inline
    add : HasIO io => obj -> elemTy -> (Inherits obj (Collection elemTy)) => io Bool
    add collection elem = primIO $ prim_add (subtyping collection) elem

    export %inline
    size : (HasIO io, Inherits obj (Collection elemTy)) => obj -> io Int
    size {elemTy} collection = primIO $ prim_size {a=elemTy} (subtyping collection)

public export
Inherits (Collection a) (Iterable a) where

namespace JList

    export
    data JList : Type -> Type where [external]

    %foreign jvm' "java/util/List" ".add" "i:java/util/List int java/lang/Object" "void"
    prim_add : JList a -> Int -> a -> PrimIO ()

    %foreign jvm' "java/util/List" ".addAll" "i:java/util/List java/util/Collection" "boolean"
    prim_addAll : JList a -> Collection a -> PrimIO Bool

    %foreign jvm' "java/util/List" ".set" "i:java/util/List int java/lang/Object" "java/lang/Object"
    prim_set : JList a -> Int -> a -> PrimIO a

    export
    add : HasIO io => JList a -> Int -> a -> io ()
    add list index value = primIO $ prim_add list index value

    export
    addAll : (HasIO io, Inherits obj (Collection a)) => JList a -> obj -> io Bool
    addAll list collection = primIO $ prim_addAll list (subtyping collection)

    %foreign "jvm:.get(i:java/util/List int java/lang/Object),java/util/List"
    prim_get : JList a -> Int -> PrimIO a

    export %inline
    get : (HasIO io, Inherits list (JList elemTy)) => list -> Int -> io elemTy
    get list index = primIO $ prim_get (subtyping list) index

    export
    set : HasIO io => JList a -> Int -> a -> io a
    set list index value = primIO $ prim_set list index value

    %foreign jvm' "java/util/Collections" "nCopies" "int java/lang/Object" "java/util/List"
    prim_nCopies : Int -> a -> PrimIO (JList a)

    export
    nCopies : HasIO io => Int -> a -> io (JList a)
    nCopies n value =primIO $ prim_nCopies n value

    %foreign jvm' "io/github/mmhelloworld/idrisjvm/runtime/IdrisList" "fromIterable" "java/lang/Iterable" "io/github/mmhelloworld/idrisjvm/runtime/IdrisList"
    prim_fromIterable : Iterable a -> PrimIO (List a)

    export
    fromIterable : (HasIO io, Inherits obj (Iterable a)) => obj -> io (List a)
    fromIterable iterable = primIO $ prim_fromIterable (subtyping iterable)

public export
Inherits (JList a) (Iterable a) where

public export
Inherits (List a) (JList a) where

namespace ArrayList
    export
    data ArrayList : Type -> Type where [external]

    %foreign "jvm:<init>(java/util/ArrayList),java/util/ArrayList"
    prim_new : PrimIO (ArrayList a)

    export %inline
    new : HasIO io => io (ArrayList elemTy)
    new = primIO prim_new

public export
Inherits (ArrayList a) (JList a) where

namespace Entry
    export
    data Entry : Type -> Type -> Type where [external]

    %foreign "jvm:<init>(java/lang/Object java/lang/Object java/util/AbstractMap$SimpleImmutableEntry),java/util/AbstractMap$SimpleImmutableEntry"
    prim_new : key -> value -> PrimIO (Entry key value)

    export
    new : HasIO io => k -> v -> io (Entry k v)
    new key value = primIO (prim_new key value)

    %foreign jvm' "java/util/Map$Entry" ".getKey" "i:java/util/Map$Entry" "java/lang/Object"
    prim_getKey : Entry key value -> PrimIO key

    export
    getKey : HasIO io => Entry k v -> io k
    getKey entry = primIO (prim_getKey entry)

    %foreign jvm' "java/util/Map$Entry" ".getValue" "i:java/util/Map$Entry" "java/lang/Object"
    prim_getValue : Entry key value -> PrimIO value

    export
    getValue : HasIO io => Entry k v -> io v
    getValue entry = primIO (prim_getValue entry)

    export
    toTuple : HasIO io => Entry k v -> io (k, v)
    toTuple entry = do
        key <- getKey {k=k} {v=v} entry
        value <- getValue {k=k} {v=v} entry
        pure (key, value)

namespace Map

  export
  data Map : (key: Type) -> (value: Type) -> Type where [external]

  %foreign "jvm:.put(i:java/util/Map java/lang/Object java/lang/Object java/lang/Object),java/util/Map"
  prim_put : Map key value -> key -> value -> PrimIO value

  %foreign "jvm:.get(i:java/util/Map java/lang/Object java/lang/Object),java/util/Map"
  prim_get : Map key value -> key -> PrimIO value

  export %inline
  put : (HasIO io, Inherits obj (Map key value)) => obj -> key -> value -> io value
  put map key value = primIO $ prim_put (subtyping map) key value

  export %inline
  get : (HasIO io, Inherits obj (Map key value)) => obj -> key -> io value
  get map key = primIO (prim_get (subtyping map) key)

  %foreign "jvm:<init>(java/lang/Object java/util/TreeMap),java/util/TreeMap"
  prim_newTreeMap : PrimIO (Map key value)

  export
  newTreeMap : HasIO io => io (Map key value)
  newTreeMap = primIO prim_newTreeMap

  export
  fromList : HasIO io => List (key, value) -> io (Map key value)
  fromList keyValues = do
      m <- Map.newTreeMap {key=key} {value=value}
      go m keyValues
      pure m
    where
      go : Map key value -> List (key, value) -> io ()
      go _ [] = pure ()
      go m ((k, v) :: rest) = do
        ignore $ Map.put m k v
        go m rest

  export
  fromLazyList : HasIO io => LazyList (key, value) -> io (Map key value)
  fromLazyList keyValues = do
      m <- Map.newTreeMap {key=key} {value=value}
      go m keyValues
      pure m
    where
      go : Map key value -> LazyList (key, value) -> io ()
      go _ [] = pure ()
      go m ((k, v) :: rest) = do
        ignore $ Map.put m k v
        go m rest

  %foreign jvm' "java/util/Map" ".containsKey" "i:java/util/Map java/lang/Object" "boolean"
  prim_containsKey : Map key value -> key -> PrimIO Bool

  export
  containsKey : HasIO io => Map key value -> key -> io Bool
  containsKey this key = primIO $ prim_containsKey this key

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "transpose" "java/util/Map" "java/util/Map"
  prim_transpose : Map key value -> PrimIO (Map value key)

  export
  transpose : HasIO io => Map k v -> io (Map v k)
  transpose m = primIO (prim_transpose m)

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "toList" "java/util/Map" "java/util/List"
  prim_toEntries : Map key value -> PrimIO (JList (Entry key value))

  export
  toEntries : HasIO io => Map key value -> io (List (Entry key value))
  toEntries m = do
    entries <- primIO (prim_toEntries m)
    JList.fromIterable entries

  export
  toList : HasIO io => Map k v -> io (List (k, v))
  toList m = do
    entries <- toEntries m
    traverse toTuple entries

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "keys" "java/util/Map" "java/util/List"
  prim_keys : Map key value -> PrimIO (JList key)

  export
  keys : HasIO io => Map key value -> io (List key)
  keys m = do
    jkeys <- primIO (prim_keys m)
    JList.fromIterable jkeys

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "values" "java/util/Map" "java/util/List"
  prim_values : Map key value -> PrimIO (JList value)

  export
  values : HasIO io => Map key value -> io (List value)
  values m = do
    jvalues <- primIO (prim_values m)
    JList.fromIterable jvalues

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "getValue2" "java/util/Map" "java/util/Map"
  prim_getValue2 : Map key (Entry value1 value2) -> PrimIO (Map key value2)

  export
  getValue2 : HasIO io => Map k (Entry v1 v2) -> io (Map k v2)
  getValue2 m = primIO (prim_getValue2 m)

namespace EnumInt
    export
    succ : Int -> Int
    succ n = n + 1

%inline
public export
runtimeClass : String
runtimeClass = getRuntimeClass "Runtime"

public export
record Scope where
    constructor MkScope
    index : Int
    parentIndex : Maybe Int
    variableTypes : Map String InferredType
    allVariableTypes : Map Int InferredType
    variableIndices : Map String Int
    allVariableIndices : Map String Int
    returnType : InferredType
    nextVariableIndex : Int
    lineNumbers : (Int, Int)
    labels : (String, String)
    childIndices : List Int

public export
record Function where
    constructor MkFunction
    idrisName : Jname
    inferredFunctionType : InferredFunctionType
    scopes : JList Scope
    dynamicVariableCounter : Int
    jvmClassMethodName : Jname
    optimizedBody : NamedCExp

namespace AsmGlobalState
    public export
    data AsmGlobalState : Type where [external]

    public export
    %foreign
        "jvm:<init>(String java/util/Map io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState),io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState"
    prim_newAsmGlobalState : String -> Map String (FC, NamedDef) -> PrimIO AsmGlobalState

    public export
    newAsmGlobalState : HasIO io => String -> Map String (FC, NamedDef) -> io AsmGlobalState
    newAsmGlobalState programName fcAndDefinitionsByName =
      primIO $ prim_newAsmGlobalState programName fcAndDefinitionsByName

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".getAssembler"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String"
                "io/github/mmhelloworld/idrisjvm/assembler/Assembler"
    prim_getAssembler : AsmGlobalState -> String -> PrimIO Assembler

    public export
    getAssembler : HasIO io => AsmGlobalState -> String -> io Assembler
    getAssembler state name = primIO $ prim_getAssembler state name

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".getProgramName"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" "String"
    prim_getProgramName : AsmGlobalState -> PrimIO String

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".getFcAndDefinition"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String" "java/lang/Object"
    prim_getFcAndDefinition : AsmGlobalState -> String -> PrimIO (FC, NamedDef)

    public export
    getProgramName : HasIO io => AsmGlobalState -> io String
    getProgramName = primIO . prim_getProgramName

    public export
    getFcAndDefinition : HasIO io => AsmGlobalState -> String -> io (FC, NamedDef)
    getFcAndDefinition state name = primIO $ prim_getFcAndDefinition state name

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".addConstructor"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String" "void"
    prim_addConstructor : AsmGlobalState -> String -> PrimIO ()

    public export
    addConstructor : HasIO io => AsmGlobalState -> String -> io ()
    addConstructor state name = primIO $ prim_addConstructor state name

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".hasConstructor"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String" "boolean"
    prim_hasConstructor : AsmGlobalState -> String -> PrimIO Bool

    public export
    hasConstructor : HasIO io => AsmGlobalState -> String -> io Bool
    hasConstructor state name = primIO $ prim_hasConstructor state name

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".getFunction"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String" "java/lang/Object"
    prim_getFunction : AsmGlobalState -> String -> PrimIO Object

    public export
    getFunction : HasIO io => AsmGlobalState -> String -> io (Maybe Function)
    getFunction state name = (believe_me . nullableToMaybe) <$> (primIO $ prim_getFunction state name)

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".addFunction"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String java/lang/Object" "void"
    prim_jaddFunction : AsmGlobalState -> String -> Object -> PrimIO ()

    public export
    jaddFunction : HasIO io => AsmGlobalState -> String -> Function -> io ()
    jaddFunction state name function = primIO $ prim_jaddFunction state name (believe_me function)

    public export
    findFunction : HasIO io => AsmGlobalState -> Jname -> io (Maybe Function)
    findFunction globalState name = getFunction globalState (getSimpleName name)

    public export
    addFunction : HasIO io => AsmGlobalState -> Jname -> Function -> io ()
    addFunction globalState name function = jaddFunction globalState (getSimpleName name) function

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".classCodeEnd"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String String String" "void"
    prim_classCodeEnd : AsmGlobalState -> String -> String -> String -> PrimIO ()

    public export
    classCodeEnd : HasIO io => AsmGlobalState -> String -> String -> String -> io ()
    classCodeEnd state outputDirectory outputFile mainClass =
        primIO $ prim_classCodeEnd state outputDirectory outputFile mainClass

public export
record AsmState where
    constructor MkAsmState
    globalState : AsmGlobalState
    currentIdrisFunction : Function
    currentMethodName : Jname
    currentScopeIndex : Int
    scopeCounter : Int
    labelCounter : Int
    lambdaCounter : Int
    lineNumberLabels : Map Int String
    assembler : Assembler

public export
data Access = Private | Public | Protected | Static | Synthetic | Final | Interface | Abstract | Transient

export
Eq Access where
  Private == Private = True
  Public == Public = True
  Protected == Protected = True
  Static == Static = True
  Synthetic == Synthetic = True
  Final == Final = True
  Interface == Interface = True
  Abstract == Abstract = True
  Transient == Transient = True
  _ == _ = False

export
Show Access where
    show Private = "Private"
    show Public = "Public"
    show Protected = "Protected"
    show Static = "Static"
    show Synthetic = "Synthetic"
    show Final = "Final"
    show Interface = "Interface"
    show Abstract = "Abstract"
    show Transient = "Transient"

public export
data FieldInstructionType = GetStatic | PutStatic | GetField | PutField
data FrameType = Full | Same | Append

public export
data Constant = DoubleConst Double
              | IntegerConst Int
              | StringConst String
              | Bits64Const Bits64
              | Int64Const Int64
              | TypeConst String

public export
data InvocationType = InvokeInterface | InvokeSpecial | InvokeStatic | InvokeVirtual

export
Show InvocationType where
    show InvokeInterface = "InvokeInterface"
    show InvokeSpecial = "InvokeSpecial"
    show InvokeStatic = "InvokeStatic"
    show InvokeVirtual = "InvokeVirtual"

public export
data ClassOpts = ComputeMaxs | ComputeFrames

export
Show ClassOpts where
    show ComputeMaxs = "ComputeMaxs"
    show ComputeFrames = "ComputeFrames"

namespace HandleTag
    public export
    data HandleTag = GetField
                   | GetStatic
                   | PutField
                   | PutStatic
                   | InvokeVirtual
                   | InvokeStatic
                   | InvokeSpecial
                   | NewInvokeSpecial
                   | InvokeInterface

record Handle where
    constructor MkHandle
    tag : HandleTag
    className  : String
    methodName : String
    descriptor : String
    isInterface : Bool

data BsmArg = BsmArgGetType String | BsmArgHandle Handle

data FieldInitialValue = IntField Int | StringField String | DoubleField Double

mutual
    public export
    data AnnotationValue = AnnInt Int
                         | AnnBoolean Bool
                         | AnnChar Char
                         | AnnDouble Double
                         | AnnString String
                         | AnnClass String
                         | AnnEnum String String
                         | AnnArray (List AnnotationValue)
                         | AnnAnnotation Annotation

    public export
    AnnotationProperty : Type
    AnnotationProperty = (String, Asm.AnnotationValue)

    public export
    data Annotation = MkAnnotation String (List AnnotationProperty)

export
getStringAnnotationValue : AnnotationValue -> Maybe String
getStringAnnotationValue (AnnString value) = Just value
getStringAnnotationValue _ = Nothing

export
getStringAnnotationValues : AnnotationValue -> List String
getStringAnnotationValues (AnnArray values) = mapMaybe getStringAnnotationValue values
getStringAnnotationValues _ = []

getAnnotationValue : AnnotationValue -> Maybe Annotation
getAnnotationValue (AnnAnnotation annotation) = Just annotation
getAnnotationValue _ = Nothing

export
getAnnotationValues : AnnotationValue -> List Annotation
getAnnotationValues (AnnArray values) = mapMaybe getAnnotationValue values
getAnnotationValues _ = []

export
getParameterAnnotationValues : AnnotationValue -> List (List Annotation)
getParameterAnnotationValues (AnnArray values) = getAnnotationValues <$> values
getParameterAnnotationValues _ = []

export
getAnnotationProperties : Annotation -> List AnnotationProperty
getAnnotationProperties (MkAnnotation _ props) = props

export
%foreign "jvm:#JAVA_VERSION(int),io/github/mmhelloworld/idrisjvm/assembler/Assembler"
javaClassFileVersion : Int

export
Show Scope where
    show scope = showType "Scope" [
        ("index", show $ index scope),
        ("parentIndex", show $ parentIndex scope),
        ("nextVariableIndex", show $ nextVariableIndex scope),
        ("variableTypes", show $ unsafePerformIO $ Map.toList $ variableTypes scope),
        ("lineNumbers", show $ lineNumbers scope),
        ("variableIndices", toString $ variableIndices scope),
        ("returnType", show $ returnType scope),
        ("nextVariableIndex", show $ nextVariableIndex scope),
        ("childIndices", show $ childIndices scope)
    ]

export
Show Function where
    show function =
        showType "Function" [
            ("idrisName", show $ idrisName function),
            ("inferredFunctionType", show $ inferredFunctionType function),
            ("jvmClassMethodName", show $ jvmClassMethodName function),
            ("optimizedBody", show $ optimizedBody function)
        ]

export
Show AsmState where
    show asmState = showType "AsmState" [
        ("currentIdrisFunction", show $ currentIdrisFunction asmState),
        ("currentMethodName", show $ currentMethodName asmState),
        ("currentScopeIndex", show $ currentScopeIndex asmState),
        ("scopeCounter", show $ scopeCounter asmState),
        ("labelCounter", show $ labelCounter asmState),
        ("lambdaCounter", show $ lambdaCounter asmState)
    ]

public export
newAsmState : HasIO io => AsmGlobalState -> Assembler -> io AsmState
newAsmState globalState assembler = do
    let defaultName = Jqualified "" ""
    scopes <- ArrayList.new {elemTy=Scope}
    lineNumberLabels <- Map.newTreeMap {key=Int} {value=String}
    let function = MkFunction defaultName (MkInferredFunctionType IUnknown []) (believe_me scopes)
                    0 defaultName (NmCrash emptyFC "uninitialized function")
    pure $ MkAsmState globalState function defaultName 0 0 0 0 lineNumberLabels assembler

public export
%foreign "jvm:crash(String java/lang/Object),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
crash : String -> Object

public export
%foreign "jvm:getStackTraceString(String),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
getStackTraceString : PrimIO String

export
asmCrash : String -> Core a
asmCrash message = do
  stackTrace <- coreLift $ primIO getStackTraceString
  throw (InternalError $ message ++ "\n" ++ stackTrace)

export
isBoolTySpec : Name -> Bool
isBoolTySpec name = name == basics "Bool" || name == (NS preludeNS (UN $ Basic "Bool"))

mutual
  tySpecFn : String -> InferredFunctionType
  tySpecFn desc = case reverse $ toList $ String.split (== '⟶') desc of
    [] => assert_total $ idris_crash ("Invalid function type descriptor: " ++ desc)
    (returnTypeStr :: argsReversed) =>
      MkInferredFunctionType (tySpecStr returnTypeStr) (reverse $ (tySpecStr <$> argsReversed))

  tySpecLambda : String -> JavaLambdaType
  tySpecLambda desc = case toList $ String.split (== ',') desc of
    [intfStr, method, methodTypeStr, implementationTypeStr] =>
      MkJavaLambdaType (tySpecStr intfStr) method (tySpecFn methodTypeStr) (tySpecFn implementationTypeStr)
    _ => assert_total $ idris_crash ("Invalid lambda type descriptor: " ++ desc)

  tySpecStr : String -> InferredType
  tySpecStr "Int"      = IInt
  tySpecStr "Int8"     = IByte
  tySpecStr "Int16"    = IShort
  tySpecStr "Int32"    = IInt
  tySpecStr "Int64"    = ILong
  tySpecStr "Integer"  = inferredBigIntegerType
  tySpecStr "String"   = inferredStringType
  tySpecStr "Double"   = IDouble
  tySpecStr "Char"     = IChar
  tySpecStr "Bool"     = IBool
  tySpecStr "long"     = ILong
  tySpecStr "void"     = IVoid
  tySpecStr "%World"   = IInt
  tySpecStr "[" = assert_total $ idris_crash "Invalid type descriptor: ["
  tySpecStr "λ" = assert_total $ idris_crash "Invalid type descriptor: λ"
  tySpecStr desc         =
    cond [(startsWith desc "[", IArray (tySpecStr (assert_total (strTail desc)))),
          (startsWith desc "λ", IFunction (tySpecLambda (assert_total (strTail desc))))
         ]
         (iref desc [])

export
structName : Name
structName = NS (mkNamespace "System.FFI") (UN $ Basic "Struct")

export
arrayName : Name
arrayName = NS (mkNamespace "Java.Lang") (UN $ Basic "Array")

parseName : String -> Maybe InferredType
parseName name =
  case words name of
    (interfaceName :: methodName :: _) => Just $ IRef interfaceName Interface []
    (className :: []) => Just $ iref className []
    _ => Nothing

export
getJvmTypeDescriptor : InferredType -> String
getJvmTypeDescriptor IByte         = "B"
getJvmTypeDescriptor IChar         = "C"
getJvmTypeDescriptor IShort        = "S"
getJvmTypeDescriptor IBool         = "Z"
getJvmTypeDescriptor IDouble       = "D"
getJvmTypeDescriptor IFloat        = "F"
getJvmTypeDescriptor IInt          = "I"
getJvmTypeDescriptor ILong         = "J"
getJvmTypeDescriptor IVoid         = "V"
getJvmTypeDescriptor (IRef ty _ _) = "L" ++ ty ++ ";"
getJvmTypeDescriptor (IArray ty)   = "[" ++ getJvmTypeDescriptor ty
getJvmTypeDescriptor (IFunction lambdaType) = getJvmTypeDescriptor (lambdaType.javaInterface)
getJvmTypeDescriptor IUnknown            = getJvmTypeDescriptor inferredObjectType
getJvmTypeDescriptor (TypeParam name) = getJvmTypeDescriptor inferredObjectType

export
getJvmReferenceTypeName : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Core String
getJvmReferenceTypeName (IRef ty _ _) = pure ty
getJvmReferenceTypeName (IArray (IRef ty _ _)) = pure ("[L" ++ ty ++ ";")
getJvmReferenceTypeName (IArray ty) = pure ("[" ++ !(getJvmReferenceTypeName ty))
getJvmReferenceTypeName (IFunction lambdaType) = getJvmReferenceTypeName (lambdaType.javaInterface)
getJvmReferenceTypeName ty = asmCrash ("Expected a reference type but found " ++ show ty)

export
getSignature : InferredType -> String
getSignature (IRef ty _ typeParams@(_ :: _)) =
  let typeParamsSignature = concat (getSignature <$> typeParams)
  in "L" ++ ty ++ "<" ++ typeParamsSignature ++ ">;"
getSignature (TypeParam name) = "T" ++ name ++ ";"
getSignature (IArray ty)   = "[" ++ getSignature ty
getSignature type = getJvmTypeDescriptor type

-- constant values from org.objectweb.asm.Opcodes
export
accessNum : Access -> Int
accessNum Public    = 0x0001
accessNum Private   = 0x0002
accessNum Protected = 0x0004
accessNum Static    = 0x0008
accessNum Final     = 0x0010
accessNum Interface = 0x0200
accessNum Abstract  = 0x0400
accessNum Synthetic = 0x1000
accessNum Transient = 0x0080

export
fieldInsTypeNum : FieldInstructionType -> Int
fieldInsTypeNum GetStatic = 178
fieldInsTypeNum PutStatic = 179
fieldInsTypeNum GetField  = 180
fieldInsTypeNum PutField  = 181

export
frameTypeNum : FrameType -> Int
frameTypeNum Full   = 0
frameTypeNum Same   = 3
frameTypeNum Append = 1

export
invocTypeNum : InvocationType -> Int
invocTypeNum InvokeInterface = 185
invocTypeNum InvokeSpecial   = 183
invocTypeNum InvokeStatic    = 184
invocTypeNum InvokeVirtual   = 182

export
handleTagOpcode : HandleTag -> Int
handleTagOpcode GetField         = 1
handleTagOpcode GetStatic        = 2
handleTagOpcode PutField         = 3
handleTagOpcode PutStatic        = 4
handleTagOpcode InvokeVirtual    = 5
handleTagOpcode InvokeStatic     = 6
handleTagOpcode InvokeSpecial    = 7
handleTagOpcode NewInvokeSpecial = 8
handleTagOpcode InvokeInterface  = 9

%foreign
    jvm' "java/lang/Integer" "valueOf" "int" "java/lang/Integer"
export
integerValueOf : Int -> JInteger

%foreign
    jvm' "java/lang/Double" "valueOf" "double" "java/lang/Double"
export
doubleValueOf : Double -> JDouble

%foreign
    jvm' "java/lang/Long" "valueOf" "long" "java/lang/Long"
export
bits64ToJLong : Bits64 -> JLong

%foreign
    jvm' "java/lang/Long" "valueOf" "long" "java/lang/Long"
export
int64ToJLong : Int64 -> JLong

export
constantToObject : {auto stateRef: Ref AsmState AsmState} -> Constant -> Object
constantToObject (DoubleConst d) = believe_me $ doubleValueOf d
constantToObject (IntegerConst n) = believe_me $ integerValueOf n
constantToObject (Int64Const n) = believe_me $ int64ToJLong n
constantToObject (Bits64Const n) = believe_me $ bits64ToJLong n
constantToObject (StringConst str) = believe_me str
constantToObject (TypeConst str) = believe_me str

toJClassOpts : ClassOpts -> Int
toJClassOpts ComputeMaxs = 1
toJClassOpts ComputeFrames = 2

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/JHandle" "<init>"
        "int String String String boolean"
        "io/github/mmhelloworld/idrisjvm/assembler/JHandle"
prim_newJHandle : Int -> String -> String -> String -> Bool -> PrimIO JHandle

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/JBsmArg$JBsmArgHandle" "<init>"
        "io/github/mmhelloworld/idrisjvm/assembler/JHandle"
        "io/github/mmhelloworld/idrisjvm/assembler/JBsmArg$JBsmArgHandle"
prim_newJBsmArgHandle : JHandle -> PrimIO JBsmArgHandle

%inline
newJBsmArgHandle : HasIO io => JHandle -> io JBsmArgHandle
newJBsmArgHandle = primIO . prim_newJBsmArgHandle

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/JBsmArg$JBsmArgGetType" "<init>"
        "String"
        "io/github/mmhelloworld/idrisjvm/assembler/JBsmArg$JBsmArgGetType"
prim_newJBsmArgGetType : String -> PrimIO JBsmArgGetType

%inline
newJBsmArgGetType : HasIO io => String -> io JBsmArgGetType
newJBsmArgGetType = primIO . prim_newJBsmArgGetType

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnString" "<init>"
        "String"
        "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnString"
prim_newJAnnString : String -> PrimIO JAnnString

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnEnum" "<init>"
        "String String"
        "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnEnum"
prim_newJAnnEnum : String -> String -> PrimIO JAnnEnum

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnInt" "<init>"
        "int" "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnInt"
prim_newJAnnInt : Int -> PrimIO JAnnInt

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnBoolean" "<init>"
        "boolean" "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnBoolean"
prim_newJAnnBoolean : Bool -> PrimIO JAnnBoolean

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnChar" "<init>"
        "char" "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnChar"
prim_newJAnnChar : Char -> PrimIO JAnnChar

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnDouble" "<init>"
        "double" "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnDouble"
prim_newJAnnDouble : Double -> PrimIO JAnnDouble

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnClass" "<init>"
        "String" "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnClass"
prim_newJAnnClass : String -> PrimIO JAnnClass

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnAnnotation" "<init>"
        "io/github/mmhelloworld/idrisjvm/assembler/Annotation"
        "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnAnnotation"
prim_newJAnnAnnotation : JAnnotation -> PrimIO JAnnAnnotation

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnArray" "<init>"
        "java/util/List"
        "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnArray"
prim_newJAnnArray : JList JAnnotationValue -> PrimIO JAnnArray

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationProperty" "<init>"
        "String io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue"
        "io/github/mmhelloworld/idrisjvm/assembler/AnnotationProperty"
prim_newJAnnotationProperty : String -> JAnnotationValue -> PrimIO JAnnotationProperty

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/Annotation" "<init>"
        "String java/util/List"
        "io/github/mmhelloworld/idrisjvm/assembler/Annotation"
prim_newJAnnotation : String -> JList JAnnotationProperty -> PrimIO JAnnotation

export
toJHandle : HasIO io => Handle -> io JHandle
toJHandle (MkHandle tag hcname hmname hdesc hIsIntf) = do
    let tagNum = handleTagOpcode tag
    primIO $ prim_newJHandle tagNum hcname hmname hdesc hIsIntf

export
toJbsmArg : HasIO io => BsmArg -> io JBsmArg
toJbsmArg (BsmArgGetType desc) = believe_me <$> newJBsmArgGetType desc
toJbsmArg (BsmArgHandle handle) = do
    jhandle <- toJHandle handle
    believe_me <$> newJBsmArgHandle jhandle

mutual
    toJAnnotationValue : HasIO io => Asm.AnnotationValue -> io JAnnotationValue
    toJAnnotationValue (AnnString s) = believe_me <$> primIO (prim_newJAnnString s)
    toJAnnotationValue (AnnEnum enum s) = believe_me <$> primIO (prim_newJAnnEnum enum s)
    toJAnnotationValue (AnnInt n) = believe_me <$> primIO (prim_newJAnnInt n)
    toJAnnotationValue (AnnBoolean n) = believe_me <$> primIO (prim_newJAnnBoolean n)
    toJAnnotationValue (AnnChar n) = believe_me <$> primIO (prim_newJAnnChar n)
    toJAnnotationValue (AnnDouble n) = believe_me <$> primIO (prim_newJAnnDouble n)
    toJAnnotationValue (AnnClass n) = believe_me <$> primIO (prim_newJAnnClass n)
    toJAnnotationValue (AnnAnnotation n) = do
        jAnn <- toJAnnotation n
        believe_me <$> primIO (prim_newJAnnAnnotation jAnn)
    toJAnnotationValue (AnnArray values) =
        believe_me <$> primIO (prim_newJAnnArray $ subtyping !(traverse toJAnnotationValue values))

    toJAnnotationProperty : HasIO io => Asm.AnnotationProperty -> io JAnnotationProperty
    toJAnnotationProperty (name, annValue) = do
      jAnnotationValue <- toJAnnotationValue annValue
      primIO $ prim_newJAnnotationProperty name jAnnotationValue

    toJAnnotation : HasIO io => Asm.Annotation -> io JAnnotation
    toJAnnotation (MkAnnotation name props) = do
      properties <- traverse toJAnnotationProperty props
      primIO $ prim_newJAnnotation name $ believe_me properties

mutual
  asmAnnotationValue : AnnotationValue -> AnnotationValue
  asmAnnotationValue (AnnArray values) = AnnArray (asmAnnotationValue <$> values)
  asmAnnotationValue (AnnAnnotation annotation) = AnnAnnotation (asmAnnotation annotation)
  asmAnnotationValue value = value

  asmAnnotationProperty : (String, AnnotationValue) -> (String, AnnotationValue)
  asmAnnotationProperty (name, value) = (name, asmAnnotationValue value)

  export
  asmAnnotation : Annotation -> Annotation
  asmAnnotation (MkAnnotation name properties) =
    MkAnnotation ("L" ++ name ++ ";") (asmAnnotationProperty <$> properties)

export
toJFieldInitialValue : FieldInitialValue -> Object
toJFieldInitialValue (IntField n) = believe_me $ integerValueOf n
toJFieldInitialValue (StringField s) = believe_me s
toJFieldInitialValue (DoubleField d) = believe_me $ doubleValueOf d

export
getMethodDescriptor : InferredFunctionType -> String
getMethodDescriptor (MkInferredFunctionType retTy []) = "()" ++ getJvmTypeDescriptor retTy
getMethodDescriptor (MkInferredFunctionType retTy argTypes) =
    let argDescs = getJvmTypeDescriptor <$> argTypes
        retTyDesc = getJvmTypeDescriptor retTy
    in "(" ++ (the String $ concat argDescs) ++ ")" ++ retTyDesc

export
getMethodSignature : InferredFunctionType -> String
getMethodSignature (MkInferredFunctionType retTy []) = "()" ++ getSignature retTy
getMethodSignature (MkInferredFunctionType retTy argTypes) =
    let argDescs = getSignature <$> argTypes
        retTyDesc = getSignature retTy
    in "(" ++ (the String $ concat argDescs) ++ ")" ++ retTyDesc

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/IdrisName" "getIdrisFunctionName"
        "String String String" "io/github/mmhelloworld/idrisjvm/runtime/IdrisList"
jgetIdrisFunctionName : String -> String -> String -> List String

export
getIdrisFunctionName : String -> String -> String -> Jname
getIdrisFunctionName programName moduleName idrisFunctionName =
    case jgetIdrisFunctionName programName moduleName idrisFunctionName of
        (className :: functionName :: _) => Jqualified className functionName
        _ => Jqualified moduleName idrisFunctionName

%inline
metafactoryDesc : String
metafactoryDesc =
    "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

export
shouldDebugAsm : Bool
shouldDebugAsm =
    let shouldDebugProperty = fromMaybe "" $ unsafePerformIO (getEnv "IDRIS_JVM_DEBUG_ASM")
    in shouldDebugProperty == "true"

export
shouldDebug : Bool
shouldDebug =
    let shouldDebugProperty = fromMaybe "" $ unsafePerformIO (getEnv "IDRIS_JVM_DEBUG")
    in shouldDebugProperty /= "" && shouldDebugProperty /= "false"

export
debugFunction : String
debugFunction = fromMaybe "<all>" $ unsafePerformIO $ getEnv "IDRIS_JVM_DEBUG"

export
shouldDebugFunction : Jname -> Bool
shouldDebugFunction jname = shouldDebug && (debugFunction == "<all>" || (debugFunction `isInfixOf` (getSimpleName jname)))

namespace LocalDateTime
    data LocalDateTime : Type where [external]

    %foreign "jvm:now(java/lang/Object java/time/LocalDateTime),java/time/LocalDateTime"
    prim_now : PrimIO LocalDateTime

    %foreign jvm' "java/time/LocalDateTime" ".toString" "java/time/LocalDateTime" "String"
    prim_toString : LocalDateTime -> PrimIO String

    export
    currentTimeString : HasIO io => io String
    currentTimeString = do
        now <- primIO prim_now
        primIO $ prim_toString now

%foreign "jvm:getCurrentThreadName(java/lang/Object java/lang/String),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
prim_getCurrentThreadName : PrimIO String

export
getCurrentThreadName : HasIO io => io String
getCurrentThreadName = primIO prim_getCurrentThreadName

export
getJvmClassMethodName : String -> Name -> Jname
getJvmClassMethodName programName name =
    let jname = jvmName name
    in getIdrisFunctionName programName (className jname) (methodName jname)

export
createAsmStateJavaName : AsmGlobalState -> String -> IO AsmState
createAsmStateJavaName globalState name = do
  assembler <- getAssembler globalState name
  newAsmState globalState assembler

export
createAsmState : AsmGlobalState -> Name -> IO AsmState
createAsmState globalState name = do
    programName <- AsmGlobalState.getProgramName globalState
    let jvmClassMethodName = getJvmClassMethodName programName name
    createAsmStateJavaName globalState (className jvmClassMethodName)

%foreign jvm' "io/github/mmhelloworld/idrisjvm/runtime/Runtime" "waitForFuturesToComplete" "java/util/List" "void"
prim_waitForFuturesToComplete : List ThreadID -> PrimIO ()

export
waitForFuturesToComplete : List ThreadID -> IO ()
waitForFuturesToComplete futures = primIO $ prim_waitForFuturesToComplete futures

export
log : Lazy String -> (result : a) -> a
log message val =
  if shouldDebug
    then unsafePerformIO $ do
      time <- currentTimeString
      threadName <- getCurrentThreadName
      putStrLn (time ++ " [" ++ threadName ++ "]: " ++ message)
      pure val
    else val

export
logAsm : {auto stateRef: Ref AsmState AsmState} -> Lazy String -> Core ()
logAsm message = log message (pure ())

public export
data FArgList : Type where
     Nil : FArgList
     (::) : {a : Type} -> (1 arg : a) -> (1 args : FArgList) -> FArgList

export
%extern prim__jvmInstance : (ret : Type) -> String -> (1 args : FArgList) -> (1 x : %World) -> IORes ret

export %inline
jvmInstance : (ret : Type) -> String -> (1 args : FArgList) -> IO ret
jvmInstance ret fn args = fromPrim (prim__jvmInstance ret fn args)

export
superName : Name
superName = NS (mkNamespace "Java.Lang") (UN $ Basic "super")

export
isSuperCall : Name -> List NamedCExp -> Bool
isSuperCall name
  [(NmExtPrim fc f@(NS ns (UN (Basic "prim__jvmStatic"))) args@(ret :: NmPrimVal primFc (Str fn):: rest))]
  = name == superName && endsWith ".<init>" fn
isSuperCall _ _ = False

public export
%inline
methodStartLabel : String
methodStartLabel = "methodStartLabel"

public export
%inline
methodEndLabel : String
methodEndLabel = "methodEndLabel"

%foreign "jvm:.invokeMethod(io/github/mmhelloworld/idrisjvm/assembler/Assembler int String String String boolean void),io/github/mmhelloworld/idrisjvm/assembler/Assembler"
asmInvokeMethod : Assembler -> Int -> (className: String) -> (methodName: String) -> (descriptor: String) -> Bool -> PrimIO ()

%foreign "jvm:.classCodeStart"
asmClassCodeStart : Assembler -> (version: Int) -> (access: Int) -> (className: String) -> (sig: String)
                  -> (parent: String) -> (intf: JList String) -> (anns: JList JAnnotation) -> PrimIO ()

%foreign "jvm:.createClass"
asmCreateClass : Assembler -> Int -> PrimIO ()

%foreign "jvm:.createField"
asmCreateField : Assembler -> (access: Int) -> (sourceFileName: String) -> (className: String) -> (fieldName: String)
               -> (descriptor: String) -> (signature: String) -> (initialValue: Object)
               -> (annotations: JList JAnnotation) -> PrimIO ()

%foreign "jvm:.createMethod"
asmCreateMethod : Assembler -> (access: Int) -> (sourceFileName: String) -> (className: String) ->
                (methodName: String) -> (descriptor: String) ->
                (signature: String) -> (exceptions: JList String) ->
                (annotations: JList JAnnotation) ->
                (parameterAnnotations: JList (JList JAnnotation)) -> PrimIO ()

%foreign "jvm:.createIdrisConstructorClass(io/github/mmhelloworld/idrisjvm/assembler/Assembler String boolean int void),io/github/mmhelloworld/idrisjvm/assembler/Assembler"
asmCreateIdrisConstructorClass : Assembler -> String -> Bool -> Int -> PrimIO ()

%foreign "jvm:.field"
asmField : Assembler -> Int -> (className: String) -> (fieldName: String) -> (descriptor: String) -> PrimIO ()

%foreign "jvm:.frame"
asmFrame : Assembler -> Int -> Int -> (signatures: JList String) -> Int -> (signatures: JList String) -> PrimIO ()

%foreign "jvm:.localVariable"
asmLocalVariable : Assembler -> (name: String) -> (descriptor: String) -> (signature: String) -> (startLabel: String)
                 -> (endLabel: String) -> (index: Int) -> PrimIO ()

%foreign "jvm:.lookupSwitch"
asmLookupSwitch : Assembler -> (defaultLabel: String) -> (labels: JList String) -> (cases: JList Int) -> PrimIO ()

%foreign "jvm:.maxStackAndLocal"
asmMaxStackAndLocal : Assembler -> Int -> Int -> PrimIO ()

parameters {auto state: Ref AsmState AsmState}
  public export
  %inline
  aaload : Core ()

  public export
  %inline
  aastore : Core ()

  public export
  %inline
  aconstnull : Core ()

  public export
  %inline
  aload : Int -> Core ()

  public export
  %inline
  anewarray : (descriptor: String) -> Core ()

  public export
  %inline
  anewbooleanarray : Core ()

  public export
  %inline
  anewbytearray : Core ()

  public export
  %inline
  anewchararray : Core ()

  public export
  %inline
  anewshortarray : Core ()

  public export
  %inline
  anewintarray : Core ()

  public export
  %inline
  anewlongarray : Core ()

  public export
  %inline
  anewfloatarray : Core ()

  public export
  %inline
  anewdoublearray : Core ()

  public export
  %inline
  arraylength : Core ()

  public export
  %inline
  areturn : Core ()

  public export
  %inline
  astore : Int -> Core ()

  public export
  %inline
  baload : Core ()

  public export
  %inline
  bastore : Core ()

  public export
  %inline
  caload : Core ()

  public export
  %inline
  castore : Core ()

  public export
  %inline
  checkcast : (descriptor: String) -> Core ()

  public export
  %inline
  classCodeStart : Int -> List Access -> (className: String) -> (signature: Maybe String) -> (parentClassName: String) ->
                      (interfaces: List String) -> List Asm.Annotation -> Core ()

  public export
  %inline
  createClass : List ClassOpts -> Core ()

  public export
  %inline
  createField : List Access -> (sourceFileName: String) -> (className: String) -> (fieldName: String) -> (descriptor: String) ->
                  (signature: Maybe String) -> Maybe FieldInitialValue -> (annotations: List Asm.Annotation) -> Core ()

  public export
  %inline
  createLabel : String -> Core ()

  public export
  %inline
  createMethod : List Access -> (sourceFileName: String) -> (className: String) ->
                  (methodName: String) -> (descriptor: String) ->
                  (signature: Maybe String) -> (exceptions: Maybe (List String)) ->
                  (annotations: List Asm.Annotation) ->
                  (parameterAnnotations: List (List Asm.Annotation)) -> Core ()

  public export
  %inline
  createIdrisConstructorClass : String -> Bool -> Int -> Core ()

  public export
  %inline
  d2i : Core ()

  public export
  %inline
  d2f : Core ()

  public export
  %inline
  d2l : Core ()

  public export
  %inline
  dadd : Core ()

  public export
  %inline
  daload : Core ()

  public export
  %inline
  dastore : Core ()

  public export
  %inline
  dcmpg : Core ()

  public export
  %inline
  dcmpl : Core ()

  public export
  %inline
  dconst : Double -> Core ()

  public export
  %inline
  ddiv : Core ()

  public export
  %inline
  debug : String -> Core ()

  public export
  %inline
  dload : Int -> Core ()

  public export
  %inline
  dmul : Core ()

  public export
  %inline
  dneg : Core ()

  public export
  %inline
  drem : Core ()

  public export
  %inline
  dreturn : Core ()

  public export
  %inline
  dstore : Int -> Core ()

  public export
  %inline
  dsub : Core ()

  public export
  %inline
  dup : Core ()

  public export
  %inline
  f2d : Core ()

  public export
  %inline
  faload : Core ()

  public export
  %inline
  fastore : Core ()

  public export
  %inline
  fconst : Double -> Core ()

  public export
  %inline
  field : FieldInstructionType -> (className: String) -> (fieldName: String) -> (descriptor: String) -> Core ()

  public export
  %inline
  fieldEnd : Core ()

  public export
  %inline
  fload : Int -> Core ()

  public export
  %inline
  frame : FrameType -> Int -> (signatures: List String) -> Int -> (signatures: List String) -> Core ()

  public export
  %inline
  freturn : Core ()

  public export
  %inline
  fstore : Int -> Core ()

  public export
  %inline
  goto : (label: String) -> Core ()

  public export
  %inline
  i2b : Core ()

  public export
  %inline
  i2c : Core ()

  public export
  %inline
  i2d : Core ()

  public export
  %inline
  i2l : Core ()

  public export
  %inline
  i2s : Core ()

  public export
  %inline
  iadd : Core ()

  public export
  %inline
  iaload : Core ()

  public export
  %inline
  iand : Core ()

  public export
  %inline
  iastore : Core ()

  public export
  %inline
  ior : Core ()

  public export
  %inline
  ixor : Core ()

  public export
  %inline
  icompl : Core ()

  public export
  %inline
  iconst : Int -> Core ()

  public export
  %inline
  idiv : Core ()

  public export
  %inline
  ifeq : (label: String) -> Core ()

  public export
  %inline
  ifge : (label: String) -> Core ()

  public export
  %inline
  ifgt : (label: String) -> Core ()

  public export
  %inline
  ificmpge : (label: String) -> Core ()

  public export
  %inline
  ificmpgt : (label: String) -> Core ()

  public export
  %inline
  ificmple : (label: String) -> Core ()

  public export
  %inline
  ificmplt : (label: String) -> Core ()

  public export
  %inline
  ificmpeq : (label: String) -> Core ()

  public export
  %inline
  ifacmpne : (label: String) -> Core ()

  public export
  %inline
  ificmpne : (label: String) -> Core ()

  public export
  %inline
  ifle : (label: String) -> Core ()

  public export
  %inline
  iflt : (label: String) -> Core ()

  public export
  %inline
  ifne : (label: String) -> Core ()

  public export
  %inline
  ifnonnull : (label: String) -> Core ()

  public export
  %inline
  ifnull : (label: String) -> Core ()

  public export
  %inline
  iload : Int -> Core ()

  public export
  %inline
  imul : Core ()

  public export
  %inline
  ineg : Core ()

  public export
  %inline
  instanceOf : (className: String) -> Core ()

  public export
  %inline
  invokeMethod : InvocationType -> (className: String) -> (methodName: String) -> (descriptor: String)
                  -> Bool -> Core ()
  public export
  %inline
  invokeDynamic : (methodName: String) -> (descriptor: String) -> Handle -> List BsmArg -> Core ()

  public export
  %inline
  irem : Core ()

  public export
  %inline
  ireturn : Core ()

  public export
  %inline
  ishl : Core ()

  public export
  %inline
  ishr : Core ()

  public export
  %inline
  istore : Int -> Core ()

  public export
  %inline
  isub : Core ()

  public export
  %inline
  iushr : Core ()

  public export
  %inline
  l2d : Core ()

  public export
  %inline
  l2i : Core ()

  public export
  %inline
  labelStart : (label: String) -> Core ()

  public export
  %inline
  ladd : Core ()

  public export
  %inline
  laload : Core ()

  public export
  %inline
  land : Core ()

  public export
  %inline
  lastore : Core ()

  public export
  %inline
  lcmp : Core ()

  public export
  %inline
  lcompl : Core ()

  public export
  %inline
  ldc : Constant -> Core ()

  public export
  %inline
  ldiv : Core ()

  public export
  %inline
  lineNumber : Int -> String -> Core ()

  public export
  %inline
  lload : Int  -> Core ()

  public export
  %inline
  lmul : Core ()

  public export
  %inline
  lneg : Core ()

  public export
  %inline
  localVariable : (name: String) -> (descriptor: String) -> (signature: Maybe String) -> (startLabel: String) ->
                      (endLabel: String) -> (index: Int) -> Core ()

  public export
  %inline
  lookupSwitch : (defaultLabel: String) -> (labels: List String) -> (cases: List Int) -> Core ()

  public export
  %inline
  lor : Core ()

  public export
  %inline
  lrem : Core ()

  public export
  %inline
  lreturn : Core ()

  public export
  %inline
  lshl : Core ()

  public export
  %inline
  lshr : Core ()

  public export
  %inline
  lstore : Int -> Core ()

  public export
  %inline
  lsub : Core ()

  public export
  %inline
  lushr : Core ()

  public export
  %inline
  lxor : Core ()

  public export
  %inline
  maxStackAndLocal : Int -> Int -> Core ()

  public export
  %inline
  methodCodeStart : Core ()

  public export
  %inline
  methodCodeEnd : Core ()

  public export
  %inline
  multianewarray : (descriptor: String) -> Int -> Core ()

  public export
  %inline
  new : (className: String) -> Core ()

  public export
  %inline
  pop : Core ()

  public export
  %inline
  pop2 : Core ()

  public export
  %inline
  return : Core ()

  public export
  %inline
  saload : Core ()

  public export
  %inline
  sastore : Core ()

  public export
  %inline
  sourceInfo : (sourceFileName: String) -> Core ()

  public export
  %inline
  getState : Core AsmState

  public export
  %inline
  setState : AsmState -> Core ()

  aaload = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aaload" [assembler state]

  aastore = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aastore" [assembler state]

  aconstnull = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aconstnull" [assembler state]

  aload n = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aload" [assembler state, n]

  anewarray desc = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewarray" [assembler state, desc]
  anewintarray     = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewintarray" [assembler state]
  anewbooleanarray = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewbooleanarray" [assembler state]
  anewbytearray    = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewbytearray" [assembler state]
  anewchararray    = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewchararray" [assembler state]
  anewshortarray   = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewshortarray" [assembler state]
  anewlongarray    = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewlongarray" [assembler state]
  anewfloatarray   = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewfloatarray" [assembler state]
  anewdoublearray  = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewdoublearray" [assembler state]
  arraylength      = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.arraylength" [assembler state]
  areturn          = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.areturn" [assembler state]
  astore n       = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.astore" [assembler state, n]
  baload           = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.baload" [assembler state]
  bastore          = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.bastore" [assembler state]
  caload           = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.caload" [assembler state]
  castore          = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.castore" [assembler state]
  checkcast desc = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.checkcast" [assembler state, desc]

  classCodeStart version access className sig parent intf anns = do
    state <- get AsmState
    coreLift $ do
      janns <- sequence $ toJAnnotation <$> anns
      primIO $ asmClassCodeStart (assembler state) version (sum $ accessNum <$> access) className (maybeToNullable sig) parent
            (the (JList String) $ believe_me intf) (the (JList JAnnotation) $ believe_me janns)

  createClass opts = do
    state <- getState
    coreLift $ primIO $ asmCreateClass (assembler state) (sum $ toJClassOpts <$> opts)

  createField accs sourceFileName className fieldName desc sig fieldInitialValue anns = do
    state <- get AsmState
    coreLift $ do
      let jaccs = sum $ accessNum <$> accs
      janns <- sequence $ toJAnnotation <$> anns
      primIO $ asmCreateField
        (assembler state) jaccs sourceFileName className fieldName desc (maybeToNullable sig)
            (maybeToNullable (toJFieldInitialValue <$> fieldInitialValue)) (the (JList JAnnotation) $ believe_me janns)

  createLabel label = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createLabel" [assembler state, label]

  createMethod accs sourceFileName className methodName desc sig exceptions anns paramAnns = do
      state <- get AsmState
      put AsmState ({ currentMethodName := Jqualified className methodName } state)
      coreLift $ do
        let jaccs = sum $ accessNum <$> accs
        janns <- sequence $ toJAnnotation <$> anns
        jparamAnns <- sequence $ (\paramAnn => sequence $ toJAnnotation <$> paramAnn) <$> paramAnns
        primIO $ asmCreateMethod
            (assembler state) jaccs sourceFileName className methodName desc (maybeToNullable sig)
                (the (JList String) $ believe_me $ maybeToNullable exceptions)
                (the (JList JAnnotation) $ believe_me janns) (the (JList (JList JAnnotation)) $ believe_me jparamAnns)

  createIdrisConstructorClass className isStringConstructor constructorParameterCount = do
    state <- getState
    coreLift $ primIO $ asmCreateIdrisConstructorClass (assembler state) className isStringConstructor
      constructorParameterCount

  d2i = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.d2i" [assembler state]
  d2f = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.d2f" [assembler state]
  d2l = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.d2l" [assembler state]
  dadd = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dadd" [assembler state]
  dcmpg = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dcmpg" [assembler state]
  dcmpl = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dcmpl" [assembler state]
  dconst n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dconst" [assembler state, n]
  daload = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.daload" [assembler state]
  dastore = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dastore" [assembler state]
  ddiv = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ddiv" [assembler state]
  debug message = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.debug"
      [assembler state, message]
  dload n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dload" [assembler state, n]
  dmul = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dmul" [assembler state]
  dneg = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dneg" [assembler state]
  drem = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.drem" [assembler state]
  dreturn = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dreturn" [assembler state]
  dstore n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dstore" [assembler state, n]
  dsub = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dsub" [assembler state]
  dup = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dup" [assembler state]
  f2d = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.f2d" [assembler state]
  faload = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.faload" [assembler state]
  fastore = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fastore" [assembler state]
  fconst n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fconst" [assembler state, n]
  field finsType cname fname desc = do
    state <- get AsmState
    coreLift $ do
      let finsTypeNum = fieldInsTypeNum finsType
      primIO $ asmField (assembler state) finsTypeNum cname fname desc

  fieldEnd = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fieldEnd" [assembler state]

  fload n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fload" [assembler state, n]

  frame frameType nLocal localSigs nStack stackSigs = do
    state <- get AsmState
    coreLift $ do
      let ftypeNum = frameTypeNum frameType
      primIO $ asmFrame
        (assembler state) ftypeNum nLocal (the (JList String) $ believe_me localSigs) nStack
            (the (JList String) $ believe_me stackSigs)

  freturn = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.freturn" [assembler state]
  fstore n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fstore" [assembler state, n]

  goto label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.gotoLabel"
      [assembler state, label]

  i2b = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2b" [assembler state]
  i2c = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2c" [assembler state]
  i2d = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2d" [assembler state]
  i2l = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2l" [assembler state]
  i2s = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2s" [assembler state]
  iadd = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iadd" [assembler state]
  iaload = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iaload" [assembler state]
  iand = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iand" [assembler state]
  iastore = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iastore" [assembler state]
  ior = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ior" [assembler state]
  ixor = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ixor" [assembler state]
  icompl = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.icompl" [assembler state]
  iconst n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iconst" [assembler state, n]
  idiv = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.idiv" [assembler state]
  ifeq label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifeq" [assembler state, label]
  ifge label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifge" [assembler state, label]
  ifgt label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifgt" [assembler state, label]
  ificmpge label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpge" [assembler state, label]
  ificmpgt label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpgt" [assembler state, label]
  ificmple label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmple" [assembler state, label]
  ificmplt label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmplt" [assembler state, label]
  ificmpeq label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpeq" [assembler state, label]
  ifacmpne label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifacmpne" [assembler state, label]
  ificmpne label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpne" [assembler state, label]
  ifle label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifle" [assembler state, label]
  iflt label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iflt" [assembler state, label]
  ifne label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifne" [assembler state, label]
  ifnonnull label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifnonnull" [assembler state, label]
  ifnull label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifnull" [assembler state, label]
  iload n = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iload" [assembler state, n]
  imul = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.imul" [assembler state]
  ineg = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ineg" [assembler state]
  instanceOf className = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.instanceOf" [assembler state, className]
  invokeMethod invocType cname mname desc isIntf = do
    state <- get AsmState

    coreLift $ do
      let invocTypeAsm = invocTypeNum invocType
      primIO $ asmInvokeMethod (assembler state) invocTypeAsm cname mname desc isIntf

  invokeDynamic mname desc handle bsmArgs = do
    state <- get AsmState
    coreLift $ do
      jbsmArgsList <- sequence $ toJbsmArg <$> bsmArgs
      jhandle <- toJHandle handle
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.invokeDynamic"
        [assembler state, mname, desc, jhandle, the (JList JBsmArg) $ believe_me jbsmArgsList]

  irem = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.irem" [assembler state]
  ireturn = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ireturn" [assembler state]
  ishl = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ishl" [assembler state]
  ishr = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ishr" [assembler state]
  istore n = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.istore" [assembler state, n]
  isub = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.isub" [assembler state]
  iushr = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iushr" [assembler state]
  l2d = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.l2d" [assembler state]
  l2i = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.l2i" [assembler state]
  labelStart label = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.labelStart" [assembler state, label]
  ladd = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ladd" [assembler state]
  land = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.land" [assembler state]
  laload = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.laload" [assembler state]
  lastore = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lastore" [assembler state]
  lcmp = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lcmp" [assembler state]
  lor = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lor" [assembler state]
  lxor = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lxor" [assembler state]
  lcompl = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lcompl" [assembler state]

  ldc (TypeConst ty) = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ldcType" [assembler state, ty]
  ldc constant = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ldc" [assembler state, constantToObject constant]

  ldiv = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ldiv" [assembler state]

  lineNumber number label = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lineNumber" [assembler state, number, label]

  lload n = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lload" [assembler state, n]
  lmul = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lmul" [assembler state]
  lneg = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lneg" [assembler state]
  lookupSwitch defaultLabel labels cases = do
    state <- get AsmState
    coreLift $ do
      let jcases = integerValueOf <$> cases
      primIO $ asmLookupSwitch
        (assembler state) defaultLabel (the (JList String) $ believe_me labels) (the (JList Int) $ believe_me jcases)

  localVariable name descriptor signature startLabel endLabel index = do
    state <- get AsmState
    coreLift $ primIO $ asmLocalVariable
      (assembler state) name descriptor (maybeToNullable signature) startLabel endLabel index

  lrem = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lrem" [assembler state]
  lreturn = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lreturn" [assembler state]
  lshl = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lshl" [assembler state]
  lshr = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lshr" [assembler state]
  lstore n = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lstore" [assembler state, n]
  lsub = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lsub" [assembler state]
  lushr = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lushr" [assembler state]
  maxStackAndLocal stack local = do
    state <- getState
    coreLift $ primIO $ asmMaxStackAndLocal (assembler state) stack local
  methodCodeStart = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.methodCodeStart" [assembler state]
  methodCodeEnd = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.methodCodeEnd" [assembler state]
  multianewarray desc dims = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.multiANewArray" [assembler state, desc, dims]
  new cname = do
    state <- getState
    coreLift $
      jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.asmNew" [assembler state, cname]
  pop = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.pop" [assembler state]
  pop2 = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.pop2" [assembler state]
  return = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.voidReturn" [assembler state]
  saload = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.saload" [assembler state]
  sastore = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.sastore" [assembler state]
  sourceInfo sourceFileName = do
    state <- getState
    coreLift $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.sourceInfo" [assembler state, sourceFileName]

  getState = get AsmState
  setState newState = put AsmState newState

export
updateState : {auto stateRef: Ref AsmState AsmState} -> (AsmState -> AsmState) -> Core ()
updateState = update AsmState

getAndUpdateState : {auto stateRef: Ref AsmState AsmState} -> (AsmState -> AsmState) -> Core AsmState
getAndUpdateState f = do
    state <- getState
    updateState f
    pure state

export
loadBigInteger : {auto stateRef: Ref AsmState AsmState} -> Integer -> Core ()
loadBigInteger 0 = field GetStatic "java/math/BigInteger" "ZERO" "Ljava/math/BigInteger;"
loadBigInteger 1 = field GetStatic "java/math/BigInteger" "ONE" "Ljava/math/BigInteger;"
loadBigInteger 10 = field GetStatic "java/math/BigInteger" "TEN" "Ljava/math/BigInteger;"
loadBigInteger value = do
    new "java/math/BigInteger"
    dup
    ldc $ StringConst $ show value
    invokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False

export
asmInvokeDynamic : {auto stateRef: Ref AsmState AsmState} -> (implClassName: String) -> (implMethodName: String)
                 -> (interfaceMethodName: String) -> (invokeDynamicDesc: String) -> (samDesc: String)
                 -> (implMethodDesc: String) -> (instantiatedMethodDesc: String) -> Core ()
asmInvokeDynamic implClassName implMethodName interfaceMethodName invokeDynamicDesc samDesc implMethodDesc
    instantiatedMethodDesc =
    let metafactoryHandle = MkHandle InvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory"
            metafactoryDesc False
        implMethodHandle = MkHandle InvokeStatic implClassName implMethodName implMethodDesc False
        metafactoryArgs = [ BsmArgGetType samDesc
                        , BsmArgHandle implMethodHandle
                        , BsmArgGetType instantiatedMethodDesc
                        ]
    in invokeDynamic interfaceMethodName invokeDynamicDesc metafactoryHandle metafactoryArgs

export
newBigInteger : {auto stateRef: Ref AsmState AsmState} -> String -> Core ()
newBigInteger "0" = field GetStatic "java/math/BigInteger" "ZERO" "Ljava/math/BigInteger;"
newBigInteger "1" = field GetStatic "java/math/BigInteger" "ONE" "Ljava/math/BigInteger;"
newBigInteger "10" = field GetStatic "java/math/BigInteger" "TEN" "Ljava/math/BigInteger;"
newBigInteger i = do
    new "java/math/BigInteger"
    dup
    ldc $ StringConst i
    invokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False

export
getGlobalState : {auto stateRef: Ref AsmState AsmState} -> Core AsmGlobalState
getGlobalState = pure $ globalState !getState

export
findFunction : {auto stateRef: Ref AsmState AsmState} -> Jname -> Core (Maybe Function)
findFunction name = coreLift $ AsmGlobalState.findFunction !getGlobalState name

export
getFunction : {auto stateRef: Ref AsmState AsmState} -> Jname -> Core Function
getFunction name = maybe (asmCrash $ "Unknown function " ++ show name) pure!(findFunction name)

export
getCurrentFunction : {auto stateRef: Ref AsmState AsmState} -> Core Function
getCurrentFunction = currentIdrisFunction <$> getState

export
getProgramName : {auto stateRef: Ref AsmState AsmState} -> Core String
getProgramName = coreLift $ AsmGlobalState.getProgramName !getGlobalState

export
getFcAndDefinition : {auto stateRef: Ref AsmState AsmState} -> String -> Core (FC, NamedDef)
getFcAndDefinition name = coreLift $ AsmGlobalState.getFcAndDefinition !getGlobalState name

export
setCurrentFunction : {auto stateRef: Ref AsmState AsmState} -> Function -> Core ()
setCurrentFunction function = updateState $ { currentIdrisFunction := function }

getAndUpdateFunction : {auto stateRef: Ref AsmState AsmState} -> (Function -> Function) -> Core Function
getAndUpdateFunction f = do
    function <- getCurrentFunction
    let newFunction = f function
    setCurrentFunction newFunction
    globalState <- getGlobalState
    coreLift $ addFunction globalState (idrisName newFunction) newFunction
    pure function

export
updateCurrentFunction : {auto stateRef: Ref AsmState AsmState} -> (Function -> Function) -> Core ()
updateCurrentFunction f = ignore $ getAndUpdateFunction f

export
loadFunction : {auto stateRef: Ref AsmState AsmState} -> Jname -> Core ()
loadFunction idrisName = do
    function <- getFunction idrisName
    updateState $ { currentIdrisFunction := function }

export
getFunctionType : {auto stateRef: Ref AsmState AsmState} -> Jname -> Core InferredFunctionType
getFunctionType name = inferredFunctionType <$> (getFunction name)

export
getFunctionParameterTypes : {auto stateRef: Ref AsmState AsmState} -> Jname -> Core (List InferredType)
getFunctionParameterTypes functionName = do
    functionType <- getFunctionType functionName
    pure $ parameterTypes functionType

export
findFunctionType : {auto stateRef: Ref AsmState AsmState} -> Jname -> Core (Maybe InferredFunctionType)
findFunctionType functionName = do
    state <- getState
    function <- findFunction functionName
    pure $ inferredFunctionType <$> function

export
getFunctionReturnType : {auto stateRef: Ref AsmState AsmState} -> Jname -> Core InferredType
getFunctionReturnType functionName =  do
    state <- getState
    function <- findFunction functionName
    pure $ maybe IUnknown (returnType . inferredFunctionType) $ function

export
getCurrentScopeIndex : {auto stateRef: Ref AsmState AsmState} -> Core Int
getCurrentScopeIndex = currentScopeIndex <$> getState

export
updateCurrentScopeIndex : {auto stateRef: Ref AsmState AsmState} -> Int -> Core ()
updateCurrentScopeIndex scopeIndex = updateState $ { currentScopeIndex := scopeIndex }

export
newScopeIndex : {auto stateRef: Ref AsmState AsmState} -> Core Int
newScopeIndex = scopeCounter <$> (getAndUpdateState $ {scopeCounter $= (+1)})

export
newDynamicVariableIndex : {auto stateRef: Ref AsmState AsmState} -> Core Int
newDynamicVariableIndex = dynamicVariableCounter <$> (getAndUpdateFunction $ {dynamicVariableCounter $= (+1)})

export
resetScope : {auto stateRef: Ref AsmState AsmState} -> Core ()
resetScope = updateState $
    {
        scopeCounter := 0,
        currentScopeIndex := 0
    }

fillNull : (HasIO io, Inherits list (JList a)) => Int -> list -> io ()
fillNull index aList = do
    let list = the (JList a) $ believe_me aList
    size <- Collection.size {elemTy=a,obj=Collection a} $ believe_me list
    nulls <- JList.nCopies {a=a} (index - size) nullValue
    ignore $ JList.addAll {a=a, obj=Collection a} list $ believe_me nulls

export
saveScope : {auto stateRef: Ref AsmState AsmState} -> Scope -> Core ()
saveScope scope = do
    scopes <- scopes <$> getCurrentFunction
    size <- coreLift $ Collection.size {elemTy=Scope, obj=Collection Scope} $ believe_me scopes
    let scopeIndex = index scope
    coreLift $
      if scopeIndex < size
          then ignore $ JList.set scopes scopeIndex scope
          else do
              fillNull {a=Scope} scopeIndex scopes
              JList.add scopes scopeIndex scope

export
getScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Core Scope
getScope scopeIndex = do
   scopes <- scopes <$> getCurrentFunction
   coreLift $ JList.get scopes scopeIndex

export
addScopeChild : {auto stateRef: Ref AsmState AsmState} -> Int -> Int -> Core ()
addScopeChild parentScopeIndex childScopeIndex = do
    scope <- getScope parentScopeIndex
    saveScope $ {childIndices $= (childScopeIndex ::)} scope

export
getRootMethodName : {auto stateRef: Ref AsmState AsmState} -> Core Jname
getRootMethodName = jvmClassMethodName <$> getCurrentFunction

export
newLabel : {auto stateRef: Ref AsmState AsmState} -> Core String
newLabel = do
    state <- getState
    let label = "L" ++ show (labelCounter state)
    updateState $ { labelCounter $= (+1) }
    pure label

hasLabelAtLine : {auto stateRef: Ref AsmState AsmState} -> Int -> Core Bool
hasLabelAtLine lineNumber = do
    state <- getState
    coreLift $ Map.containsKey {value=String} (lineNumberLabels state) lineNumber

export
addLineNumber : {auto stateRef: Ref AsmState AsmState} -> Int -> String -> Core ()
addLineNumber number label = do
    hasLabel <- hasLabelAtLine number
    when (not hasLabel) $ do
        state <- getState
        lineNumber number label
        ignore $ coreLift $ Map.put (lineNumberLabels state) number label

export
getLineNumberLabel : {auto stateRef: Ref AsmState AsmState} -> Int -> Core String
getLineNumberLabel lineNumber = do
    state <- getState
    let currentLineNumberLabels = lineNumberLabels state
    optLabel <- coreLift $ Map.get {value=String} currentLineNumberLabels lineNumber
    case nullableToMaybe optLabel of
        Just label => pure label
        Nothing => do
            label <- newLabel
            _ <- coreLift $ Map.put currentLineNumberLabels lineNumber label
            pure label

export
getClassName : {auto stateRef: Ref AsmState AsmState} -> Core String
getClassName = className . currentMethodName <$> getState

export
getMethodName : {auto stateRef: Ref AsmState AsmState} -> Core String
getMethodName = methodName . currentMethodName <$> getState

export
freshLambdaIndex : {auto stateRef: Ref AsmState AsmState} -> Core Int
freshLambdaIndex = lambdaCounter <$> (getAndUpdateState $ {lambdaCounter $= (+1)})

export
setScopeCounter : {auto stateRef: Ref AsmState AsmState} -> Int -> Core ()
setScopeCounter scopeCounter = updateState $ {scopeCounter := scopeCounter}

export
updateScopeStartLabel : {auto stateRef: Ref AsmState AsmState} -> Int -> String -> Core ()
updateScopeStartLabel scopeIndex label = do
    scope <- getScope scopeIndex
    saveScope $ {labels $= updateFirst label} scope

export
updateScopeEndLabel : {auto stateRef: Ref AsmState AsmState} -> Int -> String -> Core ()
updateScopeEndLabel scopeIndex label = do
    scope <- getScope scopeIndex
    saveScope $ {labels $= updateSecond label} scope

export
createVariable : {auto stateRef: Ref AsmState AsmState} -> String -> Core ()
createVariable var = do
    scopeIndex <- getCurrentScopeIndex
    scope <- getScope scopeIndex
    let variableIndex = nextVariableIndex scope
    _ <- coreLift $ Map.put (variableTypes scope) var IUnknown
    _ <- coreLift $ Map.put (variableIndices scope) var variableIndex
    saveScope $ { nextVariableIndex $= (+1) } scope

export
generateVariable : {auto stateRef: Ref AsmState AsmState} -> String -> Core String
generateVariable namePrefix = do
    dynamicVariableIndex <- newDynamicVariableIndex
    let variableName = namePrefix ++ show dynamicVariableIndex
    createVariable variableName
    pure variableName

namespace JAsmState
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmState" "updateVariableIndices" "java/util/Map java/util/Map" "void"
    prim_updateVariableIndices : Map key value -> Map key value -> PrimIO ()

    export
    updateVariableIndices : HasIO io => Map String Int -> Map String Int -> io ()
    updateVariableIndices resultIndicesByName indicesByName =
        primIO $ prim_updateVariableIndices resultIndicesByName indicesByName

    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmState" "getVariableNames" "java/util/Map" "java/util/List"
    prim_getVariableNames : Map key value -> PrimIO (JList key)

    export
    getVariableNames : HasIO io => Map String Int -> io (List String)
    getVariableNames indicesByName = do
        jlist <- primIO $ prim_getVariableNames indicesByName
        JList.fromIterable jlist

retrieveVariableIndicesByName : {auto stateRef: Ref AsmState AsmState} -> Int -> Core (Map String Int)
retrieveVariableIndicesByName scopeIndex = do
    variableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
    go variableIndices scopeIndex
    pure variableIndices
  where
    go : Map String Int -> Int -> Core ()
    go acc scopeIndex = go1 scopeIndex where
        go1 : Int -> Core ()
        go1 scopeIndex = do
            scope <- getScope scopeIndex
            coreLift $ updateVariableIndices acc (variableIndices scope)
            let Just nextScopeIndex = parentIndex scope
                  | Nothing => pure ()
            go1 nextScopeIndex

isParameter : {auto stateRef: Ref AsmState AsmState} -> String -> Core Bool
isParameter name = do
  scope <- getScope 0
  optIndex <- coreLift $ Map.get {value=Int} (variableIndices scope) name
  case nullableToMaybe optIndex of
    Nothing => pure False
    Just index => do
      function <- getCurrentFunction
      pure (index < cast (length (parameterTypes (inferredFunctionType function))))

export
retrieveVariables : {auto stateRef: Ref AsmState AsmState} -> Int -> Core (List String)
retrieveVariables scopeIndex = do
    variableIndicesByName <- retrieveVariableIndicesByName scopeIndex
    coreLift $ getVariableNames variableIndicesByName

retrieveVariableIndexAtScope : {auto stateRef: Ref AsmState AsmState} -> Int -> String -> Core Int
retrieveVariableIndexAtScope currentScopeIndex name = go currentScopeIndex where
    go : Int -> Core Int
    go scopeIndex = do
        scope <- getScope scopeIndex
        optIndex <- coreLift $ Map.get {value=Int} (variableIndices scope) name
        case nullableToMaybe optIndex of
            Just index => pure index
            Nothing => case parentIndex scope of
                Just parentScopeIndex => go parentScopeIndex
                Nothing => do
                  rootMethodName <- getRootMethodName
                  throw $ GenericMsg emptyFC
                    ("retrieveVariableIndexAtScope: " ++ show rootMethodName ++ ": Unknown var " ++
                      name ++ " at index " ++ show currentScopeIndex)

export
retrieveVariableIndex : {auto stateRef: Ref AsmState AsmState} -> String -> Core Int
retrieveVariableIndex name = retrieveVariableIndexAtScope !getCurrentScopeIndex name

retrieveVariableTypeAtScope : {auto stateRef: Ref AsmState AsmState} -> Int -> String -> Core InferredType
retrieveVariableTypeAtScope scopeIndex name = do
    scope <- getScope scopeIndex
    optTy <- coreLift $ Map.get (variableTypes scope) name
    case nullableToMaybe optTy of
        Just ty => pure ty
        Nothing => case parentIndex scope of
            Just parentScope => retrieveVariableTypeAtScope parentScope name
            Nothing => pure IUnknown

export
retrieveVariableTypesAtScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Core (Map Int InferredType)
retrieveVariableTypesAtScope scopeIndex = do
    typesByIndex <- coreLift $ Map.newTreeMap {key=Int} {value=InferredType}
    go typesByIndex !(retrieveVariables scopeIndex)
    pure typesByIndex
  where
    go : Map Int InferredType -> List String -> Core ()
    go acc names = go1 names where
        go1 : List String -> Core ()
        go1 [] = pure ()
        go1 (var :: vars) = do
            varIndex <- retrieveVariableIndexAtScope scopeIndex var
            ty <- retrieveVariableTypeAtScope scopeIndex var
            hasVar <- coreLift $ containsKey {value=InferredType} acc varIndex
            when (not hasVar) $ coreLift $ do
                oldTy <- Map.put acc varIndex ty
                pure ()
            go1 vars

export
getVariableIndicesByName : {auto stateRef: Ref AsmState AsmState} -> Int -> Core (Map String Int)
getVariableIndicesByName scopeIndex = allVariableIndices <$> getScope scopeIndex

export
getVariableIndexAtScope : {auto stateRef: Ref AsmState AsmState} -> Int -> String -> Core Int
getVariableIndexAtScope currentScopeIndex name = do
    variableIndicesByName <- getVariableIndicesByName currentScopeIndex
    optIndex <- coreLift $ Map.get {value=Int} variableIndicesByName name
    case nullableToMaybe optIndex of
        Just index => pure index
        Nothing => do
          rootMethodName <- getRootMethodName
          asmCrash ("getVariableIndexAtScope: " ++ show rootMethodName ++ ": Unknown var " ++
              name ++ " at index " ++ show currentScopeIndex)

export
getVariableIndex : {auto stateRef: Ref AsmState AsmState} -> String -> Core Int
getVariableIndex name = getVariableIndexAtScope !getCurrentScopeIndex name

export
getVariableTypesAtScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Core (Map Int InferredType)
getVariableTypesAtScope scopeIndex = allVariableTypes <$> getScope scopeIndex

export
getVariableTypes : {auto stateRef: Ref AsmState AsmState} -> Core (Map Int InferredType)
getVariableTypes = getVariableTypesAtScope !getCurrentScopeIndex

export
getVariableTypeAtScope : {auto stateRef: Ref AsmState AsmState} -> Int -> String -> Core InferredType
getVariableTypeAtScope scopeIndex name = do
    scope <- getScope scopeIndex
    variableIndicesByName <- getVariableIndicesByName scopeIndex
    optIndex <- coreLift $ Map.get {value=Int} variableIndicesByName name
    case nullableToMaybe optIndex of
        Just index => do
            variableTypes <- getVariableTypesAtScope scopeIndex
            optTy <- coreLift $ Map.get {value=InferredType} variableTypes index
            pure $ fromMaybe IUnknown $ nullableToMaybe optTy
        Nothing => pure IUnknown

export
getVariableType : {auto stateRef: Ref AsmState AsmState} -> String -> Core InferredType
getVariableType name = getVariableTypeAtScope !getCurrentScopeIndex name

export
updateScopeVariableTypes : {auto stateRef: Ref AsmState AsmState} -> Core ()
updateScopeVariableTypes = go (scopeCounter !getState - 1) where
    go : Int -> Core ()
    go scopeIndex =
        if scopeIndex < 0 then pure ()
        else do
            variableTypes <- retrieveVariableTypesAtScope scopeIndex
            variableIndices <- retrieveVariableIndicesByName scopeIndex
            scope <- getScope scopeIndex
            saveScope $ {allVariableTypes := variableTypes, allVariableIndices := variableIndices} scope
            go (scopeIndex - 1)

getVariableScope : {auto stateRef: Ref AsmState AsmState} -> String -> Core Scope
getVariableScope name = go !getCurrentScopeIndex where
    go : Int -> Core Scope
    go scopeIndex = do
        scope <- getScope scopeIndex
        optTy <- coreLift $ Map.get {value=InferredType} (variableTypes scope) name
        case nullableToMaybe optTy of
            Just _ => pure scope
            Nothing => case parentIndex scope of
                Just parentScopeIndex => go parentScopeIndex
                Nothing => do
                  let functionName = idrisName !getCurrentFunction
                  asmCrash ("Unknown variable \{name} in function \{show functionName}")

export
addVariableType : {auto stateRef: Ref AsmState AsmState} -> String -> InferredType -> Core ()
addVariableType _ IUnknown = pure ()
addVariableType var ty = when (not !(isParameter var)) $ do
    scope <- getVariableScope var
    ignore $ coreLift $ Map.put (variableTypes scope) var ty

export
retrieveVariableType : {auto stateRef: Ref AsmState AsmState} -> String -> Core InferredType
retrieveVariableType var = do
  scope <- getVariableScope var
  let scopeIndex = index scope
  retrieveVariableTypeAtScope scopeIndex var

%inline
export
lambdaMaxCountPerMethod: Int
lambdaMaxCountPerMethod = 50

export
getLambdaImplementationMethodName : {auto stateRef: Ref AsmState AsmState} -> String -> Core Jname
getLambdaImplementationMethodName namePrefix = do
    lambdaIndex <- freshLambdaIndex
    rootMethodJname <- getRootMethodName
    let declaringMethodName = methodName rootMethodJname
    let rootMethodClassName = className rootMethodJname
    let lambdaClassName =
        if lambdaIndex >= lambdaMaxCountPerMethod
            then rootMethodClassName ++ "$" ++ namePrefix ++ "$" ++ declaringMethodName ++ "$" ++ show (lambdaIndex `div` 100)
            else rootMethodClassName
    let lambdaMethodName =
        if lambdaIndex >= lambdaMaxCountPerMethod
            then namePrefix ++ "$" ++ show lambdaIndex
            else namePrefix ++ "$" ++ declaringMethodName ++ "$" ++ show lambdaIndex
    pure $ Jqualified lambdaClassName lambdaMethodName

mutual
  parseArrayType : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core (Maybe InferredType)
  parseArrayType expr@(NmCon _ name _ _ [elemTy]) =
    if name == arrayName then pure. Just $ IArray !(tySpec elemTy)
    else pure Nothing
  parseArrayType _ = pure Nothing

  parseLambdaType : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core (Maybe InferredType)
  parseLambdaType (NmCon _ name _ _ [interfaceType, _]) =
    if name == builtin "Pair" then parseJvmReferenceType interfaceType
    else pure Nothing
  parseLambdaType _ = pure Nothing

  parseJvmReferenceType : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core (Maybe InferredType)
  parseJvmReferenceType (NmCon _ name _ _ (NmPrimVal _ (Str namePartsStr) :: _)) =
    if name == structName
      then pure $ parseName namePartsStr
      else pure Nothing
  parseJvmReferenceType (NmCon _ name conInfo tag args) =
      if name == primio "IORes" then
        maybe (asmCrash "Expected an argument for IORes") (\res => pure $ Just !(tySpec res)) (head' args)
      else pure $ Just $ getIdrisConstructorType name
    where
      getIdrisConstructorType : Name -> InferredType
      getIdrisConstructorType name =
        if isBoolTySpec name then IBool
        else if name == basics "List" then idrisListType
        else if name == preludetypes "Maybe" then idrisMaybeType
        else if name == preludetypes "Nat" then inferredBigIntegerType
        else inferredObjectType

  parseJvmReferenceType (NmApp fc (NmRef _ name) _) = do
    (_, MkNmFun _ def) <- getFcAndDefinition (jvmSimpleName name)
      | _ => asmCrash ("Expected a function returning a tuple containing interface type and method type at " ++
               show fc)
    ty <- tySpec def
    pure $ Just ty
  parseJvmReferenceType (NmDelay _ _ expr) = pure $ Just !(tySpec expr)
  parseJvmReferenceType expr = pure Nothing

  tryParse : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core (Maybe InferredType)
  tryParse expr = do
    arrayTypeMaybe <- parseArrayType expr
    case arrayTypeMaybe of
      Nothing => do
        lambdaTypeMaybe <- parseLambdaType expr
        case lambdaTypeMaybe of
          Nothing => parseJvmReferenceType expr
          Just lambdaType => pure $ Just lambdaType
      Just arrayType => pure $ Just arrayType

  export
  tySpec : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core InferredType
  tySpec (NmCon _ (UN (Basic ty)) _ _ []) = pure $ tySpecStr ty
  tySpec (NmCon _ _ NOTHING _ []) = pure idrisMaybeType
  tySpec (NmCon _ _ JUST _ [_]) = pure idrisMaybeType
  tySpec (NmCon _ _ NIL _ []) = pure idrisListType
  tySpec (NmCon _ _ CONS _ [_, _]) = pure idrisListType
  tySpec expr@(NmCon _ (NS _ (UN (Basic "Unit"))) _ _ []) = pure IVoid
  tySpec expr = do
    ty <- tryParse expr
    pure $ fromMaybe inferredObjectType ty

export
asmReturn : {auto stateRef: Ref AsmState AsmState} -> InferredType -> Core ()
asmReturn IVoid    = return
asmReturn IBool    = ireturn
asmReturn IByte    = ireturn
asmReturn IShort   = ireturn
asmReturn IInt     = ireturn
asmReturn IChar    = ireturn
asmReturn ILong    = lreturn
asmReturn IFloat   = freturn
asmReturn IDouble  = dreturn
asmReturn _        = areturn

export
runAsm : AsmState -> (Ref AsmState AsmState -> Core a) -> IO a
runAsm asmState action = coreRun (do ref <- newRef AsmState asmState
                                     put AsmState asmState
                                     action ref)
                                 (\err: Error => do printLn err
                                                    exitWith (ExitFailure 1))
                                 pure
