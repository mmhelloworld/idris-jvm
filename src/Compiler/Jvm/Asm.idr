module Compiler.Jvm.Asm

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.Reflect
import Core.TT

import Data.List
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

%hide Debug.Trace.toString

public export
data Assembler : Type where [external]

data JAnnotation : Type where [external]
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

  goFromList : HasIO io => Map key value -> List (key, value) -> io ()
  goFromList _ [] = pure ()
  goFromList m ((k, v) :: rest) = do
    _ <- Map.put m k v
    goFromList m rest

  export
  fromList : HasIO io => List (key, value) -> io (Map key value)
  fromList keyValues = do
      m <- Map.newTreeMap {key=key} {value=value}
      goFromList m keyValues
      pure m

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

    export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".isUntypedFunction"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String" "boolean"
    prim_jisUntypedFunction : AsmGlobalState -> String -> PrimIO Bool

    jisUntypedFunction : HasIO io => AsmGlobalState -> String -> io Bool
    jisUntypedFunction state name = primIO $ prim_jisUntypedFunction state name

    public export
    isUntypedFunction : HasIO io => AsmGlobalState -> Jname -> io Bool
    isUntypedFunction globalState name = jisUntypedFunction globalState (getSimpleName name)

    public export
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".addUntypedFunction"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String" "void"
    prim_jaddUntypedFunction : AsmGlobalState -> String -> PrimIO ()

    public export
    jaddUntypedFunction : HasIO io => AsmGlobalState -> String -> io ()
    jaddUntypedFunction state name = primIO $ prim_jaddUntypedFunction state name

    public export
    addUntypedFunction : HasIO io => AsmGlobalState -> Jname -> io ()
    addUntypedFunction globalState name = jaddUntypedFunction globalState (getSimpleName name)

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

public export
data Asm : Type -> Type where
    Aaload : Asm ()
    Aastore : Asm ()
    Aconstnull : Asm ()
    Aload : Int -> Asm ()
    Anewarray : (descriptor: String) -> Asm ()

    Anewbooleanarray : Asm ()
    Anewbytearray : Asm ()
    Anewchararray : Asm ()
    Anewshortarray : Asm ()
    Anewintarray : Asm ()
    Anewlongarray : Asm ()
    Anewfloatarray : Asm ()
    Anewdoublearray : Asm ()

    Arraylength : Asm ()
    Areturn : Asm ()
    Astore : Int -> Asm ()
    Baload : Asm ()
    Bastore : Asm ()
    Caload : Asm ()
    Castore : Asm ()
    Checkcast : (descriptor: String) -> Asm ()
    ClassCodeStart : Int -> List Access -> (className: String) -> (signature: Maybe String) -> (parentClassName: String) ->
                        (interfaces: List String) -> List Asm.Annotation -> Asm ()
    CreateClass : List ClassOpts -> Asm ()
    CreateField : List Access -> (sourceFileName: String) -> (className: String) -> (fieldName: String) -> (descriptor: String) ->
                    (signature: Maybe String) -> Maybe FieldInitialValue -> (annotations: List Asm.Annotation) -> Asm ()
    CreateLabel : String -> Asm ()
    CreateMethod : List Access -> (sourceFileName: String) -> (className: String) ->
                    (methodName: String) -> (descriptor: String) ->
                    (signature: Maybe String) -> (exceptions: Maybe (List String)) ->
                    (annotations: List Asm.Annotation) ->
                    (parameterAnnotations: List (List Asm.Annotation)) -> Asm ()
    CreateIdrisConstructorClass : String -> Bool -> Int -> Asm ()
    D2i : Asm ()
    D2f : Asm ()
    D2l : Asm ()
    Dadd : Asm ()
    Daload : Asm ()
    Dastore : Asm ()
    Dcmpg : Asm ()
    Dcmpl : Asm ()
    Dconst : Double -> Asm ()
    Ddiv : Asm ()
    Debug : String -> Asm ()
    Dload : Int -> Asm ()
    Dmul : Asm ()
    Dneg : Asm ()
    Drem : Asm ()
    Dreturn : Asm ()
    Dstore : Int -> Asm ()
    Dsub : Asm ()
    Dup : Asm ()
    Error : String -> Asm ()
    F2d : Asm ()
    Faload : Asm ()
    Fastore : Asm ()
    Fconst : Double -> Asm ()
    Field : FieldInstructionType -> (className: String) -> (fieldName: String) -> (descriptor: String) -> Asm ()
    FieldEnd : Asm ()
    Fload : Int -> Asm ()
    Frame : FrameType -> Int -> (signatures: List String) -> Int -> (signatures: List String) -> Asm ()
    Freturn : Asm ()
    Fstore : Int -> Asm ()
    Goto : (label: String) -> Asm ()
    I2b : Asm ()
    I2c : Asm ()
    I2d : Asm ()
    I2l : Asm ()
    I2s : Asm ()
    Iadd : Asm ()
    Iaload : Asm ()
    Iand : Asm ()
    Iastore : Asm ()
    Ior : Asm ()
    Ixor : Asm ()
    Icompl : Asm ()
    Iconst : Int -> Asm ()
    Idiv : Asm ()
    Ifeq : (label: String) -> Asm ()
    Ifge : (label: String) -> Asm ()
    Ifgt : (label: String) -> Asm ()
    Ificmpge : (label: String) -> Asm ()
    Ificmpgt : (label: String) -> Asm ()
    Ificmple : (label: String) -> Asm ()
    Ificmplt : (label: String) -> Asm ()
    Ificmpeq : (label: String) -> Asm ()
    Ifacmpne : (label: String) -> Asm ()
    Ificmpne : (label: String) -> Asm ()
    Ifle : (label: String) -> Asm ()
    Iflt : (label: String) -> Asm ()
    Ifne : (label: String) -> Asm ()
    Ifnonnull : (label: String) -> Asm ()
    Ifnull : (label: String) -> Asm ()
    Iload : Int -> Asm ()
    Imul : Asm ()
    Ineg : Asm ()
    InstanceOf : (className: String) -> Asm ()
    InvokeMethod : InvocationType -> (className: String) -> (methodName: String) -> (descriptor: String)
                    -> Bool -> Asm ()
    InvokeDynamic : (methodName: String) -> (descriptor: String) -> Handle -> List BsmArg -> Asm ()
    Irem : Asm ()
    Ireturn : Asm ()
    Ishl : Asm ()
    Ishr : Asm ()
    Istore : Int -> Asm ()
    Isub : Asm ()
    Iushr : Asm ()
    L2d : Asm ()
    L2i : Asm ()
    LabelStart : (label: String) -> Asm ()
    Ladd : Asm ()
    Laload : Asm ()
    Land : Asm ()
    Lastore : Asm ()
    Lcmp : Asm ()
    Lcompl : Asm ()
    Ldc : Asm.Constant -> Asm ()
    Ldiv : Asm ()
    LineNumber : Int -> String -> Asm ()
    Lload : Int  -> Asm ()
    Lmul : Asm ()
    Lneg : Asm ()
    LocalVariable : (name: String) -> (descriptor: String) -> (signature: Maybe String) -> (startLabel: String) ->
                        (endLabel: String) -> (index: Int) -> Asm ()
    LookupSwitch : (defaultLabel: String) -> (labels: List String) -> (cases: List Int) -> Asm ()
    Lor : Asm ()
    Lrem : Asm ()
    Lreturn : Asm ()
    Lshl : Asm ()
    Lshr : Asm ()
    Lstore : Int -> Asm ()
    Lsub : Asm ()
    Lushr : Asm ()
    Lxor : Asm ()
    MaxStackAndLocal : Int -> Int -> Asm ()
    MethodCodeStart : Asm ()
    MethodCodeEnd : Asm ()
    Multianewarray : (descriptor: String) -> Int -> Asm ()
    New : (className: String) -> Asm ()
    Pop : Asm ()
    Pop2 : Asm ()
    Return : Asm ()
    Saload : Asm ()
    Sastore : Asm ()
    SourceInfo : (sourceFileName: String) -> Asm ()
    LiftIo : IO a -> Asm a

    Throw : FC -> String -> Asm a
    GetState : Asm AsmState
    SetState : AsmState -> Asm ()

    Pure : ty -> Asm ty
    Bind : Asm a -> (a -> Asm b) -> Asm b

export
Show Scope where
    show scope = showType "Scope" [
        ("index", show $ index scope),
        ("parentIndex", show $ parentIndex scope),
        ("nextVariableIndex", show $ nextVariableIndex scope),
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

%inline
public export
Functor Asm where
  map f a = Bind a (\a' => Pure $ f a')

%inline
public export
Applicative Asm where
  pure = Pure

  (<*>) f a = Bind f (\f' =>
              Bind a (\a' =>
              Pure (f' a')))

%inline
public export
Monad Asm where
  (>>=) = Bind

%inline
public export
HasIO Asm where
  liftIO = LiftIo

public export
newAsmState : HasIO io => AsmGlobalState -> Assembler -> io AsmState
newAsmState globalState assembler = do
    let defaultName = Jqualified "" ""
    scopes <- ArrayList.new {elemTy=Scope}
    lineNumberLabels <- Map.newTreeMap {key=Int} {value=String}
    let function = MkFunction defaultName (MkInferredFunctionType IUnknown []) (believe_me scopes)
                    0 defaultName (NmCrash emptyFC "uninitialized function")
    pure $ MkAsmState globalState function defaultName 0 0 0 0 lineNumberLabels assembler

export
updateState : (AsmState -> AsmState) -> Asm ()
updateState f = SetState $ f !GetState

getAndUpdateState : (AsmState -> AsmState) -> Asm AsmState
getAndUpdateState f = do
    state <- GetState
    SetState $ f state
    Pure state

public export
%foreign "jvm:crash(String java/lang/Object),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
crash : String -> Object

export
asmCrash : String -> Asm a
asmCrash message = Pure $ believe_me $ crash message

export
newBigInteger : String -> Asm ()
newBigInteger "0" = Field GetStatic "java/math/BigInteger" "ZERO" "Ljava/math/BigInteger;"
newBigInteger "1" = Field GetStatic "java/math/BigInteger" "ONE" "Ljava/math/BigInteger;"
newBigInteger "10" = Field GetStatic "java/math/BigInteger" "TEN" "Ljava/math/BigInteger;"
newBigInteger i = do
    New "java/math/BigInteger"
    Dup
    Ldc $ StringConst i
    InvokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False

export
getGlobalState : Asm AsmGlobalState
getGlobalState = Pure $ globalState !GetState

export
findFunction : Jname -> Asm (Maybe Function)
findFunction name = LiftIo $ AsmGlobalState.findFunction !getGlobalState name

export
getFunction : Jname -> Asm Function
getFunction name = maybe (asmCrash $ "Unknown function " ++ show name) Pure !(findFunction name)

export
getCurrentFunction : Asm Function
getCurrentFunction = currentIdrisFunction <$> GetState

export
getProgramName : Asm String
getProgramName = LiftIo $ AsmGlobalState.getProgramName !getGlobalState

export
getFcAndDefinition : String -> Asm (FC, NamedDef)
getFcAndDefinition name = LiftIo $ AsmGlobalState.getFcAndDefinition !getGlobalState name

export
isUntypedFunction : Jname -> Asm Bool
isUntypedFunction name = LiftIo $ AsmGlobalState.isUntypedFunction !getGlobalState name

export
addUntypedFunction : Jname -> Asm ()
addUntypedFunction name = LiftIo $ AsmGlobalState.addUntypedFunction !getGlobalState name

export
setCurrentFunction : Function -> Asm ()
setCurrentFunction function = updateState $ { currentIdrisFunction := function }

getAndUpdateFunction : (Function -> Function) -> Asm Function
getAndUpdateFunction f = do
    function <- getCurrentFunction
    let newFunction = f function
    setCurrentFunction newFunction
    globalState <- getGlobalState
    LiftIo $ addFunction globalState (idrisName newFunction) newFunction
    Pure function

export
updateCurrentFunction : (Function -> Function) -> Asm ()
updateCurrentFunction f = ignore $ getAndUpdateFunction f

export
loadFunction : Jname -> Asm ()
loadFunction idrisName = do
    function <- getFunction idrisName
    updateState $ { currentIdrisFunction := function }

export
getFunctionType : Jname -> Asm InferredFunctionType
getFunctionType name = inferredFunctionType <$> (getFunction name)

export
getFunctionParameterTypes : Jname -> Asm (List InferredType)
getFunctionParameterTypes functionName = do
    functionType <- getFunctionType functionName
    pure $ parameterTypes functionType

export
findFunctionType : Jname -> Asm (Maybe InferredFunctionType)
findFunctionType functionName = do
    state <- GetState
    function <- findFunction functionName
    Pure $ inferredFunctionType <$> function

export
getFunctionReturnType : Jname -> Asm InferredType
getFunctionReturnType functionName =  do
    state <- GetState
    function <- findFunction functionName
    Pure $ maybe IUnknown (returnType . inferredFunctionType) $ function

export
getCurrentScopeIndex : Asm Int
getCurrentScopeIndex = currentScopeIndex <$> GetState

export
updateCurrentScopeIndex : Int -> Asm ()
updateCurrentScopeIndex scopeIndex = updateState $ { currentScopeIndex := scopeIndex }

export
newScopeIndex : Asm Int
newScopeIndex = scopeCounter <$> (getAndUpdateState $ {scopeCounter $= (+1)})

export
newDynamicVariableIndex : Asm Int
newDynamicVariableIndex = dynamicVariableCounter <$> (getAndUpdateFunction $ {dynamicVariableCounter $= (+1)})

export
resetScope : Asm ()
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
saveScope : Scope -> Asm ()
saveScope scope = do
    scopes <- scopes <$> getCurrentFunction
    size <- LiftIo $ Collection.size {elemTy=Scope, obj=Collection Scope} $ believe_me scopes
    let scopeIndex = index scope
    LiftIo $
      if scopeIndex < size
          then ignore $ JList.set scopes scopeIndex scope
          else do
              fillNull {a=Scope} scopeIndex scopes
              JList.add scopes scopeIndex scope

export
getScope : Int -> Asm Scope
getScope scopeIndex = do
   scopes <- scopes <$> getCurrentFunction
   LiftIo $ JList.get scopes scopeIndex

export
addScopeChild : Int -> Int -> Asm ()
addScopeChild parentScopeIndex childScopeIndex = do
    scope <- getScope parentScopeIndex
    saveScope $ {childIndices $= (childScopeIndex ::)} scope

export
getRootMethodName : Asm Jname
getRootMethodName = jvmClassMethodName <$> getCurrentFunction

export
newLabel : Asm String
newLabel = do
    state <- GetState
    let label = "L" ++ show (labelCounter state)
    updateState $ { labelCounter $= (+1) }
    Pure label

hasLabelAtLine : Int -> Asm Bool
hasLabelAtLine lineNumber = do
    state <- GetState
    LiftIo $ Map.containsKey {value=String} (lineNumberLabels state) lineNumber

export
addLineNumber : Int -> String -> Asm ()
addLineNumber lineNumber label = do
    hasLabel <- hasLabelAtLine lineNumber
    when (not hasLabel) $ do
        state <- GetState
        LineNumber lineNumber label
        _ <- LiftIo $ Map.put (lineNumberLabels state) lineNumber label
        Pure ()

export
getLineNumberLabel : Int -> Asm String
getLineNumberLabel lineNumber = do
    state <- GetState
    let currentLineNumberLabels = lineNumberLabels state
    optLabel <- LiftIo $ Map.get {value=String} currentLineNumberLabels lineNumber
    case nullableToMaybe optLabel of
        Just label => Pure label
        Nothing => do
            label <- newLabel
            _ <- LiftIo $ Map.put currentLineNumberLabels lineNumber label
            Pure label

export
getClassName : Asm String
getClassName = className . currentMethodName <$> GetState

export
getMethodName : Asm String
getMethodName = methodName . currentMethodName <$> GetState

export
freshLambdaIndex : Asm Int
freshLambdaIndex = lambdaCounter <$> (getAndUpdateState $ {lambdaCounter $= (+1)})

export
setScopeCounter : Int -> Asm ()
setScopeCounter scopeCounter = updateState $ {scopeCounter := scopeCounter}

export
updateScopeStartLabel : Int -> String -> Asm ()
updateScopeStartLabel scopeIndex label = do
    scope <- getScope scopeIndex
    saveScope $ {labels $= updateFirst label} scope

export
updateScopeEndLabel : Int -> String -> Asm ()
updateScopeEndLabel scopeIndex label = do
    scope <- getScope scopeIndex
    saveScope $ {labels $= updateSecond label} scope

export
createVariable : String -> Asm ()
createVariable var = do
    scopeIndex <- getCurrentScopeIndex
    scope <- getScope scopeIndex
    let variableIndex = nextVariableIndex scope
    _ <- LiftIo $ Map.put (variableTypes scope) var IUnknown
    _ <- LiftIo $ Map.put (variableIndices scope) var variableIndex
    saveScope $ { nextVariableIndex $= (+1) } scope

export
generateVariable : String -> Asm String
generateVariable namePrefix = do
    dynamicVariableIndex <- newDynamicVariableIndex
    let variableName = namePrefix ++ show dynamicVariableIndex
    createVariable variableName
    Pure variableName

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

retrieveVariableIndicesByName : Int -> Asm (Map String Int)
retrieveVariableIndicesByName scopeIndex = do
    variableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
    go variableIndices scopeIndex
    Pure variableIndices
  where
    go : Map String Int -> Int -> Asm ()
    go acc scopeIndex = go1 scopeIndex where
        go1 : Int -> Asm ()
        go1 scopeIndex = do
            scope <- getScope scopeIndex
            LiftIo $ updateVariableIndices acc (variableIndices scope)
            maybe (Pure ()) go1 (parentIndex scope)

export
retrieveVariables : Int -> Asm (List String)
retrieveVariables scopeIndex = do
    variableIndicesByName <- retrieveVariableIndicesByName scopeIndex
    LiftIo $ getVariableNames variableIndicesByName

retrieveVariableIndexAtScope : Int -> String -> Asm Int
retrieveVariableIndexAtScope currentScopeIndex name = go currentScopeIndex where
    go : Int -> Asm Int
    go scopeIndex = do
        scope <- getScope scopeIndex
        optIndex <- LiftIo $ Map.get {value=Int} (variableIndices scope) name
        case nullableToMaybe optIndex of
            Just index => Pure index
            Nothing => case parentIndex scope of
                Just parentScopeIndex => go parentScopeIndex
                Nothing => do
                  rootMethodName <- getRootMethodName
                  Throw emptyFC
                    ("retrieveVariableIndexAtScope: " ++ show rootMethodName ++ ": Unknown var " ++
                      name ++ " at index " ++ show currentScopeIndex)

export
retrieveVariableIndex : String -> Asm Int
retrieveVariableIndex name = retrieveVariableIndexAtScope !getCurrentScopeIndex name

retrieveVariableTypeAtScope : Int -> String -> Asm InferredType
retrieveVariableTypeAtScope scopeIndex name = do
    scope <- getScope scopeIndex
    optTy <- LiftIo $ Map.get (variableTypes scope) name
    case nullableToMaybe optTy of
        Just ty => Pure ty
        Nothing => case parentIndex scope of
            Just parentScope => retrieveVariableTypeAtScope parentScope name
            Nothing => pure IUnknown

export
retrieveVariableTypesAtScope : Int -> Asm (Map Int InferredType)
retrieveVariableTypesAtScope scopeIndex = do
    typesByIndex <- LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
    go typesByIndex !(retrieveVariables scopeIndex)
    Pure typesByIndex
  where
    go : Map Int InferredType -> List String -> Asm ()
    go acc names = go1 names where
        go1 : List String -> Asm ()
        go1 [] = Pure ()
        go1 (var :: vars) = do
            varIndex <- retrieveVariableIndexAtScope scopeIndex var
            ty <- retrieveVariableTypeAtScope scopeIndex var
            hasVar <- LiftIo $ containsKey {value=InferredType} acc varIndex
            when (not hasVar) $ LiftIo $ do
                oldTy <- Map.put acc varIndex ty
                pure ()
            go1 vars

export
getVariableIndicesByName : Int -> Asm (Map String Int)
getVariableIndicesByName scopeIndex = allVariableIndices <$> getScope scopeIndex

export
getVariableIndexAtScope : Int -> String -> Asm Int
getVariableIndexAtScope currentScopeIndex name = do
    variableIndicesByName <- getVariableIndicesByName currentScopeIndex
    optIndex <- LiftIo $ Map.get {value=Int} variableIndicesByName name
    case nullableToMaybe optIndex of
        Just index => Pure index
        Nothing => do
          rootMethodName <- getRootMethodName
          asmCrash ("getVariableIndexAtScope: " ++ show rootMethodName ++ ": Unknown var " ++
              name ++ " at index " ++ show currentScopeIndex)

export
getVariableIndex : String -> Asm Int
getVariableIndex name = getVariableIndexAtScope !getCurrentScopeIndex name

export
getVariableTypesAtScope : Int -> Asm (Map Int InferredType)
getVariableTypesAtScope scopeIndex = allVariableTypes <$> getScope scopeIndex

export
getVariableTypes : Asm (Map Int InferredType)
getVariableTypes = getVariableTypesAtScope !getCurrentScopeIndex

export
getVariableTypeAtScope : Int -> String -> Asm InferredType
getVariableTypeAtScope scopeIndex name = do
    scope <- getScope scopeIndex
    variableIndicesByName <- getVariableIndicesByName scopeIndex
    optIndex <- LiftIo $ Map.get {value=Int} variableIndicesByName name
    case nullableToMaybe optIndex of
        Just index => do
            variableTypes <- getVariableTypesAtScope scopeIndex
            optTy <- LiftIo $ Map.get {value=InferredType} variableTypes index
            Pure $ fromMaybe IUnknown $ nullableToMaybe optTy
        Nothing => Pure IUnknown

export
getVariableType : String -> Asm InferredType
getVariableType name = getVariableTypeAtScope !getCurrentScopeIndex name

updateArgumentsForUntyped : Map Int InferredType -> Nat -> IO ()
updateArgumentsForUntyped _ Z = pure ()
updateArgumentsForUntyped types (S n) = do
  ignore $ Map.put types (cast {to=Int} n) inferredObjectType
  updateArgumentsForUntyped types n

export
updateScopeVariableTypes : Nat -> Asm ()
updateScopeVariableTypes arity = go (scopeCounter !GetState - 1) where
    go : Int -> Asm ()
    go scopeIndex =
        if scopeIndex < 0 then Pure ()
        else do
            variableTypes <- retrieveVariableTypesAtScope scopeIndex
            when (scopeIndex == 0) $ LiftIo $ updateArgumentsForUntyped variableTypes arity
            variableIndices <- retrieveVariableIndicesByName scopeIndex
            scope <- getScope scopeIndex
            saveScope $ {allVariableTypes := variableTypes, allVariableIndices := variableIndices} scope
            go (scopeIndex - 1)

getVariableScope : String -> Asm Scope
getVariableScope name = go !getCurrentScopeIndex where
    go : Int -> Asm Scope
    go scopeIndex = do
        scope <- getScope scopeIndex
        optTy <- LiftIo $ Map.get {value=InferredType} (variableTypes scope) name
        case nullableToMaybe optTy of
            Just _ => Pure scope
            Nothing => case parentIndex scope of
                Just parentScopeIndex => go parentScopeIndex
                Nothing => asmCrash ("Unknown variable " ++ name)

export
addVariableType : String -> InferredType -> Asm InferredType
addVariableType var IUnknown = Pure IUnknown
addVariableType var ty = do
    scope <- getVariableScope var
    let scopeIndex = index scope
    existingTy <- retrieveVariableTypeAtScope scopeIndex var
    let newTy = existingTy <+> ty
    _ <- LiftIo $ Map.put (variableTypes scope) var newTy
    Pure newTy

%inline
export
lambdaMaxCountPerMethod: Int
lambdaMaxCountPerMethod = 50

export
getLambdaImplementationMethodName : String -> Asm Jname
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
    Pure $ Jqualified lambdaClassName lambdaMethodName

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

getIdrisConstructorType : ConInfo -> (tag: Maybe Int) -> Nat -> Name -> InferredType
getIdrisConstructorType conInfo tag arity name =
  if isBoolTySpec name then IBool
  else if name == basics "List" then idrisListType
  else if name == preludetypes "Maybe" then idrisMaybeType
  else if name == preludetypes "Nat" then inferredBigIntegerType
  else inferredObjectType

parseName : String -> Maybe InferredType
parseName name =
  case words name of
    (interfaceName :: methodName :: _) => Just $ IRef interfaceName Interface []
    (className :: []) => Just $ iref className []
    _ => Nothing

mutual
  parseArrayType : NamedCExp -> Asm (Maybe InferredType)
  parseArrayType expr@(NmCon _ name _ _ [elemTy]) =
    if name == arrayName then Pure . Just $ IArray !(tySpec elemTy)
    else Pure Nothing
  parseArrayType _ = Pure Nothing

  parseLambdaType : NamedCExp -> Asm (Maybe InferredType)
  parseLambdaType (NmCon _ name _ _ [interfaceType, _]) =
    if name == builtin "Pair" then parseJvmReferenceType interfaceType
    else Pure Nothing
  parseLambdaType _ = Pure Nothing

  parseJvmReferenceType : NamedCExp -> Asm (Maybe InferredType)
  parseJvmReferenceType (NmCon _ name _ _ (NmPrimVal _ (Str namePartsStr) :: _)) =
    if name == structName
      then Pure $ parseName namePartsStr
      else Pure Nothing
  parseJvmReferenceType (NmCon _ name conInfo tag args) =
    if name == primio "IORes" then
      maybe (asmCrash "Expected an argument for IORes") (\res => Pure $ Just !(tySpec res)) (head' args)
    else Pure $ Just $ getIdrisConstructorType conInfo tag (length args) name
  parseJvmReferenceType (NmApp fc (NmRef _ name) _) = do
    (_, MkNmFun _ def) <- getFcAndDefinition (jvmSimpleName name)
      | _ => asmCrash ("Expected a function returning a tuple containing interface type and method type at " ++
               show fc)
    ty <- tySpec def
    Pure $ Just ty
  parseJvmReferenceType (NmDelay _ _ expr) = Pure $ Just !(tySpec expr)
  parseJvmReferenceType expr = Pure Nothing

  tryParse : NamedCExp -> Asm (Maybe InferredType)
  tryParse expr = do
    arrayTypeMaybe <- parseArrayType expr
    case arrayTypeMaybe of
      Nothing => do
        lambdaTypeMaybe <- parseLambdaType expr
        case lambdaTypeMaybe of
          Nothing => parseJvmReferenceType expr
          Just lambdaType => Pure $ Just lambdaType
      Just arrayType => Pure $ Just arrayType

  export
  tySpec : NamedCExp -> Asm InferredType
  tySpec (NmCon _ (UN (Basic ty)) _ _ []) = Pure $ tySpecStr ty
  tySpec (NmCon _ _ NOTHING _ []) = Pure idrisMaybeType
  tySpec (NmCon _ _ JUST _ [_]) = Pure idrisMaybeType
  tySpec (NmCon _ _ NIL _ []) = Pure idrisListType
  tySpec (NmCon _ _ CONS _ [_, _]) = Pure idrisListType
  tySpec expr@(NmCon _ (NS _ (UN (Basic "Unit"))) _ _ []) = Pure IVoid
  tySpec expr = do
    ty <- tryParse expr
    Pure $ fromMaybe inferredObjectType ty

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

export
getJvmReferenceTypeName : InferredType -> Asm String
getJvmReferenceTypeName (IRef ty _ _) = Pure ty
getJvmReferenceTypeName (IArray (IRef ty _ _)) = Pure ("[L" ++ ty ++ ";")
getJvmReferenceTypeName (IArray ty) = Pure ("[" ++ !(getJvmReferenceTypeName ty))
getJvmReferenceTypeName (IFunction lambdaType) = getJvmReferenceTypeName (lambdaType.javaInterface)
getJvmReferenceTypeName ty = asmCrash ("Expected a reference type but found " ++ show ty)

export
getSignature : InferredType -> String
getSignature (IRef ty _ typeParams@(_ :: _)) =
  let typeParamsDescriptor = concat (getJvmTypeDescriptor <$> typeParams)
  in "L" ++ ty ++ "<" ++ typeParamsDescriptor ++ ">;"
getSignature type = getJvmTypeDescriptor type

export
asmReturn : InferredType -> Asm ()
asmReturn IVoid    = Return
asmReturn IBool    = Ireturn
asmReturn IByte    = Ireturn
asmReturn IShort   = Ireturn
asmReturn IInt     = Ireturn
asmReturn IChar    = Ireturn
asmReturn ILong    = Lreturn
asmReturn IFloat   = Freturn
asmReturn IDouble  = Dreturn
asmReturn _        = Areturn

export
-- constant values from org.objectweb.asm.Opcodes
accessNum : Access -> Int
accessNum Public    = 0x0001
accessNum Private   = 0x0002
accessNum Protected   = 0x0004
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
constantToObject : Asm.Constant -> Object
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
loadBigInteger : Integer -> Asm ()
loadBigInteger 0 = Field GetStatic "java/math/BigInteger" "ZERO" "Ljava/math/BigInteger;"
loadBigInteger 1 = Field GetStatic "java/math/BigInteger" "ONE" "Ljava/math/BigInteger;"
loadBigInteger 10 = Field GetStatic "java/math/BigInteger" "TEN" "Ljava/math/BigInteger;"
loadBigInteger value = do
    New "java/math/BigInteger"
    Dup
    Ldc $ StringConst $ show value
    InvokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False

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

export
assemble : HasIO io => AsmState -> IO a -> io (a, AsmState)
assemble state m = do
    res <- primIO $ toPrim m
    pure (res, state)

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
invokeDynamic : (implClassName: String) -> (implMethodName: String) -> (interfaceMethodName: String) ->
                (invokeDynamicDesc: String) -> (samDesc: String) -> (implMethodDesc: String) ->
                (instantiatedMethodDesc: String) -> Asm ()
invokeDynamic implClassName implMethodName interfaceMethodName invokeDynamicDesc samDesc implMethodDesc
    instantiatedMethodDesc =
    let metafactoryHandle = MkHandle InvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory"
            metafactoryDesc False
        implMethodHandle = MkHandle InvokeStatic implClassName implMethodName implMethodDesc False
        metafactoryArgs = [ BsmArgGetType samDesc
                        , BsmArgHandle implMethodHandle
                        , BsmArgGetType instantiatedMethodDesc
                        ]
    in InvokeDynamic interfaceMethodName invokeDynamicDesc metafactoryHandle metafactoryArgs

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
logAsm : Lazy String -> Asm ()
logAsm message = log message (Pure ())

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

export
runAsm : HasIO io => AsmState -> Asm a -> io (a, AsmState)
runAsm state Aaload = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aaload" [assembler state]

runAsm state Aastore = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aastore" [assembler state]

runAsm state Aconstnull = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aconstnull" [assembler state]

runAsm state (Aload n) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.aload" [assembler state, n]

runAsm state (Anewarray desc) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewarray" [assembler state, desc]
runAsm state Anewintarray     = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewintarray" [assembler state]
runAsm state Anewbooleanarray = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewbooleanarray" [assembler state]
runAsm state Anewbytearray    = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewbytearray" [assembler state]
runAsm state Anewchararray    = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewchararray" [assembler state]
runAsm state Anewshortarray   = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewshortarray" [assembler state]
runAsm state Anewlongarray    = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewlongarray" [assembler state]
runAsm state Anewfloatarray   = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewfloatarray" [assembler state]
runAsm state Anewdoublearray  = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.anewdoublearray" [assembler state]
runAsm state Arraylength      = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.arraylength" [assembler state]
runAsm state Areturn          = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.areturn" [assembler state]
runAsm state (Astore n)       = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.astore" [assembler state, n]
runAsm state Baload           = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.baload" [assembler state]
runAsm state Bastore          = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.bastore" [assembler state]
runAsm state Caload           = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.caload" [assembler state]
runAsm state Castore          = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.castore" [assembler state]
runAsm state (Checkcast desc) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.checkcast" [assembler state, desc]
runAsm state (ClassCodeStart version access className sig parent intf anns) = assemble state $ do
  janns <- sequence $ toJAnnotation <$> anns
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.classCodeStart"
    [assembler state, version, sum $ accessNum <$> access, className, maybeToNullable sig, parent,
        the (JList String) $ believe_me intf, the (JList JAnnotation) $ believe_me janns]

runAsm state (CreateClass opts) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createClass"
      [assembler state, sum $ toJClassOpts <$> opts]
runAsm state (CreateField accs sourceFileName className fieldName desc sig fieldInitialValue anns) = assemble state $ do
  let jaccs = sum $ accessNum <$> accs
  janns <- sequence $ toJAnnotation <$> anns
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createField"
    [assembler state, jaccs, sourceFileName, className, fieldName, desc, maybeToNullable sig,
        maybeToNullable (toJFieldInitialValue <$> fieldInitialValue), the (JList JAnnotation) $ believe_me janns]

runAsm state (CreateLabel label) = assemble state $
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createLabel" [assembler state, label]

runAsm state (CreateMethod accs sourceFileName className methodName desc sig exceptions anns paramAnns) =
    let newState = { currentMethodName := Jqualified className methodName } state
    in assemble newState $ do
        let jaccs = sum $ accessNum <$> accs
        janns <- sequence $ toJAnnotation <$> anns
        jparamAnns <- sequence $ (\paramAnn => sequence $ toJAnnotation <$> paramAnn) <$> paramAnns
        jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createMethod"
            [assembler state, jaccs, sourceFileName, className, methodName, desc, maybeToNullable sig,
                the (JList String) $ believe_me $ maybeToNullable exceptions,
                the (JList JAnnotation) $ believe_me janns, the (JList (JList JAnnotation)) $ believe_me jparamAnns]

runAsm state (CreateIdrisConstructorClass className isStringConstructor constructorParameterCount) =
    assemble state $  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createIdrisConstructorClass"
        [assembler state, className, isStringConstructor, constructorParameterCount]

runAsm state D2i =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.d2i" [assembler state]
runAsm state D2f =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.d2f" [assembler state]
runAsm state D2l =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.d2l" [assembler state]
runAsm state Dadd =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dadd" [assembler state]
runAsm state Dcmpg =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dcmpg" [assembler state]
runAsm state Dcmpl =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dcmpl" [assembler state]
runAsm state (Dconst n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dconst" [assembler state, n]
runAsm state Daload =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.daload" [assembler state]
runAsm state Dastore =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dastore" [assembler state]
runAsm state Ddiv =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ddiv" [assembler state]
runAsm state (Debug message) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.debug"
        [assembler state, message]
runAsm state (Dload n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dload" [assembler state, n]
runAsm state Dmul =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dmul" [assembler state]
runAsm state Dneg =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dneg" [assembler state]
runAsm state Drem =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.drem" [assembler state]
runAsm state Dreturn =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dreturn" [assembler state]
runAsm state (Dstore n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dstore" [assembler state, n]
runAsm state Dsub =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dsub" [assembler state]
runAsm state Dup =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.dup" [assembler state]
runAsm state (Error err) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.error" [assembler state, err]
runAsm state F2d =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.f2d" [assembler state]
runAsm state Faload =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.faload" [assembler state]
runAsm state Fastore =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fastore" [assembler state]
runAsm state (Fconst n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fconst" [assembler state, n]
runAsm state (Field finsType cname fname desc) = assemble state $ do
  let finsTypeNum = fieldInsTypeNum finsType
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.field"
    [assembler state, finsTypeNum, cname, fname, desc]

runAsm state FieldEnd =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fieldEnd" [assembler state]

runAsm state (Fload n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fload" [assembler state, n]

runAsm state (Frame frameType nLocal localSigs nStack stackSigs) = assemble state $ do
  let ftypeNum = frameTypeNum frameType
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.frame"
    [assembler state, ftypeNum, nLocal, the (JList String) $ believe_me localSigs, nStack,
        the (JList String) $ believe_me stackSigs]

runAsm state Freturn =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.freturn" [assembler state]
runAsm state (Fstore n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.fstore" [assembler state, n]

runAsm state (Goto label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.gotoLabel"
        [assembler state, label]

runAsm state I2b =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2b" [assembler state]
runAsm state I2c =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2c" [assembler state]
runAsm state I2d =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2d" [assembler state]
runAsm state I2l =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2l" [assembler state]
runAsm state I2s =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.i2s" [assembler state]
runAsm state Iadd =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iadd" [assembler state]
runAsm state Iaload =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iaload" [assembler state]
runAsm state Iand =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iand" [assembler state]
runAsm state Iastore =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iastore" [assembler state]
runAsm state Ior =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ior" [assembler state]
runAsm state Ixor =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ixor" [assembler state]
runAsm state Icompl =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.icompl" [assembler state]
runAsm state (Iconst n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iconst" [assembler state, n]
runAsm state Idiv =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.idiv" [assembler state]
runAsm state (Ifeq label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifeq" [assembler state, label]
runAsm state (Ifge label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifge" [assembler state, label]
runAsm state (Ifgt label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifgt" [assembler state, label]
runAsm state (Ificmpge label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpge" [assembler state, label]
runAsm state (Ificmpgt label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpgt" [assembler state, label]
runAsm state (Ificmple label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmple" [assembler state, label]
runAsm state (Ificmplt label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmplt" [assembler state, label]
runAsm state (Ificmpeq label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpeq" [assembler state, label]
runAsm state (Ifacmpne label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifacmpne" [assembler state, label]
runAsm state (Ificmpne label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ificmpne" [assembler state, label]
runAsm state (Ifle label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifle" [assembler state, label]
runAsm state (Iflt label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iflt" [assembler state, label]
runAsm state (Ifne label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifne" [assembler state, label]
runAsm state (Ifnonnull label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifnonnull" [assembler state, label]
runAsm state (Ifnull label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ifnull" [assembler state, label]
runAsm state (Iload n) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iload" [assembler state, n]
runAsm state Imul = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.imul" [assembler state]
runAsm state Ineg = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ineg" [assembler state]
runAsm state (InstanceOf className) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.instanceOf" [assembler state, className]
runAsm state (InvokeMethod invocType cname mname desc isIntf) = assemble state $ do
  let invocTypeAsm = invocTypeNum invocType
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.invokeMethod"
    [assembler state, invocTypeAsm, cname, mname, desc, isIntf]

runAsm state (InvokeDynamic mname desc handle bsmArgs) = assemble state $ do
  jbsmArgsList <- sequence $ toJbsmArg <$> bsmArgs
  jhandle <- toJHandle handle
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.invokeDynamic"
    [assembler state, mname, desc, jhandle, the (JList JBsmArg) $ believe_me jbsmArgsList]

runAsm state Irem = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.irem" [assembler state]
runAsm state Ireturn = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ireturn" [assembler state]
runAsm state Ishl = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ishl" [assembler state]
runAsm state Ishr = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ishr" [assembler state]
runAsm state (Istore n) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.istore" [assembler state, n]
runAsm state Isub = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.isub" [assembler state]
runAsm state Iushr = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.iushr" [assembler state]
runAsm state L2d = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.l2d" [assembler state]
runAsm state L2i = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.l2i" [assembler state]
runAsm state (LabelStart label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.labelStart" [assembler state, label]
runAsm state Ladd = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ladd" [assembler state]
runAsm state Land = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.land" [assembler state]
runAsm state Laload = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.laload" [assembler state]
runAsm state Lastore = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lastore" [assembler state]
runAsm state Lcmp = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lcmp" [assembler state]
runAsm state Lor = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lor" [assembler state]
runAsm state Lxor = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lxor" [assembler state]
runAsm state Lcompl = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lcompl" [assembler state]

runAsm state (Ldc (TypeConst ty)) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ldcType" [assembler state, ty]
runAsm state (Ldc constant) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ldc" [assembler state, constantToObject constant]

runAsm state Ldiv = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ldiv" [assembler state]

runAsm state (LineNumber lineNumber label) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lineNumber" [assembler state, lineNumber, label]

runAsm state (Lload n) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lload" [assembler state, n]
runAsm state Lmul = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lmul" [assembler state]
runAsm state Lneg = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lneg" [assembler state]
runAsm state (LookupSwitch defaultLabel labels cases) = assemble state $ do
  let jcases = integerValueOf <$> cases
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lookupSwitch"
    [assembler state, defaultLabel, the (JList String) $ believe_me labels, the (JList Int) $ believe_me jcases]

runAsm state (LocalVariable name descriptor signature startLabel endLabel index) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.localVariable"
        [assembler state, name, descriptor, maybeToNullable signature, startLabel, endLabel, index]

runAsm state Lrem = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lrem" [assembler state]
runAsm state Lreturn = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lreturn" [assembler state]
runAsm state Lshl = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lshl" [assembler state]
runAsm state Lshr = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lshr" [assembler state]
runAsm state (Lstore n) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lstore" [assembler state, n]
runAsm state Lsub = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lsub" [assembler state]
runAsm state Lushr = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lushr" [assembler state]
runAsm state (MaxStackAndLocal stack local) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.maxStackAndLocal" [assembler state, stack, local]
runAsm state MethodCodeStart = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.methodCodeStart" [assembler state]
runAsm state MethodCodeEnd = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.methodCodeEnd" [assembler state]
runAsm state (Multianewarray desc dims) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.multiANewArray" [assembler state, desc, dims]
runAsm state (New cname) = assemble state $
    jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.asmNew" [assembler state, cname]
runAsm state Pop = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.pop" [assembler state]
runAsm state Pop2 = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.pop2" [assembler state]
runAsm state Return = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.voidReturn" [assembler state]
runAsm state Saload = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.saload" [assembler state]
runAsm state Sastore = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.sastore" [assembler state]
runAsm state (SourceInfo sourceFileName)
  = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.sourceInfo" [assembler state, sourceFileName]
runAsm state (LiftIo action) = assemble state action

runAsm state (Throw fc message) = pure (believe_me $ crash $ show fc ++ ": " ++ message, state)
runAsm state GetState = pure (state, state)
runAsm state (SetState newState) = pure ((), newState)

runAsm st (Pure value) = pure (value, st)
runAsm st (Bind action f) = do
  (result, nextSt) <- runAsm st action
  runAsm nextSt $ f result
