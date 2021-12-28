module Compiler.Jvm.Asm

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Core.Context
import Core.Name
import Core.TT

import Data.List
import Data.Maybe
import Libraries.Data.SortedSet
import Data.Vect

import Compiler.Jvm.Tuples
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.ShowUtil

import System
import System.FFI

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

namespace Collection
    export
    data CollectionNative : Type where [external]

    export
    Collection : Type -> Type
    Collection a = CollectionNative

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
%foreign "jvm:nullValue(java/lang/Object),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
nullValue : Object

public export
%foreign jvm' "java/util/Objects" "isNull" "java/lang/Object" "boolean"
isNull : Object -> Bool

public export
maybeToNullable : Maybe t -> t
maybeToNullable (Just t) = t
maybeToNullable Nothing = believe_me nullValue

public export
nullableToMaybe : Object -> Maybe Object
nullableToMaybe value = if isNull value then Nothing else Just value

namespace Iterable
    export
    data JIterable : Type where [external]

    export
    Iterable : Type -> Type
    Iterable a = JIterable

namespace JList
    export
    data JListNative : Type where [external]

    export
    JList : Type -> Type
    JList a = JListNative

    %foreign "jvm:<init>(java/lang/Object java/util/ArrayList),java/util/ArrayList"
    prim_newArrayList : PrimIO JListNative

    %foreign jvm' "java/util/List" ".add" "i:java/util/List int java/lang/Object" "void"
    prim_add : JListNative -> Int -> Object -> PrimIO ()

    %foreign jvm' "java/util/List" ".addAll" "i:java/util/List java/util/Collection" "boolean"
    prim_addAll : JListNative -> CollectionNative -> PrimIO Bool

    %foreign jvm' "java/util/List" ".set" "i:java/util/List int java/lang/Object" "java/lang/Object"
    prim_set : JListNative -> Int -> Object -> PrimIO Object

    %foreign jvm' "java/util/List" ".get" "i:java/util/List int" "java/lang/Object"
    prim_get : JListNative -> Int -> PrimIO Object

    %foreign jvm' "java/util/List" ".size" "i:java/util/List" "int"
    prim_size : JListNative -> PrimIO Int

    export
    new : HasIO io => io (JList a)
    new = believe_me <$> primIO prim_newArrayList

    export
    add : HasIO io => JList a -> Int -> a -> io ()
    add list index value = primIO $ prim_add (believe_me list) index (believe_me value)

    export
    addAll : HasIO io => JList a -> Collection a -> io Bool
    addAll list collection = primIO $ prim_addAll (believe_me list) (believe_me collection)

    export
    set : HasIO io => JList a -> Int -> a -> io a
    set list index value = believe_me <$> (primIO $ prim_set (believe_me list) index (believe_me value))

    export
    get : HasIO io => JList a -> Int -> io a
    get list index = believe_me <$> (primIO $ prim_get (believe_me list) index)

    export
    size : HasIO io => JList a -> io Int
    size list = believe_me <$> (primIO $ prim_size (believe_me list))

    %foreign jvm' "java/util/Collections" "nCopies" "int java/lang/Object" "java/util/List"
    prim_nCopies : Int -> Object -> PrimIO JListNative

    export
    nCopies : HasIO io => Int -> a -> io (JList a)
    nCopies n value = believe_me <$> (primIO $ prim_nCopies n (believe_me value))

    %foreign jvm' "io/github/mmhelloworld/idrisjvm/runtime/IdrisList" "fromIterable" "java/lang/Iterable" "io/github/mmhelloworld/idrisjvm/runtime/IdrisList"
    prim_fromIterable : JIterable -> PrimIO JListNative

    export
    fromIterable : HasIO io => Iterable a -> io (List a)
    fromIterable iterable = believe_me <$> (primIO $ prim_fromIterable (believe_me iterable))

namespace Entry
    data JEntry : Type where [external]

    export
    Entry : Type -> Type -> Type
    Entry k v = JEntry

    %foreign "jvm:<init>(java/lang/Object java/lang/Object java/util/AbstractMap$SimpleImmutableEntry),java/util/AbstractMap$SimpleImmutableEntry"
    prim_new : Object -> Object -> PrimIO JEntry

    export
    new : HasIO io => k -> v -> io (Entry k v)
    new key value = believe_me <$> primIO (prim_new (believe_me key) (believe_me value))

    %foreign jvm' "java/util/Map$Entry" ".getKey" "i:java/util/Map$Entry" "java/lang/Object"
    prim_getKey : JEntry -> PrimIO Object

    export
    getKey : HasIO io => Entry k v -> io k
    getKey entry = believe_me <$> primIO (prim_getKey (believe_me entry))

    %foreign jvm' "java/util/Map$Entry" ".getValue" "i:java/util/Map$Entry" "java/lang/Object"
    prim_getValue : JEntry -> PrimIO Object

    export
    getValue : HasIO io => Entry k v -> io v
    getValue entry = believe_me <$> primIO (prim_getValue (believe_me entry))

    export
    toTuple : HasIO io => Entry k v -> io (k, v)
    toTuple entry = do
        key <- getKey {k=k} {v=v} entry
        value <- getValue {k=k} {v=v} entry
        pure (key, value)

namespace Map
  export
  data JMap : Type where [external]

  export
  Map : Type -> Type -> Type
  Map k v = JMap

  %foreign "jvm:<init>(java/lang/Object java/util/TreeMap),java/util/TreeMap"
  prim_newTreeMap : PrimIO JMap

  export
  newTreeMap : HasIO io => io (Map key value)
  newTreeMap = believe_me <$> primIO prim_newTreeMap

  %foreign jvm' "java/util/Map" ".get" "i:java/util/Map java/lang/Object" "java/lang/Object"
  prim_get : JMap -> Object -> PrimIO Object

  export
  get : HasIO io => Map key value -> key -> io (Maybe value)
  get map key = (believe_me . nullableToMaybe) <$> (primIO $ prim_get (believe_me map) (believe_me key))

  %foreign jvm' "java/util/Map" ".put" "i:java/util/Map java/lang/Object java/lang/Object" "java/lang/Object"
  prim_put : JMap -> Object -> Object -> PrimIO Object

  export
  put : HasIO io => Map key value -> key -> value -> io (Maybe value)
  put this key value = (believe_me . nullableToMaybe) <$> (primIO $ prim_put (believe_me this) (believe_me key)
                        (believe_me value))

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
  prim_containsKey : JMap -> Object -> PrimIO Bool

  export
  containsKey : HasIO io => Map key value -> key -> io Bool
  containsKey this key = primIO $ prim_containsKey (believe_me this) (believe_me key)

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "transpose" "java/util/Map" "java/util/Map"
  prim_transpose : JMap -> PrimIO JMap

  export
  transpose : HasIO io => Map k v -> io (Map v k)
  transpose m = believe_me <$> primIO (prim_transpose $ believe_me m)

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "toList" "java/util/Map" "java/util/List"
  prim_toEntries : JMap -> PrimIO JListNative

  export
  toEntries : HasIO io => Map key value -> io (List (Entry key value))
  toEntries m = do
    entries <- primIO (prim_toEntries $ believe_me m)
    JList.fromIterable (believe_me entries)

  export
  toList : HasIO io => Map k v -> io (List (k, v))
  toList m = do
    entries <- toEntries m
    traverse toTuple entries

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "keys" "java/util/Map" "java/util/List"
  prim_keys : JMap -> PrimIO JListNative

  export
  keys : HasIO io => Map key value -> io (List key)
  keys m = do
    jkeys <- primIO (prim_keys $ believe_me m)
    JList.fromIterable (believe_me jkeys)

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "values" "java/util/Map" "java/util/List"
  prim_values : JMap -> PrimIO JListNative

  export
  values : HasIO io => Map key value -> io (List value)
  values m = do
    jvalues <- primIO (prim_values $ believe_me m)
    JList.fromIterable (believe_me jvalues)

  %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/Maps" "getValue2" "java/util/Map" "java/util/Map"
  prim_getValue2 : JMap -> PrimIO JMap

  export
  getValue2 : HasIO io => Map k (Entry v1 v2) -> io (Map k v2)
  getValue2 m = believe_me <$> primIO (prim_getValue2 (believe_me m))

namespace EnumInt
    export
    succ : Int -> Int
    succ n = n + 1

data Collection : Type where [external]

%inline
public export
runtimeClass : String
runtimeClass = getRuntimeClass "Runtime"

public export
record TailCallCategory where
    constructor MkTailCallCategory

    -- Self tail calls are eliminated using JVM's GOTO
    hasSelfTailCall : Bool

    -- Non self tails calls are trampolined using INVOKEDYNAMIC
    hasNonSelfTailCall : Bool

export
Show TailCallCategory where
    show tailCallCategory = showType "TailCallCategory" [
        ("hasSelfTailCall", show $ hasSelfTailCall tailCallCategory),
        ("hasNonSelfTailCall", show $ hasNonSelfTailCall tailCallCategory)
    ]

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
record InferredFunctionType where
    constructor MkInferredFunctionType
    returnType : InferredType
    parameterTypes : List InferredType

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
        "jvm:<init>(String java/util/Collection io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState),io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState"
    prim_newAsmGlobalState : String -> List String -> PrimIO AsmGlobalState

    public export
    newAsmGlobalState : HasIO io => String -> List String -> io AsmGlobalState
    newAsmGlobalState programName trampolinePatterns = primIO $ prim_newAsmGlobalState programName trampolinePatterns

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
    getProgramName : HasIO io => AsmGlobalState -> io String
    getProgramName = primIO . prim_getProgramName

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
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState" ".shouldTrampoline"
                "io/github/mmhelloworld/idrisjvm/assembler/AsmGlobalState String" "boolean"
    prim_shouldTrampoline : AsmGlobalState -> String -> PrimIO Bool

    public export
    shouldTrampoline : HasIO io => AsmGlobalState -> String -> io Bool
    shouldTrampoline state name = primIO $ prim_shouldTrampoline state name

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
data Access = Private | Public | Static | Synthetic | Final

export
Show Access where
    show Private = "Private"
    show Public = "Public"
    show Static = "Static"
    show Synthetic = "Synthetic"
    show Final = "Final"

public export
data FieldInstructionType = GetStatic | PutStatic | GetField | PutField
data FrameType = Full | Same | Append

public export
data Constant = DoubleConst Double
              | IntegerConst Int
              | StringConst String
              | TypeConst String

public export
data InvocationType = InvokeInterface | InvokeSpecial | InvokeStatic | InvokeVirtual

export
Show InvocationType where
    show InvokeInterface = "InvokeInterface"
    show InvokeSpecial = "InvokeSpecial"
    show InvokeStatic = "InvokeStatic"
    show InvokeVirtual = "InvokeVirtual"

export
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
    data AnnotationValue = AnnInt Int
                         | AnnBoolean Bool
                         | AnnChar Char
                         | AnnDouble Double
                         | AnnString String
                         | AnnClass String
                         | AnnEnum String String
                         | AnnArray (List AnnotationValue)
                         | AnnAnnotation Annotation

    AnnotationProperty : Type
    AnnotationProperty = (String, Asm.AnnotationValue)

    data Annotation = MkAnnotation String (List AnnotationProperty)

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
    ClassCodeStart : Int -> Access -> (className: String) -> (signature: Maybe String) -> (parentClassName: String) ->
                        (interfaces: List String) -> List Asm.Annotation -> Asm ()
    CreateClass : ClassOpts -> Asm ()
    CreateField : List Access -> (sourceFileName: String) -> (className: String) -> (fieldName: String) -> (descriptor: String) ->
                    (signature: Maybe String) -> Maybe FieldInitialValue -> Asm ()
    CreateLabel : String -> Asm ()
    CreateMethod : List Access -> (sourceFileName: String) -> (className: String) ->
                    (methodName: String) -> (descriptor: String) ->
                    (signature: Maybe String) -> (exceptions: Maybe (List String)) ->
                    (annotations: List Asm.Annotation) ->
                    (parameterAnnotations: List (List Asm.Annotation)) -> Asm ()
    CreateIdrisConstructorClass : String -> Bool -> Int -> Asm ()
    D2i : Asm ()
    D2f : Asm ()
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
    L2i : Asm ()
    LabelStart : (label: String) -> Asm ()
    Ladd : Asm ()
    Laload : Asm ()
    Land : Asm ()
    Lastore : Asm ()
    Lor : Asm ()
    Lxor : Asm ()
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
    Lrem : Asm ()
    Lreturn : Asm ()
    Lshl : Asm ()
    Lshr : Asm ()
    Lstore : Int -> Asm ()
    Lsub : Asm ()
    Lushr : Asm ()
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
Show InferredFunctionType where
    show inferredFunctionType = showType "InferredFunctionType" [
        ("returnType", show $ returnType inferredFunctionType),
        ("parameterTypes", show $ parameterTypes inferredFunctionType)
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
    scopes <- JList.new {a=Scope}
    lineNumberLabels <- Map.newTreeMap {key=Int} {value=String}
    let function = MkFunction defaultName (MkInferredFunctionType IUnknown []) scopes
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
getFunction name = maybe (Throw emptyFC $ "Unknown function " ++ show name) Pure !(findFunction name)

export
getCurrentFunction : Asm Function
getCurrentFunction = currentIdrisFunction <$> GetState

export
getProgramName : Asm String
getProgramName = LiftIo $ AsmGlobalState.getProgramName !getGlobalState

export
isUntypedFunction : Jname -> Asm Bool
isUntypedFunction name = LiftIo $ AsmGlobalState.isUntypedFunction !getGlobalState name

export
addUntypedFunction : Jname -> Asm ()
addUntypedFunction name = LiftIo $ AsmGlobalState.addUntypedFunction !getGlobalState name

export
setCurrentFunction : Function -> Asm ()
setCurrentFunction function = updateState $ record { currentIdrisFunction = function }

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
    updateState $ record { currentIdrisFunction = function }

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
updateCurrentScopeIndex scopeIndex = updateState $ record {currentScopeIndex = scopeIndex}

export
newScopeIndex : Asm Int
newScopeIndex = scopeCounter <$> (getAndUpdateState $ record {scopeCounter $= (+1)})

export
newDynamicVariableIndex : Asm Int
newDynamicVariableIndex = dynamicVariableCounter <$> (getAndUpdateFunction $ record {dynamicVariableCounter $= (+1)})

export
resetScope : Asm ()
resetScope = updateState $
    record {
        scopeCounter = 0,
        currentScopeIndex = 0
    }

fillNull : HasIO io => Int -> JList a -> io ()
fillNull index list = do
    size <- JList.size list
    nulls <- JList.nCopies (index - size) nullValue
    _ <- JList.addAll list (believe_me nulls)
    pure ()

export
saveScope : Scope -> Asm ()
saveScope scope = do
    scopes <- scopes <$> getCurrentFunction
    size <- LiftIo $ JList.size {a=Scope} scopes
    let scopeIndex = index scope
    _ <- LiftIo $
        if scopeIndex < size
            then do
                _ <- JList.set scopes scopeIndex scope
                pure ()
            else do
                fillNull scopeIndex scopes
                JList.add scopes scopeIndex scope
    pure ()

export
getScope : Int -> Asm Scope
getScope scopeIndex = do
   scopes <- scopes <$> getCurrentFunction
   LiftIo $ JList.get scopes scopeIndex

export
addScopeChild : Int -> Int -> Asm ()
addScopeChild parentScopeIndex childScopeIndex = do
    scope <- getScope parentScopeIndex
    saveScope $ record {childIndices $= (childScopeIndex ::)} scope

export
getRootMethodName : Asm Jname
getRootMethodName = jvmClassMethodName <$> getCurrentFunction

export
newLabel : Asm String
newLabel = do
    state <- GetState
    let label = "L" ++ show (labelCounter state)
    updateState $ record { labelCounter $= (+1) }
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
    case optLabel of
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
freshLambdaIndex = lambdaCounter <$> (getAndUpdateState $ record {lambdaCounter $= (+1)})

export
setScopeCounter : Int -> Asm ()
setScopeCounter scopeCounter = updateState $ record {scopeCounter = scopeCounter}

export
updateScopeStartLabel : Int -> String -> Asm ()
updateScopeStartLabel scopeIndex label = do
    scope <- getScope scopeIndex
    saveScope $ record {labels $= updateFirst label} scope

export
updateScopeEndLabel : Int -> String -> Asm ()
updateScopeEndLabel scopeIndex label = do
    scope <- getScope scopeIndex
    saveScope $ record {labels $= updateSecond label} scope

export
createVariable : String -> Asm ()
createVariable var = do
    scopeIndex <- getCurrentScopeIndex
    scope <- getScope scopeIndex
    let variableIndex = nextVariableIndex scope
    _ <- LiftIo $ Map.put (variableTypes scope) var IUnknown
    _ <- LiftIo $ Map.put (variableIndices scope) var variableIndex
    saveScope $ record { nextVariableIndex $= (+1) } scope

export
generateVariable : String -> Asm String
generateVariable namePrefix = do
    dynamicVariableIndex <- newDynamicVariableIndex
    let variableName = namePrefix ++ show dynamicVariableIndex
    createVariable variableName
    Pure variableName

namespace JAsmState
    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmState" "updateVariableIndices" "java/util/Map java/util/Map" "void"
    prim_updateVariableIndices : JMap -> JMap -> PrimIO ()

    export
    updateVariableIndices : HasIO io => Map String Int -> Map String Int -> io ()
    updateVariableIndices resultIndicesByName indicesByName =
        primIO $ prim_updateVariableIndices (believe_me resultIndicesByName) (believe_me indicesByName)

    %foreign jvm' "io/github/mmhelloworld/idrisjvm/assembler/AsmState" "getVariableNames" "java/util/Map" "java/util/List"
    prim_getVariableNames : JMap -> PrimIO JListNative

    export
    getVariableNames : HasIO io => Map String Int -> io (List String)
    getVariableNames indicesByName = do
        jlist <- primIO $ prim_getVariableNames (believe_me indicesByName)
        JList.fromIterable (believe_me jlist)

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
        case optIndex of
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
    case optTy of
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
    case optIndex of
        Just index => Pure index
        Nothing => do
          rootMethodName <- getRootMethodName
          Throw emptyFC
            ("getVariableIndexAtScope: " ++ show rootMethodName ++ ": Unknown var " ++
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
    case optIndex of
        Just index => do
            variableTypes <- getVariableTypesAtScope scopeIndex
            optTy <- LiftIo $ Map.get {value=InferredType} variableTypes index
            Pure $ fromMaybe IUnknown optTy
        Nothing => Pure IUnknown

export
getVariableType : String -> Asm InferredType
getVariableType name = getVariableTypeAtScope !getCurrentScopeIndex name

updateArgumentsForUntyped : Map Int InferredType -> Nat -> IO ()
updateArgumentsForUntyped _ Z = pure ()
updateArgumentsForUntyped types (S n) = do
  ignore $ Map.put types (cast n) inferredObjectType
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
            saveScope $ record {allVariableTypes = variableTypes, allVariableIndices = variableIndices} scope
            go (scopeIndex - 1)

getVariableScope : String -> Asm Scope
getVariableScope name = go !getCurrentScopeIndex where
    go : Int -> Asm Scope
    go scopeIndex = do
        scope <- getScope scopeIndex
        optTy <- LiftIo $ Map.get {value=InferredType} (variableTypes scope) name
        case optTy of
            Just _ => Pure scope
            Nothing => case parentIndex scope of
                Just parentScopeIndex => go parentScopeIndex
                Nothing => Throw emptyFC ("Unknown variable " ++ name)

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

export
getJvmTypeDescriptor : InferredType -> String
getJvmTypeDescriptor IByte        = "B"
getJvmTypeDescriptor IChar        = "C"
getJvmTypeDescriptor IShort       = "S"
getJvmTypeDescriptor IBool        = "Z"
getJvmTypeDescriptor IDouble      = "D"
getJvmTypeDescriptor IFloat       = "F"
getJvmTypeDescriptor IInt         = "I"
getJvmTypeDescriptor ILong        = "J"
getJvmTypeDescriptor IVoid        = "V"
getJvmTypeDescriptor IUnknown     = getJvmTypeDescriptor inferredObjectType
getJvmTypeDescriptor (IRef ty)    = "L" ++ ty ++ ";"
getJvmTypeDescriptor (IArray ty)  = "[" ++ getJvmTypeDescriptor ty

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
accessNum : Access -> Int
accessNum Final     = 16
accessNum Private   = 2
accessNum Public    = 1
accessNum Static    = 8
accessNum Synthetic = 4096

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

export
constantToObject : Asm.Constant -> Object
constantToObject (DoubleConst d) = believe_me $ doubleValueOf d
constantToObject (IntegerConst n) = believe_me $ integerValueOf n
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
prim_newJAnnArray : JListNative -> PrimIO JAnnArray

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnotationProperty" "<init>"
        "String io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue"
        "io/github/mmhelloworld/idrisjvm/assembler/AnnotationValue$AnnotationProperty"
prim_newJAnnotationProperty : String -> JAnnotationValue -> PrimIO JAnnotationProperty

%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/Annotation" "<init>"
        "String java/util/List"
        "io/github/mmhelloworld/idrisjvm/assembler/Annotation"
prim_newJAnnotation : String -> JListNative -> PrimIO JAnnotation

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
        believe_me <$> primIO (prim_newJAnnArray (believe_me values))

    toJAnnotationProperty : HasIO io => Asm.AnnotationProperty -> io JAnnotationProperty
    toJAnnotationProperty (name, annValue) = do
      jAnnotationValue <- toJAnnotationValue annValue
      primIO $ prim_newJAnnotationProperty name jAnnotationValue

    toJAnnotation : HasIO io => Asm.Annotation -> io JAnnotation
    toJAnnotation (MkAnnotation name props) = primIO $ prim_newJAnnotation name (believe_me props)

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

export
%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/IdrisName" "getIdrisConstructorClassName"
        "String" "String"
getIdrisConstructorClassName : String -> String

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
getThunkType : InferredType -> InferredType
getThunkType IInt = intThunkType
getThunkType ILong = longThunkType
getThunkType IDouble = doubleThunkType
getThunkType _ = thunkType

export
getThunkValueType : InferredType -> InferredType
getThunkValueType ty =
    if ty == intThunkType then IInt
    else if ty == longThunkType then ILong
    else if ty == doubleThunkType then IDouble
    else if ty == thunkType then inferredObjectType
    else ty

export
isThunkType : InferredType -> Bool
isThunkType ty = ty == intThunkType || ty == longThunkType || ty == doubleThunkType || ty == thunkType

export
shouldDebugAsm : Bool
shouldDebugAsm =
    let shouldDebugProperty = fromMaybe "" $ unsafePerformIO (getEnv "IDRIS_JVM_DEBUG_ASM")
    in shouldDebugProperty == "true"

export
shouldDebug : Bool
shouldDebug =
    let shouldDebugProperty = fromMaybe "" $ unsafePerformIO (getEnv "IDRIS_JVM_DEBUG")
    in shouldDebugProperty == "true"

export
debugFunction : Maybe String
debugFunction = unsafePerformIO $ getEnv "IDRIS_JVM_DEBUG_FUNCTION"

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
debug : Lazy String -> Asm ()
debug msg =
  when shouldDebug $ do
    context <- LiftIo $ do
        time <- currentTimeString
        threadName <- getCurrentThreadName
        pure $ time ++ " [" ++ threadName ++ "]"
    Debug $ context ++ ": " ++ msg

public export
data FArgList : Type where
     Nil : FArgList
     (::) : {a : Type} -> (1 arg : a) -> (1 args : FArgList) -> FArgList

export
%extern prim__jvmInstance : (ret : Type) -> String -> (1 args : FArgList) -> (1 x : %World) -> IORes ret

export
%extern prim__jvmStatic : (ret : Type) -> String -> (1 args : FArgList) -> (1 x : %World) -> IORes ret

export %inline
jvmStatic : (ret : Type) -> String -> (1 args : FArgList) -> IO ret
jvmStatic ret fn args = fromPrim (prim__jvmStatic ret fn args)

export %inline
jvmInstance : (ret : Type) -> String -> (1 args : FArgList) -> IO ret
jvmInstance ret fn args = fromPrim (prim__jvmInstance ret fn args)

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
    [assembler state, version, accessNum access, className, maybeToNullable sig, parent,
        the (JList String) $ believe_me intf, the (JList JAnnotation) $ believe_me janns]

runAsm state (CreateClass opts) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createClass"
      [assembler state, toJClassOpts opts]
runAsm state (CreateField accs sourceFileName className fieldName desc sig fieldInitialValue) = assemble state $ do
  let jaccs = sum $ accessNum <$> accs
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createField"
    [assembler state, jaccs, sourceFileName, className, fieldName, desc, maybeToNullable sig,
        maybeToNullable (toJFieldInitialValue <$> fieldInitialValue)]

runAsm state (CreateLabel label) = assemble state $
  jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.createLabel" [assembler state, label]

runAsm state (CreateMethod accs sourceFileName className methodName desc sig exceptions anns paramAnns) =
    let newState = record { currentMethodName = Jqualified className methodName } state
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
runAsm state L2i = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.l2i" [assembler state]
runAsm state (LabelStart label) =
    assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.labelStart" [assembler state, label]
runAsm state Ladd = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.ladd" [assembler state]
runAsm state Land = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.land" [assembler state]
runAsm state Laload = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.laload" [assembler state]
runAsm state Lastore = assemble state $ jvmInstance () "io/github/mmhelloworld/idrisjvm/assembler/Assembler.lastore" [assembler state]
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
