module IdrisJvm.Core.Asm

import Data.SortedMap
import IdrisJvm.IR.Types

%access public export

record JMethodName where
  constructor MkJMethodName
  jmethClsName : String
  jmethName    : String

Eq JMethodName where
  (==) (MkJMethodName c1 m1) (MkJMethodName c2 m2) = c1 == c2 && m1 == m2

Show JMethodName where
  show (MkJMethodName cname mname) = cname ++ "#" ++ mname

Ord JMethodName where
   compare (MkJMethodName c1 m1) (MkJMethodName c2 m2) =
     case c1 `compare` c2 of
       EQ => m1 `compare` m2
       classOrdering => classOrdering

data InferredType = IBool | IByte | IChar | IShort | IInt | ILong | IFloat | IDouble | Ref String
                  | IArray InferredType | IUnknown

InferredTypeStore : Type
InferredTypeStore = SortedMap Int InferredType

record InferredFunctionType where
    constructor MkInferredFunctionType
    functionName : JMethodName
    returnType : InferredType
    locsType : InferredTypeStore
    arity : Nat

rtClass : String -> String
rtClass c = "io/github/mmhelloworld/idrisjvm/runtime/" ++ c

idrisObjectType : String
idrisObjectType = rtClass "IdrisObject"

thunkClass : String
thunkClass = rtClass "Thunk"

objectClass : String
objectClass = "java/lang/Object"

futureClass : String
futureClass = "java/util/concurrent/Future"

inferredObjectType : InferredType
inferredObjectType = Ref "java/lang/Object"

inferredBigIntegerType : InferredType
inferredBigIntegerType = Ref "java/math/BigInteger"

inferredStringType : InferredType
inferredStringType = Ref "java/lang/String"

inferredIdrisObjectType : InferredType
inferredIdrisObjectType = Ref idrisObjectType

record InferenceConfig where
  constructor MkInferenceConfig
  functionName : JMethodName
  functionTypes : SortedMap JMethodName InferredFunctionType

Eq InferredType where
  IBool == IBool = True
  IByte == IByte = True
  IChar == IChar = True
  IShort == IShort = True
  IInt == IInt = True
  ILong == ILong = True
  IFloat == IFloat = True
  IDouble == IDouble = True
  (Ref ty1) == (Ref ty2) = ty1 == ty2
  (IArray elemTy1) == (IArray elemTy2) = elemTy1 == elemTy2
  IUnknown == IUnknown = True
  _ == _ = False

Show InferredType where
    show IBool = "boolean"
    show IByte = "byte"
    show IChar = "char"
    show IShort = "short"
    show IInt = "int"
    show ILong = "long"
    show IFloat = "float"
    show IDouble = "double"
    show (Ref clsName) = clsName
    show (IArray elemTy) = "Array " ++ show elemTy
    show IUnknown = "unknown"

Semigroup InferredType where
  ty1 <+> ty2 = if ty1 == ty2 then ty1 else inferredObjectType

Monoid InferredType where
  neutral = IUnknown

isPrimitive : InferredType -> Bool
isPrimitive IBool = True
isPrimitive IByte = True
isPrimitive IChar = True
isPrimitive IShort = True
isPrimitive IInt = True
isPrimitive ILong = True
isPrimitive IFloat = True
isPrimitive IDouble = True
isPrimitive _ = False

isThunk : InferredType -> Bool
isThunk (Ref ty) = ty == thunkClass
isThunk _ = False

isObject : InferredType -> Bool
isObject (Ref ty) = ty == objectClass
isObject _ = False

getLocTy : InferredTypeStore -> LVar -> InferredType
getLocTy tys (Loc varIndex) = fromMaybe IUnknown $ SortedMap.lookup varIndex tys

ReferenceName : Type
ReferenceName = String

Label : Type
Label = String

Exception : Type
Exception = String

ClassName : Type
ClassName = String

FieldName : Type
FieldName = String

MethodName : Type
MethodName = String

Descriptor : Type
Descriptor = String

Signature : Type
Signature = String

SourceFileName : Type
SourceFileName = String

mutual
  data Annotation = MkAnnotation AnnotationTypeName (List AnnotationProperty)

  data FieldInitialValue = IntField Int | StringField String | DoubleField Double
  data BsmArg = BsmArgGetType Descriptor | BsmArgHandle Handle

  data Constant = DoubleConst Double
                | IntegerConst Int
                | LongConst Bits64
                | StringConst String
                | TypeConst String

  data ReferenceTypeDescriptor = ClassDesc ClassName
                               | InterfaceDesc ClassName
                               | ArrayDesc FieldTypeDescriptor
                               | IdrisExportDesc ClassName
                               | NullableStrDesc
                               | NullableRefDesc ClassName

  data FieldTypeDescriptor = FieldTyDescByte
                           | FieldTyDescChar
                           | FieldTyDescShort
                           | FieldTyDescBoolean
                           | FieldTyDescDouble
                           | FieldTyDescFloat
                           | FieldTyDescInt
                           | FieldTyDescLong
                           | FieldTyDescReference ReferenceTypeDescriptor

  data TypeDescriptor = FieldDescriptor FieldTypeDescriptor | VoidDescriptor | ThrowableDescriptor TypeDescriptor

  Eq ReferenceTypeDescriptor where
    (ClassDesc className1          ) == (ClassDesc className2          ) = className1 == className2
    (InterfaceDesc className1      ) == (InterfaceDesc className2      ) = className1 == className2
    (ArrayDesc elemDesc1           ) == (ArrayDesc elemDesc2           ) = elemDesc1 == elemDesc2
    (IdrisExportDesc className1    ) == (IdrisExportDesc className2    ) = className1 == className2
    (NullableStrDesc               ) == (NullableStrDesc               ) = True
    (NullableRefDesc className1    ) == (NullableRefDesc className2    ) = className1 == className2
    _                                == _                                = False

  Eq FieldTypeDescriptor where
      FieldTyDescByte                == FieldTyDescByte               = True
      FieldTyDescChar                == FieldTyDescChar               = True
      FieldTyDescShort               == FieldTyDescShort              = True
      FieldTyDescBoolean             == FieldTyDescBoolean            = True
      FieldTyDescDouble              == FieldTyDescDouble             = True
      FieldTyDescFloat               == FieldTyDescFloat              = True
      FieldTyDescInt                 == FieldTyDescInt                = True
      FieldTyDescLong                == FieldTyDescLong               = True
      (FieldTyDescReference refTy1)  == (FieldTyDescReference refTy2) = refTy1 == refTy2
      _                              == _                             = False

  Eq TypeDescriptor where
    (FieldDescriptor fieldTyDesc1) == (FieldDescriptor fieldTyDesc2) = fieldTyDesc1 == fieldTyDesc2
    VoidDescriptor == VoidDescriptor = True
    (ThrowableDescriptor tyDesc1) == (ThrowableDescriptor tyDesc2) = tyDesc1 == tyDesc2
    _ == _ = False

  data MethodDescriptor = MkMethodDescriptor (List FieldTypeDescriptor) TypeDescriptor

  data ClassOpts = ComputeMaxs | ComputeFrames

  data InvocType = InvokeInterface | InvokeSpecial | InvokeStatic | InvokeVirtual

  data FieldInsType = FGetStatic | FPutStatic | FGetField | FPutField

  data FrameType = FFull | FSame | FAppend

  data Access = Private | Public | Static | Synthetic | Final

  Eq Access where
    Private == Private = True
    Public == Public = True
    Static == Static = True
    Synthetic == Synthetic = True
    Final == Final = True
    _ == _ = False

  data AnnotationValue = AnnInt Int
                       | AnnString String
                       | AnnEnum String String
                       | AnnArray (List AnnotationValue)

  data HandleTag = HGetField
                 | HGetStatic
                 | HPutField
                 | HPutStatic
                 | HInvokeVirtual
                 | HInvokeStatic
                 | HInvokeSpecial
                 | HNewInvokeSpecial
                 | HInvokeInterface

  record Handle where
    constructor MkHandle
    tag : HandleTag
    hClassName  : ClassName
    hMethodName : MethodName
    hDescriptor : Descriptor
    isInterface : Bool

  data ReferenceType = RefTyClass | RefTyInterface

  AnnotationProperty : Type
  AnnotationProperty = (String, AnnotationValue)

  AnnotationTypeName : Type
  AnnotationTypeName = String

  Show ReferenceTypeDescriptor where
    show (ClassDesc className) = "ClassDesc " ++ show className
    show (InterfaceDesc className) = "InterfaceDesc " ++ show className
    show (ArrayDesc tyDesc) = "ArrayDesc " ++ show tyDesc
    show (IdrisExportDesc className) = "IdrisExportDesc " ++ show className
    show NullableStrDesc = "NullableStrDesc"
    show (NullableRefDesc className) = "NullableRefDesc" ++ show className

  Show FieldTypeDescriptor where
    show FieldTyDescByte = "FieldTyDescByte"
    show FieldTyDescChar = "FieldTyDescChar"
    show FieldTyDescShort = "FieldTyDescShort"
    show FieldTyDescBoolean = "FieldTyDescBoolean"
    show FieldTyDescDouble = "FieldTyDescDouble"
    show FieldTyDescFloat = "FieldTyDescFloat"
    show FieldTyDescInt = "FieldTyDescInt"
    show FieldTyDescLong = "FieldTyDescLong"
    show (FieldTyDescReference referenceTypeDescriptor) = "FieldTyDescReference(" ++ show referenceTypeDescriptor ++ ")"

  Show TypeDescriptor where
    show (FieldDescriptor fieldTypeDescriptor) = "FieldDescriptor(" ++ show fieldTypeDescriptor ++ ")"
    show VoidDescriptor = "VoidDescriptor"
    show (ThrowableDescriptor tyDesc) = "ThrowableDescriptor(" ++ show tyDesc ++ ")"

mutual
    asmRefTyDesc : ReferenceTypeDescriptor -> String
    asmRefTyDesc (ClassDesc c)       = "L" ++ c ++ ";"
    asmRefTyDesc (IdrisExportDesc c) = "L" ++ c ++ ";"
    asmRefTyDesc (InterfaceDesc c)   = "L" ++ c ++ ";"
    asmRefTyDesc (NullableRefDesc c) = "L" ++ c ++ ";"
    asmRefTyDesc NullableStrDesc     = "Ljava/lang/String;"
    asmRefTyDesc (ArrayDesc ty)   = "[" ++ asmFieldTypeDesc ty

    refTyClassName : ReferenceTypeDescriptor -> ClassName
    refTyClassName (ClassDesc c)       = c
    refTyClassName (InterfaceDesc c)   = c
    refTyClassName (IdrisExportDesc c) = c
    refTyClassName (NullableRefDesc c) = c
    refTyClassName NullableStrDesc     = "java/lang/String"
    refTyClassName arr@(ArrayDesc _)   = asmRefTyDesc arr

    asmFieldTypeDesc : FieldTypeDescriptor -> String
    asmFieldTypeDesc FieldTyDescByte           = "B"
    asmFieldTypeDesc FieldTyDescChar           = "C"
    asmFieldTypeDesc FieldTyDescShort          = "S"
    asmFieldTypeDesc FieldTyDescBoolean        = "Z"
    asmFieldTypeDesc FieldTyDescDouble         = "D"
    asmFieldTypeDesc FieldTyDescFloat          = "F"
    asmFieldTypeDesc FieldTyDescInt            = "I"
    asmFieldTypeDesc FieldTyDescLong           = "J"
    asmFieldTypeDesc (FieldTyDescReference f)  = asmRefTyDesc f

asmTypeDesc : TypeDescriptor -> String
asmTypeDesc (FieldDescriptor t) = asmFieldTypeDesc t
asmTypeDesc VoidDescriptor      = "V"
asmTypeDesc (ThrowableDescriptor tyDesc) = asmTypeDesc tyDesc

asmMethodDesc : MethodDescriptor -> String
asmMethodDesc (MkMethodDescriptor args returns) = "(" ++ asmArgs ++ ")" ++ r where
  asmArgs = concat $ asmFieldTypeDesc <$> args
  r = asmTypeDesc returns

data Asm : Type -> Type where
    Aaload : Asm ()
    Aastore : Asm ()
    Aconstnull : Asm ()
    Aload : Int -> Asm ()
    Anewarray : Descriptor -> Asm ()

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
    Checkcast : Descriptor -> Asm ()
    ClassCodeStart : Int -> Access -> ClassName -> (Maybe Signature) -> ClassName -> List ClassName -> List Annotation -> Asm ()
    ClassCodeEnd : String -> Asm ()
    CreateClass : ClassOpts -> Asm ()
    CreateField : List Access -> ClassName -> FieldName -> Descriptor -> Maybe Signature -> Maybe FieldInitialValue -> Asm ()
    CreateLabel : String -> Asm Label
    CreateMethod : List Access -> ClassName -> MethodName -> Descriptor -> Maybe Signature ->
                   Maybe (List Exception) -> List Annotation -> List (List Annotation) -> Asm ()
    D2i : Asm ()
    D2f : Asm ()
    Dadd : Asm ()
    Daload : Asm ()
    Dastore : Asm ()
    Dconst : Double -> Asm ()
    Ddiv : Asm ()
    Debug : String -> Asm ()
    Dload : Int -> Asm ()
    Dmul : Asm ()
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
    Field : FieldInsType -> ClassName -> FieldName -> Descriptor -> Asm ()
    FieldEnd : Asm ()
    Fload : Int -> Asm ()
    Frame : FrameType -> Nat -> (List Signature) -> Nat -> (List Signature) -> Asm ()
    FreshIfIndex : Asm Nat
    FreshLambdaIndex : String -> Asm Nat
    FreshSwitchIndex : Asm Nat
    Freturn : Asm ()
    Fstore : Int -> Asm ()
    GetFunctionName : Asm JMethodName
    GetFunctionLocTypes : Asm InferredTypeStore
    GetFunctionRetType : Asm InferredType
    GetFunctionTypes : Asm (SortedMap JMethodName InferredFunctionType)
    GetLocalVarCount : Asm Nat
    Goto : Label -> Asm ()
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
    Ifeq : Label -> Asm ()
    Ificmpge : Label -> Asm ()
    Ificmpgt : Label -> Asm ()
    Ificmple : Label -> Asm ()
    Ificmplt : Label -> Asm ()
    Ifnonnull : Label -> Asm ()
    Ifnull : Label -> Asm ()
    Iload : Int -> Asm ()
    Imul : Asm ()
    InstanceOf : ClassName -> Asm ()
    InvokeMethod : InvocType -> ClassName -> MethodName -> Descriptor -> Bool -> Asm ()
    InvokeDynamic : MethodName -> Descriptor -> Handle -> List BsmArg -> Asm ()
    Irem : Asm ()
    Ireturn : Asm ()
    Ishl : Asm ()
    Ishr : Asm ()
    Istore : Int -> Asm ()
    Isub : Asm ()
    Iushr : Asm ()
    L2i : Asm ()
    LabelStart : Label -> Asm ()
    Ladd : Asm ()
    Laload : Asm ()
    Land : Asm ()
    Lastore : Asm ()
    Lor : Asm ()
    Lxor : Asm ()
    Lcompl : Asm ()
    Lconst : Bits64 -> Asm ()
    Ldc : Constant -> Asm ()
    Ldiv : Asm ()
    Lload : Int  -> Asm ()
    Lmul : Asm ()
    LookupSwitch : Label -> List Label -> List Int-> Asm ()
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
    Multianewarray : Descriptor -> Nat -> Asm ()
    New : ClassName -> Asm ()
    Pop : Asm ()
    Pop2 : Asm ()
    Return : Asm ()
    Saload : Asm ()
    Sastore : Asm ()
    ShouldDescribeFrame : Asm Bool
    SourceInfo : SourceFileName -> Asm ()
    Subroutine : Asm () -> Asm ()
    UpdateFunctionName : JMethodName -> Asm ()
    UpdateFunctionLocTypes : InferredTypeStore -> Asm ()
    UpdateFunctionRetType : InferredType -> Asm ()
    UpdateFunctionTypes : SortedMap JMethodName InferredFunctionType -> Asm ()
    UpdateLocalVarCount : Nat -> Asm ()
    UpdateShouldDescribeFrame : Bool -> Asm ()
    UpdateSwitchIndex : Nat -> Asm ()
    UpdateIfIndex : Nat -> Asm ()

    Pure : ty -> Asm ty
    Bind : Asm a -> (a -> Asm b) -> Asm b

namespace AsmDo
  (>>=) : Asm a -> (a -> Asm b) -> Asm b
  (>>=) = Bind

Functor Asm where
  map f a = Bind a (\a' => Pure $ f a')

Applicative Asm where
  pure = Pure

  (<*>) f a = Bind f (\f' =>
              Bind a (\a' =>
              Pure (f' a')))

getFunctionType : JMethodName -> Asm (InferredType, InferredTypeStore)
getFunctionType fname =
  maybe (IUnknown, SortedMap.empty) (\fty => (returnType fty, locsType fty))
    . SortedMap.lookup fname <$> GetFunctionTypes

isExportedDesc : FieldTypeDescriptor -> Bool
isExportedDesc (FieldTyDescReference (IdrisExportDesc _)) = True
isExportedDesc _ = False

isExportedReturnDesc : TypeDescriptor -> Bool
isExportedReturnDesc (FieldDescriptor (FieldTyDescReference (IdrisExportDesc _))) = True
isExportedReturnDesc _ = False
