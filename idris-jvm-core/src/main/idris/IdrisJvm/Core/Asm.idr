module IdrisJvm.Core.Asm

%access public export

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
                               | ArrayDesc ReferenceTypeDescriptor
                               | IdrisExportDesc ClassName
                               | NullableStrDesc
                               | NullableRefDesc ClassName

  data FieldTypeDescriptor = FieldTyDescByte
                           | FieldTyDescChar
                           | FieldTyDescShort
                           | FieldTyDescBoolean
                           | FieldTyDescArray
                           | FieldTyDescDouble
                           | FieldTyDescFloat
                           | FieldTyDescInt
                           | FieldTyDescLong
                           | FieldTyDescReference ReferenceTypeDescriptor

  data TypeDescriptor = FieldDescriptor FieldTypeDescriptor | VoidDescriptor

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

  AnnotationProperty : Type
  AnnotationProperty = (String, AnnotationValue)

  AnnotationTypeName : Type
  AnnotationTypeName = String

  Show ReferenceTypeDescriptor where
    show (ClassDesc className) = "ClassDesc " ++ show className
    show (InterfaceDesc className) = "InterfaceDesc " ++ show className
    show (ArrayDesc referenceTypeDescriptor) = "ArrayDesc " ++ show referenceTypeDescriptor
    show (IdrisExportDesc className) = "IdrisExportDesc " ++ show className
    show NullableStrDesc = "NullableStrDesc"
    show (NullableRefDesc className) = "NullableRefDesc" ++ show className

  Show FieldTypeDescriptor where
    show FieldTyDescByte = "FieldTyDescByte"
    show FieldTyDescChar = "FieldTyDescChar"
    show FieldTyDescShort = "FieldTyDescShort"
    show FieldTyDescBoolean = "FieldTyDescBoolean"
    show FieldTyDescArray = "FieldTyDescArray"
    show FieldTyDescDouble = "FieldTyDescDouble"
    show FieldTyDescFloat = "FieldTyDescFloat"
    show FieldTyDescInt = "FieldTyDescInt"
    show FieldTyDescLong = "FieldTyDescLong"
    show (FieldTyDescReference referenceTypeDescriptor) = "FieldTyDescReference(" ++ show referenceTypeDescriptor ++ ")"

  Show TypeDescriptor where
    show (FieldDescriptor fieldTypeDescriptor) = "FieldDescriptor(" ++ show fieldTypeDescriptor ++ ")"
    show VoidDescriptor = "VoidDescriptor"

record JMethodName where
  constructor MkJMethodName
  jmethClsName : String
  jmethName    : String

Eq JMethodName where
  (==) (MkJMethodName c1 m1) (MkJMethodName c2 m2) = c1 == c2 && m1 == m2

Show JMethodName where
  show (MkJMethodName cname mname) = cname ++ "#" ++ mname


asmRefTyDesc : ReferenceTypeDescriptor -> String
asmRefTyDesc (ClassDesc c)       = "L" ++ c ++ ";"
asmRefTyDesc (IdrisExportDesc c) = "L" ++ c ++ ";"
asmRefTyDesc (InterfaceDesc c)   = "L" ++ c ++ ";"
asmRefTyDesc (NullableRefDesc c) = "L" ++ c ++ ";"
asmRefTyDesc NullableStrDesc     = "Ljava/lang/String;"
asmRefTyDesc (ArrayDesc refTy)   = "[" ++ asmRefTyDesc refTy

refTyClassName : ReferenceTypeDescriptor -> ClassName
refTyClassName (ClassDesc c)       = c
refTyClassName (InterfaceDesc c)   = c
refTyClassName (IdrisExportDesc c) = c
refTyClassName (NullableRefDesc c) = c
refTyClassName NullableStrDesc     = "java/lang/String"
refTyClassName arr@(ArrayDesc _)   = asmRefTyDesc arr

asmFieldTypeDesc : FieldTypeDescriptor -> String
asmFieldTypeDesc FieldTyDescByte          = "B"
asmFieldTypeDesc FieldTyDescChar          = "C"
asmFieldTypeDesc FieldTyDescShort         = "S"
asmFieldTypeDesc FieldTyDescBoolean       = "Z"
asmFieldTypeDesc FieldTyDescArray         = "["
asmFieldTypeDesc FieldTyDescDouble        = "D"
asmFieldTypeDesc FieldTyDescFloat         = "F"
asmFieldTypeDesc FieldTyDescInt           = "I"
asmFieldTypeDesc FieldTyDescLong          = "J"
asmFieldTypeDesc (FieldTyDescReference f) = asmRefTyDesc f

asmTypeDesc : TypeDescriptor -> String
asmTypeDesc (FieldDescriptor t) = asmFieldTypeDesc t
asmTypeDesc VoidDescriptor      = "V"

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
    Astore : Int -> Asm ()
    Areturn : Asm ()
    Checkcast : Descriptor -> Asm ()
    ClassCodeStart : Int -> Access -> ClassName -> (Maybe Signature) -> ClassName -> List ClassName -> List Annotation -> Asm ()
    ClassCodeEnd : String -> Asm ()
    CreateClass : ClassOpts -> Asm ()
    CreateField : List Access -> ClassName -> FieldName -> Descriptor -> Maybe Signature -> Maybe FieldInitialValue -> Asm ()
    CreateLabel : String -> Asm Label
    CreateMethod : List Access -> ClassName -> MethodName -> Descriptor -> Maybe Signature ->
                   Maybe (List Exception) -> List Annotation -> List (List Annotation) -> Asm ()
    Dadd : Asm ()
    Ddiv : Asm ()
    Dload : Int -> Asm ()
    Dmul : Asm ()
    Drem : Asm ()
    Dreturn : Asm ()
    Dsub : Asm ()
    Dup : Asm ()
    Error : String -> Asm ()
    F2d : Asm ()
    Field : FieldInsType -> ClassName -> FieldName -> Descriptor -> Asm ()
    FieldEnd : Asm ()
    Fload : Int -> Asm ()
    Frame : FrameType -> Nat -> (List Signature) -> Nat -> (List Signature) -> Asm ()
    FreshIfIndex : Asm Nat
    FreshLambdaIndex : String -> Asm Nat
    FreshSwitchIndex : Asm Nat
    Freturn : Asm ()
    GetFunctionName : Asm JMethodName
    GetLocalVarCount : Asm Nat
    Goto : Label -> Asm ()
    I2c : Asm ()
    I2l : Asm ()
    Iadd : Asm ()
    Iand : Asm ()
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
    Iload : Int -> Asm ()
    Imul : Asm ()
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
    Land : Asm ()
    Lor : Asm ()
    Lxor : Asm ()
    Lcompl : Asm ()
    Ldc : Constant -> Asm ()
    Ldiv : Asm ()
    Lload : Int  -> Asm ()
    Lmul : Asm ()
    LookupSwitch : Label -> List Label -> List Int-> Asm ()
    Lrem : Asm ()
    Lreturn : Asm ()
    Lshl : Asm ()
    Lshr : Asm ()
    Lsub : Asm ()
    Lushr : Asm ()
    MaxStackAndLocal : Int -> Int -> Asm ()
    MethodCodeStart : Asm ()
    MethodCodeEnd : Asm ()
    New : ClassName -> Asm ()
    Pop : Asm ()
    Pop2 : Asm ()
    Return : Asm ()
    ShouldDescribeFrame : Asm Bool
    SourceInfo : SourceFileName -> Asm ()
    Subroutine : Asm () -> Asm ()
    UpdateFunctionName : JMethodName -> Asm ()
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
