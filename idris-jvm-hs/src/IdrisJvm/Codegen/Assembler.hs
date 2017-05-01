{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Assembler where

import           Control.Monad (join)
import           Data.Aeson
import qualified Data.DList    as DL
import           Data.Int      (Int32)
import           Data.List     (intercalate)
import           Data.Word     (Word64)
import qualified Data.Vector as V

data Asm = Aaload
         | Aastore
         | Aconstnull
         | Aload Int
         | Anewarray Descriptor
         | Astore Int
         | Areturn
         | Checkcast Descriptor
         | ClassCodeStart Int Access ClassName (Maybe Signature) ClassName [ClassName] [Annotation]
         | ClassCodeEnd String
         | CreateClass ClassOpts
         | CreateField [Access] ClassName FieldName Descriptor (Maybe Signature) (Maybe FieldInitialValue)
         | CreateLabel String
         | CreateMethod [Access] ClassName MethodName Descriptor (Maybe Signature) (Maybe [Exception]) [Annotation] [[Annotation]]
         | Dadd
         | Ddiv
         | Dload Int
         | Dmul
         | Drem
         | Dreturn
         | Dsub
         | Dup
         | F2d
         | Field FieldInsType ClassName FieldName Descriptor
         | FieldEnd
         | Fload Int
         | Frame FrameType Int [Signature] Int [Signature]
         | Freturn
         | Goto Label
         | I2c
         | I2l
         | Iadd
         | Iand
         | Iconst Int
         | Idiv
         | Ifeq Label
         | Ificmpge Label
         | Ificmpgt Label
         | Ificmple Label
         | Ificmplt Label
         | Iload Int
         | Imul
         | InvokeMethod InvocType ClassName MethodName Descriptor Bool
         | InvokeDynamic MethodName Descriptor Handle [BsmArg]
         | Irem
         | Ireturn
         | Ishl
         | Ishr
         | Istore Int
         | Isub
         | Iushr
         | L2i
         | LabelStart Label
         | Ladd
         | Land
         | Ldc Constant
         | Ldiv
         | Lload Int
         | Lmul
         | LookupSwitch Label [Label] [Int32]
         | Lrem
         | Lreturn
         | Lshl
         | Lshr
         | Lsub
         | Lushr
         | MaxStackAndLocal Int Int
         | MethodCodeStart
         | MethodCodeEnd
         | New ClassName
         | Pop
         | Pop2
         | Return
         | SourceInfo SourceFileName

data AnnotationValue = AnnInt Int
                     | AnnString String
                     | AnnArray [AnnotationValue] deriving (Show)

instance ToJSON AnnotationValue where
  toJSON (AnnInt n)
    = object [ "type" .= String "AnnInt"
             , "value" .= toJSON n ]
  toJSON (AnnString s)
    = object [ "type" .= String "AnnString"
             , "value" .= toJSON s ]
  toJSON (AnnArray values)
    = object [ "type" .= String "AnnArray"
             , "value" .= toJSON values ]

type AnnotationProperty = (String, AnnotationValue)

type AnnotationTypeName = String

data Annotation = Annotation AnnotationTypeName [ AnnotationProperty ] deriving (Show)

toJSONAnnotationProperty :: String -> AnnotationValue -> Value
toJSONAnnotationProperty name value
  = object [ "name" .= toJSON name
           , "value" .= toJSON value ]

instance ToJSON Annotation where
  toJSON (Annotation typeName properties)
    = object [ "type" .= String "Annotation"
             , "name" .= toJSON typeName
             , "props" .= (Array $ V.fromList $ uncurry toJSONAnnotationProperty <$> properties) ]

instance Show Asm where
  show = show . toJSON

instance ToJSON Asm where
  toJSON Aaload = object [ "type" .= String "Aaload" ]

  toJSON Aastore = object [ "type" .= String "Aastore" ]

  toJSON Aconstnull = object [ "type" .= String "Aconstnull" ]

  toJSON (Aload index)
    = object [ "type" .= String "Aload"
             , "index" .= toJSON index ]

  toJSON (Anewarray desc)
    = object [ "type" .= String "Anewarray"
             , "desc" .= toJSON desc ]

  toJSON (Astore index)
    = object [ "type" .= String "Astore"
             , "index" .= toJSON index ]

  toJSON Areturn = object [ "type" .= String "Areturn" ]

  toJSON (Checkcast desc)
    = object [ "type" .= String "Checkcast"
             , "desc" .= toJSON desc ]

  toJSON (ClassCodeStart version acc cname s super intf annotations)
    = object [ "type" .= String "ClassCodeStart"
             , "version" .= toJSON version
             , "acc" .= toJSON acc
             , "name" .= toJSON cname
             , "sig" .= maybe Null toJSON s
             , "parent" .= toJSON super
             , "interfaces" .= toJSON intf
             , "annotations" .= toJSON annotations ]

  toJSON (ClassCodeEnd out)
    = object [ "type" .= String "ClassCodeEnd"
             , "out" .= toJSON out ]

  toJSON (CreateClass flags)
    = object [ "type" .= String "CreateClass"
             , "flags" .= toJSON flags ]

  toJSON (CreateField access className fieldName desc fsig initialValue)
    = object [ "type" .= String "CreateField"
             , "acc" .= toJSON (sum $ accessNum <$> access)
             , "cname" .= toJSON className
             , "name" .= toJSON fieldName
             , "desc" .= toJSON desc
             , "sig" .= maybe Null toJSON fsig
             , "initVal" .= maybe Null toJSON initialValue ]

  toJSON (CreateLabel label)
    = object [ "type" .= String "CreateLabel"
             , "name" .= toJSON label ]

  toJSON (CreateMethod accs cname mname desc s excs anns paramAnns)
    = object [ "type" .= String "CreateMethod"
             , "acc" .= toJSON (sum $ accessNum <$> accs)
             , "cname" .= toJSON cname
             , "fname" .= toJSON mname
             , "desc" .= toJSON desc
             , "sig" .= maybe Null toJSON s
             , "excs" .= toJSON excs
             , "anns" .= toJSON anns
             , "paramAnns" .= toJSON paramAnns ]

  toJSON Dadd = object [ "type" .= String "Dadd" ]
  toJSON Ddiv = object [ "type" .= String "Ddiv" ]

  toJSON (Dload n)
    = object [ "type" .= String "Dload"
             , "n" .= toJSON n ]

  toJSON Dmul = object [ "type" .= String "Dmul" ]
  toJSON Drem = object [ "type" .= String "Drem" ]
  toJSON Dreturn = object [ "type" .= String "Dreturn" ]
  toJSON Dsub = object [ "type" .= String "Dsub" ]
  toJSON Dup = object [ "type" .= String "Dup" ]
  toJSON F2d = object [ "type" .= String "F2d" ]

  toJSON (Field ftype cname fname desc)
    = object [ "type" .= String "Field"
             , "ftype" .= toJSON ftype
             , "cname" .= toJSON cname
             , "fname" .= toJSON fname
             , "desc" .= toJSON desc ]

  toJSON FieldEnd = object [ "type" .= String "FieldEnd" ]

  toJSON (Fload n)
    = object [ "type" .= String "Fload"
             , "n" .= toJSON n ]

  toJSON (Frame frameType nlocal local nstack stack)
    = object [ "type" .= String "Frame"
             , "ftype" .= toJSON frameType
             , "nlocal" .= toJSON nlocal
             , "local" .= toJSON local
             , "nstack" .= toJSON nstack
             , "stack" .= toJSON stack ]

  toJSON Freturn = object [ "type" .= String "Freturn" ]

  toJSON (Goto label)
    = object [ "type" .= String "Goto"
             , "label" .= toJSON label ]

  toJSON I2c = object [ "type" .= String "I2c" ]

  toJSON I2l = object [ "type" .= String "I2l" ]

  toJSON Iadd = object [ "type" .= String "Iadd" ]

  toJSON Iand = object [ "type" .= String "Iand" ]

  toJSON (Iconst n)
    = object [ "type" .= String "Iconst"
             , "n" .= toJSON n ]

  toJSON Idiv = object [ "type" .= String "Idiv" ]

  toJSON (Ifeq label)
    = object [ "type" .= String "Ifeq"
             , "label" .= toJSON label ]

  toJSON (Ificmpge label)
    = object [ "type" .= String "Ificmpge"
             , "label" .= toJSON label ]

  toJSON (Ificmpgt label)
    = object [ "type" .= String "Ificmpgt"
             , "label" .= toJSON label ]

  toJSON (Ificmple label)
    = object [ "type" .= String "Ificmple"
             , "label" .= toJSON label ]

  toJSON (Ificmplt label)
    = object [ "type" .= String "Ificmplt"
             , "label" .= toJSON label ]

  toJSON (Iload n)
    = object [ "type" .= String "Iload"
             , "n" .= toJSON n ]

  toJSON Imul = object [ "type" .= String "Imul" ]

  toJSON Irem = object [ "type" .= String "Irem" ]

  toJSON Ireturn = object [ "type" .= String "Ireturn" ]

  toJSON Ishl = object [ "type" .= String "Ishl" ]

  toJSON Ishr = object [ "type" .= String "Ishr" ]

  toJSON Iushr = object [ "type" .= String "Iushr" ]

  toJSON (InvokeMethod invType cname mname desc isIntf)
    = object [ "type" .= String "InvokeMethod"
             , "invType" .= toJSON invType
             , "cname" .= toJSON cname
             , "mname" .= toJSON mname
             , "desc" .= toJSON desc
             , "isIntf" .= toJSON isIntf ]

  toJSON (InvokeDynamic name desc handle args)
    = object [ "type" .= String "InvokeDynamic"
             , "name" .= toJSON name
             , "desc" .= toJSON desc
             , "handle" .= toJSON handle
             , "args" .= toJSON args ]

  toJSON (Istore n)
    = object [ "type" .= String "Istore"
             , "n" .= toJSON n ]

  toJSON Isub = object [ "type" .= String "Isub" ]

  toJSON L2i = object [ "type" .= String "L2i" ]

  toJSON (LabelStart label)
    = object [ "type" .= String "LabelStart"
             , "label" .= toJSON label ]

  toJSON Ladd = object [ "type" .= String "Ladd" ]

  toJSON Land = object [ "type" .= String "Land" ]

  toJSON (Ldc c) = toJSON c

  toJSON Ldiv = object [ "type" .= String "Ldiv" ]

  toJSON (Lload n)
    = object [ "type" .= String "Lload"
             , "n" .= toJSON n ]

  toJSON Lmul = object [ "type" .= String "Lmul" ]

  toJSON (LookupSwitch dlabel clabels vals)
    = object [ "type" .= String "LookupSwitch"
             , "dlabel" .= toJSON dlabel
             , "clabels" .= toJSON clabels
             , "vals" .= toJSON vals ]

  toJSON Lrem = object [ "type" .= String "Lrem" ]

  toJSON Lreturn = object [ "type" .= String "Lreturn" ]

  toJSON Lshl = object [ "type" .= String "Lshl" ]

  toJSON Lshr = object [ "type" .= String "Lshr" ]

  toJSON Lsub = object [ "type" .= String "Lsub" ]

  toJSON Lushr = object [ "type" .= String "Lushr" ]

  toJSON (MaxStackAndLocal nstack nlocal)
    = object [ "type" .= String "MaxStackAndLocal"
             , "nstack" .= toJSON nstack
             , "nlocal" .= toJSON nlocal ]

  toJSON MethodCodeStart = object [ "type" .= String "MethodCodeStart" ]

  toJSON MethodCodeEnd = object [ "type" .= String "MethodCodeEnd" ]

  toJSON (New name)
    = object [ "type" .= String "New"
             , "name" .= toJSON name ]

  toJSON Pop = object [ "type" .= String "Pop" ]

  toJSON Pop2 = object [ "type" .= String "Pop2" ]

  toJSON Return = object [ "type" .= String "Return" ]

  toJSON (SourceInfo name)
    = object [ "type" .= String "SourceInfo"
             , "name" .= toJSON name ]

data FieldInitialValue = IntField Int | StringField String | DoubleField Double

instance ToJSON FieldInitialValue where
  toJSON (IntField n)    = toJSON n
  toJSON (StringField s) = toJSON s
  toJSON (DoubleField d) = toJSON d

data BsmArg = BsmArgGetType Descriptor | BsmArgHandle Handle

instance ToJSON BsmArg where
  toJSON (BsmArgGetType desc) = object ["type" .= String "BsmArgGetType"
                                    , "desc" .= toJSON desc ]
  toJSON (BsmArgHandle h) = object ["type" .= String "BsmArgHandle"
                                    , "handle" .= toJSON h ]

data Constant = DoubleConst Double
              | IntegerConst Int
              | LongConst Word64
              | StringConst String
              | TypeConst String

instance ToJSON Constant where
  toJSON (DoubleConst n)
    = object [ "type" .= String "LdcDouble"
             , "val" .= toJSON n ]
  toJSON (IntegerConst n)
    = object [ "type" .= String "LdcInteger"
             , "val" .= toJSON n ]
  toJSON (LongConst n)
    = object [ "type" .= String "LdcLong"
             , "val" .= toJSON n ]
  toJSON (StringConst s)
    = object [ "type" .= String "LdcString"
             , "val" .= toJSON s ]
  toJSON (TypeConst s)
    = object [ "type" .= String "LdcType"
             , "val" .= toJSON s ]

class Asmable a where
  asm :: a -> String

data ReferenceTypeDescriptor = ClassDesc ClassName
                             | InterfaceDesc ClassName
                             | ArrayDesc ReferenceTypeDescriptor
                             | IdrisExportDesc ClassName
                             | NullableStrDesc
                             | NullableRefDesc ClassName
                               deriving (Eq, Show)

instance Asmable ReferenceTypeDescriptor where
  asm (ClassDesc c)       = "L" ++ c ++ ";"
  asm (IdrisExportDesc c) = "L" ++ c ++ ";"
  asm (InterfaceDesc c)   = "L" ++ c ++ ";"
  asm (NullableRefDesc c) = "L" ++ c ++ ";"
  asm NullableStrDesc     = "Ljava/lang/String;"
  asm (ArrayDesc refTy)   = "[" ++ asm refTy

refTyClassName :: ReferenceTypeDescriptor -> ClassName
refTyClassName (ClassDesc c)       = c
refTyClassName (InterfaceDesc c)   = c
refTyClassName (IdrisExportDesc c) = c
refTyClassName (NullableRefDesc c) = c
refTyClassName NullableStrDesc     = "java/lang/String"
refTyClassName arr@ArrayDesc{}     = asm arr

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
                           deriving (Eq, Show)

instance Asmable FieldTypeDescriptor where
  asm FieldTyDescByte          = "B"
  asm FieldTyDescChar          = "C"
  asm FieldTyDescShort         = "S"
  asm FieldTyDescBoolean       = "Z"
  asm FieldTyDescArray         = "["
  asm FieldTyDescDouble        = "D"
  asm FieldTyDescFloat         = "F"
  asm FieldTyDescInt           = "I"
  asm FieldTyDescLong          = "J"
  asm (FieldTyDescReference f) = asm f

data TypeDescriptor = FieldDescriptor FieldTypeDescriptor | VoidDescriptor deriving (Eq, Show)

instance Asmable TypeDescriptor where
  asm (FieldDescriptor t) = asm t
  asm VoidDescriptor      = "V"

data MethodDescriptor = MethodDescriptor [FieldTypeDescriptor] TypeDescriptor

instance Asmable MethodDescriptor where
  asm (MethodDescriptor args returns)
    = within "(" as ")" ++ r
      where as = concat $ asm <$> args
            r = asm returns

type ReferenceName = String

data ReferenceType = RefTyClass | RefTyInterface

type Label = String
type Exception = String
type ClassName = String
type FieldName = String
type MethodName = String
type Descriptor = String
type Signature = String
type SourceFileName = String

data ClassOpts = ComputeMaxs | ComputeFrames

instance ToJSON ClassOpts where
  toJSON ComputeMaxs   = toJSON (1 :: Int)
  toJSON ComputeFrames = toJSON (2 :: Int)

data InvocType = InvokeInterface | InvokeSpecial | InvokeStatic | InvokeVirtual
invocTypeNum :: InvocType -> Int
invocTypeNum InvokeInterface = 185
invocTypeNum InvokeSpecial   = 183
invocTypeNum InvokeStatic    = 184
invocTypeNum InvokeVirtual   = 182

instance ToJSON InvocType where
  toJSON = toJSON . invocTypeNum


data FieldInsType = FGetStatic | FPutStatic | FGetField | FPutField
fieldInsTypeNum :: FieldInsType -> Int
fieldInsTypeNum FGetStatic = 178
fieldInsTypeNum FPutStatic = 179
fieldInsTypeNum FGetField  = 180
fieldInsTypeNum FPutField  = 181

instance ToJSON FieldInsType where
  toJSON = toJSON . fieldInsTypeNum

data FrameType = FFull | FSame | FAppend
frameTypeNum :: FrameType -> Int
frameTypeNum FFull   = 0
frameTypeNum FSame   = 3
frameTypeNum FAppend = 1

instance ToJSON FrameType where
  toJSON = toJSON . frameTypeNum

data Access = Private | Public | Static | Synthetic | Final deriving (Eq, Show)

accessNum :: Access -> Int
accessNum Final     = 16
accessNum Private   = 2
accessNum Public    = 1
accessNum Static    = 8
accessNum Synthetic = 4096

instance ToJSON Access where
  toJSON = toJSON . accessNum

data HandleTag = HGetField
               | HGetStatic
               | HPutField
               | HPutStatic
               | HInvokeVirtual
               | HInvokeStatic
               | HInvokeSpecial
               | HNewInvokeSpecial
               | HInvokeInterface
handleTagOpcode :: HandleTag -> Int
handleTagOpcode HGetField         = 1
handleTagOpcode HGetStatic        = 2
handleTagOpcode HPutField         = 3
handleTagOpcode HPutStatic        = 4
handleTagOpcode HInvokeVirtual    = 5
handleTagOpcode HInvokeStatic     = 6
handleTagOpcode HInvokeSpecial    = 7
handleTagOpcode HNewInvokeSpecial = 8
handleTagOpcode HInvokeInterface  = 9

instance ToJSON HandleTag where
  toJSON = toJSON . handleTagOpcode

data Handle = Handle { tag         :: HandleTag
                     , hClassName  :: ClassName
                     , hMethodName :: MethodName
                     , hDescriptor :: Descriptor
                     , isInterface :: Bool
                     }

instance ToJSON Handle where
  toJSON (Handle t cname mname desc isIntf)
    = object [ "tag" .= toJSON t
             , "cname" .= toJSON cname
             , "mname" .= toJSON mname
             , "desc" .= toJSON desc
             , "isIntf" .= toJSON isIntf ]

assign :: Int -> Int -> DL.DList Asm
assign from to | from == to = []
assign from to = [Aload from, Astore to]

boxDouble :: Asm
boxDouble = InvokeMethod InvokeStatic "java/lang/Double" "valueOf" "(D)Ljava/lang/Double;" False

boxBool :: Asm
boxBool = InvokeMethod InvokeStatic "java/lang/Boolean" "valueOf" "(Z)Ljava/lang/Boolean;" False

boxChar :: Asm
boxChar = InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

boxInt :: Asm
boxInt = InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False

boxLong :: Asm
boxLong = InvokeMethod InvokeStatic "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;" False

unboxBool :: Asm
unboxBool = InvokeMethod InvokeVirtual "java/lang/Boolean" "booleanValue" "()Z" False

unboxInt :: Asm
unboxInt = InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False

unboxChar :: Asm
unboxChar = InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False

unboxLong :: Asm
unboxLong = InvokeMethod InvokeVirtual "java/lang/Long" "longValue" "()J" False

unboxDouble :: Asm
unboxDouble = InvokeMethod InvokeVirtual "java/lang/Double" "doubleValue" "()D" False

unboxFloat :: Asm
unboxFloat = InvokeMethod InvokeVirtual "java/lang/Float" "floatValue" "()F" False

sig :: Int -> String
sig nArgs = "(" ++ concat (replicate nArgs "Ljava/lang/Object;") ++ ")Ljava/lang/Object;"

metafactoryDesc :: Descriptor
metafactoryDesc =
  join [ "("
         , "Ljava/lang/invoke/MethodHandles$Lookup;"
         , "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodHandle;"
         , "Ljava/lang/invoke/MethodType;"
         , ")"
         , "Ljava/lang/invoke/CallSite;"
         ]

lambdaDesc :: Descriptor
lambdaDesc = "([Ljava/lang/Object;)Ljava/lang/Object;"

within :: String -> String -> String -> String
within start str end = start ++ str ++ end

quoted :: String -> String
quoted s = within "\"" s "\""

braced :: String -> String
braced s = within "(" s ")"

sqrBracketed :: String -> String
sqrBracketed s = within "[" s "]"

sep :: String -> [String] -> String
sep = intercalate

commaSep :: [String] -> String
commaSep = sep ","
