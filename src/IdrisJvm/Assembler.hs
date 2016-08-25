{-# LANGUAGE OverloadedStrings #-}
module IdrisJvm.Assembler where

import           Data.Aeson
import           Data.List  (intercalate)

data Asm = Aaload
         | Aastore
         | Aconstnull
         | Aload Int
         | Anewarray Descriptor
         | Astore Int
         | Areturn
         | Checkcast Descriptor
         | ClassCodeStart Int Access ClassName Signature ClassName [ClassName]
         | ClassCodeEnd String
         | CreateClass ClassOpts
         | CreateLabel String
         | CreateMethod [Access] MethodName Descriptor (Maybe Signature) (Maybe [Exception])
         | Dup
         | Field FieldInsType ClassName FieldName Descriptor
         | Frame FrameType Int [Signature] Int [Signature]
         | Goto Label
         | I2c
         | I2l
         | Iadd
         | Iconst Int
         | Ifeq Label
         | Ificmpge Label
         | Ificmpgt Label
         | Ificmple Label
         | Ificmplt Label
         | Iload Int
         | Imul
         | InvokeMethod InvocType ClassName MethodName Descriptor Bool
         | InvokeDynamic MethodName Descriptor Handle [BsmArg]
         | Istore Int
         | Isub
         | LabelStart Label
         | Ldc Constant
         | LookupSwitch Label [Label] [Int]
         | MaxStackAndLocal Int Int
         | MethodCodeStart
         | MethodCodeEnd
         | New ClassName
         | Pop
         | Return
         | SourceInfo SourceFileName

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

  toJSON (ClassCodeStart version acc cname sig super intf)
    = object [ "type" .= String "ClassCodeStart"
             , "version" .= toJSON version
             , "acc" .= toJSON acc
             , "name" .= toJSON cname
             , "sig" .= if sig == "null" then Null else toJSON sig
             , "parent" .= toJSON super
             , "interfaces" .= toJSON intf]

  toJSON (ClassCodeEnd out)
    = object [ "type" .= String "ClassCodeEnd"
             , "out" .= toJSON out ]

  toJSON (CreateClass opt)
    = object [ "type" .= String "CreateClass"
             , "opt" .= toJSON opt ]

  toJSON (CreateLabel label)
    = object [ "type" .= String "CreateLabel"
             , "name" .= toJSON label ]

  toJSON (CreateMethod accs mname desc sig excs)
    = object [ "type" .= String "CreateMethod"
             , "acc" .= toJSON (sum $ accessNum <$> accs)
             , "name" .= toJSON mname
             , "desc" .= toJSON desc
             , "sig" .= maybe Null toJSON sig
             , "excs" .= toJSON excs ]

  toJSON Dup = object [ "type" .= String "Dup" ]

  toJSON (Field ftype cname fname desc)
    = object [ "type" .= String "Field"
             , "ftype" .= toJSON ftype
             , "cname" .= toJSON cname
             , "fname" .= toJSON fname
             , "desc" .= toJSON desc ]

  toJSON (Frame frameType nlocal local nstack stack)
    = object [ "type" .= String "Frame"
             , "ftype" .= toJSON frameType
             , "nlocal" .= toJSON nlocal
             , "local" .= toJSON local
             , "nstack" .= toJSON nstack
             , "stack" .= toJSON stack ]

  toJSON (Goto label)
    = object [ "type" .= String "Goto"
             , "label" .= toJSON label ]

  toJSON I2c = object [ "type" .= String "I2c" ]

  toJSON I2l = object [ "type" .= String "I2l" ]

  toJSON Iadd = object [ "type" .= String "Iadd" ]

  toJSON (Iconst n)
    = object [ "type" .= String "Iconst"
             , "n" .= toJSON n ]

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

  toJSON (LabelStart label)
    = object [ "type" .= String "LabelStart"
             , "label" .= toJSON label ]

  toJSON (Ldc c)
    = toJSON c

  toJSON (LookupSwitch dlabel clabels vals)
    = object [ "type" .= String "LookupSwitch"
             , "dlabel" .= toJSON dlabel
             , "clabels" .= toJSON clabels
             , "vals" .= toJSON vals ]

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

  toJSON Return = object [ "type" .= String "Return" ]

  toJSON (SourceInfo name)
    = object [ "type" .= String "SourceInfo"
             , "name" .= toJSON name ]

data BsmArg = BsmArgGetType Descriptor | BsmArgHandle Handle

instance ToJSON BsmArg where
  toJSON (BsmArgGetType desc) = object ["type" .= String "BsmArgGetType"
                                    , "desc" .= toJSON desc ]
  toJSON (BsmArgHandle h) = object ["type" .= String "BsmArgHandle"
                                    , "handle" .= toJSON h ]

data Constant = IntegerConst Int | StringConst String

instance ToJSON Constant where
  toJSON (IntegerConst n)
    = object [ "type" .= String "Ldc"
             , "constType" .= String "IntegerConst"
             , "val" .= toJSON n ]
  toJSON (StringConst s)
    = object [ "type" .= String "Ldc"
             , "constType" .= String "StringConst"
             , "val" .= toJSON s ]

class Asmable a where
  asm :: a -> String

data ReferenceTypeDescriptor = ClassDesc ClassName
                             | InterfaceDesc ClassName
                               deriving (Eq, Show)

instance Asmable ReferenceTypeDescriptor where
  asm (ClassDesc c) = "L" ++ c ++ ";"
  asm (InterfaceDesc c) = "L" ++ c ++ ";"

refTyClassName :: ReferenceTypeDescriptor -> ClassName
refTyClassName (ClassDesc c) = c
refTyClassName (InterfaceDesc c) = c

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
  asm FieldTyDescByte = "B"
  asm FieldTyDescChar = "C"
  asm FieldTyDescShort = "S"
  asm FieldTyDescBoolean = "Z"
  asm FieldTyDescArray = "["
  asm FieldTyDescDouble = "D"
  asm FieldTyDescFloat = "F"
  asm FieldTyDescInt = "I"
  asm FieldTyDescLong = "J"
  asm (FieldTyDescReference f) = asm f

data TypeDescriptor = FieldDescriptor FieldTypeDescriptor | VoidDescriptor

instance Asmable TypeDescriptor where
  asm (FieldDescriptor t) = asm t
  asm VoidDescriptor = "V"

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
type Arg = String

data ClassOpts = ComputeMaxs | ComputeFrames

instance ToJSON ClassOpts where
  toJSON ComputeMaxs = toJSON (1 :: Int)
  toJSON ComputeFrames = toJSON (2 :: Int)

instance Asmable ClassOpts where
  asm ComputeMaxs = "ClassWriter.COMPUTE_MAXS"
  asm ComputeFrames = "ClassWriter.COMPUTE_FRAMES"

data InvocType = InvokeInterface | InvokeSpecial | InvokeStatic | InvokeVirtual
invocTypeNum :: InvocType -> Int
invocTypeNum InvokeInterface = 185
invocTypeNum InvokeSpecial = 183
invocTypeNum InvokeStatic = 184
invocTypeNum InvokeVirtual = 182

instance ToJSON InvocType where
  toJSON = toJSON . invocTypeNum

instance Asmable InvocType where
  asm InvokeInterface = "Opcodes.INVOKEINTERFACE"
  asm InvokeSpecial = "Opcodes.INVOKESPECIAL"
  asm InvokeStatic = "Opcodes.INVOKESTATIC"
  asm InvokeVirtual = "Opcodes.INVOKEVIRTUAL"

data FieldInsType = FGetStatic | FPutStatic | FGetField | FPutField
fieldInsTypeNum :: FieldInsType -> Int
fieldInsTypeNum FGetStatic = 178
fieldInsTypeNum FPutStatic = 179
fieldInsTypeNum FGetField = 180
fieldInsTypeNum FPutField = 181

instance ToJSON FieldInsType where
  toJSON = toJSON . fieldInsTypeNum

instance Asmable FieldInsType where
  asm FGetStatic = "Opcodes.GETSTATIC"
  asm FPutStatic = "Opcodes.PUTSTATIC"
  asm FGetField = "Opcodes.GETFIELD"
  asm FPutField = "Opcodes.PUTFIELD"

data FrameType = FFull | FSame | FAppend
frameTypeNum :: FrameType -> Int
frameTypeNum FFull = 0
frameTypeNum FSame = 3
frameTypeNum FAppend = 1

instance ToJSON FrameType where
  toJSON = toJSON . frameTypeNum

instance Asmable FrameType where
  asm FFull = "Opcodes.F_FULL"
  asm FSame = "Opcodes.F_SAME"
  asm FAppend = "Opcodes.F_APPEND"

data Access = Private | Public | Static | Synthetic
accessNum :: Access -> Int
accessNum Private = 2
accessNum Public = 1
accessNum Static = 8
accessNum Synthetic = 4096

instance ToJSON Access where
  toJSON = toJSON . accessNum

instance Asmable Access where
  asm Private = "Opcodes.ACC_PRIVATE"
  asm Public  = "Opcodes.ACC_PUBLIC"
  asm Static  = "Opcodes.ACC_STATIC"
  asm Synthetic  = "Opcodes.ACC_SYNTHETIC"

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
handleTagOpcode HGetField = 1
handleTagOpcode HGetStatic = 2
handleTagOpcode HPutField = 3
handleTagOpcode HPutStatic = 4
handleTagOpcode HInvokeVirtual = 5
handleTagOpcode HInvokeStatic = 6
handleTagOpcode HInvokeSpecial = 7
handleTagOpcode HNewInvokeSpecial = 8
handleTagOpcode HInvokeInterface = 9

instance ToJSON HandleTag where
  toJSON = toJSON . handleTagOpcode

instance Asmable HandleTag where
  asm HGetField         = "Opcodes.H_GETFIELD"
  asm HGetStatic        = "Opcodes.H_GETSTATIC"
  asm HPutField         = "Opcodes.H_PUTFIELD"
  asm HPutStatic        = "Opcodes.H_PUTSTATIC"
  asm HInvokeVirtual    = "Opcodes.H_INVOKEVIRTUAL"
  asm HInvokeStatic     = "Opcodes.H_INVOKESTATIC"
  asm HInvokeSpecial    = "Opcodes.H_INVOKESPECIAL"
  asm HNewInvokeSpecial = "Opcodes.H_NEWINVOKESPECIAL"
  asm HInvokeInterface  = "Opcodes.H_INVOKEINTERFACE"

data Handle = Handle { tag         :: HandleTag
                     , hClassName  :: ClassName
                     , hMethodName :: MethodName
                     , descriptor  :: Descriptor
                     , isInterface :: Bool
                     }

instance ToJSON Handle where
  toJSON (Handle t cname mname desc isIntf)
    = object [ "tag" .= toJSON t
             , "cname" .= toJSON cname
             , "mname" .= toJSON mname
             , "desc" .= toJSON desc
             , "isIntf" .= toJSON isIntf ]

instance Asmable Handle where
  asm (Handle tag cname mname desc isInterface)
    = constructor "Handle" [ asm tag
                            , quoted cname
                            , quoted mname
                            , quoted desc
                            , asm isInterface
                            ]

iconst i | i >= 0 && i <= 5            = call "mv" "visitInsn" ["Opcodes.ICONST_" ++ show i]
         | i == (-1)                   = call "mv" "visitInsn" ["Opcodes.ICONST_M1"]
         | i >= (-128) && i <= 127     = call "mv" "visitIntInsn" ["Opcodes.BIPUSH", show i]
         | i >= (-32768) && i <= 32767 = call "mv" "visitIntInsn" ["Opcodes.SIPUSH", show i]
         | otherwise                   = call "mv" "visitLdcInsn" [constructor "Integer" [show i]]




instance Asmable Bool where
  asm False = "false"
  asm True = "true"

imports = declVarAndAssign "imports" $ constructor "JavaImporter" packages where
  packages = [ "java.lang"
             , "java.io"
             , "java.util"
             , "java.nio"
             , "Packages.mmhelloworld.idrisjvmruntime.org.objectweb.asm"
             ]

withImports :: String
withImports = "with (imports)"

within :: String -> String -> String -> String
within start str end = start ++ str ++ end

quoted :: String -> String
quoted s = within "\"" s "\""

braced s = within "(" s ")"
sqrBracketed s = within "[" s "]"

sep :: String -> [String] -> String
sep = intercalate

commaSep = sep ","
call qual meth args = qual ++ "." ++ callFn meth args
callFn meth args = meth ++ braced (commaSep args)
constructor className args = sep " " ["new", callFn className args]

assign name value = sep " = " [name, value]
declVarAndAssign name value = sep " " ["var", assign name value]
declVar v = sep " " ["var" , v]

initializeArray typ xs = call "Java" "to" [within "[" (commaSep xs) "]", quoted typ]
