module IdrisJvm.Assembler where

import           Data.List (intercalate)

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
         | GetType Descriptor
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
         | InvokeDynamic MethodName Descriptor Handle [Arg]
         | Istore Int
         | Isub
         | LabelStart Label
         | Ldc String
         | LookupSwitch Label [Label] [Int]
         | MaxStackAndLocal Int Int
         | MethodCodeStart
         | MethodCodeEnd
         | New ClassName
         | Pop
         | Return
         | SourceInfo SourceFileName

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

instance Asmable ClassOpts where
  asm ComputeMaxs = "ClassWriter.COMPUTE_MAXS"
  asm ComputeFrames = "ClassWriter.COMPUTE_FRAMES"

data InvocType = InvokeInterface | InvokeSpecial | InvokeStatic | InvokeVirtual

instance Asmable InvocType where
  asm InvokeInterface = "Opcodes.INVOKEINTERFACE"
  asm InvokeSpecial = "Opcodes.INVOKESPECIAL"
  asm InvokeStatic = "Opcodes.INVOKESTATIC"
  asm InvokeVirtual = "Opcodes.INVOKEVIRTUAL"

data FieldInsType = FGetStatic | FPutStatic | FGetField | FPutField

instance Asmable FieldInsType where
  asm FGetStatic = "Opcodes.GETSTATIC"
  asm FPutStatic = "Opcodes.PUTSTATIC"
  asm FGetField = "Opcodes.GETFIELD"
  asm FPutField = "Opcodes.PUTFIELD"

data FrameType = FFull | FSame | FAppend

instance Asmable FrameType where
  asm FFull = "Opcodes.F_FULL"
  asm FSame = "Opcodes.F_SAME"
  asm FAppend = "Opcodes.F_APPEND"

data Access = Private | Public | Static | Synthetic
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


instance Asmable Asm where
  asm Aaload = call "mv" "visitInsn" ["Opcodes.AALOAD"]
  asm Aastore = call "mv" "visitInsn" ["Opcodes.AASTORE"]
  asm Aconstnull = call "mv" "visitInsn" ["Opcodes.ACONST_NULL"]
  asm (Aload n) = call "mv" "visitVarInsn" ["Opcodes.ALOAD", show n]
  asm (Anewarray desc) = call "mv" "visitTypeInsn" ["Opcodes.ANEWARRAY", quoted desc]
  asm (Astore n) = call "mv" "visitVarInsn" ["Opcodes.ASTORE", show n]
  asm Areturn = call "mv" "visitInsn" ["Opcodes.ARETURN"]
  asm (Checkcast desc) = call "mv" "visitTypeInsn" ["Opcodes.CHECKCAST", quoted desc]

  asm (ClassCodeEnd outFileName)
   = sep "\n" [ call "cw" "visitEnd" []
              , declVarAndAssign "out" $ constructor "FileOutputStream" [quoted outFileName]
              , call "out" "write" ["cw.toByteArray()"]
              , call "out" "close" []
              , "}"
              ]

  asm (ClassCodeStart version acc cname sig super intf)
      = call "cw" "visit" [ show version
                          , asm acc
                          , quoted cname
                          , sig
                          , quoted super
                          , "null"] -- TODO: Fix me. Currently the generated classes don't implement any interfaces
  asm (CreateClass opts) = sep "\n" [ imports
                                    , sep " " [withImports, "{"]
                                    , declVarAndAssign "cw" $ constructor "ClassWriter" [asm opts]
                                    , declVar "mv"
                                    ]

  asm (CreateLabel s) = declVarAndAssign s $ constructor "Label" []

  asm (CreateMethod accs mname desc sig excs)
   = let excsStr = maybe "null" f excs
         f es = "new String[] {" ++ (commaSep . map quoted $ es) ++ "}" in
     declVarAndAssign "mv" $ call "cw" "visitMethod" [ sep " + " (map asm accs)
                                                     , quoted mname
                                                     , quoted desc
                                                     , maybe "null" quoted sig
                                                     , excsStr
                                                     ]
  asm Dup = call "mv" "visitInsn" ["Opcodes.DUP"]
  asm (Field ftype cname fname desc) = call "mv" "visitFieldInsn" [asm ftype, quoted cname, quoted fname, quoted desc ]
  asm (Frame frameType nlocal local nstack stack)
   = call "mv" "visitFrame" [ asm frameType
                            , show nlocal
                            , initializeArray "java.lang.Object[]" local
                            , show nstack
                            , initializeArray "java.lang.Object[]" stack]
  asm (GetType desc) = call "Type" "getType" [quoted desc]
  asm (Goto label) = call "mv" "visitJumpInsn" ["Opcodes.GOTO", label]
  asm I2c = call "mv" "visitInsn" [ "Opcodes.I2C" ]
  asm I2l = call "mv" "visitInsn" [ "Opcodes.I2L" ]
  asm Iadd = call "mv" "visitInsn" ["Opcodes.IADD"]
  asm (Iconst n) = iconst n
  asm (Ifeq label) = call "mv" "visitJumpInsn" ["Opcodes.IFEQ", label]
  asm (Ificmpge label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPGE", label]
  asm (Ificmpgt label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPGT", label]
  asm (Ificmple label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPLE", label]
  asm (Ificmplt label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPLT", label]
  asm (Iload n) = call "mv" "visitVarInsn" ["Opcodes.ILOAD", show n]
  asm Imul = call "mv" "visitInsn" ["Opcodes.IMUL"]
  asm (InvokeMethod invType cname mname desc isIntf)
   = call "mv" "visitMethodInsn" [asm invType, quoted cname, quoted mname, quoted desc, asm isIntf]
  asm Isub = call "mv" "visitInsn" ["Opcodes.ISUB"]
  asm (InvokeDynamic mname desc handle args)
     = call "mv" "visitInvokeDynamicInsn" invDynArgs
   where
     invDynArgs = [ quoted mname
                  , quoted desc
                  , asm handle
                  , call "Java" "to" [within "[" (commaSep args) "]", quoted "java.lang.Object[]"]
                  ]
  asm (Istore n) = call "mv" "visitVarInsn" ["Opcodes.ISTORE", show n]
  asm (LabelStart label) = call "mv" "visitLabel" [label]
  asm (Ldc s) = call "mv" "visitLdcInsn" [s]
  asm (LookupSwitch dflt lbls exprs)
   = call "mv" "visitLookupSwitchInsn" [ dflt
                                       , initializeArray "int[]" $ map show exprs
                                       , initializeArray "mmhelloworld.idrisjvmruntime.org.objectweb.asm.Label[]" lbls
                                       ]
  asm (MaxStackAndLocal nstack nlocal) = call "mv" "visitMaxs" [show nstack, show nlocal]
  asm MethodCodeEnd = call "mv" "visitEnd" []
  asm MethodCodeStart  = call "mv" "visitCode" []
  asm (New className) = call "mv" "visitTypeInsn" ["Opcodes.NEW", quoted className]
  asm Pop = call "mv" "visitInsn" ["Opcodes.POP"]
  asm Return = call "mv" "visitInsn" ["Opcodes.RETURN"]
  asm (SourceInfo fileName)  = call "mv" "visitSource" [quoted fileName, "null"]

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

within start str end = start ++ str ++ end
quoted s = within "\"" s "\""
braced s = within "(" s ")"
sqrBracketed s = within "[" s "]"
sep = intercalate
commaSep = sep ","
call qual meth args = qual ++ "." ++ callFn meth args
callFn meth args = meth ++ braced (commaSep args)
constructor className args = sep " " ["new", callFn className args]
assign name value = sep " = " [name, value]
declVarAndAssign name value = sep " " ["var", assign name value]
declVar v = sep " " ["var" , v]

initializeArray typ xs = call "Java" "to" [within "[" (commaSep xs) "]", quoted typ]
