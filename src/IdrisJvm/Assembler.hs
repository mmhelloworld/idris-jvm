module IdrisJvm.Assembler where

import Data.List (intercalate)

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
classoptsj ComputeMaxs = "ClassWriter.COMPUTE_MAXS"
classoptsj ComputeFrames = "ClassWriter.COMPUTE_FRAMES"

data InvocType = InvokeInterface | InvokeSpecial | InvokeStatic | InvokeVirtual
invocTypej InvokeInterface = "Opcodes.INVOKEINTERFACE"
invocTypej InvokeSpecial = "Opcodes.INVOKESPECIAL"
invocTypej InvokeStatic = "Opcodes.INVOKESTATIC"
invocTypej InvokeVirtual = "Opcodes.INVOKEVIRTUAL"

data FieldInsType = FGetStatic | FPutStatic | FGetField | FPutField
fieldInsTypej FGetStatic = "Opcodes.GETSTATIC"
fieldInsTypej FPutStatic = "Opcodes.PUTSTATIC"
fieldInsTypej FGetField = "Opcodes.GETFIELD"
fieldInsTypej FPutField = "Opcodes.PUTFIELD"

data FrameType = FFull | FSame | FAppend
frameTypej FFull = "Opcodes.F_FULL"
frameTypej FSame = "Opcodes.F_SAME"
frameTypej FAppend = "Opcodes.F_APPEND"

data Access = Private | Public | Static | Synthetic

accessj Private = "Opcodes.ACC_PRIVATE"
accessj Public  = "Opcodes.ACC_PUBLIC"
accessj Static  = "Opcodes.ACC_STATIC"
accessj Synthetic  = "Opcodes.ACC_SYNTHETIC"

data HandleTag = HGetField
               | HGetStatic
               | HPutField
               | HPutStatic
               | HInvokeVirtual
               | HInvokeStatic
               | HInvokeSpecial
               | HNewInvokeSpecial
               | HInvokeInterface

handleTagj HGetField         = "Opcodes.H_GETFIELD"
handleTagj HGetStatic        = "Opcodes.H_GETSTATIC"
handleTagj HPutField         = "Opcodes.H_PUTFIELD"
handleTagj HPutStatic        = "Opcodes.H_PUTSTATIC"
handleTagj HInvokeVirtual    = "Opcodes.H_INVOKEVIRTUAL"
handleTagj HInvokeStatic     = "Opcodes.H_INVOKESTATIC"
handleTagj HInvokeSpecial    = "Opcodes.H_INVOKESPECIAL"
handleTagj HNewInvokeSpecial = "Opcodes.H_NEWINVOKESPECIAL"
handleTagj HInvokeInterface  = "Opcodes.H_INVOKEINTERFACE"
  
data Handle = Handle { tag :: HandleTag
                     , hClassName :: ClassName
                     , hMethodName :: MethodName
                     , descriptor :: Descriptor
                     , isInterface :: Bool
                     }

handlej (Handle tag cname mname desc isInterface)
  = constructor "Handle" [ handleTagj tag
                          , quoted cname
                          , quoted mname
                          , quoted desc
                          , boolj isInterface
                          ]

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

iconst i | i >= 0 && i <= 5            = call "mv" "visitInsn" ["Opcodes.ICONST_" ++ show i]
         | i == (-1)                   = call "mv" "visitInsn" ["Opcodes.ICONST_M1"]
         | i >= (-128) && i <= 127     = call "mv" "visitIntInsn" ["Opcodes.BIPUSH", show i]
         | i >= (-32768) && i <= 32767 = call "mv" "visitIntInsn" ["Opcodes.SIPUSH", show i]
         | otherwise                   = call "mv" "visitLdcInsn" [constructor "Integer" [show i]]

boolj False = "false"
boolj True = "true"

imports = declVarAndAssign "imports" $ constructor "JavaImporter" packages where
  packages = [ "java.lang"
             , "java.io"
             , "java.util"
             , "java.nio"
             , "Packages.mmhelloworld.idrisjvmruntime.org.objectweb.asm"
             ]

withImports :: String
withImports = "with (imports)"
             
asmj :: Asm -> String
asmj Aaload = call "mv" "visitInsn" ["Opcodes.AALOAD"]
asmj Aastore = call "mv" "visitInsn" ["Opcodes.AASTORE"]
asmj Aconstnull = call "mv" "visitInsn" ["Opcodes.ACONST_NULL"]
asmj (Aload n) = call "mv" "visitVarInsn" ["Opcodes.ALOAD", show n]
asmj (Anewarray desc) = call "mv" "visitTypeInsn" ["Opcodes.ANEWARRAY", quoted desc]
asmj (Astore n) = call "mv" "visitVarInsn" ["Opcodes.ASTORE", show n]
asmj Areturn = call "mv" "visitInsn" ["Opcodes.ARETURN"]
asmj (Checkcast desc) = call "mv" "visitTypeInsn" ["Opcodes.CHECKCAST", quoted desc]

asmj (ClassCodeEnd outFileName)
  = sep "\n" [ call "cw" "visitEnd" []
             , declVarAndAssign "out" $ constructor "FileOutputStream" [quoted outFileName]
             , call "out" "write" ["cw.toByteArray()"]
             , call "out" "close" []
             , "}"
             ]
    
asmj (ClassCodeStart version acc cname sig super intf)
     = call "cw" "visit" [ show version
                         , accessj acc
                         , quoted cname
                         , sig
                         , quoted super
                         , "null"] -- TODO: Fix me. Currently the generated classes don't implement any interfaces
asmj (CreateClass opts) = sep "\n" [ imports
                                   , sep " " [withImports, "{"]
                                   , declVarAndAssign "cw" $ constructor "ClassWriter" [classoptsj opts]
                                   , declVar "mv"
                                   ]

asmj (CreateLabel s) = declVarAndAssign s $ constructor "Label" []

asmj (CreateMethod accs mname desc sig excs)
  = let excsStr = maybe "null" f excs
        f es = "new String[] {" ++ (commaSep . map quoted $ es) ++ "}" in
    declVarAndAssign "mv" $ call "cw" "visitMethod" [ sep " + " (map accessj accs)
                                                    , quoted mname
                                                    , quoted desc
                                                    , maybe "null" quoted sig
                                                    , excsStr
                                                    ]
asmj Dup = call "mv" "visitInsn" ["Opcodes.DUP"]
asmj (Field ftype cname fname desc) = call "mv" "visitFieldInsn" [fieldInsTypej ftype, quoted cname, quoted fname, quoted desc ]
asmj (Frame frameType nlocal local nstack stack)
  = call "mv" "visitFrame" [ frameTypej frameType
                           , show nlocal
                           , initializeArray "java.lang.Object[]" local
                           , show nstack
                           , initializeArray "java.lang.Object[]" stack]
asmj (GetType desc) = call "Type" "getType" [quoted desc] 
asmj (Goto label) = call "mv" "visitJumpInsn" ["Opcodes.GOTO", label]
asmj I2c = call "mv" "visitInsn" [ "Opcodes.I2C" ]
asmj I2l = call "mv" "visitInsn" [ "Opcodes.I2L" ]
asmj Iadd = call "mv" "visitInsn" ["Opcodes.IADD"]
asmj (Iconst n) = iconst n
asmj (Ifeq label) = call "mv" "visitJumpInsn" ["Opcodes.IFEQ", label]
asmj (Ificmpge label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPGE", label]
asmj (Ificmpgt label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPGT", label]
asmj (Ificmple label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPLE", label]
asmj (Ificmplt label) = call "mv" "visitJumpInsn" ["Opcodes.IF_ICMPLT", label]
asmj (Iload n) = call "mv" "visitVarInsn" ["Opcodes.ILOAD", show n]
asmj Imul = call "mv" "visitInsn" ["Opcodes.IMUL"]
asmj (InvokeMethod invType cname mname desc isIntf)
  = call "mv" "visitMethodInsn" [invocTypej invType, quoted cname, quoted mname, quoted desc, boolj isIntf]
asmj Isub = call "mv" "visitInsn" ["Opcodes.ISUB"]
asmj (InvokeDynamic mname desc handle args)
    = call "mv" "visitInvokeDynamicInsn" invDynArgs
  where
    invDynArgs = [ quoted mname
                 , quoted desc
                 , handlej handle
                 , call "Java" "to" [within "[" (commaSep args) "]", quoted "java.lang.Object[]"]
                 ]
asmj (Istore n) = call "mv" "visitVarInsn" ["Opcodes.ISTORE", show n]
asmj (LabelStart label) = call "mv" "visitLabel" [label]
asmj (Ldc s) = call "mv" "visitLdcInsn" [s]
asmj (LookupSwitch dflt lbls exprs)
  = call "mv" "visitLookupSwitchInsn" [ dflt
                                      , initializeArray "int[]" $ map show exprs
                                      , initializeArray "mmhelloworld.idrisjvmruntime.org.objectweb.asm.Label[]" lbls
                                      ]
asmj (MaxStackAndLocal nstack nlocal) = call "mv" "visitMaxs" [show nstack, show nlocal]
asmj MethodCodeEnd = call "mv" "visitEnd" []
asmj MethodCodeStart  = call "mv" "visitCode" []
asmj (New className) = call "mv" "visitTypeInsn" ["Opcodes.NEW", quoted className] 
asmj Pop = call "mv" "visitInsn" ["Opcodes.POP"]
asmj Return = call "mv" "visitInsn" ["Opcodes.RETURN"]
asmj (SourceInfo fileName)  = call "mv" "visitSource" [quoted fileName, "null"]
