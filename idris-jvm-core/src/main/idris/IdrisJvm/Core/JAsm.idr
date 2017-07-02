module IdrisJvm.Core.JAsm

import IdrisJvm.IO
import IdrisJvm.Core.Asm
import Java.Util
import Java.Lang

%access public export

Assembler : Type
Assembler = javaClass "idrisjvm/core/Assembler"

JAnnotation : Type
JAnnotation = javaClass "idrisjvm/core/JAnnotation"

JAnnString : Type
JAnnString = javaClass "idrisjvm/core/JAnnotationValue$JAnnString"

JAnnInt : Type
JAnnInt = javaClass "idrisjvm/core/JAnnotationValue$JAnnInt"

JAnnArray : Type
JAnnArray = javaClass "idrisjvm/core/JAnnotationValue$JAnnArray"

JAnnotationProperty : Type
JAnnotationProperty = javaClass "idrisjvm/core/JAnnotationProperty"

JAnnotationValue: Type
JAnnotationValue = javaClass "idrisjvm/core/JAnnotationValue"

JHandle: Type
JHandle = javaClass "idrisjvm/core/JHandle"

JBsmArg: Type
JBsmArg = javaClass "idrisjvm/core/JBsmArg"

JBsmArgGetType: Type
JBsmArgGetType = javaClass "idrisjvm/core/JBsmArg$JBsmArgGetType"

JBsmArgHandle: Type
JBsmArgHandle = javaClass "idrisjvm/core/JBsmArg$JBsmArgHandle"

toJClassOpts : ClassOpts -> Int
toJClassOpts ComputeMaxs = 1
toJClassOpts ComputeFrames = 2

toJAnnotationValue : Asm.AnnotationValue -> JVM_IO JAnnotationValue
toJAnnotationValue (AnnString s) = believe_me <$> FFI.new (String -> JVM_IO JAnnString) s
toJAnnotationValue (AnnInt n) = believe_me <$> FFI.new (Int -> JVM_IO JAnnInt) n
toJAnnotationValue (AnnArray values) = do
  jvalues <- ArrayList.fromList !(sequence $ toJAnnotationValue <$> values)
  believe_me <$> FFI.new (JList -> JVM_IO JAnnArray) (believe_me jvalues)

toJAnnotationProperty : Asm.AnnotationProperty -> JVM_IO JAnnotationProperty
toJAnnotationProperty (name, annValue) = do
  jAnnotationValue <- toJAnnotationValue annValue
  FFI.new (String -> JAnnotationValue -> JVM_IO JAnnotationProperty) name jAnnotationValue

toJAnnotation : Asm.Annotation -> JVM_IO JAnnotation
toJAnnotation (MkAnnotation name props) = do
  jprops <- ArrayList.fromList !(sequence $ toJAnnotationProperty <$> props)
  FFI.new (String -> JList -> JVM_IO JAnnotation) name (believe_me jprops)

toJFieldInitialValue : FieldInitialValue -> Object
toJFieldInitialValue (IntField n) = believe_me $ JInteger.valueOf n
toJFieldInitialValue (StringField s) = believe_me s
toJFieldInitialValue (DoubleField d) = believe_me $ JDouble.valueOf d

accessNum : Access -> Int
accessNum Final     = 16
accessNum Private   = 2
accessNum Public    = 1
accessNum Static    = 8
accessNum Synthetic = 4096

fieldInsTypeNum : FieldInsType -> Int
fieldInsTypeNum FGetStatic = 178
fieldInsTypeNum FPutStatic = 179
fieldInsTypeNum FGetField  = 180
fieldInsTypeNum FPutField  = 181

frameTypeNum : FrameType -> Int
frameTypeNum FFull   = 0
frameTypeNum FSame   = 3
frameTypeNum FAppend = 1

invocTypeNum : InvocType -> Int
invocTypeNum InvokeInterface = 185
invocTypeNum InvokeSpecial   = 183
invocTypeNum InvokeStatic    = 184
invocTypeNum InvokeVirtual   = 182

handleTagOpcode : HandleTag -> Int
handleTagOpcode HGetField         = 1
handleTagOpcode HGetStatic        = 2
handleTagOpcode HPutField         = 3
handleTagOpcode HPutStatic        = 4
handleTagOpcode HInvokeVirtual    = 5
handleTagOpcode HInvokeStatic     = 6
handleTagOpcode HInvokeSpecial    = 7
handleTagOpcode HNewInvokeSpecial = 8
handleTagOpcode HInvokeInterface  = 9

constantToObject : Constant -> Object
constantToObject (DoubleConst d) = believe_me $ JDouble.valueOf d
constantToObject (IntegerConst n) = believe_me $ JInteger.valueOf n
constantToObject (LongConst n) = believe_me $ Long.valueOf n
constantToObject (StringConst str) = believe_me str
constantToObject (TypeConst str) = believe_me str

toJHandle : Handle -> JVM_IO JHandle
toJHandle (MkHandle tag hcname hmname hdesc hIsIntf) = do
  let tagNum = handleTagOpcode tag
  FFI.new (Int -> String -> String -> String -> Bool -> JVM_IO JHandle) tagNum hcname hmname hdesc hIsIntf

toJbsmArg : BsmArg -> JVM_IO JBsmArg
toJbsmArg (BsmArgGetType desc) = believe_me <$> FFI.new (String -> JVM_IO JBsmArgGetType) desc
toJbsmArg (BsmArgHandle handle) = do
  jhandle <- toJHandle handle
  believe_me <$> FFI.new (JHandle -> JVM_IO JBsmArgHandle) jhandle

singleInst : List (Asm ()) -> JVM_IO a -> JVM_IO (a, List (Asm ()))
singleInst subroutines m = do
  a <- m
  pure (a, subroutines)

runAsm : List (Asm ()) -> Assembler -> Asm a -> JVM_IO (a, List (Asm ()))
runAsm subroutines assembler Aaload = singleInst subroutines $ invokeInstance "aaload" (Assembler -> JVM_IO ()) assembler

runAsm subroutines assembler Aastore = singleInst subroutines $ invokeInstance "aastore" (Assembler -> JVM_IO ()) assembler

runAsm subroutines assembler Aconstnull = singleInst subroutines $ invokeInstance "aconstnull" (Assembler -> JVM_IO ()) assembler

runAsm subroutines assembler (Aload n) = singleInst subroutines $ invokeInstance "aload" (Assembler -> Int -> JVM_IO ()) assembler n

runAsm subroutines assembler (Anewarray desc) = singleInst subroutines $ invokeInstance "anewarray" (Assembler -> String -> JVM_IO ()) assembler desc
runAsm subroutines assembler (Astore n) = singleInst subroutines $ invokeInstance "astore" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm subroutines assembler Areturn = singleInst subroutines $ invokeInstance "areturn" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Checkcast desc) = singleInst subroutines $ invokeInstance "checkcast" (Assembler -> String -> JVM_IO ()) assembler desc
runAsm subroutines assembler (ClassCodeStart version access className sig parent intf anns) = singleInst subroutines $ do
  let janns = toJAnnotation <$> anns
  interfaces <- ArrayList.fromList intf
  annotations <- ArrayList.fromList janns
  invokeInstance
    "classCodeStart"
    (Assembler -> Int -> Int -> ClassName -> (Maybe Signature) -> ClassName -> JList -> JList -> JVM_IO ())
    assembler
    version
    (accessNum access)
    className
    sig
    parent
    (believe_me interfaces)
    (believe_me annotations)

runAsm subroutines assembler (ClassCodeEnd out) = singleInst subroutines $ invokeInstance "classCodeEnd" (Assembler -> String -> JVM_IO ()) assembler out
runAsm subroutines assembler (CreateClass opts) = singleInst subroutines $ invokeInstance "createClass" (Assembler -> Int -> JVM_IO ()) assembler (toJClassOpts opts)
runAsm subroutines assembler (CreateField accs className fieldName desc sig fieldInitialValue) = singleInst subroutines $ do
  let jaccs = sum $ accessNum <$> accs
  invokeInstance
    "createField"
    (Assembler -> Int -> ClassName -> FieldName -> Descriptor -> Maybe Signature -> Maybe Object -> JVM_IO ())
    assembler
    jaccs
    className
    fieldName
    desc
    sig
    (toJFieldInitialValue <$> fieldInitialValue)

runAsm subroutines assembler (CreateLabel label) = singleInst subroutines $ do
  invokeInstance "createLabel" (Assembler -> String -> JVM_IO ()) assembler label
  pure label

runAsm subroutines assembler (CreateMethod accs className methodName desc sig exceptions anns paramAnns) = singleInst subroutines $ do
  let jaccs = sum $ accessNum <$> accs
  jexceptions <- sequence $ ArrayList.fromList <$> exceptions
  janns <- ArrayList.fromList !(sequence $ toJAnnotation <$> anns)
  jparamAnns <- ArrayList.fromList !(sequence $ (\paramAnn => ArrayList.fromList !(sequence $ toJAnnotation <$> paramAnn)) <$> paramAnns)
  invokeInstance
    "createMethod"
    (Assembler -> Int -> ClassName -> MethodName -> Descriptor -> Maybe Signature -> Maybe JList -> JList -> JList -> JVM_IO ())
    assembler
    jaccs
    className
    methodName
    desc
    sig
    (believe_me jexceptions)
    (believe_me janns)
    (believe_me jparamAnns)

runAsm subroutines assembler Dadd = singleInst subroutines $ invokeInstance "dadd" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Ddiv = singleInst subroutines $ invokeInstance "ddiv" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Dload n) = singleInst subroutines $ invokeInstance "dload" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm subroutines assembler Dmul = singleInst subroutines $ invokeInstance "dmul" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Drem = singleInst subroutines $ invokeInstance "drem" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Dreturn = singleInst subroutines $ invokeInstance "dreturn" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Dsub = singleInst subroutines $ invokeInstance "dsub" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Dup = singleInst subroutines $ invokeInstance "dup" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Error err) = singleInst subroutines $ invokeInstance "error" (Assembler -> String -> JVM_IO ()) assembler err
runAsm subroutines assembler F2d = singleInst subroutines $ invokeInstance "f2d" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Field finsType cname fname desc) = singleInst subroutines $ do
  let finsTypeNum = fieldInsTypeNum finsType
  invokeInstance
    "field"
    (Assembler -> Int -> ClassName -> FieldName -> Descriptor -> JVM_IO ())
    assembler
    finsTypeNum
    cname
    fname
    desc

runAsm subroutines assembler FieldEnd = singleInst subroutines $ invokeInstance "fieldEnd" (Assembler -> JVM_IO ()) assembler

runAsm subroutines assembler (Fload n) = singleInst subroutines $ invokeInstance "fload" (Assembler -> Int -> JVM_IO ()) assembler n

runAsm subroutines assembler (Frame frameType nLocal localSigs nStack stackSigs) = singleInst subroutines $ do
  let ftypeNum = frameTypeNum frameType
  jlocalSigs <- ArrayList.fromList localSigs
  jstackSigs <- ArrayList.fromList stackSigs
  invokeInstance
    "frame"
    (Assembler -> Int -> Int -> JList -> Int -> JList -> JVM_IO ())
    assembler
    ftypeNum
    (cast nLocal)
    (believe_me jlocalSigs)
    (cast nStack)
    (believe_me jstackSigs)

runAsm subroutines assembler FreshIfIndex = singleInst subroutines $ do
  index <- invokeInstance "freshIfIndex" (Assembler -> JVM_IO Int) assembler
  pure $ cast index

runAsm subroutines assembler (FreshLambdaIndex cname) = singleInst subroutines $ do
  index <- invokeInstance "freshLambdaIndex" (Assembler -> String -> JVM_IO Int) assembler cname
  pure $ cast index

runAsm subroutines assembler FreshSwitchIndex = singleInst subroutines $ do
  index <- invokeInstance "freshSwitchIndex" (Assembler -> JVM_IO Int) assembler
  pure $ cast index

runAsm subroutines assembler Freturn = singleInst subroutines $ invokeInstance "freturn" (Assembler -> JVM_IO ()) assembler

runAsm subroutines assembler GetFunctionName = singleInst subroutines $ do
  cname <- invokeInstance "getClassName" (Assembler -> JVM_IO String) assembler
  mname <- invokeInstance "getMethodName" (Assembler -> JVM_IO String) assembler
  pure $ MkJMethodName cname mname

runAsm subroutines assembler GetLocalVarCount = singleInst subroutines $ do
  index <- invokeInstance "getLocalVarCount" (Assembler -> JVM_IO Int) assembler
  pure $ cast index
runAsm subroutines assembler (Goto label) = singleInst subroutines $ invokeInstance "gotoLabel" (Assembler -> String -> JVM_IO ()) assembler label

runAsm subroutines assembler I2c = singleInst subroutines $ invokeInstance "i2c" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler I2l = singleInst subroutines $ invokeInstance "i2l" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Iadd = singleInst subroutines $ invokeInstance "iadd" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Iand = singleInst subroutines $ invokeInstance "iand" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Ior = singleInst subroutines $ invokeInstance "ior" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Ixor = singleInst subroutines $ invokeInstance "ixor" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Icompl = singleInst subroutines $ invokeInstance "icompl" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Iconst n) = singleInst subroutines $ invokeInstance "iconst" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm subroutines assembler Idiv = singleInst subroutines $ invokeInstance "idiv" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Ifeq label) = singleInst subroutines $ invokeInstance "ifeq" (Assembler -> String -> JVM_IO ()) assembler label
runAsm subroutines assembler (Ificmpge label) = singleInst subroutines $ invokeInstance "ificmpge" (Assembler -> String -> JVM_IO ()) assembler label
runAsm subroutines assembler (Ificmpgt label) = singleInst subroutines $ invokeInstance "ificmpgt" (Assembler -> String -> JVM_IO ()) assembler label
runAsm subroutines assembler (Ificmple label) = singleInst subroutines $ invokeInstance "ificmple" (Assembler -> String -> JVM_IO ()) assembler label
runAsm subroutines assembler (Ificmplt label) = singleInst subroutines $ invokeInstance "ificmplt" (Assembler -> String -> JVM_IO ()) assembler label
runAsm subroutines assembler (Iload n) = singleInst subroutines $ invokeInstance "iload" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm subroutines assembler Imul = singleInst subroutines $ invokeInstance "imul" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (InvokeMethod invocType cname mname desc isIntf) = singleInst subroutines $ do
  let invocTypeAsm = invocTypeNum invocType
  invokeInstance
    "invokeMethod"
    (Assembler -> Int -> ClassName -> MethodName -> Descriptor -> Bool -> JVM_IO ())
    assembler
    invocTypeAsm
    cname
    mname
    desc
    isIntf

runAsm subroutines assembler (InvokeDynamic mname desc handle bsmArgs) = singleInst subroutines $ do
  jbsmArgsList <- sequence $ toJbsmArg <$> bsmArgs
  jbsmArgs <- ArrayList.fromList jbsmArgsList
  jhandle <- toJHandle handle
  invokeInstance
    "invokeDynamic"
    (Assembler -> MethodName -> Descriptor -> JHandle -> JList -> JVM_IO ())
    assembler
    mname
    desc
    jhandle
    (believe_me jbsmArgs)

runAsm subroutines assembler Irem = singleInst subroutines $ invokeInstance "irem" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Ireturn = singleInst subroutines $ invokeInstance "ireturn" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Ishl = singleInst subroutines $ invokeInstance "ishl" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Ishr = singleInst subroutines $ invokeInstance "ishr" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Istore n) = singleInst subroutines $ invokeInstance "istore" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm subroutines assembler Isub = singleInst subroutines $ invokeInstance "isub" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Iushr = singleInst subroutines $ invokeInstance "iushr" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler L2i = singleInst subroutines $ invokeInstance "l2i" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (LabelStart label) = singleInst subroutines $ invokeInstance "labelStart" (Assembler -> String -> JVM_IO ()) assembler label
runAsm subroutines assembler Ladd = singleInst subroutines $ invokeInstance "ladd" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Land = singleInst subroutines $ invokeInstance "land" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lor = singleInst subroutines $ invokeInstance "lor" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lxor = singleInst subroutines $ invokeInstance "lxor" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lcompl = singleInst subroutines $ invokeInstance "lcompl" (Assembler -> JVM_IO ()) assembler

runAsm subroutines assembler (Ldc (TypeConst ty)) = singleInst subroutines $ invokeInstance "ldcType" (Assembler -> String -> JVM_IO ()) assembler ty
runAsm subroutines assembler (Ldc constant) = singleInst subroutines $ invokeInstance "ldc" (Assembler -> Object -> JVM_IO ()) assembler (constantToObject constant)

runAsm subroutines assembler Ldiv = singleInst subroutines $ invokeInstance "ldiv" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (Lload n) = singleInst subroutines $ invokeInstance "lload" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm subroutines assembler Lmul = singleInst subroutines $ invokeInstance "lmul" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (LookupSwitch defaultLabel labels cases) = singleInst subroutines $ do
  jlabels <- ArrayList.fromList labels
  jcases <- ArrayList.fromList $ JInteger.valueOf <$> cases
  invokeInstance
    "lookupSwitch"
    (Assembler -> String -> JList -> JList -> JVM_IO ())
    assembler
    defaultLabel
    (believe_me jlabels)
    (believe_me jcases)

runAsm subroutines assembler Lrem = singleInst subroutines $ invokeInstance "lrem" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lreturn = singleInst subroutines $ invokeInstance "lreturn" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lshl = singleInst subroutines $ invokeInstance "lshl" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lshr = singleInst subroutines $ invokeInstance "lshr" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lsub = singleInst subroutines $ invokeInstance "lsub" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Lushr = singleInst subroutines $ invokeInstance "lushr" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (MaxStackAndLocal stack local)
  = singleInst subroutines $ invokeInstance "maxStackAndLocal" (Assembler -> Int -> Int -> JVM_IO ()) assembler stack local
runAsm subroutines assembler MethodCodeStart = singleInst subroutines $ invokeInstance "methodCodeStart" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler MethodCodeEnd = singleInst subroutines $ invokeInstance "methodCodeEnd" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler (New cname) = singleInst subroutines $ invokeInstance "asmNew" (Assembler -> String -> JVM_IO ()) assembler cname
runAsm subroutines assembler (InstanceOf cname) = singleInst subroutines $ invokeInstance "asmInstanceOf" (Assembler -> String -> JVM_IO ()) assembler cname
runAsm subroutines assembler Pop = singleInst subroutines $ invokeInstance "pop" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Pop2 = singleInst subroutines $ invokeInstance "pop2" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler Return = singleInst subroutines $ invokeInstance "asmReturn" (Assembler -> JVM_IO ()) assembler
runAsm subroutines assembler ShouldDescribeFrame = singleInst subroutines $ invokeInstance "shouldDescribeFrame" (Assembler -> JVM_IO Bool) assembler
runAsm subroutines assembler (SourceInfo sourceFileName)
  = singleInst subroutines $ invokeInstance "sourceInfo" (Assembler -> String -> JVM_IO ()) assembler sourceFileName
runAsm subroutines assembler (Subroutine subroutine) = pure ((), subroutine :: subroutines)
runAsm subroutines assembler (UpdateFunctionName (MkJMethodName cname fname))
  = singleInst subroutines $ invokeInstance "updateFunctionName" (Assembler -> String -> String -> JVM_IO ()) assembler cname fname
runAsm subroutines assembler (UpdateLocalVarCount n)
  = singleInst subroutines $ invokeInstance "updateLocalVarCount" (Assembler -> Int -> JVM_IO ()) assembler (cast n)
runAsm subroutines assembler (UpdateShouldDescribeFrame shouldDescribeFrame)
  = singleInst subroutines $ invokeInstance "updateShouldDescribeFrame" (Assembler -> Bool -> JVM_IO ()) assembler shouldDescribeFrame
runAsm subroutines assembler (UpdateSwitchIndex n)
  = singleInst subroutines $ invokeInstance "updateSwitchIndex" (Assembler -> Int -> JVM_IO ()) assembler (cast n)
runAsm subroutines assembler (UpdateIfIndex n)
  = singleInst subroutines $ invokeInstance "updateIfIndex" (Assembler -> Int -> JVM_IO ()) assembler (cast n)
runAsm subroutines assembler (Pure v) = pure (v, subroutines)
runAsm subroutines assembler (Bind asm f) = do
  (v, newSubroutines) <- runAsm subroutines assembler asm
  runAsm newSubroutines assembler $ f v
