module IdrisJvm.Core.JAsm

import IdrisJvm.IO
import IdrisJvm.Core.Asm
import Java.Util
import Java.Lang
import Data.SortedMap

%access public export

Assembler : Type
Assembler = javaClass "IdrisJvm/Core/Assembler"

JAnnotation : Type
JAnnotation = javaClass "IdrisJvm/Core/Annotation"

JAnnString : Type
JAnnString = javaClass "IdrisJvm/Core/AnnotationValue$AnnString"

JAnnEnum : Type
JAnnEnum = javaClass "IdrisJvm/Core/AnnotationValue$AnnEnum"

JAnnInt : Type
JAnnInt = javaClass "IdrisJvm/Core/AnnotationValue$AnnInt"

JAnnArray : Type
JAnnArray = javaClass "IdrisJvm/Core/AnnotationValue$AnnArray"

JAnnotationProperty : Type
JAnnotationProperty = javaClass "IdrisJvm/Core/AnnotationProperty"

JAnnotationValue: Type
JAnnotationValue = javaClass "IdrisJvm/Core/AnnotationValue"

JHandle: Type
JHandle = javaClass "IdrisJvm/Core/JHandle"

JBsmArg: Type
JBsmArg = javaClass "IdrisJvm/Core/JBsmArg"

JBsmArgGetType: Type
JBsmArgGetType = javaClass "IdrisJvm/Core/JBsmArg$JBsmArgGetType"

JBsmArgHandle: Type
JBsmArgHandle = javaClass "IdrisJvm/Core/JBsmArg$JBsmArgHandle"

toJClassOpts : ClassOpts -> Int
toJClassOpts ComputeMaxs = 1
toJClassOpts ComputeFrames = 2

toJAnnotationValue : Asm.AnnotationValue -> JVM_IO JAnnotationValue
toJAnnotationValue (AnnString s) = believe_me <$> FFI.new (String -> JVM_IO JAnnString) s
toJAnnotationValue (AnnEnum enum s) = believe_me <$> FFI.new (String -> String -> JVM_IO JAnnEnum) enum s
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

record AsmState where
  constructor MkAsmState
  subroutines : List (Asm ())
  functionTypes : SortedMap JMethodName InferredFunctionType
  functionLocTypes : InferredTypeStore
  functionRetType : InferredType

singleInst : AsmState -> JVM_IO a -> JVM_IO (a, AsmState)
singleInst state m = do
  a <- m
  pure (a, state)

runAsm : AsmState -> Assembler -> Asm a -> JVM_IO (a, AsmState)
runAsm state assembler Aaload = singleInst state $ invokeInstance "aaload" (Assembler -> JVM_IO ()) assembler

runAsm state assembler Aastore = singleInst state $ invokeInstance "aastore" (Assembler -> JVM_IO ()) assembler

runAsm state assembler Aconstnull = singleInst state $ invokeInstance "aconstnull" (Assembler -> JVM_IO ()) assembler

runAsm state assembler (Aload n) = singleInst state $ invokeInstance "aload" (Assembler -> Int -> JVM_IO ()) assembler n

runAsm state assembler (Anewarray desc) = singleInst state $ invokeInstance "anewarray" (Assembler -> String -> JVM_IO ()) assembler desc
runAsm state assembler Anewintarray     = singleInst state $ invokeInstance "anewintarray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Anewbooleanarray = singleInst state $ invokeInstance "anewbooleanarray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Anewbytearray    = singleInst state $ invokeInstance "anewbytearray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Anewchararray    = singleInst state $ invokeInstance "anewchararray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Anewshortarray   = singleInst state $ invokeInstance "anewshortarray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Anewlongarray    = singleInst state $ invokeInstance "anewlongarray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Anewfloatarray   = singleInst state $ invokeInstance "anewfloatarray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Anewdoublearray  = singleInst state $ invokeInstance "anewdoublearray" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Arraylength      = singleInst state $ invokeInstance "arraylength" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Areturn          = singleInst state $ invokeInstance "areturn" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Astore n)       = singleInst state $ invokeInstance "astore" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Baload           = singleInst state $ invokeInstance "baload" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Bastore          = singleInst state $ invokeInstance "bastore" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Caload           = singleInst state $ invokeInstance "caload" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Castore          = singleInst state $ invokeInstance "castore" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Checkcast desc) = singleInst state $ invokeInstance "checkcast" (Assembler -> String -> JVM_IO ()) assembler desc
runAsm state assembler (ClassCodeStart version access className sig parent intf anns) = singleInst state $ do
  janns <- sequence $ toJAnnotation <$> anns
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

runAsm state assembler (ClassCodeEnd out) = singleInst state $ invokeInstance "classCodeEnd" (Assembler -> String -> JVM_IO ()) assembler out
runAsm state assembler (CreateClass opts) = singleInst state $ invokeInstance "createClass" (Assembler -> Int -> JVM_IO ()) assembler (toJClassOpts opts)
runAsm state assembler (CreateField accs className fieldName desc sig fieldInitialValue) = singleInst state $ do
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

runAsm state assembler (CreateLabel label) = singleInst state $ do
  invokeInstance "createLabel" (Assembler -> String -> JVM_IO ()) assembler label
  pure label

runAsm state assembler (CreateMethod accs className methodName desc sig exceptions anns paramAnns) = singleInst state $ do
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

runAsm state assembler D2i = singleInst state $ invokeInstance "d2i" (Assembler -> JVM_IO ()) assembler
runAsm state assembler D2f = singleInst state $ invokeInstance "d2f" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Dadd = singleInst state $ invokeInstance "dadd" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Dconst n) = singleInst state $ invokeInstance "dconst" (Assembler -> Double -> JVM_IO ()) assembler n
runAsm state assembler Daload = singleInst state $ invokeInstance "daload" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Dastore = singleInst state $ invokeInstance "dastore" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Ddiv = singleInst state $ invokeInstance "ddiv" (Assembler -> JVM_IO ()) assembler

runAsm state assembler (Debug msg) = singleInst state $ invokeInstance "debug" (Assembler -> String -> JVM_IO ()) assembler msg

runAsm state assembler (Dload n) = singleInst state $ invokeInstance "dload" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Dmul = singleInst state $ invokeInstance "dmul" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Drem = singleInst state $ invokeInstance "drem" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Dreturn = singleInst state $ invokeInstance "dreturn" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Dstore n) = singleInst state $ invokeInstance "dstore" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Dsub = singleInst state $ invokeInstance "dsub" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Dup = singleInst state $ invokeInstance "dup" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Error err) = singleInst state $ invokeInstance "error" (Assembler -> String -> JVM_IO ()) assembler err
runAsm state assembler F2d = singleInst state $ invokeInstance "f2d" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Faload = singleInst state $ invokeInstance "faload" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Fastore = singleInst state $ invokeInstance "fastore" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Fconst n) = singleInst state $ invokeInstance "fconst" (Assembler -> Double -> JVM_IO ()) assembler n
runAsm state assembler (Field finsType cname fname desc) = singleInst state $ do
  let finsTypeNum = fieldInsTypeNum finsType
  invokeInstance
    "field"
    (Assembler -> Int -> ClassName -> FieldName -> Descriptor -> JVM_IO ())
    assembler
    finsTypeNum
    cname
    fname
    desc

runAsm state assembler FieldEnd = singleInst state $ invokeInstance "fieldEnd" (Assembler -> JVM_IO ()) assembler

runAsm state assembler (Fload n) = singleInst state $ invokeInstance "fload" (Assembler -> Int -> JVM_IO ()) assembler n

runAsm state assembler (Frame frameType nLocal localSigs nStack stackSigs) = singleInst state $ do
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

runAsm state assembler FreshIfIndex = singleInst state $ do
  index <- invokeInstance "freshIfIndex" (Assembler -> JVM_IO Int) assembler
  pure $ cast index

runAsm state assembler (FreshLambdaIndex cname) = singleInst state $ do
  index <- invokeInstance "freshLambdaIndex" (Assembler -> String -> JVM_IO Int) assembler cname
  pure $ cast index

runAsm state assembler FreshSwitchIndex = singleInst state $ do
  index <- invokeInstance "freshSwitchIndex" (Assembler -> JVM_IO Int) assembler
  pure $ cast index

runAsm state assembler Freturn = singleInst state $ invokeInstance "freturn" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Fstore n) = singleInst state $ invokeInstance "fstore" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler GetFunctionName = singleInst state $ do
  cname <- invokeInstance "getClassName" (Assembler -> JVM_IO String) assembler
  mname <- invokeInstance "getMethodName" (Assembler -> JVM_IO String) assembler
  pure $ MkJMethodName cname mname

runAsm state assembler GetFunctionLocTypes = pure (functionLocTypes state, state)
runAsm state assembler GetFunctionRetType = pure (functionRetType state, state)
runAsm state assembler GetFunctionTypes = pure (functionTypes state, state)

runAsm state assembler GetLocalVarCount = singleInst state $ do
  index <- invokeInstance "getLocalVarCount" (Assembler -> JVM_IO Int) assembler
  pure $ cast index
runAsm state assembler (Goto label) = singleInst state $ invokeInstance "gotoLabel" (Assembler -> String -> JVM_IO ()) assembler label

runAsm state assembler I2b = singleInst state $ invokeInstance "i2b" (Assembler -> JVM_IO ()) assembler
runAsm state assembler I2c = singleInst state $ invokeInstance "i2c" (Assembler -> JVM_IO ()) assembler
runAsm state assembler I2d = singleInst state $ invokeInstance "i2d" (Assembler -> JVM_IO ()) assembler
runAsm state assembler I2l = singleInst state $ invokeInstance "i2l" (Assembler -> JVM_IO ()) assembler
runAsm state assembler I2s = singleInst state $ invokeInstance "i2s" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Iadd = singleInst state $ invokeInstance "iadd" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Iaload = singleInst state $ invokeInstance "iaload" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Iand = singleInst state $ invokeInstance "iand" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Iastore = singleInst state $ invokeInstance "iastore" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Ior = singleInst state $ invokeInstance "ior" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Ixor = singleInst state $ invokeInstance "ixor" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Icompl = singleInst state $ invokeInstance "icompl" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Iconst n) = singleInst state $ invokeInstance "iconst" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Idiv = singleInst state $ invokeInstance "idiv" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Ifeq label) = singleInst state $ invokeInstance "ifeq" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler (Ificmpge label) = singleInst state $ invokeInstance "ificmpge" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler (Ificmpgt label) = singleInst state $ invokeInstance "ificmpgt" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler (Ificmple label) = singleInst state $ invokeInstance "ificmple" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler (Ificmplt label) = singleInst state $ invokeInstance "ificmplt" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler (Ifnonnull label) = singleInst state $ invokeInstance "ifnonnull" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler (Ifnull label) = singleInst state $ invokeInstance "ifnull" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler (Iload n) = singleInst state $ invokeInstance "iload" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Imul = singleInst state $ invokeInstance "imul" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (InstanceOf className) = singleInst state $ invokeInstance "instanceOf" (Assembler -> String -> JVM_IO ()) assembler className
runAsm state assembler (InvokeMethod invocType cname mname desc isIntf) = singleInst state $ do
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

runAsm state assembler (InvokeDynamic mname desc handle bsmArgs) = singleInst state $ do
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

runAsm state assembler Irem = singleInst state $ invokeInstance "irem" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Ireturn = singleInst state $ invokeInstance "ireturn" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Ishl = singleInst state $ invokeInstance "ishl" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Ishr = singleInst state $ invokeInstance "ishr" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Istore n) = singleInst state $ invokeInstance "istore" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Isub = singleInst state $ invokeInstance "isub" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Iushr = singleInst state $ invokeInstance "iushr" (Assembler -> JVM_IO ()) assembler
runAsm state assembler L2i = singleInst state $ invokeInstance "l2i" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (LabelStart label) = singleInst state $ invokeInstance "labelStart" (Assembler -> String -> JVM_IO ()) assembler label
runAsm state assembler Ladd = singleInst state $ invokeInstance "ladd" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Land = singleInst state $ invokeInstance "land" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Laload = singleInst state $ invokeInstance "laload" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lastore = singleInst state $ invokeInstance "lastore" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lor = singleInst state $ invokeInstance "lor" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lxor = singleInst state $ invokeInstance "lxor" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lcompl = singleInst state $ invokeInstance "lcompl" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Lconst c) = singleInst state $ invokeInstance "lconst" (Assembler -> Bits64 -> JVM_IO ()) assembler c

runAsm state assembler (Ldc (TypeConst ty)) = singleInst state $ invokeInstance "ldcType" (Assembler -> String -> JVM_IO ()) assembler ty
runAsm state assembler (Ldc constant) = singleInst state $ invokeInstance "ldc" (Assembler -> Object -> JVM_IO ()) assembler (constantToObject constant)

runAsm state assembler Ldiv = singleInst state $ invokeInstance "ldiv" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Lload n) = singleInst state $ invokeInstance "lload" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Lmul = singleInst state $ invokeInstance "lmul" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (LookupSwitch defaultLabel labels cases) = singleInst state $ do
  jlabels <- ArrayList.fromList labels
  jcases <- ArrayList.fromList $ JInteger.valueOf <$> cases
  invokeInstance
    "lookupSwitch"
    (Assembler -> String -> JList -> JList -> JVM_IO ())
    assembler
    defaultLabel
    (believe_me jlabels)
    (believe_me jcases)

runAsm state assembler Lrem = singleInst state $ invokeInstance "lrem" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lreturn = singleInst state $ invokeInstance "lreturn" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lshl = singleInst state $ invokeInstance "lshl" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lshr = singleInst state $ invokeInstance "lshr" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Lstore n) = singleInst state $ invokeInstance "lstore" (Assembler -> Int -> JVM_IO ()) assembler n
runAsm state assembler Lsub = singleInst state $ invokeInstance "lsub" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Lushr = singleInst state $ invokeInstance "lushr" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (MaxStackAndLocal stack local)
  = singleInst state $ invokeInstance "maxStackAndLocal" (Assembler -> Int -> Int -> JVM_IO ()) assembler stack local
runAsm state assembler MethodCodeStart = singleInst state $ invokeInstance "methodCodeStart" (Assembler -> JVM_IO ()) assembler
runAsm state assembler MethodCodeEnd = singleInst state $ invokeInstance "methodCodeEnd" (Assembler -> JVM_IO ()) assembler
runAsm state assembler (Multianewarray desc dims)
  = singleInst state $ invokeInstance "multiANewArray" (Assembler -> String -> Int -> JVM_IO ()) assembler desc (cast dims)
runAsm state assembler (New cname) = singleInst state $ invokeInstance "asmNew" (Assembler -> String -> JVM_IO ()) assembler cname
runAsm state assembler Pop = singleInst state $ invokeInstance "pop" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Pop2 = singleInst state $ invokeInstance "pop2" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Return = singleInst state $ invokeInstance "asmReturn" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Saload = singleInst state $ invokeInstance "saload" (Assembler -> JVM_IO ()) assembler
runAsm state assembler Sastore = singleInst state $ invokeInstance "sastore" (Assembler -> JVM_IO ()) assembler
runAsm state assembler ShouldDescribeFrame = singleInst state $ invokeInstance "shouldDescribeFrame" (Assembler -> JVM_IO Bool) assembler
runAsm state assembler (SourceInfo sourceFileName)
  = singleInst state $ invokeInstance "sourceInfo" (Assembler -> String -> JVM_IO ()) assembler sourceFileName
runAsm state assembler (Subroutine subroutine) = pure ((), record { subroutines $= (subroutine ::) } state)
runAsm state assembler (UpdateFunctionName (MkJMethodName cname fname))
  = singleInst state $ invokeInstance "updateFunctionName" (Assembler -> String -> String -> JVM_IO ()) assembler cname fname
runAsm state assembler (UpdateFunctionLocTypes types) = pure ((), record { functionLocTypes = types} state)
runAsm state assembler (UpdateFunctionRetType retTy) = pure ((), record { functionRetType = retTy } state)
runAsm state assembler (UpdateFunctionTypes types) = pure ((), record { functionTypes = types } state)
runAsm state assembler (UpdateLocalVarCount n)
  = singleInst state $ invokeInstance "updateLocalVarCount" (Assembler -> Int -> JVM_IO ()) assembler (cast n)
runAsm state assembler (UpdateShouldDescribeFrame shouldDescribeFrame)
  = singleInst state $ invokeInstance "updateShouldDescribeFrame" (Assembler -> Bool -> JVM_IO ()) assembler shouldDescribeFrame
runAsm state assembler (UpdateSwitchIndex n)
  = singleInst state $ invokeInstance "updateSwitchIndex" (Assembler -> Int -> JVM_IO ()) assembler (cast n)
runAsm state assembler (UpdateIfIndex n)
  = singleInst state $ invokeInstance "updateIfIndex" (Assembler -> Int -> JVM_IO ()) assembler (cast n)
runAsm state assembler (Pure v) = pure (v, state)
runAsm state assembler (Bind asm f) = do
  (v, newState) <- runAsm state assembler asm
  runAsm newState assembler $ f v
