module IdrisJvm.Core.Common

import Data.SortedMap
import IdrisJvm.Core.Asm
import IdrisJvm.IR.Types
import IdrisJvm.IO
import Java.Lang

%access public export

jtrace : Show a => a -> b -> b
jtrace x val = unsafePerformIO {ffi=FFI_JVM} (do printLn x; pure val)

jerror : String -> a
jerror msg = believe_me . unsafePerformIO $ invokeStatic RuntimeClass "error" (Object -> JVM_IO Object) (believe_me msg)

sep : String -> List String -> String
sep x xs = cast $ intercalate (cast x) $ map cast xs

IdrisToJavaNameConverterClass : JVM_NativeTy
IdrisToJavaNameConverterClass = Class "IdrisJvm/Core/IdrisToJavaNameConverter"

jname : String -> JMethodName
jname s =
  let [cname, mname] = split (== ',') . unsafePerformIO $ invokeStatic IdrisToJavaNameConverterClass "idrisClassMethodName" (String -> JVM_IO String) s
  in MkJMethodName cname mname

locIndex : LVar -> Int
locIndex (Loc i) = i
locIndex _       = jerror "Unexpected global variable"

rtClassSig : String -> String
rtClassSig c = "io/github/mmhelloworld/idrisjvm/runtime/" ++ c

rtFuncSig : String
rtFuncSig = "L" ++ rtClassSig "Function" ++ ";"

rtThunkSig : String
rtThunkSig = "L" ++ rtClassSig "Thunk" ++ ";"

idrisObjectType : String
idrisObjectType = rtClassSig "IdrisObject"

listRange : Int -> Int -> List Int
listRange from to = if from <= to then [from .. to] else []

natRange : Nat -> Nat -> List Nat
natRange from to = if from <= to then [from .. to] else []

assign : Int -> Int -> Asm ()
assign from to = if from == to
                   then Pure ()
                   else do
                     Aload from
                     Astore to

boxDouble : Asm ()
boxDouble = InvokeMethod InvokeStatic "java/lang/Double" "valueOf" "(D)Ljava/lang/Double;" False

boxBool : Asm ()
boxBool = InvokeMethod InvokeStatic "java/lang/Boolean" "valueOf" "(Z)Ljava/lang/Boolean;" False

boxChar : Asm ()
boxChar = InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

boxInt : Asm ()
boxInt = InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False

boxLong : Asm ()
boxLong = InvokeMethod InvokeStatic "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;" False

unboxBool : Asm ()
unboxBool = InvokeMethod InvokeVirtual "java/lang/Boolean" "booleanValue" "()Z" False

unboxInt : Asm ()
unboxInt = InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False

unboxChar : Asm ()
unboxChar = InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False

unboxLong : Asm ()
unboxLong = InvokeMethod InvokeVirtual "java/lang/Long" "longValue" "()J" False

unboxDouble : Asm ()
unboxDouble = InvokeMethod InvokeVirtual "java/lang/Double" "doubleValue" "()D" False

unboxFloat : Asm ()
unboxFloat = InvokeMethod InvokeVirtual "java/lang/Float" "floatValue" "()F" False

repeatString : Nat -> String -> String
repeatString n s = concat (replicate n s)

repeatObjectDesc : Nat -> String
repeatObjectDesc n = repeatString n "Ljava/lang/Object;"

sig : Nat -> String
sig nArgs = "(" ++ repeatObjectDesc nArgs ++  ")Ljava/lang/Object;"

anewarray : FieldTypeDescriptor -> Asm ()
anewarray FieldTyDescByte          = Anewbytearray
anewarray FieldTyDescChar          = Anewchararray
anewarray FieldTyDescShort         = Anewshortarray
anewarray FieldTyDescBoolean       = Anewbooleanarray
anewarray FieldTyDescArray         = jerror $ "array is not a valid element type for a single dimensional array"
anewarray FieldTyDescDouble        = Anewdoublearray
anewarray FieldTyDescFloat         = Anewfloatarray
anewarray FieldTyDescInt           = Anewintarray
anewarray FieldTyDescLong          = Anewlongarray
anewarray (FieldTyDescReference f) = Anewarray $ asmRefTyDesc f

arrayDesc : String -> Nat -> String
arrayDesc cname dimensions =
  let arrayPrefix = cast $ replicate dimensions '['
  in arrayPrefix ++ cname

metafactoryDesc : Descriptor
metafactoryDesc =
  concat [ "("
         , "Ljava/lang/invoke/MethodHandles$Lookup;"
         , "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodHandle;"
         , "Ljava/lang/invoke/MethodType;"
         , ")"
         , "Ljava/lang/invoke/CallSite;"
         ]

idrisObjectProperty : Int -> Int -> Asm ()
idrisObjectProperty object propertyIndex = do
    Aload object
    Iconst propertyIndex
    InvokeMethod InvokeStatic idrisObjectType "getProperty"  "(Ljava/lang/Object;I)Ljava/lang/Object;" False

invokeDynamic : ClassName -> MethodName -> Nat -> Asm ()
invokeDynamic cname lambda nArgs =
    InvokeDynamic "call" desc metafactoryHandle metafactoryArgs
  where
    desc : String
    desc = "(" ++ repeatObjectDesc nArgs ++ ")" ++ rtThunkSig

    metafactoryHandle = MkHandle HInvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory" metafactoryDesc False

    lambdaHandle : Handle
    lambdaHandle = MkHandle HInvokeStatic cname lambda (sig nArgs) False

    metafactoryArgs = [ BsmArgGetType "()Ljava/lang/Object;"
                      , BsmArgHandle lambdaHandle
                      , BsmArgGetType "()Ljava/lang/Object;"
                      ]

arrayStore : FieldTypeDescriptor -> Asm ()
arrayStore FieldTyDescByte = Bastore
arrayStore FieldTyDescChar = Castore
arrayStore FieldTyDescShort = Sastore
arrayStore FieldTyDescBoolean = Bastore
arrayStore FieldTyDescArray = Aastore
arrayStore FieldTyDescDouble = Dastore
arrayStore FieldTyDescFloat = Fastore
arrayStore FieldTyDescInt = Iastore
arrayStore FieldTyDescLong = Lastore
arrayStore (FieldTyDescReference ReferenceTypeDescriptor) = Aastore

arrayLoad : FieldTypeDescriptor -> Asm ()
arrayLoad FieldTyDescByte                                = Baload
arrayLoad FieldTyDescChar                                = Caload
arrayLoad FieldTyDescShort                               = Saload
arrayLoad FieldTyDescBoolean                             = Baload
arrayLoad FieldTyDescArray                               = Aaload
arrayLoad FieldTyDescDouble                              = Daload
arrayLoad FieldTyDescFloat                               = Faload
arrayLoad FieldTyDescInt                                 = Iaload
arrayLoad FieldTyDescLong                                = Laload
arrayLoad (FieldTyDescReference ReferenceTypeDescriptor) = Aaload

typeDescToarrayElemDesc : TypeDescriptor -> FieldTypeDescriptor
typeDescToarrayElemDesc VoidDescriptor = jerror $ "An array cannot have 'void' elements"
typeDescToarrayElemDesc (FieldDescriptor desc) = desc

loadArgsForLambdaTargetMethod : Nat -> Asm ()
loadArgsForLambdaTargetMethod nArgs = case isLTE 1 nArgs of
    Yes prf => sequence_ (map (Aload . cast) [0 .. (Nat.(-) nArgs 1)])
    No contra => Pure ()

createThunkForLambda : JMethodName -> List LVar -> (MethodName -> Asm ()) -> Asm ()
createThunkForLambda caller args lambdaCode = do
  let nArgs = List.length args
  let cname = jmethClsName caller
  lambdaIndex <- FreshLambdaIndex cname
  let lambdaMethodName = sep "$" ["lambda", jmethName caller, show lambdaIndex]
  let argNums = map locIndex args
  sequence_ . map Aload $ argNums
  invokeDynamic cname lambdaMethodName nArgs
  lambdaCode lambdaMethodName

createLambda : JMethodName -> ClassName -> MethodName -> Nat -> Asm ()
createLambda (MkJMethodName cname fname) callerCname lambdaMethodName nArgs = do
  let desc = sig nArgs
  CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName desc Nothing Nothing [] []
  MethodCodeStart
  loadArgsForLambdaTargetMethod nArgs
  InvokeMethod InvokeStatic cname fname desc False -- invoke the target method
  Areturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

createThunk : JMethodName -> JMethodName -> (List LVar) -> Asm ()
createThunk caller@(MkJMethodName callerCname _) fname args = do
  let nArgs = List.length args
  let lambdaCode = \lambdaMethodName => Subroutine $ createLambda fname callerCname lambdaMethodName nArgs
  createThunkForLambda caller args lambdaCode

createParLambda : JMethodName -> ClassName -> MethodName -> Nat -> Asm ()
createParLambda (MkJMethodName cname fname) callerCname lambdaMethodName nArgs = do
  let desc = sig nArgs
  CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName desc Nothing Nothing [] []
  MethodCodeStart
  loadArgsForLambdaTargetMethod nArgs
  InvokeMethod InvokeStatic cname fname desc False -- invoke the target method
  Astore 1
  Aload 1
  InstanceOf idrisObjectType
  CreateLabel "elseLabel"
  Ifeq "elseLabel"
  Aload 1
  InvokeMethod InvokeStatic cname fname "(Ljava/lang/Object;)Ljava/lang/Object;" False
  Areturn
  LabelStart "elseLabel"
  Frame FAppend 1 ["java/lang/Object"] 0 []
  Aload 1
  Areturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

createParThunk : JMethodName -> JMethodName -> (List LVar) -> Asm ()
createParThunk caller@(MkJMethodName callerCname _) fname args = do
  let nArgs = List.length args
  let lambdaCode = \lambdaMethodName => Subroutine $ createParLambda fname callerCname lambdaMethodName nArgs
  createThunkForLambda caller args lambdaCode

addFrame : Asm ()
addFrame = do
  needFrame <- ShouldDescribeFrame
  nlocalVars <- GetLocalVarCount
  if needFrame
    then do
      Frame FFull (succ nlocalVars) (replicate (succ nlocalVars)  "java/lang/Object") 0 []
      UpdateShouldDescribeFrame False
    else Frame FSame 0 [] 0 []

defaultConstructor : ClassName -> ClassName -> Asm ()
defaultConstructor cname parent = do
  CreateMethod [Public] cname "<init>" "()V" Nothing Nothing [] []
  MethodCodeStart
  Aload 0
  InvokeMethod InvokeSpecial parent "<init>" "()V" False
  Return
  MaxStackAndLocal (-1) (-1) -- Let the asm calculate
  MethodCodeEnd

invokeError : String -> Asm ()
invokeError x = do
  Ldc $ StringConst x
  InvokeMethod InvokeStatic (rtClassSig "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False

getPrimitiveClass : String -> Asm ()
getPrimitiveClass clazz = Field FGetStatic clazz "TYPE" "Ljava/lang/Class;"

