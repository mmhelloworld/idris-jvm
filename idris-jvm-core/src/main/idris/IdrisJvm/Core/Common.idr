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

classSig : String -> String
classSig className = "L" ++ className ++ ";"

rtClass : String -> String
rtClass c = "io/github/mmhelloworld/idrisjvm/runtime/" ++ c

rtFuncSig : String
rtFuncSig = classSig $ rtClass "Function"

rtThunkSig : String
rtThunkSig = classSig $ rtClass "Thunk"

utilClass : String
utilClass = rtClass "Util"

idrisObjectType : String
idrisObjectType = rtClass "IdrisObject"

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

unwrapExportedIO : Asm ()
unwrapExportedIO = do
  InvokeMethod InvokeStatic "main/Main" "call__IO" (sig 3) False
  InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" (sig 1) False

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

unrolledConstructorPropsCount : Nat
unrolledConstructorPropsCount = 10

idrisObjectProperty : Int -> Int -> Asm ()
idrisObjectProperty object propertyIndex = do
  Aload object
  Checkcast idrisObjectType
  if propertyIndex < (cast unrolledConstructorPropsCount) then
    let fieldName = "property" ++ cast propertyIndex
    in Field FGetField idrisObjectType fieldName "Ljava/lang/Object;"
  else do
    Field FGetField idrisObjectType "properties" "[Ljava/lang/Object;"
    Iconst (propertyIndex - (cast unrolledConstructorPropsCount))
    Aaload

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
  InvokeMethod InvokeStatic (rtClass "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False

getPrimitiveClass : String -> Asm ()
getPrimitiveClass clazz = Field FGetStatic clazz "TYPE" "Ljava/lang/Class;"
