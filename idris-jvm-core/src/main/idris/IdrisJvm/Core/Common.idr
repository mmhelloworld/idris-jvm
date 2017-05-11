module IdrisJvm.Core.Common

import Data.SortedMap
import IdrisJvm.Core.Asm
import IdrisJvm.IR.Types
import IdrisJvm.IO
import Java.Lang

%access public export

jtrace : Show a => a -> b -> b
jtrace x val = unsafePerformIO {ffi=FFI_JVM} (do printLn x; pure val)

private
knownOperators : SortedMap Char String
knownOperators = SortedMap.fromList [ (' ', "space")
                                   , ('!', "excl")
                                   , ('"', "dquot")
                                   , ('#', "hash")
                                   , ('$', "dollar")
                                   , ('%', "percent")
                                   , ('&', "amper")
                                   , ('\'', "squot")
                                   , ('(', "lpar")
                                   , (')', "rpar")
                                   , ('*', "asterisk")
                                   , ('+', "plus")
                                   , (',', "comma")
                                   , ('-', "hyphen")
                                   , ('.', "dot")
                                   , ('/', "slash")
                                   , (':', "colon")
                                   , (';', "semicol")
                                   , ('<', "lt")
                                   , ('=', "eq")
                                   , ('>', "gt")
                                   , ('?', "ques")
                                   , ('@', "at")
                                   , ('^', "caret")
                                   , ('`', "grave")
                                   , ('{', "lbrace")
                                   , ('|', "pipe")
                                   , ('}', "rbrace")
                                   , ('~', "tilde")
                                   ]

private
isAllowed : Char -> Bool
isAllowed c = isAlphaNum c || c == '_' || c == '$'

jerror : String -> a
jerror msg = believe_me . unsafePerformIO $ invokeStatic RuntimeClass "error" (Object -> JVM_IO Object) (believe_me msg)

sep : String -> List String -> String
sep x xs = cast $ intercalate (cast x) $ map cast xs

private
jchar : Char -> String
jchar x = if isAllowed x
            then cast x
            else case lookup x knownOperators of
              Just subst => "_" ++ subst ++ "_"
              Nothing => "_" ++ show (ord x) ++ "_"

jname : String -> JMethodName
jname s =
  let (classNameWithDot, methodName) = span isIdentifier $ cast s
      reversedClassNameWithDot = reverse classNameWithDot
  in
    case span (not . (==) '.') reversedClassNameWithDot of
      ([], c) => MkJMethodName (createClassName $ cname1 c) (createMethodName (mname1 reversedClassNameWithDot methodName))
      (m, c) => MkJMethodName (createClassName $ cname1 c) (createMethodName $ reverse m ++ methodName)

  where
    dotsLengthAtEnd : List Char -> Nat
    dotsLengthAtEnd c = List.length $ takeWhile (== '.') c

    mname1 : List Char -> List Char -> List Char
    mname1 c m = the (List Char) $ (replicate (pred (dotsLengthAtEnd c)) '.') ++ m

    cname1 : List Char -> List Char
    cname1 s = reverse $ dropWhile (== '.') s

    isIdentifier : Char -> Bool
    isIdentifier c = isAlphaNum c || c == '.'

    createClassName : List Char -> String
    createClassName [] = "main/Main"
    createClassName xs = case split ((==) '.') xs of
      [c] => "main/" ++ cast c
      xs => cast $ the (List Char) $ intercalate ['/'] xs

    createMethodName : List Char -> String
    createMethodName xs = concat $ jchar <$> xs

locIndex : LVar -> Int
locIndex (Loc i) = i
locIndex _       = jerror "Unexpected global variable"

rtClassSig : String -> String
rtClassSig c = "mmhelloworld/idrisjvmruntime/" ++ c

rtFuncSig : String
rtFuncSig = "L" ++ rtClassSig "Function" ++ ";"

rtThunkSig : String
rtThunkSig = "L" ++ rtClassSig "Thunk" ++ ";"

createThunkSig : String
createThunkSig = "(" ++ rtFuncSig ++ "[Ljava/lang/Object;)" ++ rtThunkSig

{-hash : String -> Int
hash s = flip mod 100 . sum $ the (List Int) $ map cast (the (List Char) $ cast s)

hashedClassName : String -> String -> String
hashedClassName "call__IO" cname = cname
hashedClassName mname cname = cname ++ show (hash mname)-}

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

sig : Nat -> String
sig nArgs = "(" ++ argTypes ++  ")Ljava/lang/Object;" where
  argTypes : String
  argTypes = concat (replicate nArgs "Ljava/lang/Object;")

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

lambdaDesc : Descriptor
lambdaDesc = "([Ljava/lang/Object;)Ljava/lang/Object;"

invokeDynamic : ClassName -> MethodName -> Asm ()
invokeDynamic cname lambda = InvokeDynamic "apply" ("()" ++ rtFuncSig) metafactoryHandle metafactoryArgs where
  metafactoryHandle = MkHandle HInvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory" metafactoryDesc False

  lambdaHandle : Handle
  lambdaHandle = MkHandle HInvokeStatic cname lambda lambdaDesc False

  metafactoryArgs = [ BsmArgGetType lambdaDesc
                    , BsmArgHandle lambdaHandle
                    , BsmArgGetType lambdaDesc
                    ]

storeArgIntoArray : Int -> Int -> Asm ()
storeArgIntoArray lhs rhs = do
  Dup
  Iconst lhs
  Aload rhs
  Aastore

loadArgsFromArray : Nat -> Asm ()
loadArgsFromArray nArgs = case isLTE 1 nArgs of
    Yes prf => sequence_ (map (loadArg . cast) [0 .. (Nat.(-) nArgs 1)])
    No contra => Pure ()
  where
    loadArg : Int -> Asm ()
    loadArg n = do Aload 0; Iconst n; Aaload

createThunkForLambda : JMethodName -> List LVar -> (MethodName -> Asm ()) -> Asm ()
createThunkForLambda caller args lambdaCode = do
  let nArgs = List.length args
  let cname = jmethClsName caller
  lambdaIndex <- FreshLambdaIndex cname
  let lambdaMethodName = sep "$" ["lambda", jmethName caller, show lambdaIndex]
  invokeDynamic cname lambdaMethodName
  lambdaCode lambdaMethodName
  Iconst $ cast nArgs
  Anewarray "java/lang/Object"
  let argNums = map locIndex args
  sequence_ . map (uncurry storeArgIntoArray) $ List.zip [0.. (cast $ List.length argNums)] argNums
  InvokeMethod InvokeStatic (rtClassSig "Runtime") "thunk" createThunkSig False

createLambda : JMethodName -> ClassName -> MethodName -> Nat -> Asm ()
createLambda (MkJMethodName cname fname) callerCname lambdaMethodName nArgs = do
  CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName lambdaDesc Nothing Nothing [] []
  MethodCodeStart
  loadArgsFromArray nArgs
  InvokeMethod InvokeStatic cname fname (sig nArgs) False -- invoke the target method
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
  CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName lambdaDesc Nothing Nothing [] []
  MethodCodeStart
  loadArgsFromArray nArgs
  InvokeMethod InvokeStatic cname fname (sig nArgs) False -- invoke the target method
  Astore 1
  Aload 1
  InvokeMethod InvokeVirtual "java/lang/Object" "getClass" "()Ljava/lang/Class;" False
  InvokeMethod InvokeVirtual "java/lang/Class" "isArray" "()Z" False
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

mainMethod : Asm ()
mainMethod = do
  let cname = "main/Main"
  let mname = "$runMain0$"
  CreateMethod [Public, Static] cname "main" "([Ljava/lang/String;)V" Nothing Nothing [] []
  MethodCodeStart
  InvokeMethod InvokeStatic cname mname "()Ljava/lang/Object;" False
  Pop
  Return
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

invokeError : String -> Asm ()
invokeError x = do
  Ldc $ StringConst x
  InvokeMethod InvokeStatic (rtClassSig "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False

getPrimitiveClass : String -> Asm ()
getPrimitiveClass clazz = Field FGetStatic clazz "TYPE" "Ljava/lang/Class;"

