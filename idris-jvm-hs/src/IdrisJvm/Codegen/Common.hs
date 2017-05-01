{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Common where

import           Control.Monad.RWS
import           Data.Char                  (isAlpha, isDigit, isLetter)
import qualified Data.DList                 as DL
import           Data.List                  (elem, intercalate, uncons)
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Types
import           IRTS.Lang

createThunkForLambda :: JMethodName -> [LVar] -> (MethodName -> DL.DList Asm) -> Cg ()
createThunkForLambda caller args lambdaCode = do
  let nArgs = length args
      cname = jmethClsName caller
  lambdaIndex <- freshLambdaIndex cname
  let lambdaMethodName = sep "$" ["lambda", jmethName caller, show lambdaIndex]
  writeIns $ invokeDynamic cname lambdaMethodName
  writeDeps $ lambdaCode lambdaMethodName
  writeIns [Iconst nArgs, Anewarray "java/lang/Object"]
  let argNums = map (\(Loc i) -> i) args
      f :: (Int, Int) -> DL.DList Asm
      f (lhs, rhs) = [Dup, Iconst lhs, Aload rhs, Aastore]
  writeIns . join . fmap f . DL.fromList $ zip [0..] argNums
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Runtime") "thunk" createThunkSig False ]

createThunk :: JMethodName -> JMethodName -> [LVar] -> Cg ()
createThunk caller@(JMethodName callerCname _) fname args = do
  let nArgs = length args
      lambdaCode lambdaMethodName = createLambda fname callerCname lambdaMethodName nArgs
  createThunkForLambda caller args lambdaCode

createParThunk :: JMethodName -> JMethodName -> [LVar] -> Cg ()
createParThunk caller@(JMethodName callerCname _) fname args = do
  let nArgs = length args
      lambdaCode lambdaMethodName = createParLambda fname callerCname lambdaMethodName nArgs
  createThunkForLambda caller args lambdaCode

invokeDynamic :: ClassName -> MethodName -> DL.DList Asm
invokeDynamic cname lambda = [ InvokeDynamic "apply" ("()" ++ rtFuncSig) metafactoryHandle metafactoryArgs] where
  metafactoryHandle = Handle HInvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory" metafactoryDesc False
  metafactoryArgs = [ BsmArgGetType lambdaDesc
                    , BsmArgHandle lambdaHandle
                    , BsmArgGetType lambdaDesc
                    ]
  lambdaHandle = Handle HInvokeStatic cname lambda lambdaDesc False

createLambda :: JMethodName -> ClassName -> MethodName -> Int -> DL.DList Asm
createLambda (JMethodName cname fname) callerCname lambdaMethodName nArgs
  = DL.fromList [ CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName lambdaDesc Nothing Nothing [] []
                  , MethodCodeStart
                  ] <>
                  join (fmap (\i -> [Aload 0, Iconst i, Aaload]) [0 .. (nArgs - 1)]) <> -- Retrieve lambda args
                  [ InvokeMethod InvokeStatic cname fname (sig nArgs) False -- invoke the target method
                  , Areturn
                  , MaxStackAndLocal (-1) (-1)
                  , MethodCodeEnd
                  ]

createParLambda :: JMethodName -> ClassName -> MethodName -> Int -> DL.DList Asm
createParLambda (JMethodName cname fname) callerCname lambdaMethodName nArgs
  = DL.fromList [ CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName lambdaDesc Nothing Nothing [] []
                , MethodCodeStart
                ] <>
                join (fmap (\i -> [Aload 0, Iconst i, Aaload]) [0 .. (nArgs - 1)]) <> -- Retrieve lambda args
                [ InvokeMethod InvokeStatic cname fname (sig nArgs) False -- invoke the target method
                , Astore 1
                , Aload 1
                , InvokeMethod InvokeVirtual "java/lang/Object" "getClass" "()Ljava/lang/Class;" False
                , InvokeMethod InvokeVirtual "java/lang/Class" "isArray" "()Z" False
                , CreateLabel "elseLabel"
                , Ifeq "elseLabel"
                , Aload 1
                , InvokeMethod InvokeStatic cname fname "(Ljava/lang/Object;)Ljava/lang/Object;" False
                , Areturn
                , LabelStart "elseLabel"
                , Frame FAppend 1 ["java/lang/Object"] 0 []
                , Aload 1
                , Areturn
                , MaxStackAndLocal (-1) (-1)
                , MethodCodeEnd
                ]

addFrame :: Cg ()
addFrame = do
  needFrame <- shouldDescribeFrame <$> get
  nlocalVars <- cgStLocalVarCount <$> get
  if needFrame
    then do
      writeIns [ Frame FFull (succ nlocalVars) (replicate (succ nlocalVars)  "java/lang/Object") 0 []]
      modify . updateShouldDescribeFrame $ const False
    else writeIns [ Frame FSame 0 [] 0 []]

defaultConstructor :: ClassName -> ClassName -> Cg ()
defaultConstructor cname parent
  = writeIns [ CreateMethod [Public] cname "<init>" "()V" Nothing Nothing [] []
             , MethodCodeStart
             , Aload 0
             , InvokeMethod InvokeSpecial parent "<init>" "()V" False
             , Return
             , MaxStackAndLocal (-1) (-1) -- Let the asm calculate
             , MethodCodeEnd
             ]

mainMethod :: Cg ()
mainMethod = do
  let JMethodName cname mname = jname $ MN 0 "runMain"
  writeIns [ CreateMethod [Public, Static] "main/Main" "main" "([Ljava/lang/String;)V" Nothing Nothing [] []
           , MethodCodeStart
           , InvokeMethod InvokeStatic cname mname "()Ljava/lang/Object;" False
           , Pop
           , Return
           , MaxStackAndLocal (-1) (-1)
           , MethodCodeEnd
           ]

invokeError :: String -> Cg ()
invokeError x
  = writeIns [ Ldc $ StringConst x
             , InvokeMethod InvokeStatic (rtClassSig "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False
             ]

locIndex :: LVar -> Int
locIndex (Loc i) = i
locIndex _       = error "Unexpected global variable"

javaName :: Name -> [String]
javaName (UN n) = [T.unpack n]
javaName (NS n s) = map T.unpack (reverse s) ++ [showSep "." $ javaName n]
javaName (MN _ u) | u == txt "underscore" = ["_"]
javaName (MN i s) = ["$" ++ T.unpack s ++ show i ++ "$"]
javaName (SN special) = javaName' special
  where javaName' :: SpecialName -> [String]
        javaName' (WhereN i p c) = appendToLast ("$whr$" ++ showSep "$" (javaName c) ++ ":" ++ show i) (javaName p)
        javaName' (WithN i n) = appendToLast ("$with$" ++ show i) $ javaName n
        javaName' (ImplementationN cl impl) = ["@" ++ showSep "." (javaName cl) ++ "$impl$" ++ showSep "$" (map T.unpack impl)]
        javaName' (MethodN m) = appendToLast "$meth$" $ javaName m
        javaName' (ParentN p c) = appendToLast ("$parent$" ++ show c) $ javaName p
        javaName' (CaseN fc c) = appendToLast (showFC' fc ++ "$case$") $ javaName c
        javaName' (ElimN sn) = appendToLast "$elim$" $ javaName sn
        javaName' (ImplementationCtorN n) = appendToLast "$implctor$" $ javaName n
        javaName' (MetaN parent meta) = appendToLast ("$meta$" ++ showSep "$" (javaName meta)) $ javaName parent
        showFC' (FC' NoFC) = ""
        showFC' (FC' (FileFC f)) = "_" ++ cgFN f
        showFC' (FC' (FC f s e))
          | s == e = "_" ++ cgFN f ++
                     "_" ++ show (fst s) ++ "_" ++ show (snd s)
          | otherwise = "_" ++ cgFN f ++
                        "_" ++ show (fst s) ++ "_" ++ show (snd s) ++
                        "_" ++ show (fst e) ++ "_" ++ show (snd e)
        cgFN = concatMap (\c -> if not (isDigit c || isLetter c) then "__" else [c])
javaName (SymRef i) = error $ "can't do codegen for a symbol reference: " ++ show i

appendToLast :: String -> [String] -> [String]
appendToLast x xs
  = maybe [x] (\(h, t) -> reverse $ (h ++ x) : t) $ uncons $ reverse xs

jname :: Name -> JMethodName
jname n = JMethodName cname methName
  where
    names = concatMap jchar <$> javaName n
    (cname, methName) = f [] names

    f [] []     = ("main/Main" , "main")
    f [] [x]    = ("main/Main", x)
    f [] [x, y] = ("main/" ++ x, y)
    f p []      = (intercalate "/" $ reverse p, "main")
    f p [x, y]  = (intercalate "/" $ reverse (x:p), y)
    f p (x:xs)  = f (x:p) xs

    allowed x = x `elem` ("_$" :: String)

    knownOperators :: Map.Map Char String
    knownOperators
      = Map.fromList [ (' ', "space")
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

    jchar x | isAlpha x || isDigit x || allowed x = [x]
            | Just name <- Map.lookup x knownOperators = "$" ++ name ++ "$"
            | otherwise = "_" ++ show (fromEnum x) ++ "_"


newBigInteger :: Integer -> Cg ()
newBigInteger 0 = writeIns [ Field FGetStatic "java/math/BigInteger" "ZERO" "Ljava/math/BigInteger;" ]
newBigInteger 1 = writeIns [ Field FGetStatic "java/math/BigInteger" "ONE" "Ljava/math/BigInteger;" ]
newBigInteger 10 = writeIns [ Field FGetStatic "java/math/BigInteger" "TEN" "Ljava/math/BigInteger;"]
newBigInteger i
  = writeIns [ New "java/math/BigInteger"
             , Dup
             , Ldc $ StringConst (show i)
             , InvokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False
             ]

getPrimitiveClass :: String -> Asm
getPrimitiveClass clazz = Field FGetStatic clazz "TYPE" "Ljava/lang/Class;"
