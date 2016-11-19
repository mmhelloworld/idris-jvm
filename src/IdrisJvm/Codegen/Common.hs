{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Common where

import           Control.Monad.RWS
import           Data.Char                  (isAlpha, isDigit)
import qualified Data.DList                 as DL
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Types
import           IRTS.Lang

createThunkForLambda :: MethodName -> [LVar] -> (MethodName -> DL.DList Asm) -> Cg ()
createThunkForLambda caller args lambdaCode = do
  let nArgs = length args
  cname <- className <$> ask
  lambdaIndex <- freshLambdaIndex
  let lambdaMethodName = sep "$" ["lambda", caller, show lambdaIndex]
  writeIns $ invokeDynamic cname lambdaMethodName
  writeDeps $ lambdaCode lambdaMethodName
  writeIns [Iconst nArgs, Anewarray "java/lang/Object"]
  let argNums = map (\(Loc i) -> i) args
      f :: (Int, Int) -> DL.DList Asm
      f (lhs, rhs) = [Dup, Iconst lhs, Aload rhs, Aastore]
  writeIns . join . fmap f . DL.fromList $ zip [0..] argNums
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Runtime") "thunk" createThunkSig False ]

createThunk :: MethodName -> MethodName -> [LVar] -> Cg ()
createThunk caller fname args = do
  let nArgs = length args
  cname <- className <$> ask
  let lambdaCode lambdaMethodName = createLambda cname fname lambdaMethodName nArgs
  createThunkForLambda caller args lambdaCode

createParThunk :: MethodName -> MethodName -> [LVar] -> Cg ()
createParThunk caller fname args = do
  let nArgs = length args
  cname <- className <$> ask
  let lambdaCode lambdaMethodName = createParLambda cname fname lambdaMethodName nArgs
  createThunkForLambda caller args lambdaCode

invokeDynamic :: ClassName -> MethodName -> DL.DList Asm
invokeDynamic cname lambda = [ InvokeDynamic "apply" ("()" ++ rtFuncSig) metafactoryHandle metafactoryArgs] where
  metafactoryHandle = Handle HInvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory" metafactoryDesc False
  metafactoryArgs = [ BsmArgGetType lambdaDesc
                    , BsmArgHandle lambdaHandle
                    , BsmArgGetType lambdaDesc
                    ]
  lambdaHandle = Handle HInvokeStatic cname lambda lambdaDesc False


createLambda :: ClassName -> MethodName -> MethodName -> Int -> DL.DList Asm
createLambda cname fname lambdaMethodName nArgs
  = DL.fromList [ CreateMethod [Private, Static, Synthetic] lambdaMethodName lambdaDesc Nothing Nothing
                , MethodCodeStart
                ] <>
                join (fmap (\i -> [Aload 0, Iconst i, Aaload]) [0 .. (nArgs - 1)]) <> -- Retrieve lambda args
                [ InvokeMethod InvokeStatic cname fname (sig nArgs) False -- invoke the target method
                , Areturn
                , MaxStackAndLocal (-1) (-1)
                , MethodCodeEnd
                ]

createParLambda :: ClassName -> MethodName -> MethodName -> Int -> DL.DList Asm
createParLambda cname fname lambdaMethodName nArgs
  = DL.fromList [ CreateMethod [Private, Static, Synthetic] lambdaMethodName lambdaDesc Nothing Nothing
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

invokeError :: String -> Cg ()
invokeError x
  = writeIns [ Ldc $ StringConst x
             , InvokeMethod InvokeStatic (rtClassSig "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False
             ]

locIndex :: LVar -> Int
locIndex (Loc i) = i
locIndex _       = error "Unexpected global variable"

jname :: Name -> String
jname n = "idris_" ++ concatMap jchar idrisName
  where
    idrisName = showCG n
    jchar x | isAlpha x || isDigit x = [x]
            | otherwise = "_" ++ show (fromEnum x) ++ "_"


newBigInteger :: Integer -> Cg ()
newBigInteger i
  = writeIns [ New "java/math/BigInteger"
             , Dup
             , Ldc $ StringConst (show i)
             , InvokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False
             ]
