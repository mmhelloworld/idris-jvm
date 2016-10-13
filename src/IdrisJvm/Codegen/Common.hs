{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Common where

import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Types
import           Idris.Core.TT
import           IRTS.Lang
import           Data.Char (isAlpha, isDigit)
import           Control.Monad.RWS

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
locIndex _ = error "Unexpected global variable"

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
