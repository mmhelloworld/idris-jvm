{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Constant where

import           Data.Char                  (ord)
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Types
import           IdrisJvm.Codegen.Common

cgConst :: Const -> Cg ()
cgConst (B8 i) = writeIns [ Iconst $ fromIntegral i, boxInt ]
cgConst (B16 i) = writeIns [ Iconst $ fromIntegral i, boxInt ]
cgConst (B32 i) = writeIns [ Iconst $ fromIntegral i, boxInt ]
cgConst (B64 i) = writeIns [ Ldc $ LongConst i, boxLong ]
cgConst (I i) = writeIns [Iconst i, boxInt]
cgConst (Fl d) = writeIns [Ldc $ DoubleConst d]
cgConst (Ch c) = writeIns [ Iconst (ord c)
                          , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False]
cgConst (BI i) = newBigInteger i
cgConst (Str s) = writeIns [Ldc $ StringConst s]
cgConst TheWorld = writeIns [Iconst 0, boxInt]
cgConst x | isTypeConst x = writeIns [Iconst 0, boxInt]
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"
