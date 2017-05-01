{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Operator where

import           Control.Monad.RWS
import qualified Data.DList                 as DL
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Common
import           IdrisJvm.Codegen.Types
import           IRTS.Lang

compareObj :: MethodName -> LVar -> LVar -> Cg ()
compareObj fn l r
  = writeIns [ Aload $ locIndex l
             , Aload $ locIndex r
             , InvokeMethod InvokeStatic (rtClassSig "Util") fn "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
             ]

loadLocalIntWithCast :: LVar -> Cg ()
loadLocalIntWithCast var
  = writeIns [ Aload $ locIndex var
            , Checkcast "java/lang/Integer"
            , unboxInt ]

loadLocalLongWithCast :: LVar -> Cg ()
loadLocalLongWithCast var
  = writeIns [ Aload $ locIndex var
            , Checkcast "java/lang/Long"
            , unboxLong ]

loadLocalDoubleWithCast :: LVar -> Cg ()
loadLocalDoubleWithCast var
  = writeIns [ Aload $ locIndex var
            , Checkcast "java/lang/Double"
            , unboxDouble ]

binaryIntOp :: DL.DList Asm -> LVar -> LVar -> Cg ()
binaryIntOp ops l r = do
  loadLocalIntWithCast l
  loadLocalIntWithCast r
  writeIns ops
  writeIns [ boxInt ]

binaryLongOp :: DL.DList Asm -> LVar -> LVar -> Cg ()
binaryLongOp ops l r = do
  loadLocalLongWithCast l
  loadLocalLongWithCast r
  writeIns ops
  writeIns [ boxLong ]

binaryDoubleOp :: DL.DList Asm -> LVar -> LVar -> Cg ()
binaryDoubleOp ops l r = do
  loadLocalDoubleWithCast l
  loadLocalDoubleWithCast r
  writeIns ops
  writeIns [ boxDouble ]

cgOp :: PrimFn -> [LVar] -> Cg ()
cgOp (LAnd (ITFixed IT64)) [l, r] = binaryLongOp [Land] l r
cgOp (LAnd (ITFixed _)) [l, r] = binaryIntOp [Iand] l r

cgOp (LLt ITBig) [l, r] = cgOp (LSLt (ATInt ITBig)) [l, r]
cgOp (LLt ITNative) [l, r] = compareObj "uintLessThan" l r
cgOp (LLt (ITFixed IT8)) [l, r] = compareObj "uintLessThan" l r
cgOp (LLt (ITFixed IT16)) [l, r] = compareObj "uintLessThan" l r
cgOp (LLt (ITFixed IT32)) [l, r] = compareObj "uintLessThan" l r
cgOp (LLt (ITFixed IT64)) [l, r] = compareObj "ulongLessThan" l r

cgOp (LLe ITBig) [l, r] = cgOp (LSLe (ATInt ITBig)) [l, r]
cgOp (LLe ITNative) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOp (LLe (ITFixed IT8)) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOp (LLe (ITFixed IT16)) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOp (LLe (ITFixed IT32)) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOp (LLe (ITFixed IT64)) [l, r] = compareObj "ulongLessThanOrEqualTo" l r

cgOp (LGt ITBig) [l, r] = cgOp (LSGt (ATInt ITBig)) [l, r]
cgOp (LGt ITNative) [l, r] = compareObj "uintGreaterThan" l r
cgOp (LGt (ITFixed IT8)) [l, r] = compareObj "uintGreaterThan" l r
cgOp (LGt (ITFixed IT16)) [l, r] = compareObj "uintGreaterThan" l r
cgOp (LGt (ITFixed IT32)) [l, r] = compareObj "uintGreaterThan" l r
cgOp (LGt (ITFixed IT64)) [l, r] = compareObj "ulongGreaterThan" l r

cgOp (LGe ITBig) [l, r] = cgOp (LSGe (ATInt ITBig)) [l, r]
cgOp (LGe ITNative) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT8)) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT16)) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT32)) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT64)) [l, r] = compareObj "ulongGreaterThanOrTo" l r

cgOp (LPlus ATFloat) [l, r] = binaryDoubleOp [Dadd] l r
cgOp (LPlus (ATInt ITBig)) [lvar, rvar]
  = writeIns [ Aload $ locIndex lvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex rvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "add"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]

cgOp (LPlus (ATInt ITNative)) [l, r] = binaryIntOp [Iadd] l r
cgOp (LPlus (ATInt ITChar)) [lvar, rvar]
  = writeIns [ Aload $ locIndex lvar
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , Aload $ locIndex rvar
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , Iadd
             , I2c
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]
cgOp (LPlus (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Ladd] l r
cgOp (LPlus (ATInt (ITFixed IT8))) [l, r] = binaryIntOp [Iadd, Iconst 256, Irem] l r
cgOp (LPlus (ATInt (ITFixed _))) [l, r] = binaryIntOp [Iadd] l r

cgOp (LMinus ATFloat) [l, r] = binaryDoubleOp [Dsub] l r
cgOp (LMinus (ATInt ITBig)) [lvar, rvar]
  = writeIns [ Aload $ locIndex lvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex rvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "subtract"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]
cgOp (LMinus (ATInt ITNative)) [l, r] = binaryIntOp [Isub] l r
cgOp (LMinus (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Lsub] l r
cgOp (LMinus (ATInt (ITFixed _))) [l, r] = binaryIntOp [Isub] l r

cgOp (LTimes ATFloat) [l, r] = binaryDoubleOp [Dmul] l r
cgOp (LTimes (ATInt ITBig)) [lvar, rvar]
  = writeIns [ Aload $ locIndex lvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex rvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "multiply"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]
cgOp (LTimes (ATInt ITNative)) [l, r] = binaryIntOp [Imul] l r
cgOp (LTimes (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Lmul] l r
cgOp (LTimes (ATInt (ITFixed IT8))) [l, r]
 = binaryIntOp [Imul, Iconst 256, Irem] l r
cgOp (LTimes (ATInt (ITFixed _))) [l, r] = binaryIntOp [Imul] l r

cgOp (LSDiv ATFloat) [l, r] = binaryDoubleOp [Ddiv] l r
cgOp (LSDiv (ATInt ITBig)) [lvar, rvar]
  = writeIns [ Aload $ locIndex lvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex rvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "divide"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]

cgOp (LSDiv (ATInt ITNative)) [l, r] = binaryIntOp [Idiv] l r
cgOp (LSDiv (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Ldiv] l r
cgOp (LSDiv (ATInt (ITFixed _))) [l, r] = binaryIntOp [Idiv] l r

cgOp (LUDiv ITNative) [l, r] = binaryIntOp [udiv] l r where
  udiv = InvokeMethod InvokeStatic "java/lang/Integer" "divideUnsigned" "(II)I" False
cgOp (LUDiv (ITFixed IT32)) [l, r] = binaryIntOp [udiv] l r where
  udiv = InvokeMethod InvokeStatic "java/lang/Integer" "divideUnsigned" "(II)I" False
cgOp (LUDiv (ITFixed IT64)) [l, r] = binaryLongOp [udiv] l r where
  udiv = InvokeMethod InvokeStatic "java/lang/Long" "divideUnsigned" "(JJ)J" False
cgOp (LUDiv ITBig) args = cgOp (LSDiv (ATInt ITBig)) args
cgOp (LUDiv (ITFixed _)) [l, r] = binaryIntOp [Idiv] l r

cgOp (LSRem ATFloat) [l, r] = binaryDoubleOp [Drem] l r
cgOp (LSRem (ATInt ITBig)) [lvar, rvar]
  = writeIns [ Aload $ locIndex lvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex rvar
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "mod"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]

cgOp (LSRem (ATInt ITNative)) [l, r] = binaryIntOp [Irem] l r
cgOp (LSRem (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Lrem] l r
cgOp (LSRem (ATInt (ITFixed _))) [l, r] = binaryIntOp [Irem] l r

cgOp (LURem (ITFixed IT64)) [l, r] = binaryLongOp [urem] l r where
  urem = InvokeMethod InvokeStatic "java/lang/Long" "remainderUnsigned" "(JJ)J" False
cgOp (LURem ITNative) [l, r] = binaryIntOp [urem] l r where
  urem = InvokeMethod InvokeStatic "java/lang/Integer" "remainderUnsigned" "(II)I" False
cgOp (LURem (ITFixed IT32)) [l, r] = binaryIntOp [urem] l r where
  urem = InvokeMethod InvokeStatic "java/lang/Integer" "remainderUnsigned" "(II)I" False
cgOp (LURem ITBig) args = cgOp (LSRem (ATInt ITBig)) args
cgOp (LURem (ITFixed _)) [l, r] = binaryIntOp [Irem] l r

cgOp (LEq ATFloat) [l, r] = compareObj "objectEquals" l r
cgOp (LEq (ATInt _)) [l, r] = compareObj "objectEquals" l r

cgOp (LSLt ATFloat) [l, r] = compareObj "doubleLessThan" l r
cgOp (LSLt (ATInt ITNative)) [l, r] = compareObj "intLessThan" l r
cgOp (LSLt (ATInt ITBig)) [l, r] = compareObj "bigIntegerLessThan" l r
cgOp (LSLt (ATInt ITChar)) [l, r] = compareObj "charLessThan" l r
cgOp (LSLt (ATInt (ITFixed IT64))) [l, r] = compareObj "longLessThan" l r
cgOp (LSLt (ATInt (ITFixed _))) [l, r] = compareObj "intLessThan" l r

cgOp (LSLe ATFloat) [l, r] = compareObj "doubleLessThanOrEqualTo" l r
cgOp (LSLe (ATInt ITNative)) [l, r] = compareObj "intLessThanOrEqualTo" l r
cgOp (LSLe (ATInt ITBig)) [l, r] = compareObj "bigIntegerLessThanOrEqualTo" l r
cgOp (LSLe (ATInt ITChar)) [l, r] = compareObj "charLessThanOrEqualTo" l r
cgOp (LSLe (ATInt (ITFixed IT64))) [l, r] = compareObj "longLessThanOrEqualTo" l r
cgOp (LSLe (ATInt (ITFixed _))) [l, r] = compareObj "intLessThanOrEqualTo" l r

cgOp (LSGt ATFloat) [l, r] = compareObj "doubleGreaterThan" l r
cgOp (LSGt (ATInt ITNative)) [l, r] = compareObj "intGreaterThan" l r
cgOp (LSGt (ATInt ITBig)) [l, r] = compareObj "bigIntegerGreaterThan" l r
cgOp (LSGt (ATInt ITChar)) [l, r] = compareObj "charGreaterThan" l r
cgOp (LSGt (ATInt (ITFixed IT64))) [l, r] = compareObj "longGreaterThan" l r
cgOp (LSGt (ATInt (ITFixed _))) [l, r] = compareObj "intGreaterThan" l r

cgOp (LSGe ATFloat) [l, r] = compareObj "doubleGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt ITNative)) [l, r] = compareObj "intGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt ITBig)) [l, r] = compareObj "bigIntegerGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt ITChar)) [l, r] = compareObj "charGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt (ITFixed IT64))) [l, r] = compareObj "longGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt (ITFixed _))) [l, r] = compareObj "intGreaterThanOrEqualTo" l r

cgOp LStrEq [l,r] = compareObj "objectEquals" l r

cgOp LStrRev [var]
  = writeIns [ New "java/lang/StringBuilder"
             , Dup
             , Aload $ locIndex var
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "(Ljava/lang/String;)V" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "reverse" "()Ljava/lang/StringBuilder;" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
             ]

cgOp LStrLen [var]
  = writeIns [ Aload $ locIndex var
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeVirtual "java/lang/String" "length" "()I" False
             , boxInt
             ]

cgOp LStrHead [var]
  = writeIns [ Aload $ locIndex var
             , Checkcast "java/lang/String"
             , Iconst 0
             , InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]

cgOp LStrIndex [string, index]
  = writeIns [ Aload $ locIndex string
             , Checkcast "java/lang/String"
             , Aload $ locIndex index
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]

cgOp LStrTail [var]
  = writeIns [ Aload $ locIndex var
             , Checkcast "java/lang/String"
             , Iconst 1
             , InvokeMethod InvokeVirtual "java/lang/String" "substring" "(I)Ljava/lang/String;" False
             ]

cgOp (LZExt ITNative ITBig) [var]
  = writeIns [ Aload $ locIndex var
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , I2l
             , InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
             ]
cgOp (LZExt (ITFixed IT8) ITNative) [x] = writeIns [ Aload $ locIndex x ]
cgOp (LZExt (ITFixed IT16) ITNative) [x] = writeIns [ Aload $ locIndex x ]
cgOp (LZExt (ITFixed IT32) ITNative) [x] = writeIns [ Aload $ locIndex x ]

cgOp (LIntStr (ITFixed IT32)) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedString" "(I)Ljava/lang/String;" False
             ]

cgOp (LIntStr (ITFixed IT64)) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Long"
             , InvokeMethod InvokeStatic "java/lang/Long" "toUnsignedString" "(J)Ljava/lang/String;" False
             ]

cgOp (LIntStr _) [x]
  = writeIns [ Aload $ locIndex x
             , InvokeMethod InvokeStatic "java/util/Objects" "toString" "(Ljava/lang/Object;)Ljava/lang/String;" False
             ]
cgOp LFloatStr [x]
  = writeIns [ Aload $ locIndex x
             , InvokeMethod InvokeStatic "java/util/Objects" "toString" "(Ljava/lang/Object;)Ljava/lang/String;" False
             ]

cgOp (LChInt ITBig) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , I2l
             , InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf"  "(J)Ljava/math/BigInteger;" False
             ]

cgOp (LChInt _) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False
             ]

cgOp (LIntCh ITBig) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/math/BigInteger"
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "intValue" "()I" False
             , I2c
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]

cgOp (LIntCh _) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , I2c
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]

cgOp (LSExt _ _) [x] = writeIns [ Aload $ locIndex x]

cgOp (LTrunc (ITFixed IT64) (ITFixed IT32)) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Long"
             , InvokeMethod InvokeVirtual "java/lang/Long" "intValue" "()I" False
             , boxInt
             ]

cgOp (LTrunc ITBig ITNative) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/math/BigInteger"
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "intValue" "()I" False
             , boxInt
             ]

cgOp (LTrunc (ITFixed _) (ITFixed _)) [x] = writeIns [ Aload $ locIndex x]

cgOp LWriteStr [_, s]
  = writeIns [ Aload $ locIndex s
             , InvokeMethod InvokeStatic (rtClassSig "Runtime") "writeString" "(Ljava/lang/Object;)Ljava/lang/Integer;" False
             ]

cgOp LReadStr [_]
  = writeIns [ InvokeMethod InvokeStatic (rtClassSig "Runtime") "readString" "()Ljava/lang/String;" False ]

cgOp LStrConcat [l,r]
  = writeIns [ New "java/lang/StringBuilder"
             , Dup
             , InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
             , Aload $ locIndex l
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
             , Aload $ locIndex r
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/Object;)Ljava/lang/StringBuilder;" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
             ]

cgOp LStrCons [l,r]
  = writeIns [ New "java/lang/StringBuilder"
             , Dup
             , InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
             , Aload $ locIndex l
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/Object;)Ljava/lang/StringBuilder;" False
             , Aload $ locIndex r
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
             ]

cgOp (LStrInt ITBig) [x]
  = writeIns [ New "java/math/BigInteger"
             , Dup
             , Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False
             ]

cgOp (LStrInt (ITFixed IT8)) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , Iconst 16
             , InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;I)I" False
             , boxInt
             ]

cgOp (LStrInt _) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
             , boxInt
             ]

cgOp LStrFloat [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeStatic "java/lang/Double" "parseDouble" "(Ljava/lang/String;)D" False
             , boxDouble
             ]

cgOp (LSHL (ITFixed IT64)) [x, y] = binaryLongOp [L2i, Lshl] x y

cgOp (LSHL (ITFixed IT8)) [x, y] = binaryIntOp [Ishl, Iconst 256, Irem] x y

cgOp (LSHL (ITFixed _)) [x, y] = binaryIntOp [Ishl] x y

cgOp (LLSHR (ITFixed IT64)) [x, y] = binaryLongOp [L2i, Lushr] x y

cgOp (LLSHR (ITFixed _)) [x, y] = binaryIntOp [Iushr] x y

cgOp (LASHR (ITFixed IT64)) [x, y] = binaryLongOp [L2i, Lshr] x y

cgOp (LASHR (ITFixed _)) [x, y] = binaryIntOp [Ishr] x y

cgOp LFork [x] = do
  caller <- cgStFunctionName <$> get
  createThunk caller (jname (sMN 0 "EVAL")) [x]
  writeIns [InvokeMethod InvokeStatic (rtClassSig "Concurrent") "fork" "(Lmmhelloworld/idrisjvmruntime/Thunk;)Ljava/lang/Object;" False]

cgOp LPar [x] = do
  caller <- cgStFunctionName <$> get
  createParThunk caller (jname (sMN 0 "EVAL")) [x]
  writeIns [InvokeMethod InvokeStatic (rtClassSig "Concurrent") "par" "(Lmmhelloworld/idrisjvmruntime/Thunk;)Ljava/lang/Object;" False]

cgOp (LExternal externalOp) args = cgExternalOp externalOp args

cgOp op _ = invokeError $ "OPERATOR " ++ show op ++ " NOT IMPLEMENTED!"

cgExternalOp :: Name -> [LVar] -> Cg ()
cgExternalOp op _ | op == sUN "prim__null" = writeIns [ Aconstnull ]
cgExternalOp op _ = invokeError $ "OPERATOR " ++ show op ++ " NOT IMPLEMENTED!"
