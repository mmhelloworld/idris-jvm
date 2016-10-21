{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Operator where

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

loadLocalIntWithCast :: LVar -> DL.DList Asm
loadLocalIntWithCast i = [ Aload $ locIndex i
                         , Checkcast "java/lang/Integer"
                         , unboxInt ]

loadLocalLongWithCast :: LVar -> DL.DList Asm
loadLocalLongWithCast i = [ Aload $ locIndex i
                          , Checkcast "java/lang/Long"
                          , unboxLong ]

binaryIntOp :: DL.DList Asm -> LVar -> LVar -> Cg ()
binaryIntOp ops l r = do
  writeIns $ loadLocalIntWithCast l
  writeIns $ loadLocalIntWithCast r
  writeIns ops
  writeIns [ boxInt ]

binaryLongOp :: DL.DList Asm -> LVar -> LVar -> Cg ()
binaryLongOp ops l r = do
  writeIns $ loadLocalLongWithCast l
  writeIns $ loadLocalLongWithCast r
  writeIns ops
  writeIns [ boxLong ]

cgOp :: PrimFn -> [LVar] -> Cg ()
cgOp (LAnd (ITFixed IT64)) [l, r] = binaryLongOp [Land] l r
cgOp (LAnd (ITFixed _)) [l, r] = binaryIntOp [Iand] l r

cgOp (LPlus (ATInt ITBig)) [l, r]
  = writeIns [ Aload $ locIndex l
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex r
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "add"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]

cgOp (LPlus (ATInt ITNative)) [l, r] = binaryIntOp [Iadd] l r
cgOp (LPlus (ATInt ITChar)) [l, r]
  = writeIns [ Aload $ locIndex l
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , Aload $ locIndex r
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , Iadd
             , I2c
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]
cgOp (LPlus (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Ladd] l r
cgOp (LPlus (ATInt (ITFixed IT8))) [l, r] = binaryIntOp [Iadd, Iconst 256, Irem] l r
cgOp (LPlus (ATInt (ITFixed _))) [l, r] = binaryIntOp [Iadd] l r

cgOp (LMinus (ATInt ITBig)) [l, r]
  = writeIns [ Aload $ locIndex l
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex r
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "subtract"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]
cgOp (LMinus (ATInt ITNative)) [l, r] = binaryIntOp [Isub] l r
cgOp (LMinus (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Lsub] l r
cgOp (LMinus (ATInt (ITFixed _))) [l, r] = binaryIntOp [Isub] l r

cgOp (LTimes (ATInt ITBig)) [l, r]
  = writeIns [ Aload $ locIndex l
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex r
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "multiply"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]
cgOp (LTimes (ATInt ITNative)) [l, r] = binaryIntOp [Imul] l r
cgOp (LTimes (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Lmul] l r
cgOp (LTimes (ATInt (ITFixed IT8))) [l, r]
 = binaryIntOp [Imul, Iconst 256, Irem] l r
cgOp (LTimes (ATInt (ITFixed _))) [l, r] = binaryIntOp [Imul] l r

cgOp (LSDiv (ATInt ITBig)) [l, r]
  = writeIns [ Aload $ locIndex l
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex r
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "divide"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]

cgOp (LSDiv (ATInt ITNative)) [l, r] = binaryIntOp [Idiv] l r
cgOp (LSDiv (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Ldiv] l r
cgOp (LSDiv (ATInt (ITFixed _))) [l, r] = binaryIntOp [Idiv] l r

cgOp (LSRem (ATInt ITBig)) [l, r]
  = writeIns [ Aload $ locIndex l
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , Aload $ locIndex r
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
             , InvokeMethod InvokeVirtual "java/math/BigInteger" "mod"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
             ]

cgOp (LSRem (ATInt ITNative)) [l, r] = binaryIntOp [Irem] l r
cgOp (LSRem (ATInt (ITFixed IT64))) [l, r] = binaryLongOp [Lrem] l r
cgOp (LSRem (ATInt (ITFixed _))) [l, r] = binaryIntOp [Irem] l r

cgOp (LEq (ATInt _)) [l, r] = compareObj "objectEquals" l r

cgOp (LSLt (ATInt ITNative)) [l, r] = compareObj "intLessThan" l r
cgOp (LSLt (ATInt ITBig)) [l, r] = compareObj "bigIntegerLessThan" l r
cgOp (LSLt (ATInt ITChar)) [l, r] = compareObj "charLessThan" l r
cgOp (LSLt (ATInt (ITFixed IT64))) [l, r] = compareObj "longLessThan" l r
cgOp (LSLt (ATInt (ITFixed _))) [l, r] = compareObj "intLessThan" l r

cgOp (LSLe (ATInt ITNative)) [l, r] = compareObj "intLessThanOrEqualTo" l r
cgOp (LSLe (ATInt ITBig)) [l, r] = compareObj "bigIntegerLessThanOrEqualTo" l r
cgOp (LSLe (ATInt ITChar)) [l, r] = compareObj "charLessThanOrEqualTo" l r
cgOp (LSLe (ATInt (ITFixed IT64))) [l, r] = compareObj "longLessThanOrEqualTo" l r
cgOp (LSLe (ATInt (ITFixed _))) [l, r] = compareObj "intLessThanOrEqualTo" l r

cgOp (LSGt (ATInt ITNative)) [l, r] = compareObj "intGreaterThan" l r
cgOp (LSGt (ATInt ITBig)) [l, r] = compareObj "bigIntegerGreaterThan" l r
cgOp (LSGt (ATInt ITChar)) [l, r] = compareObj "charGreaterThan" l r
cgOp (LSGt (ATInt (ITFixed IT64))) [l, r] = compareObj "longGreaterThan" l r
cgOp (LSGt (ATInt (ITFixed _))) [l, r] = compareObj "intGreaterThan" l r

cgOp (LSGe (ATInt ITNative)) [l, r] = compareObj "intGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt ITBig)) [l, r] = compareObj "bigIntegerGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt ITChar)) [l, r] = compareObj "charGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt (ITFixed IT64))) [l, r] = compareObj "longGreaterThanOrEqualTo" l r
cgOp (LSGe (ATInt (ITFixed _))) [l, r] = compareObj "intGreaterThanOrEqualTo" l r

cgOp LStrEq [l,r] = compareObj "objectEquals" l r

cgOp LStrRev [x]
  = writeIns [ New "java/lang/StringBuilder"
             , Dup
             , Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "(Ljava/lang/String;)V" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "reverse" "()Ljava/lang/StringBuilder;" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
             ]

cgOp LStrLen [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeVirtual "java/lang/String" "length" "()I" False
             , boxInt
             ]

cgOp LStrHead [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , Iconst 0
             , InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]

cgOp LStrIndex [x, y]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , Aload $ locIndex y
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]

cgOp LStrTail [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , Iconst 1
             , InvokeMethod InvokeVirtual "java/lang/String" "substring" "(I)Ljava/lang/String;" False
             ]

cgOp (LZExt ITNative ITBig) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , I2l
             , InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
             ]
cgOp (LZExt (ITFixed IT8) ITNative) [x] = writeIns [ Aload $ locIndex x ]
cgOp (LZExt (ITFixed IT16) ITNative) [x] = writeIns [ Aload $ locIndex x ]
cgOp (LZExt (ITFixed IT32) ITNative) [x] = writeIns [ Aload $ locIndex x ]

cgOp (LIntStr _) [x]
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
{-
cgOp (LChInt _) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , InvokeMethod InvokeStatic "java/lang/Character" "getNumericValue" "(C)I" False
             , boxInt
             ]
-}

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
cgOp (LTrunc _ _) [x] = writeIns [ Aload $ locIndex x]

cgOp LWriteStr [_, s]
  = writeIns [ Field FGetStatic "java/lang/System" "out" "Ljava/io/PrintStream;"
             , Aload $ locIndex s
             , InvokeMethod InvokeVirtual "java/io/PrintStream" "print" "(Ljava/lang/Object;)V" False
             , Aconstnull
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
  = writeIns [ Aload $ locIndex x
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

cgOp (LSHL (ITFixed IT64)) [x, y] = binaryLongOp [L2i, Lshl] x y

cgOp (LSHL (ITFixed IT8)) [x, y] = binaryIntOp [Ishl, Iconst 256, Irem] x y

cgOp (LSHL (ITFixed _)) [x, y] = binaryIntOp [Ishl] x y

cgOp (LLSHR (ITFixed IT64)) [x, y] = binaryLongOp [L2i, Lushr] x y

cgOp (LLSHR (ITFixed _)) [x, y] = binaryIntOp [Iushr] x y

cgOp (LASHR (ITFixed IT64)) [x, y] = binaryLongOp [L2i, Lshr] x y

cgOp (LASHR (ITFixed _)) [x, y] = binaryIntOp [Ishr] x y

cgOp op _ = invokeError $ "OPERATOR " ++ show op ++ " NOT IMPLEMENTED!"
   -- error("Operator " ++ show op ++ " not implemented")
