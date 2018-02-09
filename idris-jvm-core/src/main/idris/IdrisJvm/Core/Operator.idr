module IdrisJvm.Core.Operator

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.Core.Foreign
import IdrisJvm.IR.Types

%access public export

compareObj : MethodName -> LVar -> LVar -> Asm ()
compareObj fn l r = do
  Aload $ locIndex l
  Aload $ locIndex r
  InvokeMethod InvokeStatic (rtClass "Util") fn "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False

loadLocalIntWithCast : LVar -> Asm ()
loadLocalIntWithCast var = do
  Aload $ locIndex var
  InvokeMethod InvokeStatic (rtClass "Util") "asInt" "(Ljava/lang/Object;)I" False

loadLocalLongWithCast : LVar -> Asm ()
loadLocalLongWithCast var = do
  Aload $ locIndex var
  Checkcast "java/lang/Long"
  unboxLong

loadLocalDoubleWithCast : LVar -> Asm ()
loadLocalDoubleWithCast var = do
  Aload $ locIndex var
  Checkcast "java/lang/Double"
  unboxDouble

binaryIntOp : Asm () -> LVar -> LVar -> Asm ()
binaryIntOp ops l r = do
  loadLocalIntWithCast l
  loadLocalIntWithCast r
  ops
  boxInt

binaryLongOp : Asm () -> LVar -> LVar -> Asm ()
binaryLongOp ops l r = do
  loadLocalLongWithCast l
  loadLocalLongWithCast r
  ops
  boxLong

unaryIntOp : Asm () -> LVar -> Asm ()
unaryIntOp ops x = do
  loadLocalIntWithCast x
  ops
  boxInt

unaryLongOp : Asm () -> LVar -> Asm ()
unaryLongOp ops x = do
  loadLocalLongWithCast x
  ops
  boxLong

binaryDoubleOp : Asm () -> LVar -> LVar -> Asm ()
binaryDoubleOp ops l r = do
  loadLocalDoubleWithCast l
  loadLocalDoubleWithCast r
  ops
  boxDouble

signExtendToBigInteger : LVar -> Asm ()
signExtendToBigInteger var = do
  Aload $ locIndex var
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False

cgExternalOp : String -> List LVar -> Asm ()
cgExternalOp op _ =
  if op == "prim__null"
    then Aconstnull
    else invokeError $ "OPERATOR " ++ show op ++ " NOT IMPLEMENTED!"

cgOpLLe : PrimFn -> List LVar -> Asm ()
cgOpLLe (LLe ITBig) [l, r] = compareObj "bigIntegerLessThanOrEqualTo" l r
cgOpLLe (LLe ITNative) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOpLLe (LLe (ITFixed IT8)) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOpLLe (LLe (ITFixed IT16)) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOpLLe (LLe (ITFixed IT32)) [l, r] = compareObj "uintLessThanOrEqualTo" l r
cgOpLLe (LLe (ITFixed IT64)) [l, r] = compareObj "ulongLessThanOrEqualTo" l r

cgOpLLt : PrimFn -> List LVar -> Asm ()
cgOpLLt (LLt ITBig) [l, r] = compareObj "bigIntegerLessThan" l r
cgOpLLt (LLt ITNative) [l, r] = compareObj "uintLessThan" l r
cgOpLLt (LLt (ITFixed IT8)) [l, r] = compareObj "uintLessThan" l r
cgOpLLt (LLt (ITFixed IT16)) [l, r] = compareObj "uintLessThan" l r
cgOpLLt (LLt (ITFixed IT32)) [l, r] = compareObj "uintLessThan" l r
cgOpLLt (LLt (ITFixed IT64)) [l, r] = compareObj "ulongLessThan" l r

cgOpLGt : PrimFn -> List LVar -> Asm ()
cgOpLGt (LGt ITBig) [l, r] = compareObj "bigIntegerGreaterThan" l r
cgOpLGt (LGt ITNative) [l, r] = compareObj "uintGreaterThan" l r
cgOpLGt (LGt (ITFixed IT8)) [l, r] = compareObj "uintGreaterThan" l r
cgOpLGt (LGt (ITFixed IT16)) [l, r] = compareObj "uintGreaterThan" l r
cgOpLGt (LGt (ITFixed IT32)) [l, r] = compareObj "uintGreaterThan" l r
cgOpLGt (LGt (ITFixed IT64)) [l, r] = compareObj "ulongGreaterThan" l r

cgOpLPlusBig : LVar -> LVar -> Asm ()
cgOpLPlusBig lvar rvar = do
  Aload $ locIndex lvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  Aload $ locIndex rvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  InvokeMethod InvokeVirtual "java/math/BigInteger" "add"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False

cgOpLPlusChar : LVar -> LVar -> Asm ()
cgOpLPlusChar lvar rvar = do
  Aload $ locIndex lvar
  Checkcast "java/lang/Character"
  InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
  Aload $ locIndex rvar
  Checkcast "java/lang/Character"
  InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
  Iadd
  I2c
  InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

cgOpLMinusBig : LVar -> LVar -> Asm ()
cgOpLMinusBig lvar rvar = do
  Aload $ locIndex lvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  Aload $ locIndex rvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  InvokeMethod InvokeVirtual "java/math/BigInteger" "subtract"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False

cgOpLTimesBig : LVar -> LVar -> Asm ()
cgOpLTimesBig lvar rvar = do
  Aload $ locIndex lvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  Aload $ locIndex rvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  InvokeMethod InvokeVirtual "java/math/BigInteger" "multiply"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False

cgOpLSDivBig : LVar -> LVar -> Asm ()
cgOpLSDivBig lvar rvar = do
  Aload $ locIndex lvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  Aload $ locIndex rvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  InvokeMethod InvokeVirtual "java/math/BigInteger" "divide"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False

cgOpLSRemBig : LVar -> LVar -> Asm ()
cgOpLSRemBig lvar rvar = do
  Aload $ locIndex lvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  Aload $ locIndex rvar
  InvokeMethod InvokeStatic (rtClass "Util") "asBigInt" "(Ljava/lang/Object;)Ljava/math/BigInteger;" False
  InvokeMethod InvokeVirtual "java/math/BigInteger" "mod"  "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False

cgOpLStrRev : LVar -> Asm ()
cgOpLStrRev var = do
  New "java/lang/StringBuilder"
  Dup
  Aload $ locIndex var
  Checkcast "java/lang/String"
  InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "(Ljava/lang/String;)V" False
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "reverse" "()Ljava/lang/StringBuilder;" False
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False

cgOpLStrLen : LVar -> Asm ()
cgOpLStrLen var = do
  Aload $ locIndex var
  Checkcast "java/lang/String"
  InvokeMethod InvokeVirtual "java/lang/String" "length" "()I" False
  boxInt

cgOpLStrHead : LVar -> Asm ()
cgOpLStrHead var = do
  Aload $ locIndex var
  Checkcast "java/lang/String"
  Iconst 0
  InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
  InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

cgOpLStrIndex : LVar -> LVar -> Asm ()
cgOpLStrIndex string index = do
  Aload $ locIndex string
  Checkcast "java/lang/String"
  Aload $ locIndex index
  Checkcast "java/lang/Integer"
  InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
  InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
  InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

cgOpLStrTail : LVar -> Asm ()
cgOpLStrTail var = do
  Aload $ locIndex var
  Checkcast "java/lang/String"
  Iconst 1
  InvokeMethod InvokeVirtual "java/lang/String" "substring" "(I)Ljava/lang/String;" False

cgOpLStrConcat : LVar -> LVar -> Asm ()
cgOpLStrConcat l r = do
  New "java/lang/StringBuilder"
  Dup
  InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
  Aload $ locIndex l
  Checkcast "java/lang/String"
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
  Aload $ locIndex r
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/Object;)Ljava/lang/StringBuilder;" False
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False

cgOpLStrCons : LVar -> LVar -> Asm ()
cgOpLStrCons l r = do
  New "java/lang/StringBuilder"
  Dup
  InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
  Aload $ locIndex l
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/Object;)Ljava/lang/StringBuilder;" False
  Aload $ locIndex r
  Checkcast "java/lang/String"
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
  InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False

cgOpLStrSubstr : LVar -> LVar -> LVar -> Asm ()
cgOpLStrSubstr offset len str = do
  Aload $ locIndex str
  Checkcast "java/lang/String"
  Aload $ locIndex offset
  Checkcast "java/lang/Integer"
  unboxInt
  Aload $ locIndex offset
  Checkcast "java/lang/Integer"
  unboxInt
  Aload $ locIndex len
  Checkcast "java/lang/Integer"
  unboxInt
  Iadd
  InvokeMethod InvokeVirtual "java/lang/String" "substring" "(II)Ljava/lang/String;" False

cgOpNotImplemented : PrimFn -> Asm ()
cgOpNotImplemented op = invokeError $ "OPERATOR " ++ show op ++ " NOT IMPLEMENTED!"

-- cgOp split into two to avoid JVM's "Method code too large" error.
cgOp2 : PrimFn -> List LVar -> Asm ()
cgOp2 (LIntStr (ITFixed IT32)) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Integer"
  InvokeMethod InvokeStatic "java/lang/Integer" "toUnsignedString" "(I)Ljava/lang/String;" False

cgOp2 (LIntStr (ITFixed IT64)) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Long"
  InvokeMethod InvokeStatic "java/lang/Long" "toUnsignedString" "(J)Ljava/lang/String;" False

cgOp2 (LIntStr _) [x] = do
  Aload $ locIndex x
  InvokeMethod InvokeStatic "java/util/Objects" "toString" "(Ljava/lang/Object;)Ljava/lang/String;" False

cgOp2 LFloatStr [x] = do
  Aload $ locIndex x
  InvokeMethod InvokeStatic "java/util/Objects" "toString" "(Ljava/lang/Object;)Ljava/lang/String;" False

cgOp2 (LChInt ITBig) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Character"
  InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
  I2l
  InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf"  "(J)Ljava/math/BigInteger;" False

cgOp2 (LChInt _) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Character"
  InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
  InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False

cgOp2 (LIntCh ITBig) [x] = do
  Aload $ locIndex x
  Checkcast "java/math/BigInteger"
  InvokeMethod InvokeVirtual "java/math/BigInteger" "intValue" "()I" False
  I2c
  InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

cgOp2 (LIntCh _) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Integer"
  InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
  I2c
  InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False

cgOp2 (LSExt ITNative ITBig) [x] = signExtendToBigInteger x

cgOp2 (LSExt (ITFixed from) ITBig) [x] = signExtendToBigInteger x

cgOp2 (LSExt _ _) [x] = Aload $ locIndex x

cgOp2 (LTrunc ITNative (ITFixed IT64)) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Integer"
  InvokeMethod InvokeVirtual "java/lang/Integer" "longValue" "()J" False
  boxLong

cgOp2 (LTrunc (ITFixed IT64) (ITFixed IT32)) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Long"
  InvokeMethod InvokeVirtual "java/lang/Long" "intValue" "()I" False
  boxInt

cgOp2 (LTrunc ITBig ITNative) [x] = do
  Aload $ locIndex x
  Checkcast "java/math/BigInteger"
  InvokeMethod InvokeVirtual "java/math/BigInteger" "intValue" "()I" False
  boxInt

cgOp2 (LTrunc (ITFixed _) (ITFixed _)) [x] = Aload $ locIndex x

cgOp2 LWriteStr [_, s] = do
  Aload $ locIndex s
  InvokeMethod InvokeStatic (rtClass "Runtime") "writeString" "(Ljava/lang/Object;)Ljava/lang/Integer;" False

cgOp2 LReadStr [_] = InvokeMethod InvokeStatic (rtClass "Runtime") "readString" "()Ljava/lang/String;" False

cgOp2 LStrConcat [l,r] = cgOpLStrConcat l r

cgOp2 LStrCons [l,r] = cgOpLStrCons l r

cgOp2 LStrSubstr [offset, len, str] = cgOpLStrSubstr offset len str

cgOp2 (LStrInt ITBig) [x] = do
  New "java/math/BigInteger"
  Dup
  Aload $ locIndex x
  Checkcast "java/lang/String"
  InvokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False

cgOp2 (LStrInt (ITFixed IT8)) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/String"
  Iconst 16
  InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;I)I" False
  boxInt

cgOp2 (LStrInt _) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/String"
  InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
  boxInt

cgOp2 LStrFloat [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/String"
  InvokeMethod InvokeStatic "java/lang/Double" "parseDouble" "(Ljava/lang/String;)D" False
  boxDouble

cgOp2 (LSHL (ITFixed IT64)) [x, y] = binaryLongOp ops x y where
  ops = do L2i; Lshl

cgOp2 (LSHL (ITFixed IT8)) [x, y] = binaryIntOp ops x y where
  ops = do Ishl; Iconst 256; Irem

cgOp2 (LSHL (ITFixed _)) [x, y] = binaryIntOp Ishl x y

cgOp2 (LLSHR (ITFixed IT64)) [x, y] = binaryLongOp ops x y where
  ops = do L2i; Lushr

cgOp2 (LLSHR (ITFixed _)) [x, y] = binaryIntOp Iushr x y

cgOp2 (LASHR (ITFixed IT64)) [x, y] = binaryLongOp ops x y where
  ops = do L2i; Lshr

cgOp2 (LASHR (ITFixed _)) [x, y] = binaryIntOp Ishr x y

cgOp2 LFork [x] = do
  caller <- GetFunctionName
  createThunk caller (jname "{EVAL_0}") [x]
  InvokeMethod InvokeStatic (rtClass "Concurrent") "fork" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False

cgOp2 LPar [x] = do
  caller <- GetFunctionName
  createParThunk caller (jname "{EVAL_0}") [x]
  InvokeMethod InvokeStatic (rtClass "Concurrent") "par" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False

cgOp2 (LIntFloat ITBig) [x] = do
  Aload $ locIndex x
  Checkcast "java/math/BigInteger"
  InvokeMethod InvokeVirtual "java/math/BigInteger" "doubleValue" "()D" False
  boxDouble

cgOp2 (LIntFloat ITNative) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Integer"
  InvokeMethod InvokeVirtual "java/lang/Integer" "doubleValue" "()D" False
  boxDouble

cgOp2 (LFloatInt ITBig) [x] = do
  New "java/math/BigDecimal"
  Dup
  Aload $ locIndex x
  Checkcast "java/lang/Double"
  unboxDouble
  InvokeMethod InvokeSpecial "java/math/BigDecimal" "<init>" "(D)V" False
  InvokeMethod InvokeVirtual "java/math/BigDecimal" "toBigInteger" "()Ljava/math/BigInteger;" False

cgOp2 (LFloatInt ITNative) [x] = do
  Aload $ locIndex x
  Checkcast "java/lang/Double"
  InvokeMethod InvokeVirtual "java/lang/Double" "intValue" "()I" False
  boxInt

cgOp2 (LExternal externalOp) args = cgExternalOp externalOp args

cgOp2 op _ = cgOpNotImplemented op

cgOp : PrimFn -> List LVar -> Asm ()
cgOp (LAnd (ITFixed IT64)) [l, r] = binaryLongOp Land l r
cgOp (LAnd (ITFixed _)) [l, r] = binaryIntOp Iand l r
cgOp (LAnd ITNative) [l, r] = binaryIntOp Iand l r

cgOp (LOr (ITFixed IT64)) [l, r] = binaryLongOp Lor l r
cgOp (LOr (ITFixed _)) [l, r] = binaryIntOp Ior l r
cgOp (LOr ITNative) [l, r] = binaryIntOp Ior l r

cgOp (LXOr (ITFixed IT64)) [l, r] = binaryLongOp Lxor l r
cgOp (LXOr (ITFixed _)) [l, r] = binaryIntOp Ixor l r
cgOp (LXOr ITNative) [l, r] = binaryIntOp Ixor l r

cgOp (LCompl (ITFixed IT64)) [x] = unaryLongOp Lcompl x
cgOp (LCompl (ITFixed _)) [x] = unaryIntOp Icompl x
cgOp (LCompl ITNative) [x] = unaryIntOp Icompl x

cgOp llt@(LLt _) args = cgOpLLt llt args

cgOp lle@(LLe _) args = cgOpLLe lle args

cgOp lgt@(LGt _) args = cgOpLGt lgt args

cgOp (LGe ITBig) [l, r] = cgOp (LSGe (ATInt ITBig)) [l, r]
cgOp (LGe ITNative) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT8)) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT16)) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT32)) [l, r] = compareObj "uintGreaterThanOrEqualTo" l r
cgOp (LGe (ITFixed IT64)) [l, r] = compareObj "ulongGreaterThanOrTo" l r

cgOp (LPlus ATFloat) [l, r] = binaryDoubleOp Dadd l r
cgOp (LPlus (ATInt ITBig)) [lvar, rvar] = cgOpLPlusBig lvar rvar

cgOp (LPlus (ATInt ITNative)) [l, r] = binaryIntOp Iadd l r
cgOp (LPlus (ATInt ITChar)) [lvar, rvar] = cgOpLPlusChar lvar rvar

cgOp (LPlus (ATInt (ITFixed IT64))) [l, r] = binaryLongOp Ladd l r
cgOp (LPlus (ATInt (ITFixed IT8))) [l, r] = binaryIntOp ops l r where
  ops = do Iadd; Iconst 256; Irem
cgOp (LPlus (ATInt (ITFixed _))) [l, r] = binaryIntOp Iadd l r

cgOp (LMinus ATFloat) [l, r] = binaryDoubleOp Dsub l r
cgOp (LMinus (ATInt ITBig)) [lvar, rvar] = cgOpLMinusBig lvar rvar

cgOp (LMinus (ATInt ITNative)) [l, r] = binaryIntOp Isub l r
cgOp (LMinus (ATInt (ITFixed IT64))) [l, r] = binaryLongOp Lsub l r
cgOp (LMinus (ATInt (ITFixed _))) [l, r] = binaryIntOp Isub l r

cgOp (LTimes ATFloat) [l, r] = binaryDoubleOp Dmul l r
cgOp (LTimes (ATInt ITBig)) [lvar, rvar] = cgOpLTimesBig lvar rvar

cgOp (LTimes (ATInt ITNative)) [l, r] = binaryIntOp Imul l r
cgOp (LTimes (ATInt (ITFixed IT64))) [l, r] = binaryLongOp Lmul l r
cgOp (LTimes (ATInt (ITFixed IT8))) [l, r] = binaryIntOp ops l r where
  ops = do Imul; Iconst 256; Irem
cgOp (LTimes (ATInt (ITFixed _))) [l, r] = binaryIntOp Imul l r

cgOp (LSDiv ATFloat) [l, r] = binaryDoubleOp Ddiv l r
cgOp (LSDiv (ATInt ITBig)) [lvar, rvar] = cgOpLSDivBig lvar rvar

cgOp (LSDiv (ATInt ITNative)) [l, r] = binaryIntOp Idiv l r
cgOp (LSDiv (ATInt (ITFixed IT64))) [l, r] = binaryLongOp Ldiv l r
cgOp (LSDiv (ATInt (ITFixed _))) [l, r] = binaryIntOp Idiv l r

cgOp (LUDiv ITNative) [l, r] = binaryIntOp udiv l r where
  udiv = InvokeMethod InvokeStatic "java/lang/Integer" "divideUnsigned" "(II)I" False
cgOp (LUDiv (ITFixed IT32)) [l, r] = binaryIntOp udiv l r where
  udiv = InvokeMethod InvokeStatic "java/lang/Integer" "divideUnsigned" "(II)I" False
cgOp (LUDiv (ITFixed IT64)) [l, r] = binaryLongOp udiv l r where
  udiv = InvokeMethod InvokeStatic "java/lang/Long" "divideUnsigned" "(JJ)J" False
cgOp (LUDiv ITBig) args = cgOp (LSDiv (ATInt ITBig)) args
cgOp (LUDiv (ITFixed _)) [l, r] = binaryIntOp Idiv l r

cgOp (LSRem ATFloat) [l, r] = binaryDoubleOp Drem l r
cgOp (LSRem (ATInt ITBig)) [lvar, rvar] = cgOpLSRemBig lvar rvar

cgOp (LSRem (ATInt ITNative)) [l, r] = binaryIntOp Irem l r
cgOp (LSRem (ATInt (ITFixed IT64))) [l, r] = binaryLongOp Lrem l r
cgOp (LSRem (ATInt (ITFixed _))) [l, r] = binaryIntOp Irem l r

cgOp (LURem (ITFixed IT64)) [l, r] = binaryLongOp urem l r where
  urem = InvokeMethod InvokeStatic "java/lang/Long" "remainderUnsigned" "(JJ)J" False
cgOp (LURem ITNative) [l, r] = binaryIntOp urem l r where
  urem = InvokeMethod InvokeStatic "java/lang/Integer" "remainderUnsigned" "(II)I" False
cgOp (LURem (ITFixed IT32)) [l, r] = binaryIntOp urem l r where
  urem = InvokeMethod InvokeStatic "java/lang/Integer" "remainderUnsigned" "(II)I" False
cgOp (LURem ITBig) args = cgOp (LSRem (ATInt ITBig)) args
cgOp (LURem (ITFixed _)) [l, r] = binaryIntOp Irem l r

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

cgOp LStrRev [var] = cgOpLStrRev var

cgOp LStrLen [var] = cgOpLStrLen var

cgOp LStrLt [l, r] = compareObj "stringLessThan" l r

cgOp LStrHead [var] = cgOpLStrHead var

cgOp LStrIndex [string, index] = cgOpLStrIndex string index

cgOp LStrTail [var] = cgOpLStrTail var

cgOp (LZExt ITNative ITBig) [var] = do
  Aload $ locIndex var
  Checkcast "java/lang/Integer"
  InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
  I2l
  InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False

cgOp (LZExt (ITFixed IT8) ITNative) [x] = Aload $ locIndex x
cgOp (LZExt (ITFixed IT16) ITNative) [x] = Aload $ locIndex x
cgOp (LZExt (ITFixed IT32) ITNative) [x] = Aload $ locIndex x

cgOp op args = cgOp2 op args
