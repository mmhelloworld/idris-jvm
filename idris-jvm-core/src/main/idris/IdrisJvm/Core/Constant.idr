module IdrisJvm.Core.Constant

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.IR.Types

import IdrisJvm.Core.Common

%access public export

newBigInteger : String -> Asm ()
newBigInteger "0" = Field FGetStatic "java/math/BigInteger" "ZERO" "Ljava/math/BigInteger;"
newBigInteger "1" = Field FGetStatic "java/math/BigInteger" "ONE" "Ljava/math/BigInteger;"
newBigInteger "10" = Field FGetStatic "java/math/BigInteger" "TEN" "Ljava/math/BigInteger;"
newBigInteger i = do
  New "java/math/BigInteger"
  Dup
  Ldc $ StringConst i
  InvokeMethod InvokeSpecial "java/math/BigInteger" "<init>" "(Ljava/lang/String;)V" False

cgConst : (InferredType -> Asm ()) -> Const -> Asm ()
cgConst ret (B8 i) = do Iconst $ prim__zextB8_Int i; ret IInt
cgConst ret (B16 i) = do Iconst $ prim__zextB16_Int i; ret IInt
cgConst ret (B32 i) = do Iconst i; ret IInt
cgConst ret (B64 i) = do Ldc $ LongConst i; ret ILong
cgConst ret (I i) = do Iconst i; ret IInt
cgConst ret (Fl d) = do Ldc $ DoubleConst d; ret IDouble
cgConst ret (Ch c) = do Iconst (cast c); ret IChar
cgConst ret (BI i) = do newBigInteger i; ret inferredBigIntegerType
cgConst ret (Str s) = do Ldc $ StringConst s; ret inferredStringType
cgConst ret TheWorld = do Iconst 0; ret IInt
cgConst ret x = if isTypeConst x
              then do Iconst 0; ret IInt
              else jerror $ "Constant " ++ show x ++ " not compilable yet"

