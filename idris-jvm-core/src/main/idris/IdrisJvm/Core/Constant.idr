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

cgConst : Const -> Asm ()
cgConst (B8 i) = do
  Iconst $ prim__zextB8_Int i
  boxInt
cgConst (B16 i) = do
  Iconst $ prim__zextB16_Int i
  boxInt
cgConst (B32 i) = do
  Iconst i
  boxInt
cgConst (B64 i) = do
  Ldc $ LongConst i
  boxLong
cgConst (I i) = do Iconst i; boxInt
cgConst (Fl d) = do
  Ldc $ DoubleConst d
  boxDouble
cgConst (Ch c) = do
  Iconst (cast c)
  InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
cgConst (BI i) = newBigInteger i
cgConst (Str s) = Ldc $ StringConst s
cgConst TheWorld = do Iconst 0; boxInt
cgConst x = if isTypeConst x
              then do Iconst 0; boxInt
              else jerror $ "Constant " ++ show x ++ " not compilable yet"

