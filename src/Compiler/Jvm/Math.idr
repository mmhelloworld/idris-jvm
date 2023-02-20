module Compiler.Jvm.Math

import Core.TT
import Compiler.Jvm.Asm

export
longDivideUnsigned : Asm ()
longDivideUnsigned = InvokeMethod InvokeStatic "java/lang/Long" "divideUnsigned" "(JJ)J" False

export
longRemainderUnsigned : Asm ()
longRemainderUnsigned = InvokeMethod InvokeStatic "java/lang/Long" "remainderUnsigned" "(JJ)J" False

export
longCompareUnsigned : Asm ()
longCompareUnsigned = InvokeMethod InvokeStatic "java/lang/Long" "compareUnsigned" "(JJ)I" False

export
integerDivideUnsigned : Asm ()
integerDivideUnsigned = InvokeMethod InvokeStatic "java/lang/Integer" "divideUnsigned" "(II)I" False

export
integerRemainderUnsigned : Asm ()
integerRemainderUnsigned = InvokeMethod InvokeStatic "java/lang/Integer" "remainderUnsigned" "(II)I" False

export
integerCompareUnsigned : Asm ()
integerCompareUnsigned = InvokeMethod InvokeStatic "java/lang/Integer" "compareUnsigned" "(II)I" False

export
add : IntKind -> Asm ()
add (Signed Unlimited) = InvokeMethod InvokeVirtual "java/math/BigInteger" "add"
                           "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
add (Signed (P 64)) = Ladd
add (Signed (P 32)) = Iadd
add (Signed (P n)) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("add" ++ show n)
                      "(II)I" False
add (Unsigned 64) = Ladd
add (Unsigned 32) = Iadd
add (Unsigned n) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("uadd" ++ show n)
                    "(II)I" False

export
sub : IntKind -> Asm ()
sub (Signed Unlimited) = InvokeMethod InvokeVirtual "java/math/BigInteger" "subtract"
                           "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
sub (Signed (P 64)) = Lsub
sub (Signed (P 32)) = Isub
sub (Signed (P n)) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("sub" ++ show n)
                      "(II)I" False
sub (Unsigned 64) = Lsub
sub (Unsigned 32) = Isub
sub (Unsigned n) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("usub" ++ show n)
                      "(II)I" False

export
mul : IntKind -> Asm ()
mul (Signed Unlimited) = InvokeMethod InvokeVirtual "java/math/BigInteger" "multiply"
                           "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
mul (Signed (P 64)) = Lmul
mul (Signed (P 32)) = Imul
mul (Signed (P n)) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("mul" ++ show n)
                      "(II)I" False
mul (Unsigned 64) = Lmul
mul (Unsigned 32) = Imul
mul (Unsigned n) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("umul" ++ show n)
                      "(II)I" False

export
div : IntKind -> Asm ()
div (Signed Unlimited) = InvokeMethod InvokeVirtual "java/math/BigInteger" "divide"
                           "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
div (Signed (P 64)) = Ldiv
div (Signed (P 32)) = Idiv
div (Signed (P n)) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("div" ++ show n)
                       "(II)I" False
div (Unsigned 64) = longDivideUnsigned
div (Unsigned 32) = integerDivideUnsigned
div (Unsigned n) = Idiv

export
mod : IntKind -> Asm ()
mod (Signed Unlimited) = InvokeMethod InvokeVirtual "java/math/BigInteger" "remainder"
                           "(Ljava/math/BigInteger;)Ljava/math/BigInteger;" False
mod (Signed (P 64)) = Lrem
mod (Signed (P n)) = Irem
mod (Unsigned 64) = longRemainderUnsigned
mod (Unsigned 32) = integerRemainderUnsigned
mod (Unsigned n) = Irem

export
shl : IntKind -> Asm ()
shl (Signed Unlimited) = do
  InvokeMethod InvokeVirtual "java/math/BigInteger" "intValueExact" "()I" False
  InvokeMethod InvokeVirtual "java/math/BigInteger" "shiftLeft" "(I)Ljava/math/BigInteger;" False
shl (Signed (P 64)) = do L2i; Lshl
shl (Signed (P 32)) = Ishl
shl (Signed (P n)) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("shl" ++ show n)
                       "(II)I" False
shl (Unsigned 64) = do L2i; Lshl
shl (Unsigned 32) = Ishl
shl (Unsigned n) = InvokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("ushl" ++ show n)
                      "(II)I" False
