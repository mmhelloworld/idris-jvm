module Compiler.Jvm.Math

import Core.TT
import Core.Context
import Core.Core
import Compiler.Jvm.Asm

export
longDivideUnsigned : {auto stateRef: Ref AsmState AsmState} -> Core ()
longDivideUnsigned = invokeMethod InvokeStatic "java/lang/Long" "divideUnsigned" "(JJ)J" False

export
longRemainderUnsigned : {auto stateRef: Ref AsmState AsmState} -> Core ()
longRemainderUnsigned = invokeMethod InvokeStatic "java/lang/Long" "remainderUnsigned" "(JJ)J" False

export
longCompareUnsigned : {auto stateRef: Ref AsmState AsmState} -> Core ()
longCompareUnsigned = invokeMethod InvokeStatic "java/lang/Long" "compareUnsigned" "(JJ)I" False

export
integerDivideUnsigned : {auto stateRef: Ref AsmState AsmState} -> Core ()
integerDivideUnsigned = invokeMethod InvokeStatic "java/lang/Integer" "divideUnsigned" "(II)I" False

export
integerRemainderUnsigned : {auto stateRef: Ref AsmState AsmState} -> Core ()
integerRemainderUnsigned = invokeMethod InvokeStatic "java/lang/Integer" "remainderUnsigned" "(II)I" False

export
integerCompareUnsigned : {auto stateRef: Ref AsmState AsmState} -> Core ()
integerCompareUnsigned = invokeMethod InvokeStatic "java/lang/Integer" "compareUnsigned" "(II)I" False

export
add : {auto stateRef: Ref AsmState AsmState} -> IntKind -> Core ()
add (Signed Unlimited) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisInteger" "add"
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
add (Signed (P 64)) = ladd
add (Signed (P 32)) = iadd
add (Signed (P n)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("add" ++ show n)
                      "(II)I" False
add (Unsigned 64) = ladd
add (Unsigned 32) = iadd
add (Unsigned n) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("uadd" ++ show n)
                    "(II)I" False

export
sub : {auto stateRef: Ref AsmState AsmState} -> IntKind -> Core ()
sub (Signed Unlimited) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisInteger" "subtract"
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
sub (Signed (P 64)) = lsub
sub (Signed (P 32)) = isub
sub (Signed (P n)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("sub" ++ show n)
                      "(II)I" False
sub (Unsigned 64) = lsub
sub (Unsigned 32) = isub
sub (Unsigned n) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("usub" ++ show n)
                      "(II)I" False

export
mul : {auto stateRef: Ref AsmState AsmState} -> IntKind -> Core ()
mul (Signed Unlimited) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisInteger" "multiply"
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
mul (Signed (P 64)) = lmul
mul (Signed (P 32)) = imul
mul (Signed (P n)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("mul" ++ show n)
                      "(II)I" False
mul (Unsigned 64) = lmul
mul (Unsigned 32) = imul
mul (Unsigned n) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("umul" ++ show n)
                      "(II)I" False

export
div : {auto stateRef: Ref AsmState AsmState} -> IntKind -> Core ()
div (Signed Unlimited) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisInteger" "euclidDiv"
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
div (Signed (P 64)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" "euclidDiv"
                        "(JJ)J" False
div (Signed (P 32)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" "euclidDiv"
                        "(II)I" False
div (Signed (P n)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("div" ++ show n)
                       "(II)I" False
div (Unsigned 64) = longDivideUnsigned
div (Unsigned 32) = integerDivideUnsigned
div (Unsigned n) = idiv

export
mod : {auto stateRef: Ref AsmState AsmState} -> IntKind -> Core ()
mod (Signed Unlimited) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisInteger" "euclidMod"
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
mod (Signed (P 64)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" "euclidMod"
                        "(JJ)J" False
mod (Signed (P n)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" "euclidMod"
                       "(II)I" False
mod (Unsigned 64) = longRemainderUnsigned
mod (Unsigned 32) = integerRemainderUnsigned
mod (Unsigned n) = irem

export
shl : {auto stateRef: Ref AsmState AsmState} -> IntKind -> Core ()
shl (Signed Unlimited) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisInteger" "shiftLeft"
                           "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
shl (Signed (P 64)) = do l2i; lshl
shl (Signed (P 32)) = ishl
shl (Signed (P n)) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("shl" ++ show n)
                       "(II)I" False
shl (Unsigned 64) = do l2i; lshl
shl (Unsigned 32) = ishl
shl (Unsigned n) = invokeMethod InvokeStatic "io/github/mmhelloworld/idrisjvm/runtime/IdrisMath" ("ushl" ++ show n)
                      "(II)I" False
