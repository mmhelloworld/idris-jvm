module Compiler.Jvm.ExtPrim

import Core.Context
import Core.Name
import Debug.Trace

||| Extended primitives for the scheme backend, outside the standard set of primFn
public export
data ExtPrim = JvmStaticMethodCall | JvmInstanceMethodCall | JvmSuper
             | JavaLambda
             | NewIORef | ReadIORef | WriteIORef
             | NewArray | ArrayGet | ArraySet
             | JvmNewArray | JvmSetArray | JvmGetArray | JvmArrayLength
             | GetStaticField | SetStaticField
             | GetInstanceField | SetInstanceField
             | JvmClassLiteral | JvmInstanceOf | JvmRefEq
             | VoidElim
             | SysOS | SysCodegen
             | MakeFuture
             | Unknown Name

export
Show ExtPrim where
  show JvmStaticMethodCall = "JvmStaticMethodCall"
  show JvmInstanceMethodCall = "JvmInstanceMethodCall"
  show JavaLambda = "JavaLambda"
  show JvmSuper = "JvmSuper"
  show JvmClassLiteral = "JvmClassLiteral"
  show JvmInstanceOf = "JvmInstanceOf"
  show JvmRefEq = "JvmRefEq"
  show NewIORef = "NewIORef"
  show ReadIORef = "ReadIORef"
  show WriteIORef = "WriteIORef"
  show NewArray = "NewArray"
  show ArrayGet = "ArrayGet"
  show ArraySet = "ArraySet"
  show JvmNewArray = "JvmNewArray"
  show JvmSetArray = "JvmSetArray"
  show JvmGetArray = "JvmGetArray"
  show JvmArrayLength = "JvmArrayLength"
  show GetStaticField = "GetStaticField"
  show SetStaticField = "SetStaticField"
  show GetInstanceField = "GetInstanceField"
  show SetInstanceField = "SetInstanceField"
  show VoidElim = "VoidElim"
  show SysOS = "SysOS"
  show SysCodegen = "SysCodegen"
  show MakeFuture = "MakeFuture"
  show (Unknown n) = "Unknown " ++ show n

export
isSuper : Name -> Bool
isSuper (UN (Basic "prim__jvmSuper")) = True
isSuper n = False

||| Match on a user given name to get the scheme primitive
export
toPrim : Name -> ExtPrim
toPrim pn@(NS _ n)
    = cond [(n == UN (Basic "prim__jvmStatic"), JvmStaticMethodCall),
            (n == UN (Basic "prim__jvmInstance"), JvmInstanceMethodCall),
            (isSuper n, JvmSuper),
            (n == UN (Basic "prim__javaLambda"), JavaLambda),
            (n == UN (Basic "prim__jvmClassLiteral"), JvmClassLiteral),
            (n == UN (Basic "prim__jvmInstanceOf"), JvmInstanceOf),
            (n == UN (Basic "prim__jvmRefEq"), JvmRefEq),
            (n == UN (Basic "prim__newIORef"), NewIORef),
            (n == UN (Basic "prim__readIORef"), ReadIORef),
            (n == UN (Basic "prim__writeIORef"), WriteIORef),
            (n == UN (Basic "prim__newArray"), NewArray),
            (n == UN (Basic "prim__arrayGet"), ArrayGet),
            (n == UN (Basic "prim__arraySet"), ArraySet),
            (n == UN (Basic "prim__jvmNewArray"), JvmNewArray),
            (n == UN (Basic "prim__jvmSetArray"), JvmSetArray),
            (n == UN (Basic "prim__jvmGetArray"), JvmGetArray),
            (n == UN (Basic "prim__jvmArrayLength"), JvmArrayLength),
            (n == UN (Basic "prim__getStaticField"), GetStaticField),
            (n == UN (Basic "prim__setStaticField"), SetStaticField),
            (n == UN (Basic "prim__getInstanceField"), GetInstanceField),
            (n == UN (Basic "prim__setInstanceField"), SetInstanceField),
            (n == UN (Basic "void"), VoidElim),
            (n == UN (Basic "prim__void"), VoidElim),
            (n == UN (Basic "prim__os"), SysOS),
            (n == UN (Basic "prim__codegen"), SysCodegen),
            (n == UN (Basic "prim__makeFuture"), MakeFuture)
            ]
           (Unknown pn)
toPrim pn = Unknown pn
