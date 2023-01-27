module Compiler.Jvm.ExtPrim

import Core.Context
import Core.Name

||| Extended primitives for the scheme backend, outside the standard set of primFn
public export
data ExtPrim = JvmStaticMethodCall | JvmInstanceMethodCall
             | NewIORef | ReadIORef | WriteIORef
             | NewArray | ArrayGet | ArraySet
             | GetField | SetField
             | VoidElim
             | SysOS | SysCodegen
             | MakeFuture
             | Unknown Name

export
Show ExtPrim where
  show JvmStaticMethodCall = "JvmStaticMethodCall"
  show JvmInstanceMethodCall = "JvmInstanceMethodCall"
  show NewIORef = "NewIORef"
  show ReadIORef = "ReadIORef"
  show WriteIORef = "WriteIORef"
  show NewArray = "NewArray"
  show ArrayGet = "ArrayGet"
  show ArraySet = "ArraySet"
  show GetField = "GetField"
  show SetField = "SetField"
  show VoidElim = "VoidElim"
  show SysOS = "SysOS"
  show SysCodegen = "SysCodegen"
  show MakeFuture = "MakeFuture"
  show (Unknown n) = "Unknown " ++ show n

||| Match on a user given name to get the scheme primitive
export
toPrim : Name -> ExtPrim
toPrim pn@(NS _ n)
    = cond [(n == UN (Basic "prim__jvmStatic"), JvmStaticMethodCall),
            (n == UN (Basic "prim__jvmInstance"), JvmInstanceMethodCall),
            (n == UN (Basic "prim__newIORef"), NewIORef),
            (n == UN (Basic "prim__readIORef"), ReadIORef),
            (n == UN (Basic "prim__writeIORef"), WriteIORef),
            (n == UN (Basic "prim__newArray"), NewArray),
            (n == UN (Basic "prim__arrayGet"), ArrayGet),
            (n == UN (Basic "prim__arraySet"), ArraySet),
            (n == UN (Basic "prim__getField"), GetField),
            (n == UN (Basic "prim__setField"), SetField),
            (n == UN (Basic "void"), VoidElim),
            (n == UN (Basic "prim__void"), VoidElim),
            (n == UN (Basic "prim__os"), SysOS),
            (n == UN (Basic "prim__codegen"), SysCodegen),
            (n == UN (Basic "prim__makeFuture"), MakeFuture)
            ]
           (Unknown pn)
toPrim pn = Unknown pn
