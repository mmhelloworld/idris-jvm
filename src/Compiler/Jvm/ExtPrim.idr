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
  show (Unknown n) = "Unknown " ++ show n

||| Match on a user given name to get the scheme primitive
export
toPrim : Name -> ExtPrim
toPrim pn@(NS _ n)
    = cond [(n == UN "prim__jvmStatic", JvmStaticMethodCall),
            (n == UN "prim__jvmInstance", JvmInstanceMethodCall),
            (n == UN "prim__newIORef", NewIORef),
            (n == UN "prim__readIORef", ReadIORef),
            (n == UN "prim__writeIORef", WriteIORef),
            (n == UN "prim__newArray", NewArray),
            (n == UN "prim__arrayGet", ArrayGet),
            (n == UN "prim__arraySet", ArraySet),
            (n == UN "prim__getField", GetField),
            (n == UN "prim__setField", SetField),
            (n == UN "void", VoidElim),
            (n == UN "prim__os", SysOS),
            (n == UN "prim__codegen", SysCodegen)
            ]
           (Unknown pn)
toPrim pn = Unknown pn