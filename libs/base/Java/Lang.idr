module Java.Lang

import System.FFI

public export
interface Inherits child parent where
    constructor MkInherits

    export %inline
    subtyping : child -> parent
    subtyping = believe_me

public export
Inherits a a where

namespace Object
    public export
    Object : Type
    Object = Struct "java/lang/Object" []

    %foreign "jvm:.toString(java/lang/Object java/lang/String),java/lang/Object"
    prim_toString : Object -> PrimIO String

    %foreign "jvm:<init>,java/lang/Object"
    prim_new : PrimIO Object

    export %inline
    new : HasIO io => io Object
    new = primIO prim_new

    export %inline
    toString : (HasIO io, Inherits a Object) => a -> io String
    toString obj = primIO $ prim_toString (subtyping obj)

public export
Inherits a Object where

namespace Class
  public export
  Class : Type -> Type
  Class ty = Struct "java/lang/Class" [("<>", ty)]

%extern prim__jvmClassLiteral : (ty: Type) -> Class ty

public export
%extern prim__jvmInstanceOf : a -> (ty: Type) -> Bool

%extern prim__jvmRefEq : a -> b -> Bool

public export %inline
classLiteral : {ty: Type} -> Class ty
classLiteral {ty} = prim__jvmClassLiteral ty

public export %inline
jvmInstanceOf : a -> (ty: Type) -> Bool
jvmInstanceOf = prim__jvmInstanceOf

public export
jvmRefEq : a -> b -> Bool
jvmRefEq = prim__jvmRefEq

export
%extern prim__javaLambda : (lambdaTy : Type) -> (intfTy : Type) -> (f: lambdaTy) -> intfTy

public export
%inline
jlambda : {fTy: Type} -> (f: fTy) -> {intfTy: Type} -> intfTy
jlambda {fTy} f {intfTy} = prim__javaLambda fTy intfTy f

public export
data FArgList : Type where
     Nil : FArgList
     (::) : {a : Type} -> (1 arg : a) -> (1 args : FArgList) -> FArgList

export
%extern prim__jvmSuper : Type -> (1 args : FArgList) -> PrimIO ()

export %inline
super : Type -> (1 args : FArgList) -> IO ()
super clazz args = fromPrim (prim__jvmSuper clazz args)

namespace JInteger
  public export
  JInteger : Type
  JInteger = Struct "java/lang/Integer" []

  export
  %foreign "jvm:valueOf,java/lang/Integer"
  box : Int -> JInteger

  export
  %foreign "jvm:.intValue"
  unbox : JInteger -> Int

public export
data Array : (elemTy: Type) -> Type where

export
%extern prim__jvmNewArray : (ty: Type) -> Int -> PrimIO (Array ty)

export
%extern prim__jvmSetArray : (a: Type) -> Int -> a -> Array a -> PrimIO ()

export
%extern prim__jvmGetArray : (a: Type) -> Int -> Array a -> PrimIO a

export
%extern prim__jvmArrayLength : (a: Type) -> Array a -> Int

isPrimitive : Type -> Bool
isPrimitive Bool = True
isPrimitive Char = True
isPrimitive Int8 = True
isPrimitive Int16 = True
isPrimitive Int32 = True
isPrimitive Int = True
isPrimitive Int64 = True
isPrimitive Bits8 = True
isPrimitive Bits16 = True
isPrimitive Bits32 = True
isPrimitive Bits64 = True
isPrimitive Double = True
isPrimitive _ = False

public export
%foreign "jvm:nullValue(java/lang/Object),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
nullValue : a

public export
%foreign jvm' "java/util/Objects" "isNull" "java/lang/Object" "boolean"
isNull : Object -> Bool

namespace Long

  public export
  Long : Type
  Long = Struct "java/lang/Long" []

  %foreign "jvm:valueOf,java/lang/Long"
  export
  valueOf : Int64 -> Long

namespace Array
    %inline
    public export
    new : HasIO io => {elem: Type} -> Int -> io (Array elem)
    new {elem} size = primIO $ prim__jvmNewArray elem size

    %inline
    public export
    set : HasIO io => {elem: Type} -> Array elem -> Int -> elem -> io Bool
    set {elem} array index value = do
        let len = prim__jvmArrayLength elem array
        if index >= 0 && index < len
            then do
                    primIO $ prim__jvmSetArray elem index value array
                    pure True
            else pure False

    %inline
    public export
    get : HasIO io => {elem: Type} -> Array elem -> Int -> io (Maybe elem)
    get {elem} array index = do
     let len = prim__jvmArrayLength elem array
     if index >= 0 && index < len
        then do
            value <- primIO $ prim__jvmGetArray elem index array
            if isPrimitive elem
                then pure (Just value)
                else
                    if isNull (believe_me value)
                        then pure Nothing
                        else pure (Just value)
        else pure Nothing

public export
Runnable : Type
Runnable = (Struct "java/lang/Runnable run" [], PrimIO ())

namespace Thread

    public export %inline
    Thread : Type
    Thread = Struct "java/lang/Thread" []

    %foreign "jvm:<init>"
    prim_new : Runnable -> PrimIO Thread

    %foreign "jvm:.start"
    prim_start : Thread -> PrimIO ()

    %foreign "jvm:.join"
    prim_join : Thread -> PrimIO ()

    export
    new : HasIO io => PrimIO () -> io Thread
    new runnable = primIO $ prim_new (jlambda runnable)

    export
    start : HasIO io => Thread -> io ()
    start = primIO . prim_start

    export
    join : HasIO io => Thread -> io ()
    join = primIO . prim_join
