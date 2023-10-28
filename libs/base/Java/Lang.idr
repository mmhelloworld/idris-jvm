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

public export %inline
classLiteral : {ty: Type} -> Class ty
classLiteral {ty} = prim__jvmClassLiteral ty

public export
data Array : (elemTy: Type) -> Type where
  BoolArray : Array Bool
  ByteArray : Array Int8
  CharArray : Array Char
  ShortArray : Array Int16
  IntArray : Array Int
  LongArray : Array Int64
  DoubleArray : Array Double
  RefArray: (ty: Type) -> Array ty

%extern prim__jvmNewPrimitiveArray : (ty: Type) -> Array ty

%extern prim__jvmNewReferenceArray : (ty: Type) -> Array ty

{-
%foreign "jvm:toIntArray(java/util/ArrayList [int),io/github/mmhelloworld/idrisjvm/runtime/Arrays"
prim_toIntArray: a -> JvmArray

%foreign "jvm:toArray(java/util/ArrayList java/lang/Class [java/lang/Object),io/github/mmhelloworld/idrisjvm/runtime/Arrays"
prim_toArray: Class elemTy -> array -> JvmArray

toArray : (elemTy: Type) -> ArrayData elemTy -> JvmArray elemTy
toArray Int array = prim_toIntArray array
toArray String array = prim_toArray (classLiteral "java/lang/String") array
toArray _ array = prim_toArray (classLiteral "java/lang/Object") array-}
