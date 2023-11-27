module Java.Util

import Java.Lang
import System.FFI

namespace Arrays
    public export
    Arrays : Type
    Arrays = Struct "java/util/Arrays" []

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_boolArrayToString : Array Bool -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_charArrayToString : Array Char -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_byteArrayToString : Array Int8 -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_shortArrayToString : Array Int16 -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_int32ArrayToString : Array Int32 -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_intArrayToString : Array Int -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_int64ArrayToString : Array Int64 -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_doubleArrayToString : Array Double -> PrimIO String

    export
    %foreign "jvm:toString,java/util/Arrays"
    prim_arrayToString : Array Object -> PrimIO String

    public export %inline
    fromList : HasIO io => (a: Type) -> List a -> io (Array a)
    fromList a xs = do
        let len = length xs
        arr <- primIO $ prim__jvmNewArray a (cast len)
        let setter = \index: Int, value: a => prim__jvmSetArray a index value arr
        go setter 0 xs
        pure arr
      where
        go : (Int -> a -> PrimIO ()) -> Int -> List a -> io ()
        go _ _ [] = pure ()
        go setter index (x :: xs) = do
          primIO $ setter index x
          go setter (index + 1) xs

namespace Objects
  %foreign "jvm:hash,java/util/Objects"
  prim_hash : Array Object -> PrimIO Int

  export %inline
  hash : (HasIO io) => Array Object -> io Int
  hash array = primIO $ prim_hash array

namespace Collection
    public export
    Collection : Type -> Type
    Collection elem = Struct "i:java/util/Collection" [("<>", elem)]

    %foreign "jvm:.add(i:java/util/Collection java/lang/Object Bool),java/util/Collection"
    prim_add : Collection a -> a -> PrimIO Bool

    %foreign "jvm:.size(i:java/util/Collection int),java/util/Collection"
    prim_size : Collection a -> PrimIO Int

    export %inline
    add : HasIO io => obj -> elemTy -> (Inherits obj (Collection elemTy)) => io Bool
    add collection elem = primIO $ prim_add (subtyping collection) elem

    export %inline
    size : (HasIO io, Inherits obj (Collection elemTy)) => obj -> io Int
    size {elemTy} collection = primIO $ prim_size {a=elemTy} (subtyping collection)

namespace JList

    public export
    JList : Type -> Type
    JList elem = Struct "i:java/util/List" [("<>", elem)]

    %foreign "jvm:.get(i:java/util/List int java/lang/Object),java/util/List"
    prim_get : JList a -> Int -> PrimIO a

    export %inline
    get : (HasIO io, Inherits list (JList elemTy)) => list -> Int -> io elemTy
    get list index = primIO $ prim_get (subtyping list) index

public export
Inherits (JList a) (Collection a) where

public export
Inherits obj (JList a) => Inherits obj (Collection a) where

namespace ArrayList
    export
    data ArrayList : Type -> Type where [external]

    %foreign "jvm:<init>(java/util/ArrayList),java/util/ArrayList"
    prim_new : PrimIO (ArrayList a)

    export %inline
    new : HasIO io => io (ArrayList elemTy)
    new = primIO prim_new

public export
Inherits (ArrayList a) (JList a) where

namespace Map
    public export
    Map : (key: Type) -> (value: Type) -> Type
    Map key value = Struct "i:java/util/Map" [("<>", key), ("<>", value)]

    %foreign "jvm:.put(i:java/util/Map java/lang/Object java/lang/Object java/lang/Object),java/util/Map"
    prim_put : Map key value -> key -> value -> PrimIO value

    %foreign "jvm:.get(i:java/util/Map java/lang/Object java/lang/Object),java/util/Map"
    prim_get : Map key value -> key -> PrimIO value

    export %inline
    put : (HasIO io, Inherits obj (Map key value)) => obj -> key -> value -> io value
    put map key value = primIO $ prim_put (subtyping map) key value

    export %inline
    get : (HasIO io, Inherits obj (Map key value)) => obj -> key -> io value
    get map key = primIO (prim_get (subtyping map) key)

namespace HashMap
    public export
    HashMap : (key: Type) -> (value: Type) -> Type
    HashMap key value = Struct "java/util/HashMap" [("<>", key), ("<>", value)]

    %foreign "jvm:<init>(java/util/HashMap),java/util/HashMap"
    prim_new : PrimIO (HashMap key value)

    export %inline
    new : HasIO io => io (HashMap key value)
    new = primIO prim_new

public export
Inherits (HashMap key value) (Map key value) where
