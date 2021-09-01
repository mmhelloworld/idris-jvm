-- Additional FFI help for more interesting C types
-- Some assumptions are made about the existence of this module in
-- Compiler.CompileExpr

module System.FFI

%default total

export
data Struct : String -> -- C struct name
              List (String, Type) -> -- field names and types
              Type where

public export
data FieldType : String -> Type -> List (String, Type) -> Type where
     First : FieldType n t ((n, t) :: ts)
     Later : FieldType n t ts -> FieldType n t (f :: ts)

%extern
prim__getField : {s : _} -> forall fs, ty .
                         Struct s fs -> (n : String) ->
                         FieldType n ty fs -> ty
%extern
prim__setField : {s : _} -> forall fs, ty .
                         Struct s fs -> (n : String) ->
                         FieldType n ty fs -> ty -> PrimIO ()

public export %inline
getField : {s : _} -> Struct s fs -> (n : String) ->
           {auto fieldok : FieldType n ty fs} -> ty
getField s n = prim__getField s n fieldok

public export %inline
setField : {s : _} -> Struct s fs -> (n : String) ->
           {auto fieldok : FieldType n ty fs} -> ty -> IO ()
setField s n val = primIO (prim__setField s n fieldok val)

%foreign "C:idris2_malloc, libidris2_support, idris_memory.h"
         "jvm:malloc(int java/lang/Object),io/github/mmhelloworld/idrisjvm/runtime/IdrisSystem"
prim__malloc : (size : Int) -> PrimIO AnyPtr

%foreign "C:idris2_free, libidris2_support, idris_memory.h"
         jvm' runtimeClass "free" "java/lang/Object" "void"
prim__free : AnyPtr -> PrimIO ()

||| Allocate memory with libc `malloc`.
export
malloc : HasIO io => (size : Int) -> io AnyPtr
malloc size = primIO $ prim__malloc size

||| Release memory with libc `free`.
export
free : HasIO io => AnyPtr -> io ()
free ptr = primIO $ prim__free ptr

public export
jvm' : String -> String -> String -> String -> String
jvm' className methodName arguments ret = "jvm:" ++ methodName ++ "(" ++
    arguments ++ " " ++ ret ++ ")," ++ className

public export
jvm : String -> String -> String
jvm className methodName = "jvm:" ++ methodName ++ "," ++ className

public export
runtimeClass : String
runtimeClass = "io/github/mmhelloworld/idrisjvm/runtime/Runtime"
