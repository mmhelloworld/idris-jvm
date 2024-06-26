||| Managing error codes.
module System.Errno

import System.FFI

%default total

%foreign "C:idris2_getErrno, libidris2_support, idris_support.h"
         jvm' runtimeClass "getErrorNumber" "java/lang/Object" "int"
prim__getErrno : PrimIO Int

%foreign "C:idris2_strerror, libidris2_support, idris_support.h"
         "node:lambda:errno=>'Error code: '+errno"
         jvm' runtimeClass "getErrorMessage" "int" "String"
prim__strerror : Int -> PrimIO String

||| Fetch libc `errno` global variable.
export
getErrno : HasIO io => io Int
getErrno = primIO prim__getErrno

||| Convert numeric `errno` to string.
export
strerror : Int -> String
strerror errno = unsafePerformIO $ primIO $ prim__strerror errno
