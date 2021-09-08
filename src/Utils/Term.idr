module Utils.Term

import System.FFI

%default total

libterm : String -> String
libterm s = "C:" ++ s ++ ", libidris2_support"

%foreign
    libterm "idris2_setupTerm"
    "jvm:setup(java/lang/Object void),io/github/mmhelloworld/idrisjvm/runtime/Terminal"
prim__setupTerm : PrimIO ()

%foreign
    libterm "idris2_getTermCols"
    "jvm:getColumns(java/lang/Object int),io/github/mmhelloworld/idrisjvm/runtime/Terminal"
prim__getTermCols : PrimIO Int

%foreign
    libterm "idris2_getTermLines"
    "jvm:getRows(java/lang/Object int),io/github/mmhelloworld/idrisjvm/runtime/Terminal"
prim__getTermLines : PrimIO Int

export
setupTerm : IO ()
setupTerm = primIO prim__setupTerm

export
getTermCols : IO Int
getTermCols = primIO prim__getTermCols

export
getTermLines : IO Int
getTermLines = primIO prim__getTermLines
