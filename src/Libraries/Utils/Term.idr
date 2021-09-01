module Libraries.Utils.Term

%default total

libterm : String -> String
libterm s = "C:" ++ s ++ ", libidris2_support, idris_term.h"

%foreign libterm "idris2_setupTerm"
         "jvm:setup,io/github/mmhelloworld/idrisjvm/runtime/Terminal"
prim__setupTerm : PrimIO ()

%foreign libterm "idris2_getTermCols"
         "jvm:getColumns,io/github/mmhelloworld/idrisjvm/runtime/Terminal"
prim__getTermCols : PrimIO Int

%foreign libterm "idris2_getTermLines"
         "jvm:getRows,io/github/mmhelloworld/idrisjvm/runtime/Terminal"
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
