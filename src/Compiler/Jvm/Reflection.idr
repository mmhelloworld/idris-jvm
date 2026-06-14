||| Thin FFI bridge to `ClasspathReflector` (the ASM-based, classpath-resolving
||| introspector in the assembler library). All parsing/rendering lives in the pure
||| `Compiler.Jvm.FfiGen`; this module only crosses the FFI boundary.
module Compiler.Jvm.Reflection

import public Compiler.Jvm.FfiGen

%foreign "jvm:reflect(java/lang/String java/lang/String),io/github/mmhelloworld/idrisjvm/assembler/ClasspathReflector"
prim_reflect : String -> PrimIO String

||| Reflect a single class off the compiler's classpath (internal name, e.g.
||| "java/util/ArrayList") into a `ClassInfo`, or `Left` with a diagnostic.
export
reflectClass : HasIO io => String -> io (Either String ClassInfo)
reflectClass className = do
  dump <- primIO (prim_reflect className)
  pure (parseDump dump)
