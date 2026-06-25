||| Thin FFI bridge to `ClasspathReflector` (the ASM-based, classpath-resolving
||| introspector in the assembler library). All parsing/rendering lives in the pure
||| `Compiler.Jvm.FfiGen`; this module only crosses the FFI boundary.
module Compiler.Jvm.Reflection

import public Compiler.Jvm.FfiGen

%foreign "jvm:reflect(java/lang/String java/lang/String java/lang/String),io/github/mmhelloworld/idrisjvm/assembler/ClasspathReflector"
prim_reflect : String -> String -> PrimIO String

||| Reflect a single class (internal name, e.g. "java/util/ArrayList") into a `ClassInfo`, or
||| `Left` with a diagnostic. `classpath` is a path-separator-separated list of jars/dirs the class
||| (and its supertypes / referenced types) is resolved against; "" searches only the compiler's own
||| classpath and the JDK platform modules (JDK classes always resolve; project deps need a classpath).
export
reflectClass : HasIO io => (classpath : String) -> String -> io (Either String ClassInfo)
reflectClass classpath className = do
  dump <- primIO (prim_reflect classpath className)
  pure (parseDump dump)
