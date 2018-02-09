# Compiling Idris to class files without a main module

   Usually Idris requires a main module for code generation, in this case, for generating class files,
   or an Idris program can be compiled to an Idris package which doesn't involve any code generation which can then be
   used in an Idris program with a main module.

   To compile Idris files without a main module into class files, we need to provide `export` declarations and then use `--interface` option for the compiler.
   The `interface` option tells the compiler to use export declarations as the root instead of a `main` function.

   **Example**

   Foo.idr:

   ```idris
   module Foo.Foo

   import IdrisJvm.FFI
   import IdrisJvm.IO
   import Java.Util
   
   %access public export
   
   FooThread : Type
   FooThread = javaClass "foo/FooThread"
   
   setName : FooThread -> String -> JVM_IO ()
   setName this name = invokeInstance "setName" (FooThread -> String -> JVM_IO ()) this name
   
   getThreadName : FooThread -> JVM_IO String
   getThreadName = invokeInstance "getThreadName" (FooThread -> JVM_IO String)
   
   run : FooThread -> JVM_IO ()
   run this = do
     printLn "Hello from Idris"
     setName this "foo-thread"
     printLn !(getThreadName this)
   
   exports : FFI_Export FFI_JVM "Foo/FooThread extends java/lang/Thread implements java/lang/Runnable" []
   exports =
     Fun run ExportDefault $
     Fun getThreadName (Super "getName") $
     End

   ```

   Bar.idr:
   ```idris
   module Foo.Bar

   import IdrisJvm.FFI
   import IdrisJvm.IO
   import Java.Lang

   %access public export

   pythag : Int -> List (Int, Int, Int)
   pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                             x * x + y * y == z * z]
   jpythag : Int -> String
   jpythag n = show $ pythag n

   helloIdris : String -> JVM_IO ()
   helloIdris name = printLn name

   jmain : StringArray -> JVM_IO ()
   jmain args = do
     printLn $ jpythag 50
     helloIdris "Bar"

   exports : FFI_Export FFI_JVM "Foo/JBar" []
   exports =
     Fun jpythag (ExportStatic "pythag") $
     Fun helloIdris (ExportStatic "helloIdris") $
     Fun jmain (ExportStatic "main") $
     End

   ```

   These files can be compiled with `idris --interface --portable-codegen jvm -p idrisjvmffi Foo/Foo.idr Foo/Bar.idr -o target`.

   This would result in `Foo/FooThread.class` and `Foo/JBar.class` among other class files under `target`.
   `FooThread` extends `java.lang.Thread` and implements `java.lang.Runnable` with idris function `run` and
   in the same way `JBar` would have the exported Idris functions.
