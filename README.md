idris-jvm
=========

JVM bytecode backend for Idris

Install
=======
```
git clone https://github.com/mmhelloworld/idris-jvm.git
cd idris-jvm
stack install
```
Add the installed executable location to the `PATH` if it is not already on PATH.

Install Idris JVM runtime
-------------------------
```
cd runtime
idris --install idris-jvm-runtime.ipkg
```

Example
=======

* pythag.idr

    ```idris
    module Main

    pythag : Int -> List (Int, Int, Int)
    pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                              x * x + y *y == z * z]

    main : IO ()
    main = print (pythag 50)
    ```

* Get [Idris Runtime JAR](https://github.com/mmhelloworld/idrisjvm-runtime/releases/download/1.0-SNAPSHOT/idrisjvm-runtime-1.0-SNAPSHOT.jar) and set the environment variable `IDRIS_JVM_LIB` to the path to the JAR.
* Make sure `java` is on your path. JDK is not necessary, Just JRE is enough but Java 8 is required. `java` can also be explicitly specified using `IDRIS_JVM` environment variable.
* `$ idris pythag.idr --codegen jvm -o Pythag`
* `$ java -cp $IDRIS_JVM_LIB:. Pythag`

Status / Future improvements
============================

* This is still work in progress so only basic primitive types (No BigIntegers, doubles) are supported at the moment.
* FFI is still in progress. Currently only static Java methods that take `int` or `String` are supported but FFI is actively in development now.
* Tail recursion is eliminated using JVM's `GOTO`. For the following code, `sum 50000` wouldn't blow up the stack.
    ```idris
    sum : Nat -> Nat
    sum n = go 0 n where
      go : Nat -> Nat -> Nat
      go acc Z = acc
      go acc n@(S k) = go (acc + n) k
    ```

* Tail call is handled using Trampolines. For the following code, `evenT 10909000007` would work just fine and return the result after few seconds. `IO` is used here as otherwise Idris inlines the function calls and the functions end up being tail recursive instead of mutually recursive.
    ```idris
    mutual
      evenT : Nat -> IO Bool
      evenT Z = pure True
      evenT (S k) = oddT k

      oddT : Nat -> IO Bool
      oddT Z = pure False
      oddT (S k) = evenT k
    ```

* It compiles to Java 8 class files. Tail calls are delayed using Java 8 lambdas and use JVM's `invokedynamic`.
* All case trees are compiled to JVM's `LookupSwitch` but small *range* case expressions can be optimized using `TableSwitch`
* With a bit of local type inference, primitive boxing / unboxing can be reduced.

Why ASM (Java assembler) and not hs-java (Haskell's JVM bytecode assembler)
===========================================================================
This is one thing I would love to do, to use hs-java instead of ASM as it eliminates the need for an external `java` process invocation during compilation, especially given JVM's startup time. Unfortunately hs-java doesn't yet support Java 8, specifically "invokedynamic" is missing and it doesn't provide nice API to specify "Stack map frames" and "Stack map frames" are mandatory in Java 8 class files. ASM does all this and it can actually calculate stack map frames by bytecode data flow analysis, calculate maximum stack size and number of local variables. ASM calculating stack map frames is not really useful here as ASM would need to load class dependencies in bytecode which mandates us to manage dependencies during idris code generation (Maven anyone? :)) so we are not letting ASM to calculate stack map frames here but ASM calculating maximum stack size is really useful.
