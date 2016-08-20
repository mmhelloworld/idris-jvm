idris-jvm
=========

JVM bytecode backend for Idris

Install
=======

**Prerequisites:**
- Haskell Stack
- JDK 8

  JDK is necessary because we are installing from source and Idris code generator written in Haskell calls out to JVM via C to generate JVM bytecode using ASM Java library. It needs some C header files from JDK to interface with. Haskell's JVM bytecode assemblers would have been ideal for this but unfortunately Haskell assemblers are not up to date and not feature rich as ASM.

**Steps:**

1. Set environment variable `IDRIS_JAVA_HOME` or `JAVA_HOME` to point to JDK 8 root directory.
1. git clone https://github.com/mmhelloworld/idris-jvm.git.
1. `cd idris-jvm`
1. `./install.sh`. (Windows setup coming up!)

Stack installs the executable in some location and the location is displayed in the terminal once it is done. Add that location to PATH if it is not already on PATH.

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
* `$ idris pythag.idr --codegen jvm -o Pythag`
* `$ java -cp ~/.idris-jvm/*:. Pythag`

Status / Future improvements
============================

* This is still work in progress. Basic types, integers and strings are supported. BigIntegers and double are not supported yet.
* FFI is still in progress. Currently static methods, instance methods, constructors are all supported. JVM arrays, extending classes, implementing interfaces are not supported yet.
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
* With a bit of local type inference, primitive boxing / unboxing can be reduced. Currently all the primitives are boxed until the very last moment where a primitive operator needs to be invoked.
