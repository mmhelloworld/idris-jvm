idris-jvm
=========

JVM bytecode backend for Idris

Install
=======

1. `git clone https://github.com/mmhelloworld/idris-jvm.git`
1. `cd idris-jvm`
1. `bin/setup`. Windows users, follow the instructions [here](docs/windows.md) before running this.

For details on what the `setup` script does, please see [here](docs/setup.md).

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

* Make sure `java` (from Java 8 JDK or JRE) is on your path.
* `$ bin/idris pythag.idr --codegen jvm -o Pythag`
* `$ java -cp ~/.idrisjvm/idrisjvm-runtime-1.0-SNAPSHOT.jar:. Pythag`

Status / Future improvements
============================

* This is still work in progress. Basic types, integers and strings are supported. BigIntegers and double are not supported yet.
* FFI is still in progress. Currently Java static methods, instance methods, constructors are all supported. JVM arrays, extending classes, implementing interfaces, exporting idris functions are not supported yet.
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
