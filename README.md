# idris-jvm

[![Join the chat at https://gitter.im/mmhelloworld/idris-jvm](https://badges.gitter.im/mmhelloworld/idris-jvm.svg)](https://gitter.im/mmhelloworld/idris-jvm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/mmhelloworld/idris-jvm.svg?branch=master)](https://travis-ci.org/mmhelloworld/idris-jvm)

JVM bytecode backend for Idris

## Prerequisites

1. [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
2. Java 8

## Install

1. `git clone https://github.com/mmhelloworld/idris-jvm.git`
1. `cd idris-jvm`
1. `bin/setup`. Windows users, please follow the instructions [here](docs/windows.md) before running this.

For details on what the `setup` script does, please see [here](docs/setup.md).

## Example

* pythag.idr

    ```idris
    module Main

    pythag : Int -> List (Int, Int, Int)
    pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                              x * x + y *y == z * z]

    main : IO ()
    main = print (pythag 50)
    ```

* `$ bin/idrisjvm pythag.idr -o pythag`
* `$ java -cp ~/.idrisjvm/idris-jvm-runtime-1.0-SNAPSHOT.jar:pythag main.Main`

## Status

* All Idris types are supported. Idris `Int` is mapped to Java primitive `int`. Idris `String` is mapped to Java `String`. Idris `Integer` is represented as Java `BigInteger`.
Idris `Double` is mapped to Java `double`. Idris `Bits8`, `Bits16`, `Bits32` are mapped to Java `int`.
Idris `Bits64` is mapped to Java `long`.
* **FFI - Calling Java from Idris:** From Idris, invoking Java static methods, instance methods, constructors are all supported.
See [here](https://github.com/mmhelloworld/idris-jvm/blob/master/test/tests/ffi/ffi.idr) for an example.
* **FFI: Calling Idris from Java:** Idris functions can be exported as Java instance methods, static methods and constructors. The exported class with Idris implementations can also extend a Java class and implement interfaces. It can also have static and instance fields and the field values can be set from Idris. Idris types (monomorphic, for example, `List Int`) can also be exported as a Java class. See [here](https://github.com/mmhelloworld/idris-jvm/blob/master/test/tests/ffi/ffi.idr) for an example.
* **Tail recursion** is eliminated using JVM's `GOTO`. For the following code, `sum 50000` wouldn't blow up the stack.
    ```idris
    sum : Nat -> Nat
    sum n = go 0 n where
      go : Nat -> Nat -> Nat
      go acc Z = acc
      go acc n@(S k) = go (acc + n) k
    ```

* **Non-recursive tail call** is handled using trampolines. For the following code, `evenT 10909000007` would work just fine and return the result after few seconds. `IO` is used here as otherwise Idris inlines the function calls and the functions end up being tail recursive instead of mutually recursive.
    ```idris
    mutual
      evenT : Nat -> IO Bool
      evenT Z = pure True
      evenT (S k) = oddT k

      oddT : Nat -> IO Bool
      oddT Z = pure False
      oddT (S k) = evenT k
    ```

* It compiles to **Java 8 class files**. Tail calls are delayed using Java 8 lambdas and use JVM's `invokedynamic`.
* Idris primitives `par` and `fork` for running in parallel and creating threads are supported using Java's `ForkJoin` and `ExecutorService`. See [here](https://github.com/mmhelloworld/idris-jvm/blob/master/test/tests/forkpar/forkpar.idr) for an example.
