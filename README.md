# idris-jvm

[![Join the chat at https://gitter.im/mmhelloworld/idris-jvm](https://badges.gitter.im/mmhelloworld/idris-jvm.svg)](https://gitter.im/mmhelloworld/idris-jvm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/mmhelloworld/idris-jvm.svg?branch=master)](https://travis-ci.org/mmhelloworld/idris-jvm)

JVM bytecode back end for Idris

## Prerequisites

1. [Idris](https://github.com/idris-lang/Idris-dev)
2. Java 8

## Install
1. Download and extract JVM bytecode back end from [here](https://github.com/mmhelloworld/idris-jvm/releases). Make sure to download the release corresponding to your Idris version.
1. From the extracted directory, run `idris-jvm/bin/install` to install Idris packages for idris-jvm.
1. Add `<IDRIS_JVM_EXTRACTED_DIRECTORY>/idris-jvm/codegen/bin` to `PATH`.

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

* Compiling
    * On Linux/Mac OS:  `$ idris --portable-codegen jvm pythag.idr -o target`
    * On Windows:  `idris --portable-codegen jvm.bat pythag.idr -o target`

* Running
    * On Linux/Mac OS:  `$ java -cp <IDRIS_JVM_EXTRACTED_DIR>/idris-jvm/idris-jvm-runtime.jar:target main.Main`
    * On Windows:  `$ java -cp <IDRIS_JVM_EXTRACTED_DIR>/idris-jvm/idris-jvm-runtime.jar;target main.Main`

## Status

* All Idris types are supported. Idris `Int` is mapped to Java primitive `int`. Idris `String` is mapped to Java `String`. Idris `Integer` is represented as Java `BigInteger`.
Idris `Double` is mapped to Java `double`. Idris `Bits8`, `Bits16`, `Bits32` are mapped to Java `int`.
Idris `Bits64` is mapped to Java `long`.

* **FFI - Calling Java from Idris:** From Idris, invoking Java static methods, instance methods, constructors are all supported.
See [here](https://github.com/mmhelloworld/idris-jvm/blob/master/idris-jvm-integration-test/src/test/resources/idris-test-sources/ffi/ffi.idr) for an example.

* **FFI: Calling Idris from Java:** Idris functions can be exported as Java instance methods, static methods and constructors. The exported class with Idris implementations can also extend a Java class and implement interfaces. It can also have static and instance fields and the field values can be set from Idris. Idris types (monomorphic, for example, `List Int`) can also be exported as a Java class. See [here](https://github.com/mmhelloworld/idris-jvm/blob/master/idris-jvm-integration-test/src/test/resources/idris-test-sources/ffi/ffi.idr) for an example.
* **Tail recursion** is eliminated using JVM's `GOTO`. For the following code, `sum 50000` wouldn't blow up the stack.
    ```idris
    sum : Nat -> Nat
    sum n = go 0 n where
      go : Nat -> Nat -> Nat
      go acc Z = acc
      go acc n@(S k) = go (acc + n) k
    ```

* **Non-recursive tail call** is handled using trampolines. For the following code, `evenT 10909000007` would work just fine and return the result after few seconds. `IO` is used here as otherwise Idris inlines the function calls and the functions end up being tail recursive instead of mutually recursive. Non-recursive tail calls are delayed using Java 8 lambdas with JVM's `invokedynamic`.

    ```idris
    mutual
      evenT : Nat -> IO Bool
      evenT Z = pure True
      evenT (S k) = oddT k

      oddT : Nat -> IO Bool
      oddT Z = pure False
      oddT (S k) = evenT k
    ```

* **Idris functions as Java lambdas:** Idris functions can be passed as Java lambdas in FFI. JVM's `invokedynamic` instruction is used to create target functional interface objects just like how javac does. 

```idris
main : JVM_IO ()
main = ArrayList.fromList ["foobar", "hello", "world"] >>=
       stream >>=                                           -- Java 8 Stream from Java's ArrayList
       filter (jlambda (not . isPrefixOf "foo")) >>=        -- Idris function as Java "Predicate" lambda
       map (jlambda Strings.reverse) >>=                    -- Idris function as Java "Function" lambda
       collect !toList >>=
       Objects.toString >>=
       printLn
```

* Idris primitives `par` and `fork` for running in parallel and creating threads are supported using Java's `ForkJoin` and `ExecutorService`. See [here](https://github.com/mmhelloworld/idris-jvm/blob/master/idris-jvm-integration-test/src/test/resources/idris-test-sources/forkpar/forkpar.idr) for an example.

* `Maybe` type can be used in an FFI function to avoid Java `null` getting into Idris code. `Maybe` used in an
argument position will pass `null` to the Java code if the value is `Nothing` otherwise the unwrapped value will be passed to
Java. In the similar way, `Maybe` type used in the return type position would return `Nothing` if the FFI function returns `null` otherwise returns the actual value in `Just`.
