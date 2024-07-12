.. _tutorial-index:

########
Overview
########

* Idris JVM compiles Idris programs into Java 8 bytecode.
  The compiler is self hosting so the compiler itself is written in Idris and runs on the JVM.

* Idris basic types are mapped to Java types. Idris modules and functions are compiled into Java classes and
  static methods respectively.

  +------------------------+---------------------+
  | Idris                  | JVM                 |
  +========================+=====================+
  | Int8, Int16, Int32,    |                     |
  | Int,                   | int                 |
  | Bits8, Bits16, Bits32, |                     |
  | Bool                   |                     |
  +------------------------+---------------------+
  | Int64, Bits64          | long                |
  +------------------------+---------------------+
  | Double                 | double              |
  +------------------------+---------------------+
  | Nat, Integer           | BigInteger          |
  +------------------------+---------------------+
  | Char                   | char                |
  +------------------------+---------------------+
  | String                 | String              |
  +------------------------+---------------------+
  | Module, Namespace      | Class               |
  +------------------------+---------------------+
  | Function               | Static methods      |
  +------------------------+---------------------+
  |                        | Java 8 lambda -     |
  |                        | ``invokedynamic``   |
  | Lambda                 | targeting           |
  |                        | ``Function``        |
  |                        | interface           |
  +------------------------+---------------------+
  | Nil arity function     | Memoized lambda     |
  +------------------------+---------------------+

* **Tail recursion** is eliminated using JVM's ``GOTO`` (``while`` loop equivalent in Java). For the following code,
  ``sum 50000`` wouldn't overflow the stack. See :ref:`tail-call-optimization-self-tail-recursion` for more details.

.. code-block:: idris

    sum : Nat -> Nat
    sum n = go 0 n where
      go : Nat -> Nat -> Nat
      go acc Z = acc
      go acc n@(S k) = go (acc + n) k

* **Non-recursive tail calls** are handled using trampolines. For the following code, ``isOdd 100000`` wouldn't
  overflow the stack. See :ref:`tail-call-optimization-mutual-tail-recursion` for more details.

.. code-block:: idris

    mutual
      isEven : Nat -> Bool
      isEven Z = True
      isEven (S k) = isOdd k

      isOdd : Nat -> Bool
      isOdd Z = False
      isOdd (S k) = isEven k

* **FFI - Calling Java from Idris:** From Idris, invoking Java static methods, instance methods,
  constructors are all supported. An example is shown below. See :ref:`ffi-calling-java-from-idris` for more details.

.. code-block:: idris

    %foreign "jvm:toBinaryString(int java/lang/String),java/lang/Integer"
    intToBinaryString : Int -> String

    main : IO ()
    main = printLn $ intToBinaryString 128

* **FFI - Calling Idris from Java**
  Idris functions can be exported to Java as static functions, instance functions and constructors. Java classes with
  fields can be created. Java annotations can be generated as well.
  See :ref:`ffi-calling-idris-from-java` for more details.
