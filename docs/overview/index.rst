.. _tutorial-index:

#########################
Overview
#########################

* Idris basic types are mapped to Java types. Idris modules and functions are compiled into Java classes and
  static methods respectively.

  +------------------------+---------------------+
  | Idris                  | JVM                 |
  +========================+=====================+
  | Int8, Int16, Int32,    |                     |
  | Int,                   | int                 |
  | Bits8, Bits16, Bits32  |                     |
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
  |                        | `invokedynamic`     |
  | Lambda                 | targeting `Function`|
  |                        | interface           |
  +------------------------+---------------------+
  | Nil arity function     | Memoized lambda     |
  +------------------------+---------------------+

* **Tail recursion** is eliminated using JVM's `GOTO` (`while` loop equivalent in Java). For the following code,
  `sum 50000` wouldn't overflow the stack.

.. code-block:: idris

    sum : Nat -> Nat
    sum n = go 0 n where
      go : Nat -> Nat -> Nat
      go acc Z = acc
      go acc n@(S k) = go (acc + n) k

* **Non-recursive tail calls** are handled using trampolines. For the following code, `isOdd 100000` wouldn't
  overflow the stack.

.. code-block:: idris

    mutual
      isEven : Nat -> Bool
      isEven Z = True
      isEven (S k) = isOdd k

      isOdd : Nat -> Bool
      isOdd Z = False
      isOdd (S k) = isEven k

* **FFI - Calling Java from Idris:** From Idris, invoking Java static methods, instance methods,
  constructors are all supported. Here is a small example.

.. code-block:: idris

    %foreign "jvm:toBinaryString(int java/lang/String),java/lang/Integer"
    intToBinaryString : Int -> String

    main : IO ()
    main = printLn $ intToBinaryString 128

More details and examples here: :ref:`ffi-calling-java-from-idris`

