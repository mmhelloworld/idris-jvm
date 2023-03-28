.. _tutorial-index:

#########################
Overview
#########################

* Idris JVM compiles Idris programs into Java 8 bytecode.
  The compiler is self hosting so the compiler itself is written in Idris and runs on the JVM.

* **Install**

  * Download the latest Idris 2 JVM release from `releases page`_.
  * Extract the archive and add ``idris2`` launcher script directory ``<EXTRACTED_DIRECTORY_ROOT>/exec`` to ``PATH``.
  * Create an environment variable ``IDRIS2_PREFIX`` pointing to ``<EXTRACTED_DIRECTORY_ROOT>/env``

* **Hello World**

.. code-block:: idris

  module Main

    data Tree a = Leaf
                | Node (Tree a) a (Tree a)

    inorder : Tree a -> List a
    inorder Leaf = []
    inorder (Node left a right) = inorder left ++ [a] ++ inorder right

    tree : Tree String
    tree = Node
            (Node
              (Node Leaf "3" Leaf)
              "+"
              (Node Leaf "7" Leaf))
            "/"
            (Node Leaf "2" Leaf)

    main : IO ()
    main = printLn $ inorder tree

**Compile**

``idris2 helloworld.idr -o main``

**Run**

.. code-block:: shell

  % build/exec/main
  ["3", "+", "7", "/", "2"]

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
  ``sum 50000`` wouldn't overflow the stack. See :ref:`general-tail-call-optimization` for more details.

.. code-block:: idris

    sum : Nat -> Nat
    sum n = go 0 n where
      go : Nat -> Nat -> Nat
      go acc Z = acc
      go acc n@(S k) = go (acc + n) k

* **Non-recursive tail calls** are handled using trampolines. For the following code, ``isOdd 100000`` wouldn't
  overflow the stack. See :ref:`general-tail-call-optimization` for more details.

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

* **FFI - Calling Idris from Java** - This is currently in progress.

.. _releases page: https://github.com/mmhelloworld/idris-jvm/releases/latest
