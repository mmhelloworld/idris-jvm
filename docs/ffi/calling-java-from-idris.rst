.. _ffi-calling-java-from-idris:

#########################
Calling Java from Idris
#########################


Calling static methods
======================

.. code-block:: idris

    %foreign "jvm:toBinaryString(int java/lang/String),java/lang/Integer"
    intToBinaryString : Int -> String

    main : IO ()
    main = printLn $ intToBinaryString 128

The foreign specifier starting with ``jvm:`` indicates a JVM function invocation.
There may be other runtime specifiers on the same function which will be ignored by the Idris JVM compiler.
The specifier starts with the static method name and then specifies the argument types and return type.
Last part is the class name. JVM types use ``/`` to separate package parts which is the JVM representation of a class
unlike Java's ``.`` (dot).

Calling instance methods
========================

.. code-block:: idris

    %foreign "jvm:.substring(java/lang/String int int java/lang/String),java/lang/String"
    substring : String -> Int -> Int -> String

    main : IO ()
    main = printLn $ substring "foobarbaz" 3 6

This is similar to static method call except that the method name starts with a dot and the type of first argument is the type of instance that the method will be invoked on.

Calling constructors
====================

.. code-block:: idris

    import Java.Lang
    import Java.Util

    namespace ArrayList
      export
      data ArrayList : Type -> Type where [external]

      %foreign "jvm:<init>(java/util/ArrayList),java/util/ArrayList"
      prim_new : PrimIO (ArrayList a)

      export %inline
      new : HasIO io => io (ArrayList elemTy)
      new = primIO prim_new

    main : IO ()
    main = do
      list <- ArrayList.new
      printLn !(toString list)

Foreign specifier for constructors start with ``<init>`` which is the JVM name for constructors.
Similar to method calls, constructors specify argument types and return type. In this case, we invoke a zero-argument
constructor returning `java/util/ArrayList`.

Accessing fields
================

.. code-block:: idris

    data Point : Type where [external]

    -- Get an instance field
    %foreign "jvm:#x(java/awt/Point int),java/awt/Point"
    prim_getX : Point -> PrimIO Int

    -- Set an instance field
    %foreign "jvm:#=x(java/awt/Point int void),java/awt/Point"
    prim_setX : Point -> Int -> PrimIO ()

    -- Get a static field
    %foreign "jvm:#MAX_VALUE(int),java/lang/Integer"
    intMax : Int

    -- Set a static field
    %foreign "jvm:#=bar(java/lang/String void),io/github/mmhelloworld/helloworld/Main"
    prim_setBar : String -> PrimIO ()


Field foreign specifiers start with ``#``. To mutate fields, foreign specifier starts with ``#=``. Instance field
accessors will have an additional parameter to pass the instance.

Inheritance
===========

Idris JVM allows calling methods from interfaces or parent classes through ``Inherits`` interface.
``Inherits`` interface doesn't have any functions to implement but is just a marker interface for foreign types.
We already saw an example of inheritance above that calls ``toString`` from ``Object`` class on an `ArrayList` instance.
Here is a detailed example showing the hierarchy between Java's ``Collection``, ``List`` and ``ArrayList``:

.. code-block:: idris

  namespace Collection
    export
    data Collection : Type -> Type where [external]

    %foreign "jvm:.add(i:java/util/Collection java/lang/Object Bool),java/util/Collection"
    prim_add : Collection a -> a -> PrimIO Bool

    %foreign "jvm:.size(i:java/util/Collection int),java/util/Collection"
    prim_size : Collection a -> PrimIO Int

    export %inline
    add : HasIO io => obj -> elemTy -> (Inherits obj (Collection elemTy)) => io Bool
    add collection elem = primIO $ prim_add (subtyping collection) elem

    export %inline
    size : (HasIO io, Inherits obj (Collection elemTy)) => obj -> io Int
    size {elemTy} collection = primIO $ prim_size {a=elemTy} (subtyping collection)

  namespace JList

      export
      data JList : Type -> Type where [external]

      %foreign "jvm:.get(i:java/util/List int java/lang/Object),java/util/List"
      prim_get : JList a -> Int -> PrimIO a

      export %inline
      get : (HasIO io, Inherits list (JList elemTy)) => list -> Int -> io elemTy
      get list index = primIO $ prim_get (subtyping list) index

  public export
  Inherits (JList a) (Collection a) where

  public export
  Inherits obj (JList a) => Inherits obj (Collection a) where

  namespace ArrayList
      export
      data ArrayList : Type -> Type where [external]

      %foreign "jvm:<init>(java/util/ArrayList),java/util/ArrayList"
      prim_new : PrimIO (ArrayList a)

      export %inline
      new : HasIO io => io (ArrayList elemTy)
      new = primIO prim_new

  public export
  Inherits (ArrayList a) (JList a) where

  main : IO ()
  main = do
      list <- ArrayList.new {elemTy=String}
      ignore $ add list "hello"
      ignore $ add list "world"
      elem <- JList.get {elemTy=String} list 1
      printLn elem
      printLn !(size {elemTy=String} list)
      printLn !(toString list)

Here, we create an ``ArrayList`` instance and call ``get`` method from ``List`` and methods from ``Collection`` such as
``add`` and ``size``. We are able to pass ``ArrayList`` instance to the ``List`` and ``Collection`` functions because of
``Inherits`` interface instances for ``ArrayList``. Another note: In JVM, invoking methods on interface is different
from class methods invocation so the foreign specifiers on interface methods have ``i:`` prefix for the first parameter
that represents the instance that the methods are called on.

Class literals
================

``classLiteral`` function can be used to get Java's ``Class`` instances for JVM types.

.. code-block:: console

    Main> :module Java.Lang
    Imported module Java.Lang
    Main> :t classLiteral
    Java.Lang.classLiteral : Class ty

.. code-block:: idris

    import Java.Lang

    main : IO ()
    main = do
      printLn !(Object.toString $ classLiteral {ty=Int})
      printLn !(Object.toString $ classLiteral {ty=Integer})
      printLn !(Object.toString $ classLiteral {ty=String})

The above example prints:

.. code-block:: console

   "int"
   "class java.math.BigInteger"
   "class java.lang.String"

JVM reference equality
======================
Reference equality should be avoided in Idris but it might be useful to interface with Java, for example, for overriding ``equals`` method in Idris.

.. code-block:: console

    Main> :module Java.Lang
    Imported module Java.Lang
    Main> :exec printLn (jvmRefEq "foo" "foo")
    True
    Main> :exec printLn (jvmRefEq "foo" ("fo" ++ "o"))
    False
