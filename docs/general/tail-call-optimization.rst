.. _general-tail-call-optimization:

######################
Tail Call Optimization
######################

Idris JVM backend optimizes tail recursion, both self and mutual tail calls. Self tail calls are eliminated using JVM's
``GOTO`` and mutual tail calls are eliminated using trampolines. Here we will see examples for each of those cases and
how they are compiled into JVM bytecode.

Self tail recursion
===================

This example demonstrates self tail recursion in ``go`` function. This program wouldn't overflow the stack if it is
called with large values like ``50000`` as the recursive call would be essentially turned into a loop in the bytecode.

.. code-block:: idris

    sum : Nat -> Nat
    sum n = go 0 n where
      go : Nat -> Nat -> Nat
      go acc Z = acc
      go acc n@(S k) = go (acc + n) k

    main : IO ()
    main = printLn (sum 50000)

Bytecode
--------
The following bytecode shows how this is compiled into. In the bytecode, the function call in tail position would be
replaced with ``GOTO`` so the function doesn't call itself instead it transfers the control back to the beginning of
the function with updated argument values for next iteration as shown in the last five lines of the bytecode.

.. code-block::

  public static java.lang.Object $n2810$7795$go(java.lang.Object, java.lang.Object, java.lang.Object);
    Code:
       0: aload_2
       1: checkcast     #71                 // class java/math/BigInteger
       4: astore_3
       5: iconst_m1
       6: istore        4
       8: aload_3
       9: invokevirtual #241                // Method java/math/BigInteger.hashCode:()I
      12: lookupswitch  { // 1
                     0: 32
               default: 48
          }
      32: aload_3
      33: getstatic     #234                // Field java/math/BigInteger.ZERO:Ljava/math/BigInteger;
      36: invokevirtual #245                // Method java/math/BigInteger.equals:(Ljava/lang/Object;)Z
      39: ifeq          48
      42: iconst_0
      43: istore        4
      45: goto          48
      48: iload         4
      50: lookupswitch  { // 1
                     0: 68
               default: 70
          }
      68: aload_1
      69: areturn
      70: aload_2
      71: checkcast     #71                 // class java/math/BigInteger
      74: getstatic     #248                // Field java/math/BigInteger.ONE:Ljava/math/BigInteger;
      77: invokevirtual #252                // Method java/math/BigInteger.subtract:(Ljava/math/BigInteger;)Ljava/math/BigInteger;
      80: astore        5
      82: aload_1
      83: checkcast     #71                 // class java/math/BigInteger
      86: aload_2
      87: checkcast     #71                 // class java/math/BigInteger
      90: invokevirtual #255                // Method java/math/BigInteger.add:(Ljava/math/BigInteger;)Ljava/math/BigInteger;
      93: astore        6
      95: aload         5
      97: astore        7
      99: aload         6
     101: astore_1
     102: aload         7
     104: astore_2
     105: goto          0

Decompiled Java code
--------------------
The decompiled Java code below shows bit more clearly that the recursive call is eliminated with a loop in the last
`default` block.

.. code-block:: java

  public static Object $n2810$7795$go(Object arg$0, Object arg$1, Object arg$2) {
      while(true) {
          BigInteger constantCaseExpr0 = (BigInteger)arg$2;
          int hashCodePosition1 = -1;
          switch (constantCaseExpr0.hashCode()) {
              case 0:
                  if (constantCaseExpr0.equals(BigInteger.ZERO)) {
                      hashCodePosition1 = 0;
                  }
              default:
                  switch (hashCodePosition1) {
                      case 0:
                          return arg$1;
                      default:
                          Object e$0 = ((BigInteger)arg$2).subtract(BigInteger.ONE);
                          BigInteger tailRecArg2 = ((BigInteger)arg$1).add((BigInteger)arg$2);
                          arg$1 = tailRecArg2;
                          arg$2 = e$0;
                  }
          }
      }
  }

Mutual tail recursion
=====================
The example below shows two functions ``isEven`` and ``isOdd`` calling each other. This is compiled using trampolines
where the actual function calls are replaced with constructor ``TcContinue`` and the result is returned in ``TcDone``.
The constructor ``TcContinue`` will have an index indicating which function to call and the arguments for
the function. There will be a top-level function in Idris JVM runtime called ``tailRec`` which iterates as long as
``TcContinue`` object is returned and returns the result when it gets a ``TcDone`` object.
This basically ensures that the functions don't call each other and we trade off the heap for the stack to hold the
function arguments. This works for any number of functions, not just two, calling each other in tail position.

.. code-block:: idris

  mutual
    isEven : Nat -> Bool
    isEven Z = True
    isEven (S k) = isOdd k

    isOdd : Nat -> Bool
    isOdd Z = False
    isOdd (S k) = isEven k

Decompiled Java code
--------------------
In the decompiled code below, the Idris top-level function ``isOdd`` simply calls the runtime function ``tailRec`` that
iterates between mutually recursive functions. ``$tcOpt$1`` determines which tail-call optimized function to call based
on the `TcContinue` constructor id, and the function is passed to `tailRec` which keeps on calling this function until
it encounters `TcDone`. ``isEven$tc1`` and ``isOdd$tc2`` are the tail-call optimized versions of respective Idris
functions where the recursive call is replaced with ``TcContinue`` and the result is returned in ``TcDone``.

.. code-block:: java

  public static Object isOdd(Object arg$0) {
      return Runtime.tailRec(Main::$tcOpt$1, new TcContinue_1(2, arg$0));
  }

  public static Object $tcOpt$1(Object $a$0) {
      Object $a$0 = (IdrisObject)$a$0;
      Object arg$0;
      switch ($a$0.getConstructorId()) {
          case 1:
              arg$0 = ((IdrisObject)$a$0).getProperty(0);
              return isEven$tc1(arg$0);
          case 2:
              arg$0 = ((IdrisObject)$a$0).getProperty(0);
              return isOdd$tc2(arg$0);
          default:
              return null;
      }
  }

  public static Object isEven$tc1(Object arg$0) {
      BigInteger constantCaseExpr0 = (BigInteger)arg$0;
      int hashCodePosition1 = -1;
      switch (constantCaseExpr0.hashCode()) {
          case 0:
              if (constantCaseExpr0.equals(BigInteger.ZERO)) {
                  hashCodePosition1 = 0;
              }
          default:
              switch (hashCodePosition1) {
                  case 0:
                      return new TcDone(0, 1);
                  default:
                      Object e$0 = ((BigInteger)arg$0).subtract(BigInteger.ONE);
                      return new TcContinue_1(2, e$0);
              }
      }
  }

  public static Object isOdd$tc2(Object arg$0) {
      BigInteger constantCaseExpr0 = (BigInteger)arg$0;
      int hashCodePosition1 = -1;
      switch (constantCaseExpr0.hashCode()) {
          case 0:
              if (constantCaseExpr0.equals(BigInteger.ZERO)) {
                  hashCodePosition1 = 0;
              }
          default:
              switch (hashCodePosition1) {
                  case 0:
                      return new TcDone(0, 0);
                  default:
                      Object e$0 = ((BigInteger)arg$0).subtract(BigInteger.ONE);
                      return new TcContinue_1(1, e$0);
              }
      }
  }

Functions can be both self and mutually tail recursive
======================================================
A function can be both self and mutually tail recursive and both of the optimizations shown above would be applied in
that case.

Consider this Idris function, for example, where it calls itself as well as another function which calls back:

.. code-block:: idris

  mutual
    isEven : Nat -> Bool
    isEven Z = True
    isEven (S k) = isOdd k

    isOdd : Nat -> Bool
    isOdd Z = False
    isOdd (S (S k)) = isOdd k
    isOdd (S k) = isEven k

Decompiled Java code
--------------------
The JVM bytecode for this function will have a loop and trampoline as we can see in the following decompiled code.
Here the last `default` block has `TcContinue` constructor in one branch and overwrites argument value in the
other branch for next iteration.

.. code-block:: java

  public static Object isOdd$tc2(Object arg$0) {
      while(true) {
          BigInteger constantCaseExpr0 = (BigInteger)arg$0;
          int hashCodePosition1 = -1;
          switch (constantCaseExpr0.hashCode()) {
              case 0:
                  if (constantCaseExpr0.equals(BigInteger.ZERO)) {
                      hashCodePosition1 = 0;
                  }
              default:
                  switch (hashCodePosition1) {
                      case 0:
                          return new TcDone(0, 0);
                      default:
                          Object e$0 = ((BigInteger)arg$0).subtract(BigInteger.ONE);
                          BigInteger constantCaseExpr2 = (BigInteger)e$0;
                          int hashCodePosition3 = -1;
                          switch (constantCaseExpr2.hashCode()) {
                              case 0:
                                  if (constantCaseExpr2.equals(BigInteger.ZERO)) {
                                      hashCodePosition3 = 0;
                                  }
                              default:
                                  switch (hashCodePosition3) {
                                      case 0:
                                          return new TcContinue_1(1, e$0);
                                      default:
                                          Object e$1 = ((BigInteger)e$0).subtract(BigInteger.ONE);
                                          arg$0 = e$1;
                                  }
                          }
                  }
          }
      }
  }
