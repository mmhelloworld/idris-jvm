.. _tail-call-optimization-self-tail-recursion:

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
