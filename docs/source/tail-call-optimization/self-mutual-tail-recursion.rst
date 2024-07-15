.. _tail-call-optimization-self-mutual-tail-recursion:

Self and Mutually Tail Recursive Functions
==========================================
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
