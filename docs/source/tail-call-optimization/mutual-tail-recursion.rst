.. _tail-call-optimization-mutual-tail-recursion:

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




