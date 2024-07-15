.. Idris JVM documentation master file, created by
   sphinx-quickstart on Wed Mar  8 16:36:16 2023.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Idris JVM's documentation!
=====================================

`Idris 2`_ is a purely functional programming language with first class types. Idris JVM compiles Idris programs to JVM
bytecode enabling Idris programs to be run on the JVM. Idris JVM backend is written in Idris and
compiled with JVM backend so the compiler itself is self hosting and runs on Java 8 or above.

.. _Idris 2: https://idris2.readthedocs.io/en/latest/index.html

.. toctree::
   :caption: Idris JVM Documentation
   :maxdepth: 4

   general/index
   general/install
   general/hello-world-compile-run

.. toctree::
   :caption: Tail Call Optimization
   :maxdepth: 4

   tail-call-optimization/index
   tail-call-optimization/self-tail-recursion
   tail-call-optimization/mutual-tail-recursion
   tail-call-optimization/self-mutual-tail-recursion

.. toctree::
   :caption: Interoperability
   :maxdepth: 4

   ffi/index
   ffi/calling-java-from-idris
   ffi/calling-idris-from-java
