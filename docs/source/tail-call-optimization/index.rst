.. _tail-call-optimization-overview:

########
Overview
########

Idris JVM backend optimizes tail recursion, both self and mutual tail calls. Self tail calls are eliminated using JVM's
``GOTO`` and mutual tail calls are eliminated using trampolines. Here we will see examples for each of those cases and
how they are compiled into JVM bytecode.
