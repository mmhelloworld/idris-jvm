package io.github.mmhelloworld.idris2.runtime;

@FunctionalInterface
public interface LongSupplierE<E extends Exception> {
    long get() throws E;
}
