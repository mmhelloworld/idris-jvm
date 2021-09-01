package io.github.mmhelloworld.idrisjvm.runtime;

@FunctionalInterface
public interface LongSupplierE<E extends Exception> {
    long get() throws E;
}
