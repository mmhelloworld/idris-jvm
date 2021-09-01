package io.github.mmhelloworld.idrisjvm.runtime;

@FunctionalInterface
public interface IntSupplierE<E extends Exception> {
    int get() throws E;
}
