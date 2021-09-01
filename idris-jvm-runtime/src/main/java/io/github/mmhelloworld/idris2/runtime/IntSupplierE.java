package io.github.mmhelloworld.idris2.runtime;

@FunctionalInterface
public interface IntSupplierE<E extends Exception> {
    int get() throws E;
}
