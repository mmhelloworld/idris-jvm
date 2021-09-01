package io.github.mmhelloworld.idrisjvm.runtime;

@FunctionalInterface
public interface SupplierE<T, E extends Exception> {
    T get() throws E;
}
