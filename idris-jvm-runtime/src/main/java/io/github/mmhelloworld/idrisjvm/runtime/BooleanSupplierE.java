package io.github.mmhelloworld.idrisjvm.runtime;

@FunctionalInterface
public interface BooleanSupplierE<E extends Exception> {
    boolean get() throws E;
}
