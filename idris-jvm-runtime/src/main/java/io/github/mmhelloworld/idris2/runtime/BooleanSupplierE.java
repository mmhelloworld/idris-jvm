package io.github.mmhelloworld.idris2.runtime;

@FunctionalInterface
public interface BooleanSupplierE<E extends Exception> {
    boolean get() throws E;
}
