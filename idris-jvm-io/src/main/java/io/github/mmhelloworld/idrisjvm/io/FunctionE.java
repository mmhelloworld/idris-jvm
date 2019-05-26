package io.github.mmhelloworld.idrisjvm.io;

@FunctionalInterface
public interface FunctionE<T, R, E extends Exception> {
    R apply(T arg) throws E;
}
