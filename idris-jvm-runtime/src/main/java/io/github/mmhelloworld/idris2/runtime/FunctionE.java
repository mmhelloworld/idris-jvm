package io.github.mmhelloworld.idris2.runtime;

@FunctionalInterface
public interface FunctionE<T, R, E extends Exception> {
    R apply(T arg) throws E;
}
