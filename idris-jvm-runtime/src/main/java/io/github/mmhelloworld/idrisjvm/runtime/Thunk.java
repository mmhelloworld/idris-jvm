package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.NoSuchElementException;

import static java.util.Objects.requireNonNull;

@FunctionalInterface
public interface Thunk {
    Thunk evaluate();

    default boolean isRedex() {
        return true;
    }

    default Object getObject() {
        throw new NoSuchElementException("Unevaluated thunk");
    }

    default int getInt() {
        Thunk thunk = this;
        while (thunk != null && thunk.isRedex()) {
            thunk = thunk.evaluate();
        }
        requireNonNull(thunk, "No int value at thunk");
        return thunk.getInt();
    }

    default long getLong() {
        Thunk thunk = this;
        while (thunk != null && thunk.isRedex()) {
            thunk = thunk.evaluate();
        }
        requireNonNull(thunk, "No long value at thunk");
        return thunk.getLong();
    }

    default double getDouble() {
        Thunk thunk = this;
        while (thunk != null && thunk.isRedex()) {
            thunk = thunk.evaluate();
        }
        requireNonNull(thunk, "No double value at thunk");
        return thunk.getDouble();
    }

}
