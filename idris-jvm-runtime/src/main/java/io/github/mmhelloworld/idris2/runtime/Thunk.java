package io.github.mmhelloworld.idris2.runtime;

import static java.util.Objects.requireNonNull;

@FunctionalInterface
public interface Thunk {
    Thunk evaluate();

    default boolean isRedex() {
        return true;
    }

    default Object getObject() {
        Thunk thunk = this;
        while (thunk != null && thunk.isRedex()) {
            thunk = thunk.evaluate();
        }
        return thunk == null ? null : thunk.getObject();
    }

    default int getInt() {
        Thunk thunk = this;
        while (thunk != null && thunk.isRedex()) {
            thunk = thunk.evaluate();
        }
        requireNonNull(thunk, "No int value at thunk");
        return thunk.getInt();
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
