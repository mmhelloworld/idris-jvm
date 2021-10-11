package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.NoSuchElementException;

public interface IdrisObject {
    default int getConstructorId() {
        throw new UnsupportedOperationException("Not a data constructor");
    }

    default String getStringConstructorId() {
        throw new UnsupportedOperationException("Not a type constructor");
    }

    default Object getProperty(int index) {
        throw new NoSuchElementException("No property at " + index);
    }
}
