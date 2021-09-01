package io.github.mmhelloworld.idris2.runtime;

import static java.lang.String.format;

public final class IntThunkResult implements IntThunk {
    public final int result;

    public IntThunkResult(int result) {
        this.result = result;
    }

    @Override
    public Thunk evaluate() {
        return this;
    }

    @Override
    public boolean isRedex() {
        return false;
    }

    @Override
    public Object getObject() {
        return result;
    }

    @Override
    public int getInt() {
        return result;
    }

    @Override
    public double getDouble() {
        throw new ClassCastException(
            format("int value %d cannot be converted to double", result));
    }
}