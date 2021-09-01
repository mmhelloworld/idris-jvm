package io.github.mmhelloworld.idris2.runtime;

import static java.lang.String.format;

public final class DoubleThunkResult implements DoubleThunk {
    public final double result;

    public DoubleThunkResult(double result) {
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
        throw new ClassCastException(
            format("double value %s cannot be converted to int", result));
    }

    @Override
    public double getDouble() {
        return result;
    }
}