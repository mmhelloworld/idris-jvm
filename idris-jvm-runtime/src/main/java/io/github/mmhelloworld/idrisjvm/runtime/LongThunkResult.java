package io.github.mmhelloworld.idrisjvm.runtime;

import static java.lang.String.format;

public final class LongThunkResult implements LongThunk {
    public final long result;

    public LongThunkResult(long result) {
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
        throw new NumberFormatException(format("long %s cannot be converted to int", result));
    }

    @Override
    public long getLong() {
        return result;
    }

    @Override
    public double getDouble() {
        throw new ClassCastException(
            format("int value %d cannot be converted to double", result));
    }
}
