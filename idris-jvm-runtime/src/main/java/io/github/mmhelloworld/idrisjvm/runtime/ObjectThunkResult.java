package io.github.mmhelloworld.idrisjvm.runtime;

public final class ObjectThunkResult implements Thunk {
    public final Object result;

    public ObjectThunkResult(Object result) {
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
        return Conversion.toInt(result);
    }

    @Override
    public long getLong() {
        return (long) result;
    }

    @Override
    public double getDouble() {
        return (double) result;
    }
}

