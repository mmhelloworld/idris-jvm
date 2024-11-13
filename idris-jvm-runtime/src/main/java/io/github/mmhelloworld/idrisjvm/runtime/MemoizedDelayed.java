package io.github.mmhelloworld.idrisjvm.runtime;

public final class MemoizedDelayed implements Delayed {
    private boolean initialized;
    private Delayed delayed;

    public MemoizedDelayed(Delayed delayed) {
        this.delayed = () -> {
            synchronized (this) {
                if (!initialized) {
                    Object value = delayed.evaluate();
                    this.delayed = () -> value;
                    initialized = true;
                }
            }
            return this.delayed.evaluate();
        };
    }

    public Object evaluate() {
        return delayed.evaluate();
    }
}
