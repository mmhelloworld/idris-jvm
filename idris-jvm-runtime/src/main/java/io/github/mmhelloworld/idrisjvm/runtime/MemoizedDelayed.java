package io.github.mmhelloworld.idrisjvm.runtime;

public final class MemoizedDelayed implements Delayed {
    private volatile boolean initialized;
    private Delayed delayed;
    private Object value;

    public MemoizedDelayed(Delayed delayed) {
        this.delayed = delayed;
    }

    // Double-checked locking: the hot path after initialization is a single
    // volatile read plus a field read — no monitor, no lambda indirection.
    // (The previous shape re-entered a synchronized closure through a mutable
    // Delayed field on every read and dominated thunk-heavy profiles.)
    public Object evaluate() {
        if (!initialized) {
            synchronized (this) {
                if (!initialized) {
                    value = delayed.evaluate();
                    delayed = null;
                    initialized = true;
                }
            }
        }
        return value;
    }
}
