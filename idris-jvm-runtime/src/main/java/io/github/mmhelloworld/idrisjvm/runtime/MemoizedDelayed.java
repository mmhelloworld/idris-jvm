package io.github.mmhelloworld.idrisjvm.runtime;

import static io.github.mmhelloworld.idrisjvm.runtime.Runtime.unwrap;

public final class MemoizedDelayed implements Delayed {
    private boolean initialized;
    private Delayed delayed;

    public MemoizedDelayed(Delayed delayed) {
        this.delayed = () -> {
            synchronized(this) {
                if(!initialized) {
                    Object value = unwrap(delayed.evaluate());
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
