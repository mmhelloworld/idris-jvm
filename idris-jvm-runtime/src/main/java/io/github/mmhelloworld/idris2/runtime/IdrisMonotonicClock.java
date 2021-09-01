package io.github.mmhelloworld.idris2.runtime;

import static java.util.concurrent.TimeUnit.NANOSECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;

public final class IdrisMonotonicClock implements IdrisClock {
    private final long time;

    public IdrisMonotonicClock() {
        this.time = System.nanoTime();
    }

    @Override
    public long getSeconds() {
        return SECONDS.convert(time, NANOSECONDS);
    }

    @Override
    public long getNanoSeconds() {
        return time % 1000000000L;
    }
}
