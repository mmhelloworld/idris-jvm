package io.github.mmhelloworld.idrisjvm.runtime;

import static java.util.concurrent.TimeUnit.NANOSECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;

public final class IdrisProcessClock implements IdrisClock {
    private final long time;

    public IdrisProcessClock() {
        this.time = System.nanoTime();
    }

    @Override
    public long getSeconds() {
        return SECONDS.convert(time - Runtime.START_TIME, NANOSECONDS);
    }

    @Override
    public long getNanoSeconds() {
        return (time - Runtime.START_TIME) % 1000000000L;
    }
}
