package io.github.mmhelloworld.idris2.runtime;

import static java.util.concurrent.TimeUnit.NANOSECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;

public class IdrisThreadClock implements IdrisClock {
    private static final ThreadLocal<IdrisThreadClock> THREAD_INSTANCE =
        ThreadLocal.withInitial(() -> new IdrisThreadClock(System.nanoTime()));

    private final long time;

    private IdrisThreadClock(long time) {
        this.time = time;
    }

    public static IdrisThreadClock getInstance() {
        return THREAD_INSTANCE.get();
    }

    @Override
    public long getSeconds() {
        return SECONDS.convert(time - Runtime.START_TIME, NANOSECONDS);
    }

    @Override
    public long getNanoSeconds() {
        return time - Runtime.START_TIME % 1000000000L;
    }
}
