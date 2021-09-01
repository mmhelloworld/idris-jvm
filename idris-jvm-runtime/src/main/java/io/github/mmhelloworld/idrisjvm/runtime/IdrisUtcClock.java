package io.github.mmhelloworld.idrisjvm.runtime;

import java.time.Clock;
import java.time.Instant;

public final class IdrisUtcClock implements IdrisClock {
    private final Instant instant;

    public IdrisUtcClock() {
        instant = Clock.systemUTC().instant();
    }

    @Override
    public long getSeconds() {
        return instant.getEpochSecond();
    }

    @Override
    public long getNanoSeconds() {
        return instant.getNano() % 1000000000L;
    }
}
