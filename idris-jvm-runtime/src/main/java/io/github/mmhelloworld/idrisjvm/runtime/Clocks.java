package io.github.mmhelloworld.idrisjvm.runtime;

import java.math.BigInteger;

public final class Clocks {
    private Clocks() {
    }

    public static IdrisClock getMonotonicClock() {
        return new IdrisMonotonicClock();
    }

    public static IdrisClock getUtcClock() {
        return new IdrisUtcClock();
    }

    public static IdrisClock getProcessClock() {
        return new IdrisProcessClock();
    }

    public static IdrisClock getThreadClock() {
        return IdrisThreadClock.getInstance();
    }

    public static IdrisClock getGcCpuClock() {
        return null;
    }

    public static IdrisClock getGcRealClock() {
        return null;
    }

    public static int isValid(IdrisClock clock) {
        return clock == null ? 0 : 1;
    }

    public static BigInteger getSeconds(Object clock) {
        return BigInteger.valueOf(((IdrisClock) clock).getSeconds());
    }

    public static BigInteger getNanoSeconds(Object clock) {
        return BigInteger.valueOf(((IdrisClock) clock).getNanoSeconds());
    }
}
