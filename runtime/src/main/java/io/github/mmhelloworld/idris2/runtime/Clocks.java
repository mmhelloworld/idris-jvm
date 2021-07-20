package io.github.mmhelloworld.idris2.runtime;

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

    public static long getSeconds(Object clock) {
        return ((IdrisClock) clock).getSeconds();
    }

    public static long getNanoSeconds(Object clock) {
        return ((IdrisClock) clock).getNanoSeconds();
    }
}
