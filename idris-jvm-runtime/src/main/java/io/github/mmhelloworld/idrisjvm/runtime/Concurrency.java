package io.github.mmhelloworld.idrisjvm.runtime;

public final class Concurrency {
    private static final ThreadLocal<Object> THREAD_LOCAL = new InheritableThreadLocal<>();

    private Concurrency() {
    }

    public static Object getThreadData() {
        return THREAD_LOCAL.get();
    }

    public static void setThreadData(Object value) {
        THREAD_LOCAL.set(value);
    }
}
