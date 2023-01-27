package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.concurrent.LinkedBlockingQueue;

public final class Concurrency {
    private static final ThreadLocal<Object> threadLocal = new InheritableThreadLocal<>();

    private Concurrency() {
    }

    public static void setThreadData(Object type, Object value) {
        threadLocal.set(value);
    }

    public static Object getThreadData(Object type) {
        return threadLocal.get();
    }

    public static Object channelGet(Object type, Object channel) throws InterruptedException {
        return ((LinkedBlockingQueue) channel).take();
    }

    public static void channelPut(Object type, Object channel, Object value) throws InterruptedException {
        ((LinkedBlockingQueue) channel).put(value);
    }
}
