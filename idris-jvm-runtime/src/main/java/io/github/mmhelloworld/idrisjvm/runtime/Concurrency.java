package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.concurrent.LinkedBlockingQueue;

public final class Concurrency {
    private static final ThreadLocal<Object> THREAD_LOCAL = new InheritableThreadLocal<>();

    private Concurrency() {
    }

    public static void setThreadData(Object type, Object value) {
        THREAD_LOCAL.set(value);
    }

    public static Object getThreadData(Object type) {
        return THREAD_LOCAL.get();
    }

    public static Object channelGet(Object type, Object channel) throws InterruptedException {
        return ((LinkedBlockingQueue) channel).take();
    }

    public static void channelPut(Object type, Object channel, Object value) throws InterruptedException {
        ((LinkedBlockingQueue) channel).put(value);
    }
}
