package io.github.mmhelloworld.idris2.runtime;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class ResettableCountDownLatch {
    private final int initialCount;
    private final AtomicReference<CountDownLatch> latchHolder = new AtomicReference<>();

    public ResettableCountDownLatch(int count) {
        initialCount = count;
        latchHolder.set(new CountDownLatch(initialCount));
    }

    public ResettableCountDownLatch reset() {
        CountDownLatch oldLatch = latchHolder.getAndSet(new CountDownLatch(initialCount));
        if (oldLatch != null) {
            while (oldLatch.getCount() > 0L) {
                oldLatch.countDown();
            }
        }
        return this;
    }

    public int getCount() {
        return initialCount;
    }

    public void countDown() {
        latchHolder.get().countDown();
    }

    public void await() throws InterruptedException {
        latchHolder.get().await();
    }

    public boolean await(long timeout, TimeUnit unit) throws InterruptedException {
        return latchHolder.get().await(timeout, unit);
    }
}
