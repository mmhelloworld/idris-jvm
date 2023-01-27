package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

import static java.util.concurrent.TimeUnit.MICROSECONDS;

public final class IdrisCondition {
    private Lock lock;
    private Condition condition;

    public void await(Lock mutex) throws InterruptedException {
        initialize(mutex);
        condition.await();
    }

    public void await(Lock mutex, int microseconds) throws InterruptedException {
        initialize(mutex);
        condition.await(microseconds, MICROSECONDS);
    }

    public void signal() {
        if (condition != null) {
            condition.signal();
        }
    }

    public void signalAll() {
        if (condition != null) {
            condition.signalAll();
        }
    }

    private void initialize(Lock mutex) {
        if (this.lock == null) {
            this.lock = mutex;
            this.condition = lock.newCondition();
        }
    }
}
