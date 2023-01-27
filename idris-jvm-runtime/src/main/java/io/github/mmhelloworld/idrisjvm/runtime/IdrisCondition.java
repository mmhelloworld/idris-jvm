package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

import static java.util.concurrent.TimeUnit.MICROSECONDS;

public class IdrisCondition {
    private Lock lock;
    private Condition condition;

    public void await(Lock mutex) throws InterruptedException {
        initialize(mutex);
        condition.await();
    }

    private void initialize(Lock mutex) {
        if (this.lock == null) {
            this.lock = mutex;
            this.condition = lock.newCondition();
        }
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
}
