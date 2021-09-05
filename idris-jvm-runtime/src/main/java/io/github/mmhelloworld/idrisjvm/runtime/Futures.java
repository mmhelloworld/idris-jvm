package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public final class Futures {
    private Futures() {
    }

    public static Object await(Object ty, Future<Object> future) throws ExecutionException, InterruptedException {
        return future.get();
    }
}
