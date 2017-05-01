package mmhelloworld.idrisjvmruntime;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.TimeUnit;

import static java.lang.Runtime.getRuntime;
import static java.util.concurrent.ForkJoinTask.inForkJoinPool;

public class Concurrent {
    private static final ForkJoinPool fjpool = new ForkJoinPool(2 * getRuntime().availableProcessors());
    private static final ExecutorService executor = Executors.newCachedThreadPool();

    public static Object par(Thunk thunk) {
        Callable<Object> callable = asCallable(thunk);
        ForkJoinTask<?> task = inForkJoinPool() ? ForkJoinTask.adapt(callable).fork() : fjpool.submit(callable);
        return task.join();
    }

    private static Callable<Object> asCallable(Thunk thunk) {
        // This could be a lambda or Thunk could extend Callable but android doesn't like those
        // complaining about "Lambda coming from jar file need their interfaces on the classpath to be compiled,
        // unknown interfaces are java.util.concurrent.Callable"
        return new Callable<Object>() {
            @Override
            public Object call() throws Exception {
                return thunk.call();
            }
        };
    }

    public static Object fork(Thunk thunk) {
        return executor.submit(asCallable(thunk));
    }

    public static void shutdownExecutor() {
        executor.shutdown();
    }

    public static void executorAwaitTermination(long timeout, TimeUnit timeUnit) {
        try {
            executor.awaitTermination(timeout, timeUnit);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
