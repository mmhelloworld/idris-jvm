package io.github.mmhelloworld.idris2.runtime;

import java.nio.channels.Channels;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.Future;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.concurrent.ForkJoinPool.commonPool;
import static java.util.stream.Collectors.toList;

public final class Runtime {
    static final ChannelIo stdin = new ChannelIo(null, Channels.newChannel(System.in));
    static final ChannelIo stdout = new ChannelIo(null, Channels.newChannel(System.out));
    static final ChannelIo stderr = new ChannelIo(null, Channels.newChannel(System.err));
    private static final ThreadLocal<Integer> ERROR_NUMBER = ThreadLocal.withInitial(() -> 0);
    public static final long START_TIME = System.nanoTime();
    private static IdrisList programArgs;

    private Runtime() {
    }

    public static IdrisList getProgramArgs() {
        return programArgs;
    }

    public static void setProgramArgs(String programName, String[] args) {
        if (programArgs == null) {
            programArgs = IdrisList.fromIterable(Stream.concat(Stream.of(programName), Stream.of(args))
                .collect(toList()));
        }
    }

    public static Object getStdin() {
        return stdin;
    }

    public static Object getStdout() {
        return stdout;
    }

    public static Object getStderr() {
        return stderr;
    }

    public static <T> T crash(String message) {
        throw new RuntimeException(message);
    }

    public static <T> T nullValue() {
        return null;
    }

    public static int getErrorNumber() {
        return ERROR_NUMBER.get();
    }

    static void setErrorNumber(int errorNumber) {
        ERROR_NUMBER.set(errorNumber);
    }

    public static IntThunk createThunk(int value) {
        return new IntThunkResult(value);
    }

    public static IntThunk unboxToIntThunk(Thunk value) {
        return () -> value;
    }

    public static DoubleThunk unboxToDoubleThunk(Thunk value) {
        return () -> value;
    }

    public static DoubleThunk createThunk(double value) {
        return new DoubleThunkResult(value);
    }

    public static Thunk createThunk(Object value) {
        return value instanceof Thunk ? (Thunk) value : new ObjectThunkResult(value);
    }

    public static Object unwrap(Object possibleThunk) {
        if (possibleThunk instanceof Thunk) {
            return ((Thunk) possibleThunk).getObject();
        } else {
            return possibleThunk;
        }
    }

    public static Object force(Object delayed) {
        return unwrap(((Delayed) unwrap(delayed)).evaluate());
    }

    public static int unwrapIntThunk(Object possibleThunk) {
        if (possibleThunk instanceof Thunk) {
            return ((Thunk) possibleThunk).getInt();
        } else {
            return (int) possibleThunk;
        }
    }

    public static double unwrapDoubleThunk(Object possibleThunk) {
        if (possibleThunk instanceof Thunk) {
            return ((Thunk) possibleThunk).getDouble();
        } else {
            return (double) possibleThunk;
        }
    }

    public static ForkJoinTask<?> fork(Function<Object, Object> action) {
        return commonPool().submit((Runnable) () -> action.apply(0));
    }

    public static void waitForFuturesToComplete(List<? extends Future<?>> tasks) {
        tasks.forEach(future -> {
            try {
                future.get();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                e.printStackTrace();
            } catch (ExecutionException e) {
                e.printStackTrace();
            }
        });
    }

    public static String getCurrentThreadName() {
        return Thread.currentThread().getName();
    }
}
