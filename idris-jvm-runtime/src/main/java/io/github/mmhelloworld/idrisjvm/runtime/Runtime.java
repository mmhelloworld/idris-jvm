package io.github.mmhelloworld.idrisjvm.runtime;

import java.lang.management.ManagementFactory;
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
    public static final long START_TIME = System.nanoTime();
    static final ChannelIo stdin = new ChannelIo(null, Channels.newChannel(System.in));
    static final ChannelIo stdout = new ChannelIo(null, Channels.newChannel(System.out));
    static final ChannelIo stderr = new ChannelIo(null, Channels.newChannel(System.err));
    private static final ThreadLocal<Integer> ERROR_NUMBER = ThreadLocal.withInitial(() -> 0);
    private static final int EAGAIN = 11;
    private static String[] programArgs;
    private static IdrisList programArgsList;
    private static Exception exception;

    private Runtime() {
    }

    public static IdrisList getProgramArgs() {
        return programArgsList;
    }

    public static String getProgramArg(int index) {
        return programArgs[index];
    }

    public static int getProgramArgCount() {
        return programArgs.length;
    }

    public static void setProgramArgs(String programName, String[] args) {
        if (programArgs == null) {
            String[] argsWithProgramName = new String[args.length + 1];
            argsWithProgramName[0] = programName;
            System.arraycopy(args, 0, argsWithProgramName, 1, args.length);
            programArgs = argsWithProgramName;
            programArgsList = IdrisList.fromIterable(Stream.concat(Stream.of(programName), Stream.of(args))
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

    public static int getPid() {
        return Integer.parseInt(ManagementFactory.getRuntimeMXBean().getName().split("@")[0]);
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

    public static Exception getException() {
        return exception;
    }

    static void setException(Exception exception) {
        Runtime.exception = exception;
    }

    static void setErrorNumber(int errorNumber) {
        ERROR_NUMBER.set(errorNumber);
    }

    public static int getEagain() {
        return EAGAIN;
    }

    public static void free(Object object) {
    }

    public static IntThunk unboxToIntThunk(Thunk value) {
        return () -> value;
    }

    public static DoubleThunk unboxToDoubleThunk(Thunk value) {
        return () -> value;
    }

    public static IntThunk createThunk(int value) {
        return new IntThunkResult(value);
    }

    public static LongThunk createThunk(long value) {
        return new LongThunkResult(value);
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

    public static long unwrapLongThunk(Object possibleThunk) {
        if (possibleThunk instanceof Thunk) {
            return ((Thunk) possibleThunk).getLong();
        } else {
            return (long) possibleThunk;
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
        return commonPool().submit((Runnable) () -> {
            try {
                action.apply(0);
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    public static ForkJoinTask<?> fork(Delayed action) {
        return commonPool().submit(action::evaluate);
    }

    public static void await(ForkJoinTask<?> task) {
        task.join();
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
