package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.management.ManagementFactory;
import java.nio.channels.Channels;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.Future;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;

public final class Runtime {
    public static final long START_TIME = System.nanoTime();
    static final ChannelIo STDIN = new ChannelIo(null, Channels.newChannel(System.in));
    static final ChannelIo STDOUT = new ChannelIo(null, Channels.newChannel(System.out));
    static final ChannelIo STDERR = new ChannelIo(null, Channels.newChannel(System.err));
    private static final ForkJoinPool FORK_JOIN_POOL = new ForkJoinPool(
        java.lang.Runtime.getRuntime().availableProcessors() * 2);
    private static final ThreadLocal<Integer> ERROR_NUMBER = ThreadLocal.withInitial(() -> 0);
    private static final int EAGAIN = 11;
    private static String[] programArgs;
    private static IdrisList programArgsList;
    private static Exception exception;

    private Runtime() {
    }

    public static Object tailRec(Object fObj, Object initialValueObj) {
        Function<IdrisObject, IdrisObject> f = (Function) fObj;
        IdrisObject obj = (IdrisObject) initialValueObj;
        while (obj.getConstructorId() != 0) { // until `TcDone` constructor
            obj = f.apply(obj);
        }
        return obj.getProperty(0); // return the result from `TcDone`
    }

    /**
     * Mutable trampoline frame for mutually tail-recursive function groups. One frame is acquired
     * per trampoline entry and reused across all iterations: {@code fn} selects the next function in
     * the group (0 = done, result in {@code args[0]}) and {@code args} carries its arguments. The
     * single concrete class keeps every dispatch and argument read monomorphic, and exhausted frames
     * return to a small per-thread pool since entries (every top-level call into a mutual group) far
     * outnumber the frames live at once.
     */
    public static final class TcFrame {
        public int fn;
        public final Object[] args;

        public TcFrame(int arity) {
            this.args = new Object[arity];
        }
    }

    private static final int TC_FRAME_POOL_LIMIT = 64;
    private static final ThreadLocal<ArrayDeque<TcFrame>> TC_FRAME_POOL =
        ThreadLocal.withInitial(ArrayDeque::new);

    public static Object tcNewFrame(int arity) {
        var size = Math.max(1, arity);
        var frame = TC_FRAME_POOL.get().pollLast();
        return frame != null && frame.args.length >= size ? frame : new TcFrame(size);
    }

    public static Object tcSet(Object frame, int index, Object value) {
        ((TcFrame) frame).args[index] = value;
        return frame;
    }

    public static Object tcSetFn(Object frame, int fn) {
        ((TcFrame) frame).fn = fn;
        return frame;
    }

    public static Object tcGet(Object frame, int index) {
        return ((TcFrame) frame).args[index];
    }

    public static int tcGetFn(Object frame) {
        return ((TcFrame) frame).fn;
    }

    public static Object tcDone(Object frame, Object value) {
        var typedFrame = (TcFrame) frame;
        typedFrame.fn = 0;
        typedFrame.args[0] = value;
        return frame;
    }

    public static Object tailRecFrame(Object fObj, Object frameObj) {
        var f = (Function<Object, Object>) fObj;
        var frame = (TcFrame) frameObj;
        while (frame.fn != 0) {
            f.apply(frame);
        }
        var result = frame.args[0];
        // clear before pooling so a parked frame does not pin the values it carried
        Arrays.fill(frame.args, null);
        var pool = TC_FRAME_POOL.get();
        if (pool.size() < TC_FRAME_POOL_LIMIT) {
            pool.offerLast(frame);
        }
        return result;
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
        return STDIN;
    }

    public static Object getStdout() {
        return STDOUT;
    }

    public static Object getStderr() {
        return STDERR;
    }

    public static int getPid() {
        return Integer.parseInt(ManagementFactory.getRuntimeMXBean().getName().split("@")[0]);
    }

    public static int getAvailableProcessors() {
        return java.lang.Runtime.getRuntime().availableProcessors();
    }

    public static <T> T crash(String message) {
        System.out.println("ERROR: " + message);
        System.exit(1);
        return null;
    }

    public static <T> T nullValue() {
        return null;
    }

    public static int getErrorNumber() {
        return ERROR_NUMBER.get();
    }

    static void setErrorNumber(int errorNumber) {
        if (errorNumber == 0) {
            setException(null);
        }
        ERROR_NUMBER.set(errorNumber);
    }

    public static Exception getException() {
        return exception;
    }

    static void setException(Exception exception) {
        Runtime.exception = exception;
    }

    public static int getEagain() {
        return EAGAIN;
    }

    public static String getErrorMessage(int errorNumber) {
        return "Error code: " + errorNumber;
    }

    public static String getStackTraceString() {
        StackTraceElement[] trace = new Throwable().getStackTrace();
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter, true);
        for (int index = 1; index < trace.length; index++) {
            StackTraceElement traceElement = trace[index];
            printWriter.println("\tat " + traceElement);
        }
        printWriter.flush();
        return stringWriter.toString();
    }

    public static void free(Object object) {
    }

    public static Object unwrap(Object possibleThunk) {
        // Method to be removed in next version
        return possibleThunk;
    }

    public static Object force(Object delayed) {
        return ((Delayed) delayed).evaluate();
    }

    public static ForkJoinTask<?> fork(Function<Object, Object> action) {
        return FORK_JOIN_POOL.submit(() -> {
            try {
                action.apply(0);
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    public static ForkJoinTask<?> fork(Delayed action) {
        return FORK_JOIN_POOL.submit(action::evaluate);
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
