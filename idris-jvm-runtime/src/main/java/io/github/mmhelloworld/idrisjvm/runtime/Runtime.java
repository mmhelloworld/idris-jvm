package io.github.mmhelloworld.idrisjvm.runtime;

import io.github.mmhelloworld.idrisjvm.io.Files;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import static java.lang.System.currentTimeMillis;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

public class Runtime {

    private static List<String> programArgs = emptyList();
    private static BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in, UTF_8));

    private static boolean isStdinEof = false;

    public static boolean isStdinEof() {
        return isStdinEof;
    }

    public static char readChar() throws IOException {
        int ch = stdin.read();
        isStdinEof = ch == -1;
        return (char) ch;
    }

    public static String readString() throws IOException {
        String line = stdin.readLine();
        isStdinEof = line == null;
        return isStdinEof ? "" : line;
    }

    public static void flushStdout() {
        System.out.flush();
    }

    public static void flushStderr() {
        System.err.flush();
    }

    public static List<String> getProgramArgs() {
        return programArgs;
    }

    public static void setProgramArgs(String[] args) {
        // "java" as the executable name for the first argument to conform to Idris' getArgs function
        programArgs = Stream.concat(Stream.of("java"), Arrays.stream(args))
                .collect(toList());
    }

    public static String showThrowable(Throwable throwable) {
        StringWriter out = new StringWriter();
        throwable.printStackTrace(new PrintWriter(out));
        return out.toString();
    }

    public static String systemInfo(int index) {
        switch (index) {
            case 0:
                return "jvm";
            case 1:
                return System.getProperty("os.name");
            default:
                return "";
        }
    }

    public static Object crash(String message) {
        System.err.println(message);
        System.exit(1);
        return null;
    }


    public static Integer writeString(Object s) {
        System.out.print(s);
        return 0;
    }

    public static int writeString(String s) {
        System.out.print(s);
        return 0;
    }

    public static String substring(String s, int offset, int len) {
        int fromIndex = Math.max(offset, 0);
        int safeLength = bounded(len, 0, s.length());
        int toIndex = bounded(fromIndex + safeLength, 0, s.length());
        return s.substring(fromIndex, toIndex);
    }

    public static Object error(Object s) {
        throw new RuntimeException(s.toString());
    }

    public static Object rethrow(Throwable t) throws Throwable {
        throw t;
    }

    public static Object unwrap(Object value) {
        while (value instanceof Thunk) {
            value = ((Thunk) value).call();
        }
        return value;
    }

    public static int constructorIndex(Object obj) {
        final int constructor;
        if (obj instanceof IdrisObject) {
            constructor = ((IdrisObject) obj).constructorId;
        } else if (obj instanceof Integer) {
            constructor = (int) obj;
        } else {
            constructor = 0;
        }
        return constructor;
    }

    public static BigInteger time() {
        return BigInteger.valueOf(Duration.ofMillis(currentTimeMillis()).getSeconds());
    }

    public static int runCommand(String command) throws IOException, InterruptedException {
        String[] cmdarray = parseCommand(command).toArray(new String[0]);
        ProcessBuilder processBuilder = new ProcessBuilder(cmdarray)
                .inheritIO()
                .directory(new File(Files.getWorkingDir()));
        return processBuilder.start().waitFor();
    }

    public static void usleep(int microseconds) throws InterruptedException {
        TimeUnit.MICROSECONDS.sleep(microseconds);
    }

    private static int bounded(int n, int lowerBound, int upperBound) {
        return n < lowerBound ? lowerBound : Math.min(n, upperBound);
    }

    // This may not be adequate but simple enough for basic cases
    private static List<String> parseCommand(final String command) {
        List<String> commandWithArgs = new ArrayList<>();
        int start = 0;
        boolean inQuotes = false;
        for (int current = 0; current < command.length(); current++) {
            if (isUnescapedDoubleQuotes(command, current)) {
                inQuotes = !inQuotes;
            }

            boolean atLastChar = current == command.length() - 1;
            if (atLastChar) {
                commandWithArgs.add(unescapeDoubleQuotes(trimDoubleQuotes(command.substring(start))));
            } else if (command.charAt(current) == ' ' && !inQuotes) {
                commandWithArgs.add(unescapeDoubleQuotes(trimDoubleQuotes(command.substring(start, current))));
                start = current + 1;
            }
        }
        return commandWithArgs;
    }

    private static boolean isUnescapedDoubleQuotes(final String str, final int index) {
        return str.charAt(index) == '"' && (index == 0 || str.charAt(index - 1) != '\\');
    }

    private static String unescapeDoubleQuotes(String str) {
        return str.replaceAll("\\\\\"", "\"");
    }

    private static String trimDoubleQuotes(String str) {
        if (str.startsWith("\"") && str.endsWith("\"")) {
            return str.substring(1, str.length() - 1);
        } else {
            return str;
        }
    }

}
