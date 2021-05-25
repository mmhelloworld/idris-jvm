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
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.lang.System.currentTimeMillis;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;

public class Runtime {
    public static final String OS_NAME;

    private static List<String> programArgs = emptyList();
    private static BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in, UTF_8));

    private static boolean isStdinEof = false;

    static {
        // To conform to support/chez/support.ss
        String osNameProperty = getOsNameProperty();
        if (osNameProperty.startsWith("windows")) {
            OS_NAME = "windows";
        } else if (osNameProperty.startsWith("mac")) {
            OS_NAME = "darwin";
        } else if (Stream.of("linux", "aix", "solaris", "sunos", "freebsd", "openbsd", "netbsd")
            .anyMatch(osNameProperty::startsWith)) {
            OS_NAME = "unix";
        } else {
            OS_NAME = "unknown";
        }
    }

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
        String[] commandParts = getCommand(command);
        return new ProcessBuilder(commandParts)
            .directory(new File(Files.getWorkingDir()))
            .start()
            .waitFor();
    }

    public static void usleep(int microseconds) throws InterruptedException {
        TimeUnit.MICROSECONDS.sleep(microseconds);
    }

    private static int bounded(int n, int lowerBound, int upperBound) {
        return n < lowerBound ? lowerBound : Math.min(n, upperBound);
    }
    private static String getOsNameProperty() {
        try {
            return System.getProperty("os.name").toLowerCase(Locale.ROOT);
        } catch (SecurityException exception) {
            return "";
        }
    }

    private static String[] getCommand(String command) {
        boolean isWindows = OS_NAME.equals("windows");
        String shell = isWindows ? "cmd.exe" : "sh";
        String shellSwitch = isWindows ? "/c" : "-c";
        Pattern pattern = Pattern.compile(format("\\s*%s\\s+%s\\s+(.*)", shell, shellSwitch));
        Matcher matcher = pattern.matcher(command);
        if (matcher.find()) {
            return new String[] {shell, shellSwitch, matcher.group(1)};
        }
        pattern = Pattern.compile(format("\\s*%s\\s+(.*)", shell));
        matcher = pattern.matcher(command);
        if (matcher.find()) {
            return new String[]{shell, shellSwitch, matcher.group(1)};
        }
        return new String[] {shell, shellSwitch, command};
    }

}
