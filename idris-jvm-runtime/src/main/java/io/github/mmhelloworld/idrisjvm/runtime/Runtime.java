package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigInteger;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import static java.lang.System.currentTimeMillis;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.attribute.PosixFilePermission.GROUP_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.GROUP_READ;
import static java.nio.file.attribute.PosixFilePermission.GROUP_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_READ;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_READ;
import static java.nio.file.attribute.PosixFilePermission.OWNER_WRITE;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

public class Runtime {
    private static final boolean isPosix = FileSystems.getDefault().supportedFileAttributeViews().contains("posix");
    private static final Map<Integer, PosixFilePermission> modeToPermissions = new HashMap<>();
    private static List<String> programArgs = emptyList();
    private static BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in, UTF_8));
    private static String workingDir = System.getProperty("user.dir");
    private static boolean isStdinEof = false;

    static {
        modeToPermissions.put(256, OWNER_READ);
        modeToPermissions.put(128, OWNER_WRITE);
        modeToPermissions.put(64, OWNER_EXECUTE);
        modeToPermissions.put(32, GROUP_READ);
        modeToPermissions.put(16, GROUP_WRITE);
        modeToPermissions.put(8, GROUP_EXECUTE);
        modeToPermissions.put(4, OTHERS_READ);
        modeToPermissions.put(2, OTHERS_WRITE);
        modeToPermissions.put(1, OTHERS_EXECUTE);
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
        return line;
    }

    public static void flushStdout() {
        System.out.flush();
    }

    public static void flushStderr() {
        System.err.flush();
    }

    public static void setProgramArgs(String[] args) {
        // "java" as the executable name for the first argument to conform to Idris' getArgs function
        programArgs = Stream.concat(Stream.of("java"), Arrays.stream(args))
                .collect(toList());
    }

    public static List<String> getProgramArgs() {
        return programArgs;
    }

    public static String getTemporaryFileName() throws IOException {
        String prefix = "idris";
        Path tempFile = Files.createTempFile(prefix, null);
        tempFile.toFile().delete();
        return tempFile.toString();
    }

    public static void chmod(String file, int mode) throws IOException {
        if (isPosix) {
            Files.setPosixFilePermissions(createPath(file), createPosixFilePermissions(mode));
        }
    }

    public static boolean changeDir(String dir) {
        workingDir = createPath(dir).toString();
        return true;
    }

    public static String getWorkingDir() {
        return workingDir;
    }

    public static String showThrowable(Throwable throwable) {
        StringWriter out = new StringWriter();
        throwable.printStackTrace(new PrintWriter(out));
        return out.toString();
    }

    public static Path createPath(String pathStr) {
        Path path = Paths.get(pathStr);
        if (path.isAbsolute()) {
            return path;
        } else {
            if (pathStr.equals(".")) {
                return Paths.get(workingDir);
            } else if (pathStr.equals("..")) {
                return Paths.get(workingDir).getParent();
            } else {
                return Paths.get(workingDir, pathStr);
            }
        }
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

    private static Set<PosixFilePermission> createPosixFilePermissions(int mode) {
        return modeToPermissions.entrySet().stream()
                .filter(modeAndPermission -> (mode & modeAndPermission.getKey()) == modeAndPermission.getKey())
                .map(Entry::getValue)
                .collect(toSet());
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
        int toIndex = offset + len;
        return toIndex >= s.length() ? s.substring(offset) : s.substring(offset, toIndex);
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
        ProcessBuilder processBuilder = new ProcessBuilder(cmdarray).inheritIO();
        return processBuilder.start().waitFor();
    }

    public static void usleep(int microseconds) throws InterruptedException {
        TimeUnit.MICROSECONDS.sleep(microseconds);
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
