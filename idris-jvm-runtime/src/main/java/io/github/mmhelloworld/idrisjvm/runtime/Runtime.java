package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.IOException;
import java.math.BigInteger;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

import static java.lang.System.currentTimeMillis;
import static java.util.Collections.emptyList;

public class Runtime {
    public static List<String> programArgs = emptyList();

    private static Scanner inputScanner = new Scanner(System.in);

    public static String readString() {
        return inputScanner.nextLine();
    }

    public static char readChar() {
        return inputScanner.nextLine().charAt(0);
    }

    public static Integer writeString(Object s) {
        System.out.print(s);
        return 0;
    }

    public static int writeString(String s) {
        System.out.print(s);
        return 0;
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
        if (obj instanceof IdrisObject) {
            return ((IdrisObject) obj).constructorId;
        } else {
            return obj == null ? 0 : (int) obj;
        }
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
