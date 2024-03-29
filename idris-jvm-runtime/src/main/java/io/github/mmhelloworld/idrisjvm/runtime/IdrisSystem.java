package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static io.github.mmhelloworld.idrisjvm.runtime.Directories.getWorkingDir;
import static java.lang.String.format;
import static java.lang.System.currentTimeMillis;
import static java.util.Locale.ROOT;
import static java.util.concurrent.TimeUnit.MICROSECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;

public final class IdrisSystem {
    public static final String OS_NAME;
    private static final Map<String, String> ENVIRONMENT_VARIABLES;
    private static final List<String> ENVIRONMENT_VARIABLE_NAMES;

    static {
        ENVIRONMENT_VARIABLES = new LinkedHashMap<>(System.getenv());
        ENVIRONMENT_VARIABLES.putAll((Map) System.getProperties());
        ENVIRONMENT_VARIABLE_NAMES = new ArrayList<>(ENVIRONMENT_VARIABLES.keySet());

        // To conform to support/chez/support.ss
        String osNameProperty = getOsNameProperty();
        if (osNameProperty.contains("mac") || osNameProperty.contains("darwin")) {
            OS_NAME = "darwin";
        } else if (osNameProperty.contains("win")) {
            OS_NAME = "windows";
        } else if (Stream.of("nix", "nux", "aix", "solaris", "sunos", "freebsd", "openbsd", "netbsd")
            .anyMatch(osNameProperty::contains)) {
            OS_NAME = "unix";
        } else {
            OS_NAME = "unknown";
        }
    }

    private IdrisSystem() {
    }

    public static int time() {
        return (int) Duration.ofMillis(currentTimeMillis()).getSeconds();
    }

    public static int runCommand(String command) throws IOException, InterruptedException {
        String[] commandParts = getCommand(command);
        return new ProcessBuilder(commandParts)
            .directory(new File(getWorkingDir()))
            .inheritIO()
            .start()
            .waitFor();
    }

    public static void usleep(int microseconds) throws InterruptedException {
        MICROSECONDS.sleep(microseconds);
    }

    public static void sleep(int seconds) throws InterruptedException {
        SECONDS.sleep(seconds);
    }

    public static String getEnv(String name) {
        return System.getProperty(name, System.getenv(name));
    }

    public static int clearEnv(String name) {
        System.clearProperty(name);
        return 0;
    }

    public static int setEnv(String name, String value, int shouldOverwrite) {
        if (shouldOverwrite == 1 || getEnv(name) == null) {
            System.setProperty(name, value);
        }
        return 0;
    }

    public static String getEnvPair(int index) {
        if (index >= ENVIRONMENT_VARIABLE_NAMES.size()) {
            return null;
        } else {
            String name = ENVIRONMENT_VARIABLE_NAMES.get(index);
            return name + "=" + ENVIRONMENT_VARIABLES.get(name);
        }
    }

    public static void exit(int exitCode) {
        System.exit(exitCode);
    }

    public static String getOsName() {
        return OS_NAME;
    }

    public static Object malloc(int length) {
        return new byte[length];
    }

    public static String[] getCommand(String command) {
        boolean isWindows = OS_NAME.equals("windows");
        String shell = isWindows ? "cmd.exe" : "sh";
        String shellSwitch = isWindows ? "/c" : "-c";
        Pattern pattern = Pattern.compile(format("\\s*%s\\s+%s\\s+(.*)", shell, shellSwitch));
        Matcher matcher = pattern.matcher(command);
        if (matcher.matches()) {
            return new String[]{shell, shellSwitch, matcher.group(1)};
        }
        pattern = Pattern.compile(format("\\s*%s\\s+(.*)", shell));
        matcher = pattern.matcher(command);
        if (matcher.matches()) {
            return new String[]{shell, shellSwitch, matcher.group(1)};
        }
        return new String[]{shell, shellSwitch, command};
    }

    private static String getOsNameProperty() {
        try {
            return System.getProperty("os.name").toLowerCase(ROOT);
        } catch (SecurityException exception) {
            return "";
        }
    }
}
