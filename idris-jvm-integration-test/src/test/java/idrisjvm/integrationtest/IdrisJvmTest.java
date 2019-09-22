package idrisjvm.integrationtest;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.lang.System.getProperty;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.StandardOpenOption.APPEND;
import static java.nio.file.StandardOpenOption.CREATE;
import static java.util.Comparator.comparing;
import static java.util.concurrent.TimeUnit.MINUTES;
import static java.util.stream.Collectors.toList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeTrue;

@RunWith(Parameterized.class)
public class IdrisJvmTest {
    private static final String IDRIS_JVM_HOME = Optional.ofNullable(System.getProperty("IDRIS_JVM_HOME"))
        .orElseGet(() -> Optional.ofNullable(System.getenv("IDRIS_JVM_HOME"))
            .orElseGet(() -> System.getProperty("user.home")));
    private static final Pattern FAILED_TEST_PATTERN = Pattern.compile("test\\[([^]]+)].*");
    public static final int TEST_DIR_SEARCH_DEPTH = 1;
    private static Path failedLogPath;

    @Parameter
    public String testName;

    @Parameter(1)
    public File testDir;

    @Parameter(2)
    public File expectedOutputFile;

    @Rule
    public TestWatcher testWatcher = new TestWatcher() {
        @Override
        protected void failed(Throwable e, Description description) {
            try {
                String failedTestName = format("%s%n", description.getDisplayName());
                Files.write(failedLogPath, failedTestName.getBytes(UTF_8), CREATE, APPEND);
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    };

    @Parameters(name = "{0}")
    public static Collection<Object[]> data() throws IOException {
        Path testSourceDir = getTestSourceDirectory();
        List<Object[]> testcases = getTests(testSourceDir)
            .map(Path::toFile)
            .filter(File::exists)
            .map(IdrisJvmTest::createTestCase)
            .sorted(comparing(testcase -> (String) testcase[0]))
            .collect(toList());
        getFailedLogPath().toFile().delete();
        return testcases;
    }

    @BeforeClass
    public static void beforeClass() {
        if (shouldStartIdrisJvmServer()) {
            boolean isDeleted = getPortFile().delete();
            if (!isDeleted) {
                throw new RuntimeException(format("Unable to remove port file %s", getPortFile()));
            }
        }
        failedLogPath = getFailedLogPath();
        if (!shouldRerunFailed()) {
            failedLogPath.toFile().delete();
        }
    }

    @AfterClass
    public static void afterClass() throws IOException {
        Path failedLogPath = getFailedLogPath();
        if (failedLogPath.toFile().exists()) {
            Files.write(failedLogPath, Files.lines(failedLogPath)
                .sorted()
                .collect(toList()));
        }
    }

    @Test
    public void test() throws IOException, InterruptedException {
        assumeTrue("Test has JVM test source", hasJvmTestSource(testDir));
        runTestRunner();

        List<String> actualOutput = readFile(getOutputFile()).stream()
            .map(this::stripLineNo)
            .collect(toList());

        String[] expectedOutput = readFile(expectedOutputFile).stream()
            .map(this::stripLineNo)
            .toArray(String[]::new);

        assertThat(actualOutput)
            .contains(expectedOutput);
    }

    private String stripLineNo(String line) {
        return line.replaceAll("^\\d+\\s", "");
    }

    private static Stream<Path> getTests(Path testSourceDir) throws IOException {
        if (shouldRerunFailed()) {
            return Files.lines(getFailedLogPath())
                .map(IdrisJvmTest::getFailedTestName)
                .filter(Objects::nonNull)
                .map(testName -> findTest(testSourceDir, testName))
                .filter(Objects::nonNull);
        } else {
            return Files.find(testSourceDir, TEST_DIR_SEARCH_DEPTH, (path, attr) -> attr.isDirectory());
        }
    }

    private static Path findTest(Path testSourceDir, String testName) {
        try {
            return Files.find(testSourceDir, TEST_DIR_SEARCH_DEPTH, (path, attr) -> path.toString().endsWith(testName))
                .findAny()
                .orElse(null);
        } catch (IOException e) {
            throw new IdrisTestException(format("Unable to find test %s under %s", testName, testSourceDir), e);
        }
    }

    private static String getFailedTestName(String line) {
        Matcher matcher = FAILED_TEST_PATTERN.matcher(line);
        if (matcher.find()) {
            return matcher.group(1);
        } else {
            return null;
        }
    }

    private static Path getFailedLogPath() {
        return Paths.get(getTestSourceDirectory().toString(), "failed");
    }

    private static Path getTestSourceDirectory() {
        return Paths.get(getProperty("test-source-dir"));
    }

    private static boolean shouldRerunFailed() {
        return parseBooleanProperty("rerunFailed");
    }

    private static boolean parseBooleanProperty(String propertyName) {
        String propertyValue = getProperty(propertyName, "false");
        return propertyValue.trim().isEmpty() || Boolean.parseBoolean(propertyValue);
    }

    private static boolean shouldStartIdrisJvmServer() {
        return parseBooleanProperty("start-idris-jvm-server");
    }

    private static File getPortFile() {
        return new File(IDRIS_JVM_HOME, ".idrisjvmport");
    }

    private static Object[] createTestCase(File testDir) {
        final File expected = new File(testDir, "expected");
        return new Object[]{testDir.getName(), testDir, expected};
    }

    private static boolean hasJvmTestSource(File testDir) {
        try {
            return new File(testDir, "run").exists() &&
                !testDir.toString().endsWith("-disabled") &&
                !hasCOrNode(testDir) && !hasNonJvmFfi(testDir);
        } catch (IOException e) {
            return false;
        }
    }

    private static boolean hasNonJvmFfi(File testDir) throws IOException {
        return Files.find(testDir.toPath(), 10, IdrisJvmTest::isIdrisSource)
            .anyMatch(IdrisJvmTest::isNonJvmFfi);
    }

    private static boolean isIdrisSource(Path path, BasicFileAttributes attr) {
        return attr.isRegularFile() && path.toString().endsWith(".idr");
    }

    private static boolean hasCOrNode(File testDir) {
        try {
            return Files.lines(Paths.get(testDir.getPath(), "run"))
                .anyMatch(line -> line.contains("${CC") || line.contains("node"));
        } catch (IOException e) {
            return false;
        }
    }

    private static boolean isNonJvmFfi(Path src) {
        try {
            boolean isNonJvmFfi = Files.lines(src)
                .anyMatch(line ->
                    hasFfi(line, "C")
                        || hasFfi(line, "JS") || line.contains("Ptr") || line.contains("fopen")
                        || line.contains("%include C") || line.contains("%link C") || line.contains("%lib C"));
            if (isNonJvmFfi) {
                System.out.println("Ignoring C or JS FFI test in " + src);
            }
            return isNonJvmFfi;
        } catch (IOException e) {
            return false;
        }
    }

    private static boolean hasFfi(String line, String ffi) {
        return line.contains(format("FFI_%s", ffi));
    }

    private static List<String> readFile(File f) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(f))) {
            return br.lines().collect(toList());
        }
    }

    private File getOutputFile() {
        return new File(testDir, "output");
    }

    private File getInputFile() {
        return new File(testDir, "input");
    }

    private void runTestRunner() throws IOException, InterruptedException {
        if (!new File(testDir, "run").exists()) {
            fail(format("Executable for test %s not found", testName));
        }
        File runnerInput = getInputFile();
        File runnerOut = getOutputFile();
        Process runner = runTestRunner(runnerInput, runnerOut);
        final boolean hasRunnerExited = runner.waitFor(2, MINUTES);
        if (!hasRunnerExited) {
            fail(format("Test %s timed out", testName));
        }
    }

    private Process runTestRunner(File runnerInput, File runnerOut) throws IOException {
        ProcessBuilder processBuilder = new ProcessBuilder("./run");
        processBuilder.directory(testDir);
        processBuilder.redirectErrorStream(true);
        processBuilder.redirectOutput(Redirect.to(runnerOut));
        if (runnerInput.exists()) {
            processBuilder.redirectInput(runnerInput);
        }
        return processBuilder.start();
    }
}

