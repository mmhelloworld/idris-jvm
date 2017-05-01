package idrisjvm;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.web.client.RestTemplate;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import static java.io.File.pathSeparator;
import static java.util.stream.Collectors.toList;
import static org.hamcrest.CoreMatchers.hasItems;
import static org.junit.Assert.assertThat;

@RunWith(Parameterized.class)
public class IdrisJvmTest {

    private static File testOutputRootDir;

    @Parameter
    public String testName;

    @Parameter(1)
    public File sourceFile;

    @Parameter(2)
    public File expectedOutputFile;

    @Parameters(name = "{0}")
    public static Collection<Object[]> data() throws IOException {
        final PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        final Resource[] resources = resolver.getResources("idris-test-sources");
        return Arrays.stream(resources)
            .flatMap(IdrisJvmTest::getTestDirs)
            .map(IdrisJvmTest::getTestFiles)
            .collect(toList());
    }

    @BeforeClass
    public static void beforeClass() throws InterruptedException, TimeoutException {
        CountDownLatch countDown = new CountDownLatch(1);
        final RestTemplate restTemplate = new RestTemplate();
        Timer timer = new Timer("idrisjvm-codegen-server-wait-timer");
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                checkServerStatus(countDown, restTemplate, timer);
            }
        }, Duration.ofSeconds(2).toMillis(), Duration.ofSeconds(2).toMillis());
        final boolean hasStarted = countDown.await(Duration.ofSeconds(30).toMillis(), TimeUnit.MILLISECONDS);
        timer.cancel();
        if (!hasStarted) {
            throw new TimeoutException("Timed out waiting for Idris JVM server!");
        }
        testOutputRootDir = new File(System.getProperty("test.output", System.getProperty("user.dir")));
    }

    private static void checkServerStatus(final CountDownLatch countDown,
                                          final RestTemplate restTemplate,
                                          final Timer timer) {
        try {
            final String status = restTemplate.getForEntity("http://localhost:8081/health", IdrisJvmServerStatus.class)
                .getBody()
                .getStatus();
            if (status.equals("UP")) {
                timer.cancel();
                countDown.countDown();
            }
        } catch (Exception ignore) {

        }
    }

    @Test
    public void test() throws IOException, InterruptedException {
        File testOutputDir = new File(testOutputRootDir, testName);
        testOutputDir.mkdir();
        File compilerOut = new File(testOutputDir, "compiler.log");
        File jvmOut = new File(testOutputDir, "jvm.log");

        compile(testOutputDir, compilerOut);
        run(testOutputDir, jvmOut);

        List<String> actualOutput = readFile(jvmOut);
        List<String> expectedOutput = readFile(expectedOutputFile);

        assertThat(actualOutput, hasItems(expectedOutput.toArray(new String[expectedOutput.size()])));
    }

    private void run(final File testOutputDir, final File jvmOut) throws IOException, InterruptedException {
        String homeDirName = System.getProperty("user.home");
        String runtimeJarPath = Paths.get(homeDirName, ".idrisjvm", "idris-jvm-runtime-1.0-SNAPSHOT.jar").toString();
        String classpath = runtimeJarPath + pathSeparator + testOutputDir.getPath();
        ProcessBuilder jvmProcessBuilder = new ProcessBuilder("java", "-cp", classpath, "main.Main");
        jvmProcessBuilder.redirectErrorStream(true);
        jvmProcessBuilder.redirectOutput(Redirect.to(jvmOut));
        Process jvm = jvmProcessBuilder.start();
        final boolean hasJvmExited = jvm.waitFor(30, TimeUnit.SECONDS);
        if (!hasJvmExited) {
            Files.copy(jvmOut.toPath(), System.err);
            throw new RuntimeException("JVM timed out!");
        }
        if (jvm.exitValue() != 0) {
            Files.copy(jvmOut.toPath(), System.err);
            throw new RuntimeException("Runtime error!");
        }
    }

    private void compile(final File testOutputDir, final File compilerOut) throws IOException, InterruptedException {
        ProcessBuilder idrisCompilerProcessBuilder = new ProcessBuilder("idris", "--portable-codegen", "jvmtest", "-p",
            "idrisjvmffi", "-p", "effects", sourceFile.getPath(), "-o", testOutputDir.getPath());
        idrisCompilerProcessBuilder.directory(testOutputDir);

        idrisCompilerProcessBuilder.redirectErrorStream(true);
        idrisCompilerProcessBuilder.redirectOutput(Redirect.to(compilerOut));

        Process idrisCompiler = idrisCompilerProcessBuilder.start();
        final boolean hasCompilerExited = idrisCompiler.waitFor(3, TimeUnit.MINUTES);
        if (!hasCompilerExited) {
            Files.copy(compilerOut.toPath(), System.err);
            throw new RuntimeException("Compilation timed out!");
        }
        if (idrisCompiler.exitValue() != 0) {
            Files.copy(compilerOut.toPath(), System.err);
            throw new RuntimeException("Compilation error!");
        }
    }

    private static Stream<? extends File> getTestDirs(final Resource resource) {
        try {
            return Arrays.stream(resource.getFile().listFiles());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static Object[] getTestFiles(final File testDir) {
        final File sourceFile = new File(testDir, testDir.getName() + ".idr");
        final File expected = new File(testDir, "expected");
        return new Object[]{testDir.getName(), sourceFile, expected};
    }

    private List<String> readFile(File f) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(f))) {
            return br.lines().collect(toList());
        }
    }

    public static class IdrisJvmServerStatus {
        private final String status;

        public IdrisJvmServerStatus(@JsonProperty("status") final String status) {
            this.status = status;
        }

        public String getStatus() {
            return status;
        }
    }
}

