package idrisjvm;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Stream;

import static java.io.File.pathSeparator;
import static java.lang.Integer.parseInt;
import static java.util.stream.Collectors.toList;
import static org.hamcrest.CoreMatchers.hasItems;
import static org.junit.Assert.assertThat;

@RunWith(Parameterized.class)
public class IdrisJvmTest {
    private static final String IDRIS_JVM_HOME = System.getProperty("IDRIS_JVM_HOME", System.getProperty("user.home"));

    private static File testOutputRootDir;
    private static String runtimeJarPath;

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
        getPortFile().delete();
        testOutputRootDir = new File(System.getProperty("test.output", System.getProperty("user.dir")));
        runtimeJarPath = Paths.get(System.getProperty("runtime.jar.path")).toString();
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

    private void compile(final File testOutputDir, final File compilerOut) throws IOException, InterruptedException {
        ProcessBuilder idrisCompilerProcessBuilder = new ProcessBuilder("idris", "--portable-codegen", "jvm", "-p",
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

    private void run(final File testOutputDir, final File jvmOut) throws IOException, InterruptedException {
        String classpath = runtimeJarPath + pathSeparator + testOutputDir.getPath();
        ProcessBuilder jvmProcessBuilder = new ProcessBuilder("java", "-cp", classpath, "main.Main");
        jvmProcessBuilder.redirectErrorStream(true);
        jvmProcessBuilder.redirectOutput(Redirect.to(jvmOut));
        Process jvm = jvmProcessBuilder.start();
        final boolean hasJvmExited = jvm.waitFor(2, TimeUnit.MINUTES);
        if (!hasJvmExited) {
            Files.copy(jvmOut.toPath(), System.err);
            throw new RuntimeException("JVM timed out!");
        }
        if (jvm.exitValue() != 0) {
            Files.copy(jvmOut.toPath(), System.err);
            throw new RuntimeException("Runtime error!");
        }
    }

    private static Optional<Integer> getPort() {
        try {
            return Optional.of(parseInt(readFile(getPortFile()).get(0)));
        } catch (IOException e) {
            return Optional.empty();
        }
    }

    private static File getPortFile() {
        return new File(IDRIS_JVM_HOME, ".idrisjvmport");
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

    private static List<String> readFile(File f) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(f))) {
            return br.lines().collect(toList());
        }
    }
}

