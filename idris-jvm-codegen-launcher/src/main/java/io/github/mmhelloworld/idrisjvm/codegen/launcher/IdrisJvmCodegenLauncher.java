package io.github.mmhelloworld.idrisjvm.codegen.launcher;

import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.client.DefaultResponseErrorHandler;
import org.springframework.web.client.RestTemplate;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import static java.lang.Integer.parseInt;
import static java.lang.String.format;
import static java.util.stream.Collectors.toList;

public class IdrisJvmCodegenLauncher {
    private static final String IDRIS_JVM_HOME = Optional.ofNullable(System.getenv("IDRIS_JVM_HOME"))
        .orElseGet(() -> System.getProperty("IDRIS_JVM_HOME", System.getProperty("user.home")));

    private final RestTemplate restTemplate;

    public IdrisJvmCodegenLauncher() {
        restTemplate = new RestTemplate();
        restTemplate.setErrorHandler(new DefaultResponseErrorHandler() {
            @Override
            public void handleError(ClientHttpResponse response) throws IOException {
                if (response.getStatusText() != null) {
                    System.err.println(response.getStatusText());
                }
            }
        });
    }

    public static void main(String[] args) throws Exception {
        new IdrisJvmCodegenLauncher().run(args);
    }

    private void run(String[] args) throws Exception {
        final boolean isServerRunning = getPort().filter(this::isServerUp).isPresent();
        if (!isServerRunning) {
            startServer();
        }
        waitForServer();
        send(args);
    }

    private void startServer() throws IOException, InterruptedException {
        final String basedir = System.getProperty("basedir");
        ProcessBuilder jvmProcessBuilder = new ProcessBuilder("java", "-jar",
            basedir + File.separator + "idris-jvm-server.jar");
        jvmProcessBuilder.redirectErrorStream(true);
        File jvmOut = new File(basedir, "idris-jvm-server.log");
        jvmProcessBuilder.redirectOutput(ProcessBuilder.Redirect.to(jvmOut));
        jvmProcessBuilder.start();
    }

    private void waitForServer() {
        CountDownLatch countDown = new CountDownLatch(1);
        Timer timer = new Timer("idrisjvm-codegen-server-wait-timer");
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                getPort()
                    .filter(IdrisJvmCodegenLauncher.this::isServerUp)
                    .ifPresent(port -> {
                        timer.cancel();
                        countDown.countDown();
                    });
            }

        }, 0, 100);
        boolean hasStarted;
        try {
            hasStarted = countDown.await(Duration.ofSeconds(30).toMillis(), TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            hasStarted = false;
        }
        timer.cancel();
        if (!hasStarted) {
            throw new RuntimeException("Timed out waiting for Idris JVM server!");
        }
    }

    private void send(final String[] args) {
        List<String> endpointArgs = new ArrayList<>(Arrays.asList(args));
        endpointArgs.add(System.getProperty("user.dir"));
        int port = getPort().orElseThrow(() -> new RuntimeException("Idris JVM codegen server is not running"));
        ResponseEntity<String> response = restTemplate.postForEntity("http://localhost:" + port, new HttpEntity<>(endpointArgs), String.class);
        if (!response.getStatusCode().is2xxSuccessful()) {
            System.err.println(response.getBody());
        }
    }

    private List<String> readFile(File f) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(f))) {
            return br.lines().collect(toList());
        }
    }

    private Optional<Integer> getPort() {
        try {
            return Optional.of(parseInt(readFile(new File(IDRIS_JVM_HOME, ".idrisjvmport")).get(0)));
        } catch (IOException e) {
            return Optional.empty();
        }
    }

    private boolean isServerUp(int port) {
        try {
            final String url = format("http://localhost:%d/health", port);
            final String status = (String) restTemplate.getForEntity(url, Map.class)
                .getBody()
                .get("status");
            return "UP".equals(status);
        } catch (Exception ignore) {

        }
        return false;
    }
}
