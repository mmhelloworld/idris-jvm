package io.github.mmhelloworld.idrisjvm.runtime;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Path;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.assertj.core.api.Assertions.assertThat;

@Disabled
class IdrisSocketTest {

  private static final int AF_UNIX = 1;
  private static final int AF_INET = 2;
  private static final int STREAM_SOCKET_TYPE = 1;

  @TempDir
  Path tempDir;

  private ExecutorService executor;

  @BeforeEach
  void setUp() {
    executor = Executors.newSingleThreadExecutor();
  }

  @AfterEach
  void tearDown() {
    executor.shutdownNow();
  }

  // Mirrors the AF_UNIX branch in Echo.idr: server accepts, receives, echoes;
  // client connects, sends, receives the echo.
  @Test
  @Timeout(10)
  void echoOverUnixSocket() throws Exception {
    String socketPath = tempDir.resolve("idris2_test.socket").toString();
    String clientMsg = "hello world from a unix socket!";
    String expectedEcho = "echo: " + clientMsg;

    IdrisSocket serverSocket = IdrisSocket.create(AF_UNIX, STREAM_SOCKET_TYPE, 0);
    assertThat(serverSocket).isNotNull();
    assertThat(serverSocket.bind(AF_UNIX, STREAM_SOCKET_TYPE, socketPath, 0)).isEqualTo(0);
    assertThat(serverSocket.listen(1)).isEqualTo(0);

    CountDownLatch serverReadyToReceive = new CountDownLatch(1);
    AtomicReference<String> serverReceived = new AtomicReference<>();

    // Server thread: accept → receive → echo back (mirrors serve() in Echo.idr)
    Future<?> serverFuture = executor.submit(() -> {
      Object addr = IdrisSocket.createSocketAddress();
      IdrisSocket accepted = serverSocket.accept(addr);
      if (accepted == null) return null;
      accepted.registerReadWrite();
      serverReadyToReceive.countDown(); // server is about to enter receive loop
      IdrisSocket.ResultPayload<String> received = accepted.receive(1024);
      serverReceived.set(received.getPayload());
      accepted.send("echo: " + received.getPayload());
      accepted.close();
      return null;
    });

    // Client: connect, send, receive echo (mirrors runClient() in Echo.idr)
    IdrisSocket clientSocket = IdrisSocket.create(AF_UNIX, STREAM_SOCKET_TYPE, 0);
    assertThat(clientSocket).isNotNull();
    assertThat(clientSocket.connect(AF_UNIX, STREAM_SOCKET_TYPE, socketPath, 0)).isEqualTo(0);
    assertThat(serverReadyToReceive.await(5, TimeUnit.SECONDS)).isTrue();
    assertThat(clientSocket.send(clientMsg)).isGreaterThan(0);

    IdrisSocket.ResultPayload<String> clientReceived = clientSocket.receive(expectedEcho.length());

    serverFuture.get(5, TimeUnit.SECONDS);

    assertThat(serverReceived.get()).isEqualTo(clientMsg);
    assertThat(clientReceived.getPayload()).isEqualTo(expectedEcho);

    clientSocket.close();
    serverSocket.close();
  }

  // Mirrors the AF_INET branch in Echo.idr with byte buffers.
  @Test
  @Timeout(10)
  void echoOverInetSocket() throws Exception {
    String clientMsg = "hello world from a ipv4 socket!";
    String expectedEcho = "echo: " + clientMsg;
    byte[] clientMsgBytes = clientMsg.getBytes(UTF_8);
    byte[] echoBytes = expectedEcho.getBytes(UTF_8);

    IdrisSocket serverSocket = IdrisSocket.create(AF_INET, STREAM_SOCKET_TYPE, 0);
    assertThat(serverSocket).isNotNull();
    assertThat(serverSocket.bind(AF_INET, STREAM_SOCKET_TYPE, "localhost", 0)).isEqualTo(0);
    assertThat(serverSocket.listen(1)).isEqualTo(0);
    int port = serverSocket.getPort();

    CountDownLatch serverReadyToReceive = new CountDownLatch(1);
    AtomicReference<byte[]> serverReceived = new AtomicReference<>();

    Future<?> serverFuture = executor.submit(() -> {
      Object addr = IdrisSocket.createSocketAddress();
      IdrisSocket accepted = serverSocket.accept(addr);
      if (accepted == null) return null;
      accepted.registerReadWrite();
      serverReadyToReceive.countDown();
      byte[] buf = new byte[1024];
      IdrisSocket.ResultPayload<byte[]> received = accepted.receive(buf, clientMsgBytes.length);
      serverReceived.set(received.getPayload());
      byte[] reply = ("echo: " + new String(received.getPayload(), 0, clientMsgBytes.length, UTF_8))
        .getBytes(UTF_8);
      accepted.send(reply, reply.length);
      accepted.close();
      return null;
    });

    IdrisSocket clientSocket = IdrisSocket.create(AF_INET, STREAM_SOCKET_TYPE, 0);
    assertThat(clientSocket).isNotNull();
    assertThat(clientSocket.connect(AF_INET, STREAM_SOCKET_TYPE, "localhost", port)).isEqualTo(0);
    assertThat(serverReadyToReceive.await(5, TimeUnit.SECONDS)).isTrue();
    assertThat(clientSocket.send(clientMsgBytes, clientMsgBytes.length)).isGreaterThan(0);

    byte[] replyBuf = new byte[echoBytes.length];
    IdrisSocket.ResultPayload<byte[]> clientReceived = clientSocket.receive(replyBuf, echoBytes.length);

    serverFuture.get(5, TimeUnit.SECONDS);

    assertThat(new String(serverReceived.get(), 0, clientMsgBytes.length, UTF_8))
      .isEqualTo(clientMsg);
    assertThat(new String(clientReceived.getPayload(), 0, echoBytes.length, UTF_8))
      .isEqualTo(expectedEcho);

    clientSocket.close();
    serverSocket.close();
  }
}
