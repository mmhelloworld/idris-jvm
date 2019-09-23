package io.github.mmhelloworld.idrisjvm.io;

import java.io.Closeable;
import java.io.IOException;
import java.net.InetAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.ExecutionException;

public final class ClientServerSocket implements ReadableByteChannel, WritableByteChannel, Closeable {
    private final ServerSocket serverSocket;
    private final SocketChannel client;
    private final ByteBufferIo byteBufferIo;

    private ClientServerSocket(ServerSocket serverSocket, SocketChannel client) {
        this.serverSocket = serverSocket;
        this.client = client;
        this.byteBufferIo = new ByteBufferIo(this::read, this::write);
    }

    public static ClientServerSocket listenAndAccept(InetAddress host, int port) throws IOException, ExecutionException,
        InterruptedException {
        ServerSocket serverSocket = new ServerSocket();
        serverSocket.bind(host, port);
        serverSocket.listen();
        return accept(serverSocket);
    }

    public static ClientServerSocket listenAndAccept(String host, int port) throws IOException, ExecutionException,
        InterruptedException {
        return listenAndAccept(InetAddress.getByName(host), port);
    }

    public static ClientServerSocket accept(ServerSocket serverSocket) throws InterruptedException, ExecutionException,
        ClosedChannelException {
        SocketChannel client = serverSocket.accept().get();
        return new ClientServerSocket(serverSocket, client);
    }

    public int write(ByteBuffer buffer) {
        try {
            return serverSocket.write(client, buffer).get();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (ExecutionException e) {
            throw new RuntimeException(e);
        }
        return 0;
    }

    public int read(ByteBuffer buffer) {
        try {
            return serverSocket.read(client, buffer).get();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } catch (ExecutionException e) {
            throw new RuntimeException(e);
        }
        return 0;
    }

    public char getChar() throws IOException {
        return byteBufferIo.getChar();
    }

    public String getLine() throws IOException {
        return byteBufferIo.getLine();
    }

    public void writeString(String str) throws IOException {
        byteBufferIo.writeString(str);
    }

    @Override
    public boolean isOpen() {
        return client.isOpen();
    }

    @Override
    public void close() throws IOException {
        serverSocket.close();
    }
}
