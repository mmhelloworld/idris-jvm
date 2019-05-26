package io.github.mmhelloworld.idrisjvm.io;

import java.io.Closeable;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static java.nio.channels.SelectionKey.OP_ACCEPT;
import static java.nio.channels.SelectionKey.OP_READ;
import static java.nio.channels.SelectionKey.OP_WRITE;
import static java.util.concurrent.CompletableFuture.completedFuture;
import static java.util.concurrent.CompletableFuture.supplyAsync;

public final class ServerSocket implements Runnable, Closeable {

    private final Map<SocketChannel, ClientSocketReaderState> readerStates = new HashMap<>();
    private final Map<SocketChannel, ClientSocketWriterState> writerStates = new HashMap<>();
    private final ExecutorService executorService = Executors.newFixedThreadPool(
        java.lang.Runtime.getRuntime().availableProcessors() * 2);
    private final BlockingQueue<SocketChannel> newConnections = new ArrayBlockingQueue<>(10);
    private Selector selector;
    private ServerSocketChannel serverChannel;

    public ServerSocket() throws IOException {
        selector = SelectorProvider.provider().openSelector();
    }

    public void bind(InetAddress host, int port) throws IOException {
        serverChannel = ServerSocketChannel.open();
        serverChannel.configureBlocking(false);
        serverChannel.socket()
            .bind(new InetSocketAddress(host, port));
        serverChannel.register(selector, OP_ACCEPT);
    }

    public void listen() {
        new Thread(this).start();
    }

    public Future<SocketChannel> accept() throws ClosedChannelException {
        return supplyAsync(() -> {
            try {
                return newConnections.take();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }, executorService);
    }

    public Future<Integer> read(SocketChannel socketChannel, ByteBuffer buffer) {
        SelectionKey selectionKey = socketChannel.keyFor(selector);
        if (selectionKey == null || !selectionKey.isValid()) {
            return completedFuture(null);
        }
        CountDownLatch readSignal = new CountDownLatch(1);
        ClientSocketReaderState state = new ClientSocketReaderState(buffer, readSignal);
        readerStates.put(socketChannel, state);
        selectionKey.interestOps(OP_READ);
        return supplyAsync(() -> {
            try {
                readSignal.await();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            return state.getBytesRead();
        }, executorService);
    }

    public Future<Integer> write(SocketChannel socketChannel, ByteBuffer buffer) {
        CountDownLatch writeSignal = new CountDownLatch(1);
        ClientSocketWriterState state = new ClientSocketWriterState(buffer, writeSignal);
        writerStates.put(socketChannel, state);
        return supplyAsync(() -> {
            try {
                socketChannel.keyFor(selector).interestOps(OP_WRITE);
                selector.wakeup();
                writeSignal.await();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            return state.getBytesWritten();
        }, executorService);
    }

    @Override
    public void run() {
        while (serverChannel.isOpen() && selector.isOpen()) {
            try {
                selector.select();
                if (!selector.isOpen()) {
                    break;
                }
                Iterator<SelectionKey> selectedKeys = selector.selectedKeys().iterator();
                while (selectedKeys.hasNext()) {
                    SelectionKey key = selectedKeys.next();
                    selectedKeys.remove();
                    processKey(key);
                }
            } catch (IOException e) {
                e.printStackTrace();
                break;
            }
        }
    }

    private void processKey(SelectionKey key) throws IOException {
        // Check what event is available and deal with it
        if (key.isValid() && key.isAcceptable()) {
            doAccept(key);
        }

        if (key.isValid() && key.isReadable()) {
            doRead(key);
        }

        if (key.isValid() && key.isWritable()) {
            doWrite(key);
        }
    }

    private void doRead(SelectionKey key) {
        SocketChannel channel = (SocketChannel) key.channel();
        ClientSocketReaderState state = readerStates.get(channel);
        if (state == null) {
            return;
        }
        CountDownLatch readSignal = state.getDoneSignal();
        ByteBuffer readBuffer = state.getBuffer();
        readBuffer.clear();
        int read;
        try {
            read = channel.read(readBuffer);
        } catch (IOException e) {
            key.cancel();
            closeClient(channel);
            return;
        }

        if (read == -1) {
            closeClient(channel);
            key.cancel();
        }
        state.setBytesRead(read);
        readSignal.countDown();
    }

    private void closeClient(SocketChannel channel) {
        try {
            channel.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        ClientSocketReaderState readerState = readerStates.get(channel);
        ClientSocketWriterState writerState = writerStates.get(channel);
        if (readerState != null) {
            readerState.getDoneSignal().countDown();
            readerStates.remove(channel);
        }
        if (writerState != null) {
            writerState.getDoneSignal().countDown();
            writerStates.remove(channel);
        }
    }

    private void doAccept(SelectionKey key) throws IOException {
        ServerSocketChannel serverSocketChannel = (ServerSocketChannel) key.channel();

        SocketChannel socketChannel = serverSocketChannel.accept();
        socketChannel.configureBlocking(false)
            .register(selector, OP_READ + OP_WRITE);

        newConnections.offer(socketChannel);
    }

    private void doWrite(SelectionKey key) throws IOException {
        SocketChannel socketChannel = (SocketChannel) key.channel();
        ClientSocketWriterState state = writerStates.get(socketChannel);
        if (state == null) {
            return;
        }
        int bytesWritten = socketChannel.write(state.getBuffer());
        state.setBytesWritten(bytesWritten);
        state.getDoneSignal().countDown();
        key.interestOps(OP_READ);
    }

    @Override
    public void close() throws IOException {
        selector.close();
        serverChannel.close();
        HashSet<SocketChannel> channels = new HashSet<>(readerStates.keySet());
        channels.addAll(writerStates.keySet());
        channels.forEach(this::closeClient);
        executorService.shutdownNow();
    }

    public ServerSocketChannel getServerChannel() {
        return serverChannel;
    }
}
