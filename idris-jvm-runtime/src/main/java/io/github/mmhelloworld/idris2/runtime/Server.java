package io.github.mmhelloworld.idris2.runtime;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channel;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.stream.Stream;

public class Server {
    private static final Map<Channel, ClientSocketReaderState> readerStates = new HashMap<>();
    private static final Map<Channel, ClientSocketWriterState> writerStates = new HashMap<>();
    private static final BlockingQueue<SocketChannel> newConnections = new ArrayBlockingQueue<>(10);
    private static Selector selector;
    private static boolean isStarted;

    static {
        try {
            selector = Selector.open();
        } catch (IOException exception) {
            exception.printStackTrace();
        }
    }

    public static synchronized void start() throws IOException {
        if (!isStarted) {
            isStarted = true;
            Thread thread = new Thread(() -> {
                try {
                    while (selector.isOpen()) {
                        try {
                            selector.select();
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
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            thread.setDaemon(true);
            thread.start();
        }

    }

    public static void stop() {
        Stream.concat(readerStates.keySet().stream(), writerStates.keySet().stream())
            .forEach(Server::closeClient);
    }

    public static SocketChannel acceptClient() throws InterruptedException {
        return newConnections.take();
    }

    public static SelectionKey register(AbstractSelectableChannel channel, int key) throws ClosedChannelException {
        return channel.register(selector, key);
    }

    public static void putState(AbstractSelectableChannel channel, ClientSocketReaderState state) {
        readerStates.put(channel, state);
    }

    public static void putState(AbstractSelectableChannel channel, ClientSocketWriterState state) {
        writerStates.put(channel, state);
    }

    public static ClientSocketReaderState getReaderState(AbstractSelectableChannel channel) {
        return readerStates.get(channel);
    }

    public static ClientSocketWriterState getWriterState(AbstractSelectableChannel channel) {
        return writerStates.get(channel);
    }

    public static SelectionKey getKey(AbstractSelectableChannel channel) {
        return channel.keyFor(selector);
    }

    public static void wakeup() {
        selector.wakeup();
    }

    private static void processKey(SelectionKey key) throws IOException, InterruptedException {
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

    private static void doAccept(SelectionKey key) throws IOException {
        ServerSocketChannel serverSocketChannel = (ServerSocketChannel) key.channel();
        SocketChannel clientChannel = serverSocketChannel.accept();
        if (clientChannel != null) {
            newConnections.offer(clientChannel);
        }
    }

    private static void doRead(SelectionKey key) throws InterruptedException {
        SocketChannel channel = (SocketChannel) key.channel();
        ClientSocketReaderState state = readerStates.get(channel);
        if (state == null) {
            return;
        }
        ByteBuffer readBuffer = state.getBuffer();
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
        state.getDoneSignal().countDown();
    }

    private static void doWrite(SelectionKey key) throws IOException {
        SocketChannel socketChannel = (SocketChannel) key.channel();
        ClientSocketWriterState state = writerStates.get(socketChannel);
        if (state == null) {
            return;
        }
        ByteBuffer buffer = state.getBuffer();
        int bytesWritten = socketChannel.write(buffer);
        state.setBytesWritten(bytesWritten);
        state.getDoneSignal().countDown();
    }

    private static void closeClient(Channel channel) {
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
}
