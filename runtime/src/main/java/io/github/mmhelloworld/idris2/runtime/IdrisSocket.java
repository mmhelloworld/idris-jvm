package io.github.mmhelloworld.idris2.runtime;

import java.io.Closeable;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.BindException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NoRouteToHostException;
import java.net.ProtocolException;
import java.net.StandardProtocolFamily;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.AbstractSelectableChannel;
import java.nio.file.AccessDeniedException;
import java.nio.file.NoSuchFileException;

import static java.nio.channels.SelectionKey.OP_ACCEPT;
import static java.nio.channels.SelectionKey.OP_READ;
import static java.nio.channels.SelectionKey.OP_WRITE;
import static java.nio.charset.StandardCharsets.UTF_8;

public final class IdrisSocket implements Closeable {

    private static final int STREAM_SOCKET_TYPE = 1;
    private static final int DATAGRAM_SOCKET_TYPE = 2;
    private static final int INET_PROTOCOL_FAMILY = 2;
    private static final int INET6_PROTOCOL_FAMILY = 10;
    private AbstractSelectableChannel channel;
    private Exception exception;
    private int socketType;

    public IdrisSocket(int socketType, AbstractSelectableChannel channel) throws IOException {
        this.socketType = socketType;
        if (channel != null) {
            initialize(channel);
        }
    }

    public static IdrisSocket create(int socketFamily, int socketType, int protocolNumber) {
        try {
            switch (socketType) { // socket type represents idris socket type values from Network.Socket.Data.idr
                case STREAM_SOCKET_TYPE:
                    return new IdrisSocket(socketType, null);
                case DATAGRAM_SOCKET_TYPE:
                    DatagramChannel channel = DatagramChannel.open(
                        socketFamily == INET6_PROTOCOL_FAMILY ? StandardProtocolFamily.INET6 :
                            StandardProtocolFamily.INET);
                    channel.configureBlocking(false);
                    return new IdrisSocket(socketType, channel);
                default:
                    Runtime.setErrorNumber(ErrorCodes.UNSUPPORTED_SOCKET_TYPE);
                    return null;
            }
        } catch (Exception exception) {
            Runtime.setErrorNumber(getErrorNumber(exception));
            return null;
        }
    }

    public static Object createSocketAddress() {
        return new Object[1];
    }

    public static void free(Object ptr) {
    }

    public static int getSocketAddressFamily(Object socketAddressPointer) {
        InetAddress socketAddress = (InetAddress) ((Object[]) socketAddressPointer)[0];
        return socketAddress instanceof Inet6Address ? INET6_PROTOCOL_FAMILY : INET_PROTOCOL_FAMILY;
    }

    public static String getSocketAddressHostName(Object socketAddressPointer) {
        InetAddress socketAddress = (InetAddress) ((Object[]) socketAddressPointer)[0];
        return socketAddress.getHostAddress();
    }

    public int bind(int socketFamily, int socketType, String hostName, int port) {
        return withExceptionHandling(() -> {
            InetSocketAddress socketAddress = new InetSocketAddress(hostName, port);
            switch (socketType) {
                case STREAM_SOCKET_TYPE:
                    channel = ServerSocketChannel.open();
                    ((ServerSocketChannel) channel).socket().bind(socketAddress);
                    initialize(channel);
                    break;
                case DATAGRAM_SOCKET_TYPE:
                    ((DatagramChannel) channel).socket().bind(socketAddress);
                    break;
                default:
                    int errorCode = ErrorCodes.UNSUPPORTED_SOCKET_TYPE;
                    Runtime.setErrorNumber(errorCode);
                    return errorCode;
            }
            this.socketType = socketType;
            Server.register(channel, OP_ACCEPT);
            Server.start();
            return 0;
        }, -1);
    }

    public int connect(int socketFamily, int socketType, String hostName, int port) {
        return withExceptionHandling(() -> {
            InetAddress inetAddress = InetAddress.getByName(hostName);
            switch (socketType) {
                case STREAM_SOCKET_TYPE:
                    channel = SocketChannel.open(new InetSocketAddress(inetAddress, port));
                    initialize(channel);
                    return 0;
                case DATAGRAM_SOCKET_TYPE:
                    ((DatagramChannel) channel).socket().connect(inetAddress, port);
                    return 0;
                default:
                    return -1;
            }
        }, -1);
    }

    public IdrisSocket accept(Object address) {
        return withExceptionHandling(() -> {
            SocketChannel client = Server.acceptClient();
            ((Object[]) address)[0] = client.socket().getInetAddress();
            return new IdrisSocket(socketType, client);
        });
    }

    public void registerReadWrite() throws ClosedChannelException {
        SelectionKey key = Server.getKey(channel);
        if (key != null) {
            key.interestOps(OP_READ + OP_WRITE);
        } else {
            Server.register(channel, OP_READ + OP_WRITE);
        }
    }

    public int listen(int numberOfIncomingCalls) {
        return 0;
    }

    public int getSocketPort() {
        switch (socketType) {
            case STREAM_SOCKET_TYPE:
                return ((ServerSocketChannel) channel).socket().getLocalPort();
            case DATAGRAM_SOCKET_TYPE:
                return ((DatagramChannel) channel).socket().getLocalPort();
            default:
                return -1;
        }
    }

    public ChannelIo toFile(String mode) {
        SocketChannel clientChannel = (SocketChannel) channel;
        return new ChannelIo(channel, new ByteBufferIo(clientChannel::read, clientChannel::write));
    }

    public int send(String data) {
        return withExceptionHandling(() ->
            ((SocketChannel) channel).write(UTF_8.encode(data)), -1);
    }

    public String receive(int length) {
        return withExceptionHandling(() -> {
            ByteBuffer buffer = ByteBuffer.allocate(length * Character.BYTES);
            SocketChannel channel = (SocketChannel) this.channel;
            do {
                buffer.rewind();
            } while (channel.read(buffer) <= 0);

            StringBuilder builder = new StringBuilder(length);
            int read;
            do {
                buffer.flip();
                builder.append(UTF_8.decode(buffer));
                buffer.rewind();
                read = channel.read(buffer);
            } while (read > 0 && builder.length() < length);
            return builder.toString();
        });
    }

    @Override
    public void close() {
        withExceptionHandling(() -> {
            channel.close();
            return null;
        });
    }

    public void handleException(Exception e) {
        this.exception = e;
        if (exception != null) {
            exception.printStackTrace();
        }
        Runtime.setErrorNumber(getErrorNumber(e));
    }

    private void initialize(AbstractSelectableChannel channel) throws IOException {
        this.channel = channel;
        channel.configureBlocking(false);
        Server.register(channel, channel.validOps());
        ClientSocketReaderState readerState = new ClientSocketReaderState(ByteBuffer.allocate(1024 * 8),
            new ResettableCountDownLatch(1));
        Server.putState(channel, readerState);
        ClientSocketWriterState writerState = new ClientSocketWriterState(ByteBuffer.allocate(1024 * 8),
            new ResettableCountDownLatch(1));
        Server.putState(channel, writerState);
    }

    private <T> T withExceptionHandling(SupplierE<T, ? extends Exception> action) {
        exception = null;
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception e) {
            handleException(e);
            return null;
        }
    }

    private int withExceptionHandling(IntSupplierE<? extends Exception> action) {
        return withExceptionHandling(action, 0);
    }

    private int withExceptionHandling(IntSupplierE<? extends Exception> action, int fallback) {
        exception = null;
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception exception) {
            handleException(exception);
            return fallback;
        }
    }

    private boolean withExceptionHandling(BooleanSupplierE<? extends Exception> action, boolean fallback) {
        exception = null;
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception exception) {
            handleException(exception);
            return fallback;
        }
    }

    private long withExceptionHandling(LongSupplierE<? extends Exception> action, long fallback) {
        exception = null;
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception exception) {
            handleException(exception);
            return fallback;
        }
    }

    static int getErrorNumber(Exception exception) {
        if (exception != null) {
            exception.printStackTrace();
        }
        // To return error codes to conform to Idris functions with C FFIs
        if (exception == null) {
            return 0;
        } else if (exception instanceof FileNotFoundException || exception instanceof NoSuchFileException) {
            return ErrorCodes.NO_SUCH_FILE;
        } else if (exception instanceof InterruptedIOException || exception instanceof InterruptedException) {
            Thread.currentThread().interrupt();
            return ErrorCodes.INTERRUPTED;
        } else if (exception instanceof AccessDeniedException || exception instanceof SecurityException) {
            return ErrorCodes.SOCKET_ACCESS_DENIED;
        } else if (exception instanceof BindException) {
            return ErrorCodes.CANNOT_ASSIGN_REQUESTED_ADDRESS;
        } else if (exception instanceof ProtocolException) {
            return ErrorCodes.PROTOCOL_ERROR;
        } else if (exception instanceof NoRouteToHostException || exception instanceof UnknownHostException) {
            return ErrorCodes.NO_ROUTE_TO_HOST;
        } else {
            return ErrorCodes.IO_ERROR;
        }
    }
}
