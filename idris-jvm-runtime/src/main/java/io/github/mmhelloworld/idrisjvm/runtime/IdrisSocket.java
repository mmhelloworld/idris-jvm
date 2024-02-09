package io.github.mmhelloworld.idrisjvm.runtime;

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
import java.net.SocketAddress;
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
    private static final int AF_UNSPEC = 0;
    private static final int AF_UNIX = 1;
    private static final int AF_INET = 2;
    private static final int AF_INET6 = 10;
    private static final int EAGAIN = 112;
    private static final int ONE_KB = 1024;
    private static final int SOCKET_BUFFER_CAPACITY_IN_KB = 8;
    private AbstractSelectableChannel channel;
    private int socketType;

    public IdrisSocket(int socketType, AbstractSelectableChannel channel) throws IOException {
        this.socketType = socketType;
        if (channel != null) {
            initialize(channel);
        }
    }

    public static IdrisSocket create(int socketFamily, int socketType, int protocolNumber) {
        Runtime.setErrorNumber(0);
        try {
            switch (socketType) { // socket type represents idris socket type values from Network.Socket.Data.idr
                case STREAM_SOCKET_TYPE:
                    return new IdrisSocket(socketType, null);
                case DATAGRAM_SOCKET_TYPE:
                    DatagramChannel channel = DatagramChannel.open(
                        socketFamily == AF_INET6 ? StandardProtocolFamily.INET6 : StandardProtocolFamily.INET);
                    channel.configureBlocking(false);
                    return new IdrisSocket(socketType, channel);
                default:
                    Runtime.setErrorNumber(ErrorCodes.UNSUPPORTED_SOCKET_TYPE);
                    return null;
            }
        } catch (Exception exception) {
            handleException(exception);
            return null;
        }
    }

    public static Object createSocketAddress() {
        return new Object[1];
    }

    public static int getSocketAddressFamily(Object socketAddressPointer) {
        SocketAddress socketAddress = (SocketAddress) ((Object[]) socketAddressPointer)[0];
        InetSocketAddress inetSocketAddress = (InetSocketAddress) socketAddress;
        return inetSocketAddress.getAddress() instanceof Inet6Address ? AF_INET6 : AF_INET;
    }

    public static String getIpv4Address(Object socketAddressPointer) {
        SocketAddress socketAddress = (SocketAddress) ((Object[]) socketAddressPointer)[0];
        InetSocketAddress inetSocketAddress = (InetSocketAddress) socketAddress;
        return inetSocketAddress.getAddress().getHostAddress();
    }

    public static int getIpv4Port(SocketAddress socketAddress) throws UnknownHostException {
        InetSocketAddress inetSocketAddress = (InetSocketAddress) socketAddress;
        InetAddress inetAddress = inetSocketAddress.getAddress();
        if (inetAddress instanceof Inet6Address) {
            return inetSocketAddress.getPort();
        } else {
            throw new UnknownHostException("Not an IpV4 address: " + socketAddress);
        }
    }

    public static int peek(Object buf, int offset) {
        byte[] bufArray = (byte[]) buf;
        return Byte.toUnsignedInt(bufArray[offset]);
    }

    public static void poke(Object buf, int offset, char value) {
        byte[] bufArray = (byte[]) buf;
        bufArray[offset] = (byte) value;
    }

    public static int getAfUnspec() {
        return AF_UNSPEC;
    }

    public static int getAfUnix() {
        return AF_UNIX;
    }

    public static int getAfInet() {
        return AF_INET;
    }

    public static int getAfInet6() {
        return AF_INET6;
    }

    public static int getEagain() {
        return EAGAIN;
    }

    private static void handleException(Exception e) {
        Runtime.setException(e);
        Runtime.setErrorNumber(getErrorNumber(e));
    }

    static int getErrorNumber(Exception exception) {
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

    public int bind(int socketFamily, int newSocketType, String hostName, int port) {
        return withExceptionHandling(() -> {
            InetSocketAddress socketAddress = new InetSocketAddress(hostName, port);
            switch (newSocketType) {
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
            this.socketType = newSocketType;
            Server.register(channel, OP_ACCEPT);
            Server.start();
            return 0;
        }, -1);
    }

    public int connect(int socketFamily, int newSocketType, String hostName, int port) {
        return withExceptionHandling(() -> {
            InetAddress inetAddress = InetAddress.getByName(hostName);
            switch (newSocketType) {
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
            ((Object[]) address)[0] = client.socket().getRemoteSocketAddress();
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

    public int getPort() {
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
        return withExceptionHandling(() -> ((SocketChannel) channel).write(UTF_8.encode(data)), -1);
    }

    public int send(Object buffer, int length) {
        return withExceptionHandling(() ->
            ((SocketChannel) channel).write(ByteBuffer.wrap((byte[]) buffer, 0, length)), -1);
    }

    public int sendTo(String data, String host, int port, int family) {
        return withExceptionHandling(() -> {
            IdrisSocket server = create(family, socketType, 0);
            if (server == null) {
                return -1;
            }
            int connectionResult = server.connect(family, socketType, host, port);
            return connectionResult != -1 ? server.send(data) : connectionResult;
        }, -1);
    }

    public int sendToBuffer(Object bufferArray, int length, String host, int port, int family) {
        return withExceptionHandling(() -> {
            IdrisSocket server = create(family, socketType, 0);
            if (server == null) {
                return -1;
            }
            int connectionResult = server.connect(family, socketType, host, port);
            return connectionResult != -1 ? server.send(bufferArray, length) : connectionResult;
        }, -1);
    }

    public ResultPayload<String> receive(int length) {
        SocketChannel socketChannel = (SocketChannel) this.channel;
        ResultPayload<String> resultPayload = withExceptionHandling(() -> {
            ByteBuffer buffer = ByteBuffer.allocate(length * Character.BYTES);
            do {
                buffer.rewind();
            } while (socketChannel.read(buffer) <= 0);

            StringBuilder builder = new StringBuilder(length);
            int read;
            do {
                buffer.flip();
                builder.append(UTF_8.decode(buffer));
                buffer.rewind();
                read = socketChannel.read(buffer);
            } while (read > 0 && builder.length() < length);
            return new ResultPayload<>(builder.length(), builder.toString(), socketChannel.getRemoteAddress());
        });
        return resultPayload != null ? resultPayload : new ResultPayload<>(Runtime.getErrorNumber(), null, null);
    }

    public ResultPayload<byte[]> receive(Object arrayObject, int length) {
        ResultPayload<byte[]> resultPayload = withExceptionHandling(() -> {
            SocketChannel socketChannel = (SocketChannel) this.channel;
            ByteBuffer buffer = ByteBuffer.allocate(length * Character.BYTES);
            do {
                buffer.rewind();
            } while (socketChannel.read(buffer) <= 0);
            int read;
            int size = 0;
            byte[] array = (byte[]) arrayObject;
            do {
                buffer.flip();
                System.arraycopy(buffer.array(), 0, array, size, Math.min(buffer.limit(), array.length));
                buffer.rewind();
                read = socketChannel.read(buffer);
                if (read > 0) {
                    size += read;
                }
            } while (read > 0 && size < length);
            return new ResultPayload<>(0, array, socketChannel.getRemoteAddress());
        });
        return resultPayload != null ? resultPayload : new ResultPayload<>(Runtime.getErrorNumber(), null, null);
    }

    public <T> T getPayload(ResultPayload<T> resultPayload) {
        return resultPayload.getPayload();
    }

    @Override
    public void close() {
        withExceptionHandling(() -> {
            channel.close();
            return null;
        });
    }

    private void initialize(AbstractSelectableChannel newChannel) throws IOException {
        this.channel = newChannel;
        newChannel.configureBlocking(false);
        Server.register(newChannel, newChannel.validOps());
        ClientSocketReaderState readerState = new ClientSocketReaderState(
            ByteBuffer.allocate(ONE_KB * SOCKET_BUFFER_CAPACITY_IN_KB), new ResettableCountDownLatch(1));
        Server.putState(newChannel, readerState);
        ClientSocketWriterState writerState = new ClientSocketWriterState(ByteBuffer.allocate(
            ONE_KB * SOCKET_BUFFER_CAPACITY_IN_KB), new ResettableCountDownLatch(1));
        Server.putState(newChannel, writerState);
    }

    private <T> T withExceptionHandling(SupplierE<T, ? extends Exception> action) {
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
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception exception) {
            handleException(exception);
            return fallback;
        }
    }

    private boolean withExceptionHandling(BooleanSupplierE<? extends Exception> action, boolean fallback) {
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception exception) {
            handleException(exception);
            return fallback;
        }
    }

    private long withExceptionHandling(LongSupplierE<? extends Exception> action, long fallback) {
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception exception) {
            handleException(exception);
            return fallback;
        }
    }

    public static final class ResultPayload<T> {
        private final int result;
        private final T payload;
        private final SocketAddress remoteAddress;

        private ResultPayload(int result, T payload, SocketAddress remoteAddress) {
            this.result = result;
            this.payload = payload;
            this.remoteAddress = remoteAddress;
        }

        public int getResult() {
            return result;
        }

        public T getPayload() {
            return payload;
        }

        public SocketAddress getRemoteAddress() {
            return remoteAddress;
        }
    }
}
