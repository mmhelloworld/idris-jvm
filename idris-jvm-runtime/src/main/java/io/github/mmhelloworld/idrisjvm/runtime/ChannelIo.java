package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.Closeable;
import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channel;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SeekableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.AccessDeniedException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import static io.github.mmhelloworld.idrisjvm.runtime.Directories.getWorkingDir;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.StandardOpenOption.APPEND;
import static java.nio.file.StandardOpenOption.CREATE;
import static java.nio.file.StandardOpenOption.READ;
import static java.nio.file.StandardOpenOption.TRUNCATE_EXISTING;
import static java.nio.file.StandardOpenOption.WRITE;
import static java.nio.file.attribute.PosixFilePermission.GROUP_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.GROUP_READ;
import static java.nio.file.attribute.PosixFilePermission.GROUP_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_READ;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_READ;
import static java.nio.file.attribute.PosixFilePermission.OWNER_WRITE;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static java.util.concurrent.TimeUnit.SECONDS;
import static java.util.stream.Collectors.toSet;

public final class ChannelIo implements ReadableByteChannel, WritableByteChannel, Closeable, IdrisFile<ChannelIo> {
    private static final boolean IS_POSIX = FileSystems.getDefault().supportedFileAttributeViews().contains("posix");
    private static final Map<Integer, PosixFilePermission> MODE_TO_PERMISSIONS = new HashMap<>();
    private static final int MISSING_FILE_ERROR_CODE = 2;
    private static final int SECURITY_ERROR_CODE = 3;
    private static final int EXISTING_FILE_ERROR_CODE = 4;
    private static final int GENERAL_ERROR_CODE = 10;

    static {
        PosixFilePermission[] permissions = {
            OTHERS_EXECUTE, OTHERS_WRITE, OTHERS_READ,
            GROUP_EXECUTE, GROUP_WRITE, GROUP_READ,
            OWNER_EXECUTE, OWNER_WRITE, OWNER_READ};
        for (int index = 0; index < permissions.length; index++) {
            MODE_TO_PERMISSIONS.put(1 << index, permissions[index]);
        }
    }

    private final Path path;
    private final Channel channel;
    private final ByteBufferIo byteBufferIo;
    private Exception exception;

    ChannelIo(Path path, Channel channel) {
        this.path = path;
        this.channel = channel;
        FunctionE<ByteBuffer, Integer, IOException> reader =
            channel instanceof ReadableByteChannel ? ((ReadableByteChannel) channel)::read : buffer -> {
                throw new IOException("File is not readable");
            };
        FunctionE<ByteBuffer, Integer, IOException> writer =
            channel instanceof WritableByteChannel ? ((WritableByteChannel) channel)::write : buffer -> {
                throw new IOException("File is not writable");
            };
        this.byteBufferIo = new ByteBufferIo(reader, writer);
    }

    ChannelIo(Channel channel, ByteBufferIo byteBufferIo) {
        this.path = null;
        this.channel = channel;
        this.byteBufferIo = byteBufferIo;
    }

    public ChannelIo(Path path) {
        this(path, null);
    }

    public static char readChar(ChannelIo file) {
        return file.readChar();
    }

    public static String readChars(int count, ChannelIo file) {
        return file.readChars(count);
    }

    public static String readLine(ChannelIo file) {
        return file.readLine();
    }

    public static int writeLine(ChannelIo file, String str) {
        return file.writeLine(str);
    }

    public static int isEof(ChannelIo file) {
        return file.isEof();
    }

    public static ChannelIo open(String name, String mode) {
        Path path = Paths.createPath(name);
        try {
            if (!isReadOnlyMode(mode)) {
                ensureParentDirectory(path);
            }
            return open(path, getOpenOptions(mode).toArray(new OpenOption[]{}));
        } catch (Exception exception) {
            Runtime.setErrorNumber(ChannelIo.getErrorNumber(exception));
            return null;
        }
    }

    public static ChannelIo popen(String command, String mode) throws IOException {
        Process process = new ProcessBuilder(IdrisSystem.getCommand(command))
            .directory(new File(getWorkingDir()))
            .start();
        return new ChannelIo(null, new ReadableWritableChannel(Channels.newChannel(process.getInputStream()),
            Channels.newChannel(process.getOutputStream())));
    }

    public static void close(ChannelIo file) {
        file.close();
    }

    public static int chmod(String file, int mode) {
        return new ChannelIo(Paths.createPath(file))
            .chmod(mode);
    }

    public static void createDirectories(Path path) throws IOException {
        if (path != null && (!Files.isSymbolicLink(path) || !Files.exists(path))) {
            Files.createDirectories(path);
        }
    }

    public static void writeFile(String pathString, String content) throws IOException {
        Path path = Paths.createPath(pathString);
        byte[] bytes = content.getBytes(UTF_8);
        createDirectories(path.getParent());
        Files.write(path, bytes);
    }

    public static int flush(ChannelIo file) {
        return file.flush();
    }

    public static int size(ChannelIo file) {
        return file.size();
    }

    public static int delete(String file) {
        try {
            Files.delete(Paths.createPath(file));
            return 0;
        } catch (IOException e) {
            Runtime.setErrorNumber(getErrorNumber(e));
            return -1;
        }
    }

    public static int getModifiedTime(ChannelIo file) {
        return (int) file.getModifiedTime();
    }

    public static int getAccessTime(ChannelIo file) {
        return file.getAccessTime();
    }

    public static int getStatusTime(ChannelIo file) {
        return (int) file.getStatusTime();
    }

    public static int getErrorNumber(ChannelIo file) {
        return file.getErrorNumber();
    }

    private static ChannelIo open(Path path, OpenOption... openOptions) throws IOException {
        ensureParentDirectory(path);
        return new ChannelIo(path, FileChannel.open(path, openOptions));
    }

    private static void ensureParentDirectory(Path path) throws IOException {
        Path parent = path.getParent();
        if (parent != null) {
            createDirectories(parent);
        }
    }

    private static boolean isReadOnlyMode(String mode) {
        return "r".equalsIgnoreCase(mode);
    }

    private static Collection<OpenOption> getOpenOptions(String mode) {
        switch (mode.toLowerCase()) {
            case "r":
            case "rb":
                return singletonList(READ);
            case "w":
            case "wb":
                return asList(CREATE, WRITE, TRUNCATE_EXISTING);
            case "a":
            case "ab":
                return asList(CREATE, APPEND);
            case "r+":
            case "rb+":
                return asList(READ, WRITE);
            case "w+":
            case "wb+":
                return asList(CREATE, READ, WRITE);
            case "a+":
            case "ab+":
                return asList(CREATE, READ, APPEND);
            default:
                throw new IllegalArgumentException("Unknown file mode " + mode);
        }
    }

    private static Set<PosixFilePermission> createPosixFilePermissions(int mode) {
        return MODE_TO_PERMISSIONS.entrySet().stream()
            .filter(modeAndPermission -> (mode & modeAndPermission.getKey()) == modeAndPermission.getKey())
            .map(Map.Entry::getValue)
            .collect(toSet());
    }

    static int getErrorNumber(Exception currentException) {
        if (currentException == null) {
            return 0;
        } else if (currentException instanceof FileNotFoundException
            || currentException instanceof NoSuchFileException) {
            return MISSING_FILE_ERROR_CODE; // To return error codes to conform to Idris functions with C FFIs
        } else if (currentException instanceof AccessDeniedException
            || currentException instanceof SecurityException) {
            return SECURITY_ERROR_CODE;
        } else if (currentException instanceof FileAlreadyExistsException) {
            return EXISTING_FILE_ERROR_CODE;
        } else {
            return GENERAL_ERROR_CODE;
        }
    }

    @Override
    public char readChar() {
        return (char) withExceptionHandling(() -> {
            char c = byteBufferIo.getChar();
            if (c == (char) -1) {
                throw new EOFException();
            }
            return c;
        }, -1);
    }

    @Override
    public String readLine() {
        String chars = withExceptionHandling(byteBufferIo::getLine);
        return exception != null ? "" : chars;
    }

    @Override
    public int seekLine() {
        return withExceptionHandling(byteBufferIo::seekLine);
    }

    public void handleException(Exception e) {
        this.exception = e;
        Runtime.setException(exception);
        Runtime.setErrorNumber(getErrorNumber(e));
    }

    @Override
    public int writeLine(String str) {
        int bytesWrittenCount = withExceptionHandling(() -> {
            int numberOfBytesWritten = byteBufferIo.writeString(str);
            flush();
            if (!str.isEmpty() && numberOfBytesWritten <= 0) {
                throw new EOFException();
            }
            return numberOfBytesWritten;
        });
        return exception != null || bytesWrittenCount == -1 ? 0 : 1;
    }

    public int writeChar(char value) {
        int bytesWrittenCount = withExceptionHandling(() -> {
            int numberOfBytesWritten = byteBufferIo.writeChar(value);
            flush();
            if (numberOfBytesWritten == 0) {
                throw new EOFException();
            }
            return numberOfBytesWritten;
        });
        return exception != null || bytesWrittenCount == -1 ? 0 : 1;
    }

    public int chmod(int mode) {
        return withExceptionHandling(() -> {
            if (IS_POSIX && path != null) {
                Files.setPosixFilePermissions(path, createPosixFilePermissions(mode));
            }
            return 0;
        }, -1);
    }

    public int flush() {
        withExceptionHandling(() -> {
            if (channel instanceof FileChannel) {
                ((FileChannel) channel).force(true);
            }
            return null;
        });
        return exception == null ? 0 : 1;
    }

    public int isEof() {
        boolean isEof = withExceptionHandling(byteBufferIo::isEof, true);
        return isEof || exception != null ? 1 : 0;
    }

    public int size() {
        return (int) withExceptionHandling(() -> {
            if (channel instanceof SeekableByteChannel) {
                return ((SeekableByteChannel) channel).size();
            } else {
                return -1;
            }
        }, -1);
    }

    @Override
    public boolean isOpen() {
        return channel.isOpen();
    }

    @Override
    public void close() {
        withExceptionHandling(() -> {
            if (channel != null) {
                channel.close();
            }
            return null;
        });
    }

    @Override
    public int getErrorNumber() {
        return getErrorNumber(exception);
    }

    @Override
    public String readChars(int count) {
        String chars = withExceptionHandling(() -> {
            int index = 0;
            StringBuilder builder = new StringBuilder();
            char c;
            while (index < count && (c = byteBufferIo.getChar()) != (char) -1) {
                builder.append(c);
                index++;
            }
            String line = builder.toString();
            return line.isEmpty() && byteBufferIo.isEof() ? null : line;
        });
        return exception != null ? null : chars;
    }

    public int delete() {
        return withExceptionHandling(() -> {
            if (path != null) {
                Files.delete(path);
            }
            return 0;
        }, -1);
    }

    @Override
    public int getModifiedTime() {
        return (int) getTimeAttribute(BasicFileAttributes::lastModifiedTime);
    }

    @Override
    public int getAccessTime() {
        return (int) getTimeAttribute(BasicFileAttributes::lastAccessTime);
    }

    @Override
    public int getStatusTime() {
        return (int) getTimeAttribute(BasicFileAttributes::creationTime);
    }

    public long getTimeAttribute(Function<BasicFileAttributes, FileTime> attributeGetter) {
        return withExceptionHandling(() ->
            path == null ? 0 : attributeGetter.apply(Files.readAttributes(path, BasicFileAttributes.class))
                .to(SECONDS), -1);
    }

    @Override
    public int read(ByteBuffer dst) throws IOException {
        return ((ReadableByteChannel) channel).read(dst);
    }

    @Override
    public int write(ByteBuffer src) throws IOException {
        return ((WritableByteChannel) channel).write(src);
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
        } catch (Exception newException) {
            handleException(newException);
            return fallback;
        }
    }

    private boolean withExceptionHandling(BooleanSupplierE<? extends Exception> action, boolean fallback) {
        exception = null;
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception currentException) {
            handleException(currentException);
            return fallback;
        }
    }

    private long withExceptionHandling(LongSupplierE<? extends Exception> action, long fallback) {
        exception = null;
        Runtime.setErrorNumber(0);
        try {
            return action.get();
        } catch (Exception currentException) {
            handleException(currentException);
            return fallback;
        }
    }
}
