package io.github.mmhelloworld.idrisjvm.io;

import java.io.Closeable;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;

import static java.util.concurrent.TimeUnit.SECONDS;

public class FileChannelIo implements ReadableByteChannel, WritableByteChannel, Closeable {
    private final Path path;
    private final FileChannel channel;
    private final ByteBufferIo byteBufferIo;

    private FileChannelIo(Path path, FileChannel channel) {
        this.path = path;
        this.channel = channel;
        byteBufferIo = new ByteBufferIo(this.channel::read, this.channel::write);
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

    public static FileChannelIo open(Path path, OpenOption... openOptions) throws IOException {
        if (path.getParent() != null) {
            Files.createDirectories(path.getParent());
        }
        return new FileChannelIo(path, FileChannel.open(path, openOptions));
    }

    public void flush() throws IOException {
        channel.force(true);
    }

    public boolean isEof() throws IOException {
        return !byteBufferIo.hasChar();
    }

    public long size() throws IOException {
        return channel.size();
    }

    @Override
    public boolean isOpen() {
        return channel.isOpen();
    }

    @Override
    public void close() throws IOException {
        channel.close();
    }

    public FileChannel getChannel() {
        return channel;
    }

    public Path getPath() {
        return path;
    }

    public BigInteger getFileModifiedTime() throws IOException {
        long lastModifiedTime = Files.readAttributes(path, BasicFileAttributes.class)
            .lastModifiedTime()
            .to(SECONDS);
        return BigInteger.valueOf(lastModifiedTime);
    }

    public BigInteger getFileAccessTime() throws IOException {
        long lastAccessTime = Files.readAttributes(path, BasicFileAttributes.class)
            .lastAccessTime()
            .to(SECONDS);
        return BigInteger.valueOf(lastAccessTime);
    }

    public BigInteger getFileStatusTime() throws IOException {
        long fileStatusTime = Files.readAttributes(path, BasicFileAttributes.class)
            .creationTime()
            .to(SECONDS);
        return BigInteger.valueOf(fileStatusTime);
    }

    @Override
    public int read(ByteBuffer dst) throws IOException {
        return channel.read(dst);
    }

    @Override
    public int write(ByteBuffer src) throws IOException {
        return channel.write(src);
    }
}
