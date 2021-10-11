package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;

final class ReadableWritableChannel implements ReadableByteChannel, WritableByteChannel {
    private final ReadableByteChannel reader;
    private final WritableByteChannel writer;

    ReadableWritableChannel(ReadableByteChannel reader, WritableByteChannel writer) {
        this.reader = reader;
        this.writer = writer;
    }

    @Override
    public int read(ByteBuffer byteBuffer) throws IOException {
        return reader.read(byteBuffer);
    }

    @Override
    public int write(ByteBuffer byteBuffer) throws IOException {
        return writer.write(byteBuffer);
    }

    @Override
    public boolean isOpen() {
        return reader.isOpen() || writer.isOpen();
    }

    @Override
    public void close() throws IOException {
        reader.close();
        writer.close();
    }
}
