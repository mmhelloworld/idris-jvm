package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.StandardOpenOption.CREATE;
import static java.nio.file.StandardOpenOption.READ;
import static java.nio.file.StandardOpenOption.TRUNCATE_EXISTING;
import static java.nio.file.StandardOpenOption.WRITE;

public final class IdrisBuffer {

    private final ByteBuffer buffer;

    public IdrisBuffer(int size) {
        this.buffer = ByteBuffer.allocate(size);
        this.buffer.order(ByteOrder.LITTLE_ENDIAN);
    }

    public int size() {
        return buffer.capacity();
    }

    public void copy(int start, int len, IdrisBuffer to, int loc) {
        if (loc >= 0 && loc + len <= to.size()) {
            to.buffer.position(loc);
            for (int i = start; i < start + len; i++) {
                to.buffer.put(this.buffer.get(i));
            }
        }
    }

    public void setByte(int loc, byte b) {
        if (loc >= 0 && loc < size()) {
            this.buffer.put(loc, b);
        }
    }

    public void setChar(int loc, char val) {
        if (loc >= 0 && loc + Character.BYTES <= size()) {
            buffer.putChar(loc, val);
        }
    }

    public void setInt(int loc, int val) {
        if (loc >= 0 && loc + Integer.BYTES <= size()) {
            buffer.putInt(loc, val);
        }
    }

    public void setDouble(int loc, double val) {
        if (loc >= 0 && loc + Double.BYTES <= size()) {
            buffer.putDouble(loc, val);
        }
    }

    public void setString(int loc, String str) {
        byte[] bytes = str.getBytes(UTF_8);
        int len = bytes.length;

        int end = loc + len;
        if (loc >= 0 && end <= size()) {
            buffer.position(loc);
            int sourceIndex = 0;
            for (int targetIndex = loc; targetIndex < end; targetIndex++) {
                buffer.put(bytes[sourceIndex++]);
            }
        }
    }

    public byte getByte(int loc) {
        if (loc >= 0 && loc < size()) {
            return buffer.get(loc);
        } else {
            return 0;
        }
    }

    public int getInt(int loc) {
        if (loc >= 0 && loc + Integer.BYTES <= size()) {
            return buffer.getInt(loc);
        } else {
            return 0;
        }
    }

    public double getDouble(int loc) {
        double d;
        if (loc >= 0 && loc + Double.BYTES <= size()) {
            return buffer.getDouble(loc);
        } else {
            return 0;
        }
    }

    public String getString(int loc, int len) {
        len = loc >= 0 && loc + len <= size() ? len : 0;
        if (len == 0) {
            return "";
        }
        byte[] data = new byte[len];
        int targetIndex = 0;
        for (int i = loc; i < loc + len; i++) {
            data[targetIndex++] = buffer.get(i);
        }
        return new String(data, UTF_8);
    }

    int readFromFile(File file, int loc, int len) throws IOException {
        return readFromFile(FileChannel.open(file.toPath(), READ), loc, len);
    }

    public void writeToFile(File file, int loc, int len) throws IOException {
        writeToFile(FileChannel.open(file.toPath(), WRITE, CREATE, TRUNCATE_EXISTING), loc, len);
    }

    public int readFromFile(ReadableByteChannel file, int loc, int len) throws IOException {
        int size = size();
        if (loc >= 0 && loc < size) {
            if (loc + len > size) {
                len = size - loc;
            }
            int end = loc + len;
            buffer.position(loc);
            byte[] data = new byte[1024];
            int numberOfBytesRead = 0;
            for (int index = loc; index < end; index += 1024) {
                int limit = Math.min(end - index, 1024);
                numberOfBytesRead += file.read(ByteBuffer.wrap(data, 0, limit));
                buffer.put(data, 0, limit);
            }
            return numberOfBytesRead;
        } else {
            return 0;
        }
    }

    public void writeToFile(WritableByteChannel file, int loc, int len) throws IOException {
        int size = size();
        if (loc >= 0 && loc < size) {
            if (loc + len > size) {
                len = size - loc;
            }
            int end = loc + len;
            byte[] data = new byte[1024];
            buffer.position(loc);
            for (int index = loc; index < end; index += 1024) {
                int limit = Math.min(end - index, 1024);
                buffer.get(data, 0, limit);
                file.write(ByteBuffer.wrap(data, 0, limit));
            }
        }
    }
}
