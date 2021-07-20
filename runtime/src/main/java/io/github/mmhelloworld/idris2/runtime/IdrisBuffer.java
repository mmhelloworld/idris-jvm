package io.github.mmhelloworld.idris2.runtime;

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

    public static Object create(int size) {
        return new IdrisBuffer(size);
    }

    public static int size(Object buffer) {
        return ((IdrisBuffer) buffer).size();
    }

    public static void setByte(Object buffer, int location, int value) {
        ((IdrisBuffer) buffer).setByte(location, (byte) value);
    }

    public static void setChar(Object buffer, int location, int value) {
        ((IdrisBuffer) buffer).setChar(location, (char) value);
    }

    public static void setShort(Object buffer, int location, int value) {
        ((IdrisBuffer) buffer).setShort(location, (short) value);
    }

    public static void setInt(Object buffer, int location, int value) {
        ((IdrisBuffer) buffer).setInt(location, value);
    }

    public static void setLong(Object buffer, int location, long value) {
        ((IdrisBuffer) buffer).setLong(location, value);
    }

    public static void setDouble(Object buffer, int location, double value) {
        ((IdrisBuffer) buffer).setDouble(location, value);
    }

    public static void setString(Object buffer, int location, String value) {
        ((IdrisBuffer) buffer).setString(location, value);
    }

    public static int getByte(Object buffer, int location) {
        return Byte.toUnsignedInt(((IdrisBuffer) buffer).getByte(location));
    }

    public static int getShort(Object buffer, int location) {
        return Short.toUnsignedInt(((IdrisBuffer) buffer).getShort(location));
    }

    public static int getInt(Object buffer, int location) {
        return ((IdrisBuffer) buffer).getInt(location);
    }

    public static long getLong(Object buffer, int location) {
        return ((IdrisBuffer) buffer).getLong(location);
    }

    public static double getDouble(Object buffer, int location) {
        return ((IdrisBuffer) buffer).getDouble(location);
    }

    public static String getString(Object buffer, int location, int length) {
        return ((IdrisBuffer) buffer).getString(location, length);
    }

    public static void writeToFile(Object buffer, File file, int location, int length) throws IOException {
        ((IdrisBuffer) buffer).writeToFile(file, location, length);
    }

    public static int readFromFile(Object buffer,
                                   ReadableByteChannel file,
                                   int location,
                                   int length) throws IOException {
        return ((IdrisBuffer) buffer).readFromFile(file, location, length);
    }

    public static Object readFromFile(String fileName) {
        try {
            FileChannel channel = FileChannel.open(Paths.createPath(fileName), READ);
            int size = (int) channel.size();
            IdrisBuffer buffer = new IdrisBuffer(size);
            buffer.readFromFile(channel, 0, size);
            return buffer;
        } catch (IOException exception) {
            return null;
        }
    }

    public static int getErrorCode(Object buffer) {
        return buffer != null ? 0 : -1;
    }

    public static void writeToFile(Object buffer, WritableByteChannel file, int location, int length)
        throws IOException {
        ((IdrisBuffer) buffer).writeToFile(file, location, length);
    }

    public int size() {
        return buffer.capacity();
    }

    public void copy(int start, int length, IdrisBuffer to, int location) {
        if (location >= 0 && location + length <= to.size()) {
            to.buffer.position(location);
            for (int i = start; i < start + length; i++) {
                to.buffer.put(this.buffer.get(i));
            }
        }
    }

    public static void copy(Object from, int start, int length, Object to, int location) {
        ((IdrisBuffer) from).copy(start, length, (IdrisBuffer) to, location);
    }

    public void setByte(int location, byte value) {
        if (location >= 0 && location < size()) {
            this.buffer.put(location, value);
        }
    }

    public void setChar(int location, char value) {
        if (location >= 0 && location + Character.BYTES <= size()) {
            buffer.putChar(location, value);
        }
    }

    public void setShort(int location, short value) {
        if (location >= 0 && location + Short.BYTES <= size()) {
            buffer.putShort(location, value);
        }
    }

    public void setInt(int location, int value) {
        if (location >= 0 && location + Integer.BYTES <= size()) {
            buffer.putInt(location, value);
        }
    }

    public void setLong(int location, long value) {
        if (location >= 0 && location + Long.BYTES <= size()) {
            buffer.putLong(location, value);
        }
    }

    public void setDouble(int location, double value) {
        if (location >= 0 && location + Double.BYTES <= size()) {
            buffer.putDouble(location, value);
        }
    }

    public void setString(int location, String value) {
        byte[] bytes = value.getBytes(UTF_8);
        int length = bytes.length;

        int end = location + length;
        if (location >= 0 && end <= size()) {
            buffer.position(location);
            int sourceIndex = 0;
            for (int targetIndex = location; targetIndex < end; targetIndex++) {
                buffer.put(bytes[sourceIndex++]);
            }
        }
    }

    public byte getByte(int location) {
        if (location >= 0 && location < size()) {
            return buffer.get(location);
        } else {
            return 0;
        }
    }

    public short getShort(int location) {
        if (location >= 0 && location + Short.BYTES <= size()) {
            return buffer.getShort(location);
        } else {
            return 0;
        }
    }

    public int getInt(int location) {
        if (location >= 0 && location + Integer.BYTES <= size()) {
            return buffer.getInt(location);
        } else {
            return 0;
        }
    }

    public long getLong(int location) {
        if (location >= 0 && location + Long.BYTES <= size()) {
            return buffer.getLong(location);
        } else {
            return 0;
        }
    }

    public double getDouble(int location) {
        if (location >= 0 && location + Double.BYTES <= size()) {
            return buffer.getDouble(location);
        } else {
            return 0;
        }
    }

    public String getString(int location, int length) {
        length = location >= 0 && location + length <= size() ? length : 0;
        if (length == 0) {
            return "";
        }
        byte[] data = new byte[length];
        int targetIndex = 0;
        for (int i = location; i < location + length; i++) {
            data[targetIndex++] = buffer.get(i);
        }
        return new String(data, UTF_8);
    }

    public void writeToFile(File file, int location, int length) throws IOException {
        writeToFile(FileChannel.open(file.toPath(), WRITE, CREATE, TRUNCATE_EXISTING), location, length);
    }

    public int readFromFile(ReadableByteChannel file, int location, int length) throws IOException {
        int size = size();
        if (location >= 0 && location < size) {
            if (location + length > size) {
                length = size - location;
            }
            int end = location + length;
            buffer.position(location);
            byte[] data = new byte[1024];
            int numberOfBytesRead = 0;
            for (int index = location; index < end; index += 1024) {
                int limit = Math.min(end - index, 1024);
                numberOfBytesRead += file.read(ByteBuffer.wrap(data, 0, limit));
                buffer.put(data, 0, limit);
            }
            return numberOfBytesRead;
        } else {
            return 0;
        }
    }

    public void writeToFile(WritableByteChannel file, int location, int length) throws IOException {
        int size = size();
        if (location >= 0 && location < size) {
            if (location + length > size) {
                length = size - location;
            }
            int end = location + length;
            byte[] data = new byte[1024];
            buffer.position(location);
            for (int index = location; index < end; index += 1024) {
                int limit = Math.min(end - index, 1024);
                buffer.get(data, 0, limit);
                file.write(ByteBuffer.wrap(data, 0, limit));
            }
        }
    }

    public static int writeToFile(String fileName, Object buffer, int length) {
        try {
            FileChannel channel = FileChannel.open(Paths.createPath(fileName), CREATE, WRITE);
            ((IdrisBuffer) buffer).writeToFile(channel, 0, length);
            return 0;
        } catch (IOException exception) {
            return -1;
        }
    }

    int readFromFile(File file, int location, int length) throws IOException {
        return readFromFile(FileChannel.open(file.toPath(), READ), location, length);
    }
}
