package io.github.mmhelloworld.idrisjvm.io;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import static java.lang.System.lineSeparator;
import static java.nio.charset.StandardCharsets.UTF_8;

public class ByteBufferIo {
    private final FunctionE<ByteBuffer, Integer, IOException> reader;
    private final FunctionE<ByteBuffer, Integer, IOException> writer;
    private final Charset charset;
    private final ByteBuffer buffer;
    private CharBuffer charBuffer;

    ByteBufferIo(FunctionE<ByteBuffer, Integer, IOException> reader,
                 FunctionE<ByteBuffer, Integer, IOException> writer) {
        this(reader, writer, UTF_8, 1024);
    }

    private ByteBufferIo(FunctionE<ByteBuffer, Integer, IOException> reader,
                         FunctionE<ByteBuffer, Integer, IOException> writer,
                         Charset charset,
                         int bufferSize) {
        this.reader = reader;
        this.writer = writer;
        this.charset = charset;
        this.buffer = ByteBuffer.allocate(bufferSize);
    }

    public boolean hasChar() throws IOException {
        return ensureBuffer();
    }

    public char getChar() throws IOException {
        ensureBuffer();
        return charBuffer.get();
    }

    String getLine() throws IOException {
        if (!hasChar()) {
            return null;
        } else {
            String lineSeparator = lineSeparator();
            int lineSeparatorLength = lineSeparator.length();
            StringBuilder sb = new StringBuilder();
            do {
                sb.append(getChar());
                if (endsWith(sb, lineSeparator)) {
                    return sb.substring(0, sb.length() - lineSeparatorLength);
                }
            } while (hasChar());
            return sb.toString();
        }
    }

    int writeString(String str) throws IOException {
        return writer.apply(charset.encode(str));
    }

    private boolean endsWith(StringBuilder stringBuilder, String str) {
        int sbLen = stringBuilder.length();
        int strLen = str.length();
        return sbLen >= strLen && stringBuilder.subSequence(sbLen - strLen, sbLen).equals(str);
    }

    private boolean ensureBuffer() throws IOException {
        final boolean hasRemaining;
        if (charBuffer == null || !charBuffer.hasRemaining()) {
            buffer.rewind();
            int read = reader.apply(buffer);
            buffer.flip();
            if (read > 0) {
                charBuffer = UTF_8.decode(buffer);
                hasRemaining = charBuffer.hasRemaining();
            } else {
                hasRemaining = false;
            }
        } else {
            hasRemaining = true;
        }
        return hasRemaining;
    }
}
