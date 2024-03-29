package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import static java.nio.charset.StandardCharsets.UTF_8;

public final class ByteBufferIo {
    public static final int DEFAULT_BUFFER_SIZE = 1024;
    private final FunctionE<ByteBuffer, Integer, IOException> reader;
    private final FunctionE<ByteBuffer, Integer, IOException> writer;
    private final Charset charset;
    private final ByteBuffer buffer;
    private CharBuffer charBuffer;
    private String lastReadLine;
    private boolean isEof;

    ByteBufferIo(FunctionE<ByteBuffer, Integer, IOException> reader,
                 FunctionE<ByteBuffer, Integer, IOException> writer) {
        this(reader, writer, UTF_8, DEFAULT_BUFFER_SIZE);
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

    public char getChar() throws IOException {
        if (!ensureBuffer()) {
            isEof = true;
            return (char) -1;
        }
        return charBuffer.get();
    }

    public boolean isEof() throws IOException {
        if (charBuffer == null || lastReadLine == null) {
            return !ensureBuffer();
        }
        if (!lastReadLine.isEmpty()) {
            isEof = false;
        }
        lastReadLine = null;
        return isEof;
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

    String getLine() throws IOException {
        StringBuilder sb = new StringBuilder();
        char c;
        while ((c = getChar()) != (char) -1) {
            sb.append(c);
            if (c == '\n') {
                isEof = false;
                break;
            }
        }
        String line = sb.toString();
        lastReadLine = line;
        return line;
    }

    // follows Idris C implementation "idris2_seekLine"
    int seekLine() throws IOException {
        while (true) {
            char c = getChar();
            if (c == (char) -1) {
                if (isEof()) {
                    return 0;
                } else {
                    return -1;
                }
            }
            if (c == '\n') {
                return 0;
            }
        }
    }

    int writeString(String str) throws IOException {
        return writer.apply(charset.encode(str));
    }

    int writeChar(char c) throws IOException {
        return writer.apply(charset.encode(CharBuffer.wrap(new char[]{c})));
    }
}
