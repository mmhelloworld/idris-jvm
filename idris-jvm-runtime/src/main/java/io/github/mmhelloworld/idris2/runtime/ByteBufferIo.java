package io.github.mmhelloworld.idris2.runtime;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;

import static java.nio.charset.StandardCharsets.UTF_8;

public class ByteBufferIo {
    private final FunctionE<ByteBuffer, Integer, IOException> reader;
    private final FunctionE<ByteBuffer, Integer, IOException> writer;
    private final Charset charset;
    private final ByteBuffer buffer;
    private CharBuffer charBuffer;
    private String lastReadLine;
    private boolean isEof;

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

    public char getChar() throws IOException {
        if (!ensureBuffer()) {
            isEof = true;
            return (char) -1;
        }
        return charBuffer.get();
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

    int writeString(String str) throws IOException {
        return writer.apply(charset.encode(str));
    }

    int writeChar(char c) throws IOException {
        return writer.apply(charset.encode(CharBuffer.wrap(new char[]{c})));
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
