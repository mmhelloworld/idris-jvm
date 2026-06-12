package io.github.mmhelloworld.idrisjvm.runtime;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.lang.Thread.currentThread;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Objects.requireNonNull;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.arguments;

class ChannelIoTest {

    static Stream<Arguments> readFile() throws IOException {
        return Stream.of(
            createArguments("file1.txt"),
            createArguments("file2.txt"));
    }

    private static Arguments createArguments(String fileName) throws IOException {
        String path = getPath(fileName);
        return arguments(path, Files.readString(Paths.get(path)));
    }

    private static String getPath(String name) {
        return new File(requireNonNull(currentThread().getContextClassLoader().getResource(name)).getFile()).getPath();
    }

    // The check-EOF-then-read idiom used by the Idris standard library (System.File.ReadWrite.fRead)
    @ParameterizedTest
    @MethodSource("readFile")
    void readFile(String fileName, String expectedContent) {
        ChannelIo file = ChannelIo.open(fileName, "r");
        StringBuilder content = new StringBuilder();
        while (file.isEof() != 1) {
            content.append(file.readLine());
        }
        assertThat(content.toString()).isEqualTo(expectedContent);
    }

    @Test
    void isEofShouldNotReadFromChannel() throws IOException {
        Deque<byte[]> chunks = new ArrayDeque<>();
        chunks.add("((:load-file \"foo.idr\") 1)\n".getBytes(UTF_8));
        AtomicInteger readCount = new AtomicInteger();
        ByteBufferIo io = new ByteBufferIo(buffer -> {
            readCount.incrementAndGet();
            byte[] chunk = chunks.poll();
            if (chunk == null) {
                throw new IOException("read would block: no data available");
            }
            buffer.put(chunk);
            return chunk.length;
        }, buffer -> 0);

        // feof semantics: no read has hit end-of-stream, so this must answer without reading
        assertThat(io.isEof()).isFalse();
        assertThat(readCount).hasValue(0);

        assertThat(io.getLine()).isEqualTo("((:load-file \"foo.idr\") 1)\n");

        // the request consumed the buffer; checking EOF before replying must still not block
        assertThat(io.isEof()).isFalse();
        assertThat(readCount).hasValue(1);
    }

    @Test
    void isEofShouldReportEndOfStreamAfterReadHitsIt() throws IOException {
        Deque<byte[]> chunks = new ArrayDeque<>();
        chunks.add("last line".getBytes(UTF_8));
        ByteBufferIo io = new ByteBufferIo(buffer -> {
            byte[] chunk = chunks.poll();
            if (chunk == null) {
                return -1;
            }
            buffer.put(chunk);
            return chunk.length;
        }, buffer -> 0);

        assertThat(io.isEof()).isFalse();
        assertThat(io.getLine()).isEqualTo("last line");
        assertThat(io.isEof()).isTrue();
    }
}
