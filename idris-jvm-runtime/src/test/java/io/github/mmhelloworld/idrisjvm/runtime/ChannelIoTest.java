package io.github.mmhelloworld.idrisjvm.runtime;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static java.lang.Thread.currentThread;
import static java.util.Objects.requireNonNull;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.arguments;

@Disabled
class ChannelIoTest {

    static Stream<Arguments> readFile() throws IOException {
        return Stream.of(
            createArguments("file1.txt"),
            createArguments("file2.txt"));
    }

    private static Arguments createArguments(String fileName) throws IOException {
        String path = getPath(fileName);
        List<String> content = readFile(path);
        return arguments(path, content);
    }

    private static List<String> readFile(String path) throws IOException {
        BufferedReader bufferedReader = Files.newBufferedReader(Paths.get(path));
        int currentChar;
        List<String> content = new ArrayList<>();
        StringBuilder line = new StringBuilder();
        while ((currentChar = bufferedReader.read()) != -1) {
            line.append((char) currentChar);
            if (currentChar == '\n') {
                content.add(line.toString());
                line = new StringBuilder();
            }
        }
        String lineStr = line.toString();
        if (!lineStr.isEmpty()) {
            content.add(lineStr);
        }
        return content;
    }

    private static String getPath(String name) {
        return new File(requireNonNull(currentThread().getContextClassLoader().getResource(name)).getFile()).getPath();
    }

    @ParameterizedTest
    @MethodSource("readFile")
    void readFile1(String fileName, List<String> expectedContent) {
        // when
        List<String> actualContent = testReadFile1(fileName);

        // then
        assertThat(actualContent).containsExactlyElementsOf(expectedContent);
    }

    @ParameterizedTest
    @MethodSource("readFile")
    void readFile2(String fileName, List<String> expectedContent) {
        // when
        List<String> actualContent = testReadFile2(fileName);

        // then
        assertThat(actualContent).containsExactlyElementsOf(expectedContent);
    }

    private List<String> testReadFile1(String fileName) {
        ChannelIo file = ChannelIo.open(fileName, "r");
        List<String> content = new ArrayList<>();
        while (true) {
            String line = file.readLine();
            if (file.isEof() == 1) {
                break;
            }
            content.add(line);
        }
        return content;
    }

    private List<String> testReadFile2(String fileName) {
        ChannelIo file = ChannelIo.open(fileName, "r");
        List<String> content = new ArrayList<>();
        while (file.isEof() != 1) {
            String line = file.readLine();
            content.add(line);
        }
        return content;
    }
}
