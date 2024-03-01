package io.github.mmhelloworld.idrisjvm.runtime;

import org.jline.reader.EndOfFileException;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.terminal.TerminalBuilder;

import java.io.IOException;
import java.io.PrintWriter;

public final class Terminal {
    private static org.jline.terminal.Terminal terminal;
    private static LineReader lineReader;
    private static PrintWriter writer;

    static {
        try {
            terminal = TerminalBuilder.builder()
                .dumb(true)
                .build();
            lineReader = LineReaderBuilder.builder()
                .terminal(terminal)
                .build();
            writer = terminal.writer();
        } catch (IOException ignored) {
        }
    }

    public static void setup() {
    }

    public static int getColumns() {
        return terminal.getWidth();
    }

    public static int getRows() {
        return terminal.getHeight();
    }

    public static void printString(String value) {
        writer.print(value);
        writer.flush();
    }

    public static String readString() {
        try {
            return lineReader.readLine();
        } catch (EndOfFileException exception) {
            return "";
        }
    }

    public static void printChar(char value) {
        writer.print(value);
        writer.flush();
    }

    public static char readChar() {
        try {
            return ((char) terminal.reader().read());
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }
    }
}
