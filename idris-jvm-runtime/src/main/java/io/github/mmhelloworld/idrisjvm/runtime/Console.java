package io.github.mmhelloworld.idrisjvm.runtime;

public final class Console {

    private Console() {
    }

    public static void printString(String string) {
        Runtime.STDOUT.writeLine(string);
    }

    public static String getString() {
        String line = Runtime.STDIN.readLine();
        // conforming to Idris C implementation
        return line == null ? "" : line.replaceAll("[\\r\\n]+$", "");
    }

    public static void putChar(char c) {
        Runtime.STDOUT.writeChar(c);
    }

    public static char getChar() {
        return Runtime.STDIN.readChar();
    }
}
