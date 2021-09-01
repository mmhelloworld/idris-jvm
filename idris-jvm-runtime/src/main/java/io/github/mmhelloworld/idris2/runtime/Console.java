package io.github.mmhelloworld.idris2.runtime;

public final class Console {

    private Console() {
    }

    public static void printString(String string) {
        Runtime.stdout.writeLine(string);
    }

    public static String getString() {
        String line = Runtime.stdin.readLine();
        // conforming to Idris C implementation
        return line == null ? "" : line.replaceAll("[\\r\\n]+$", "");
    }

    public static void putChar(char c) {
        Runtime.stdout.writeChar(c);
    }

    public static char getChar() {
        return Runtime.stdin.readChar();
    }
}
