package io.github.mmhelloworld.idris2.runtime;

import static io.github.mmhelloworld.idris2.runtime.Runtime.stdin;
import static io.github.mmhelloworld.idris2.runtime.Runtime.stdout;

public final class Console {

    private Console() {
    }

    public static void printString(String string) {
        stdout.writeLine(string);
    }

    public static String getString() {
        String line = stdin.readLine();
        // conforming to Idris C implementation
        return line == null ? "" : line.replaceAll("[\\r\\n]+$", "");
    }

    public static void putChar(char c) {
        stdout.writeChar(c);
    }

    public static char getChar() {
        return stdin.readChar();
    }
}
