package io.github.mmhelloworld.idris2.runtime;

import static java.nio.charset.StandardCharsets.UTF_8;

public final class Strings {
    private Strings() {
    }

    public static String substring(int offset, int length, String str) {
        // Mimics Idris scheme backend
        int strLength = str.length();
        int start = Math.max(0, offset);
        int nonNegativeLength = Math.max(0, length);
        int end = Math.min(strLength, start + nonNegativeLength);
        return start > strLength ? "" : str.substring(start, end);
    }

    public static String fromIdrisList(Object idrisList) {
        StringBuilder builder = new StringBuilder();
        IdrisObject current = (IdrisObject) idrisList;
        while (current.getConstructorId() != 0) {
            builder.append(current.getProperty(0));
            current = (IdrisObject) current.getProperty(1);
        }
        return builder.toString();
    }

    public static IdrisList toIdrisList(String string) {
        return IdrisList.fromArray(string.toCharArray());
    }

    public static int bytesLengthUtf8(String str) {
        return str.getBytes(UTF_8).length;
    }
}
