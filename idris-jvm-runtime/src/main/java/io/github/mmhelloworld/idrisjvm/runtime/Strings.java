package io.github.mmhelloworld.idrisjvm.runtime;

import static java.nio.charset.StandardCharsets.UTF_8;

public final class Strings {
    private Strings() {
    }

    public static char strHead(String str) {
        return strIndex(str, 0);
    }

    public static char strIndex(String str, int index) {
        /*
         * Similar to Idris C back end behavior that returns
         * '\NUL' when index is >= length of the string
         */
        return index >= str.length() ? 0 : str.charAt(index);
    }

    public static int bytesLengthUtf8(String str) {
        return str.getBytes(UTF_8).length;
    }
}
