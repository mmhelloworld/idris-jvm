package io.github.mmhelloworld.idrisjvm.runtime;

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
}
