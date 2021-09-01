package io.github.mmhelloworld.idris2.runtime;

public final class Objects {
    private Objects() {
    }

    public static int isNull(Object object) {
        return object == null ? 1 : 0;
    }

    public static String getString(Object string) {
        return (String) string;
    }
}
