package io.github.mmhelloworld.idrisjvm.runtime;

import java.text.DecimalFormat;

public final class Doubles {
    private Doubles() {
    }

    public static String toString(double d) {
        return new DecimalFormat("#.################")
            .format(d);
    }
}
