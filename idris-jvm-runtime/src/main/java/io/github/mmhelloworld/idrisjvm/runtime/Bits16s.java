package io.github.mmhelloworld.idrisjvm.runtime;

public final class Bits16s {
    private Bits16s() {
    }

    public static int divideUnsigned(int b1, int b2) {
        int n1 = Short.toUnsignedInt((short) b1);
        int n2 = Short.toUnsignedInt((short) b2);
        return n1 / n2;
    }

}
