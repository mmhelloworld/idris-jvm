package io.github.mmhelloworld.idrisjvm.runtime;

public final class Bits8s {
    private Bits8s() {
    }

    public static int divideUnsigned(int b1, int b2) {
        int n1 = Byte.toUnsignedInt((byte) b1);
        int n2 = Byte.toUnsignedInt((byte) b2);
        return n1 / n2;
    }

}
