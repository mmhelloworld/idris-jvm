package io.github.mmhelloworld.idrisjvm.runtime;

public final class IdrisMath {
    private IdrisMath() {
    }

    public static int uadd8(int x, int y) {
        return uadd(x, y, Byte.SIZE);
    }

    public static int add8(int x, int y) {
        return (byte) (x + y);
    }

    public static int uadd16(int x, int y) {
        return uadd(x, y, Short.SIZE);
    }

    public static int add16(int x, int y) {
        return (short) (x + y);
    }

    public static int usub8(int x, int y) {
        return usub(x, y, Byte.SIZE);
    }

    public static int sub8(int x, int y) {
        return (byte) (x - y);
    }

    public static int usub16(int x, int y) {
        return usub(x, y, Short.SIZE);
    }

    public static int sub16(int x, int y) {
        return (short) (x - y);
    }

    public static int umul8(int x, int y) {
        return umul(x, y, Byte.SIZE);
    }

    public static int mul8(int x, int y) {
        return (byte) (x * y);
    }

    public static int umul16(int x, int y) {
        return umul(x, y, Short.SIZE);
    }

    public static int mul16(int x, int y) {
        return (short) (x * y);
    }

    public static int div8(int x, int y) {
        return (byte) (x / y);
    }

    public static int div16(int x, int y) {
        return (short) (x / y);
    }

    public static int ushl8(int num, int bits) {
        return ushl(num, bits, Byte.SIZE);
    }

    public static int shl8(int num, int bits) {
        return (byte) (num << bits);
    }

    public static int ushl16(int num, int bits) {
        return ushl(num, bits, Short.SIZE);
    }

    public static int shl16(int num, int bits) {
        return (short) (num << bits);
    }

    public static int intMax(int bits) {
        return (1 << bits) - 1;
    }

    private static int ushl(int num, int bits, int radix) {
        return (num << bits) & intMax(radix);
    }

    private static int uadd(int x, int y, int bits) {
        return (x + y) & intMax(bits);
    }

    private static int usub(int x, int y, int bits) {
        return (x - y) & intMax(bits);
    }

    private static int umul(int x, int y, int bits) {
        return (x * y) & intMax(bits);
    }
}
