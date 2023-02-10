package io.github.mmhelloworld.idrisjvm.runtime;

public final class IdrisMath {
    private IdrisMath() {
    }

    public static int uadd8(int x, int y) {
        return uadd(x, y, 8);
    }

    public static int uadd16(int x, int y) {
        return uadd(x, y, 16);
    }

    public static int usub8(int x, int y) {
        return usub(x, y, 8);
    }

    public static int usub16(int x, int y) {
        return usub(x, y, 16);
    }

    public static int umul8(int x, int y) {
        return umul(x, y, 8);
    }

    public static int umul16(int x, int y) {
        return umul(x, y, 16);
    }

    public static int ushl8(int num, int bits) {
        return ushl(num, bits, 8);
    }

    public static int ushl16(int num, int bits) {
        return ushl(num, bits, 16);
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

    private static int intMax(int bits) {
        return (1 << bits) - 1;
    }

    public static void main(String[] args) {
        System.out.println(Long.parseLong(args[0]) % 2L);
    }
}
