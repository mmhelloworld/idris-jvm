package io.github.mmhelloworld.idrisjvm.runtime;

import java.math.BigInteger;

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
        return (byte) euclidDiv(x, y);
    }

    public static int div16(int x, int y) {
        return (short) euclidDiv(x, y);
    }

    // Division and modulo on signed types follow Euclidean semantics (remainder is always non-negative),
    // matching "blodwen-euclidDiv"/"blodwen-euclidMod" in the reference backends.
    public static int euclidDiv(int x, int y) {
        var quotient = x / y;
        if (x % y < 0) {
            return y > 0 ? quotient - 1 : quotient + 1;
        }
        return quotient;
    }

    public static int euclidMod(int x, int y) {
        var remainder = x % y;
        if (remainder < 0) {
            return y > 0 ? remainder + y : remainder - y;
        }
        return remainder;
    }

    public static long euclidDiv(long x, long y) {
        var quotient = x / y;
        if (x % y < 0) {
            return y > 0 ? quotient - 1 : quotient + 1;
        }
        return quotient;
    }

    public static long euclidMod(long x, long y) {
        var remainder = x % y;
        if (remainder < 0) {
            return y > 0 ? remainder + y : remainder - y;
        }
        return remainder;
    }

    public static BigInteger euclidDiv(BigInteger x, BigInteger y) {
        var quotientAndRemainder = x.divideAndRemainder(y);
        var quotient = quotientAndRemainder[0];
        if (quotientAndRemainder[1].signum() < 0) {
            return y.signum() > 0 ? quotient.subtract(BigInteger.ONE) : quotient.add(BigInteger.ONE);
        }
        return quotient;
    }

    public static BigInteger euclidMod(BigInteger x, BigInteger y) {
        var remainder = x.remainder(y);
        if (remainder.signum() < 0) {
            return y.signum() > 0 ? remainder.add(y) : remainder.subtract(y);
        }
        return remainder;
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
