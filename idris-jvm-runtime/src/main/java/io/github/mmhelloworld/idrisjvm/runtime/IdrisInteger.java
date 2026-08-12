package io.github.mmhelloworld.idrisjvm.runtime;

import java.math.BigInteger;

/**
 * Runtime operations for Idris {@code Integer} values.
 *
 * <p>An {@code Integer} value is represented as {@link Long} while it fits in
 * 64 bits and as {@link BigInteger} only beyond that (the canonical form).
 * Every operation here accepts any {@link Number} — {@code believe_me} can
 * route {@link java.lang.Integer}-boxed values into Integer-typed positions —
 * and always returns the canonical form, so identity-sensitive consumers
 * (hash-code switches, equality) see one representation per value.
 */
public final class IdrisInteger {

    private static final BigInteger LONG_MIN = BigInteger.valueOf(Long.MIN_VALUE);
    private static final BigInteger LONG_MAX = BigInteger.valueOf(Long.MAX_VALUE);

    private IdrisInteger() {
    }

    private static boolean isBig(Object value) {
        return value instanceof BigInteger;
    }

    private static long longOf(Object value) {
        return ((Number) value).longValue();
    }

    private static BigInteger bigOf(Object value) {
        return value instanceof BigInteger big ? big : BigInteger.valueOf(longOf(value));
    }

    private static Object canonical(BigInteger value) {
        return value.bitLength() < Long.SIZE ? (Object) value.longValue() : (Object) value;
    }

    /** Canonicalize a value arriving from outside generated code (FFI). */
    public static Object canonical(Object value) {
        return isBig(value) ? canonical((BigInteger) value) : (Object) longOf(value);
    }

    public static Object fromInt(int value) {
        return (long) value;
    }

    public static Object fromLong(long value) {
        return value;
    }

    public static Object fromUnsignedLong(long value) {
        return value >= 0 ? (Object) value : (Object) new BigInteger(Long.toUnsignedString(value));
    }

    public static Object fromString(String value) {
        return canonical(Conversion.toInteger(value));
    }

    public static Object fromDouble(double value) {
        return canonical(java.math.BigDecimal.valueOf(value).toBigInteger());
    }

    public static BigInteger toBigInteger(Object value) {
        return bigOf(value);
    }

    public static int toInt(Object value) {
        return isBig(value) ? ((BigInteger) value).intValue() : (int) longOf(value);
    }

    public static long toLong(Object value) {
        return isBig(value) ? ((BigInteger) value).longValue() : longOf(value);
    }

    public static double toDouble(Object value) {
        return isBig(value) ? ((BigInteger) value).doubleValue() : (double) longOf(value);
    }

    public static int toUnsignedInt(Object value, int numberOfBits) {
        return (int) toUnsignedLong(value, numberOfBits);
    }

    public static long toUnsignedLong(Object value, int numberOfBits) {
        if (isBig(value)) {
            return Conversion.toUnsignedLong((BigInteger) value, numberOfBits);
        }
        long unsigned = longOf(value);
        return numberOfBits >= Long.SIZE
            ? unsigned
            : Math.floorMod(unsigned, 1L << numberOfBits);
    }

    public static String toString(Object value) {
        return value.toString();
    }

    /**
     * Hash consistent with the canonical form: {@link Long#hashCode(long)}
     * for values that fit, {@link BigInteger#hashCode()} beyond. The
     * compiler computes the same function for switch constants.
     */
    public static int hash(Object value) {
        return isBig(value) ? value.hashCode() : Long.hashCode(longOf(value));
    }

    public static int equals(Object left, Object right) {
        return compare(left, right) == 0 ? 1 : 0;
    }

    public static int compare(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return bigOf(left).compareTo(bigOf(right));
        }
        return Long.compare(longOf(left), longOf(right));
    }

    public static Object add(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(bigOf(left).add(bigOf(right)));
        }
        long l = longOf(left);
        long r = longOf(right);
        long result = l + r;
        // Overflow check as in Math.addExact, without the exception
        if (((l ^ result) & (r ^ result)) < 0) {
            return bigOf(left).add(bigOf(right));
        }
        return result;
    }

    public static Object subtract(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(bigOf(left).subtract(bigOf(right)));
        }
        long l = longOf(left);
        long r = longOf(right);
        long result = l - r;
        if (((l ^ r) & (l ^ result)) < 0) {
            return bigOf(left).subtract(bigOf(right));
        }
        return result;
    }

    public static Object multiply(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(bigOf(left).multiply(bigOf(right)));
        }
        long l = longOf(left);
        long r = longOf(right);
        long high = Math.multiplyHigh(l, r);
        long result = l * r;
        if (high != (result >> 63)) {
            return bigOf(left).multiply(bigOf(right));
        }
        return result;
    }

    public static Object negate(Object value) {
        if (isBig(value)) {
            return canonical(((BigInteger) value).negate());
        }
        long v = longOf(value);
        return v == Long.MIN_VALUE ? (Object) LONG_MIN.negate() : (Object) (-v);
    }

    public static Object euclidDiv(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(IdrisMath.euclidDiv(bigOf(left), bigOf(right)));
        }
        long l = longOf(left);
        long r = longOf(right);
        // The only long division that overflows
        if (l == Long.MIN_VALUE && r == -1) {
            return LONG_MIN.negate();
        }
        return IdrisMath.euclidDiv(l, r);
    }

    public static Object euclidMod(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(IdrisMath.euclidMod(bigOf(left), bigOf(right)));
        }
        return IdrisMath.euclidMod(longOf(left), longOf(right));
    }

    public static Object shiftLeft(Object left, Object right) {
        int bits = toInt(right);
        if (!isBig(left)) {
            long l = longOf(left);
            // A long value shifted within 63 bits total cannot overflow
            if (bits >= 0 && bits < Long.SIZE && (64 - Long.numberOfLeadingZeros(l < 0 ? ~l : l)) + bits < 63) {
                return l << bits;
            }
        }
        return canonical(bigOf(left).shiftLeft(bits));
    }

    public static Object shiftRight(Object left, Object right) {
        int bits = toInt(right);
        if (!isBig(left)) {
            return bits >= Long.SIZE ? (Object) (longOf(left) >> 63) : (Object) (longOf(left) >> bits);
        }
        return canonical(bigOf(left).shiftRight(bits));
    }

    public static Object and(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(bigOf(left).and(bigOf(right)));
        }
        return longOf(left) & longOf(right);
    }

    public static Object or(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(bigOf(left).or(bigOf(right)));
        }
        return longOf(left) | longOf(right);
    }

    public static Object xor(Object left, Object right) {
        if (isBig(left) || isBig(right)) {
            return canonical(bigOf(left).xor(bigOf(right)));
        }
        return longOf(left) ^ longOf(right);
    }
}
