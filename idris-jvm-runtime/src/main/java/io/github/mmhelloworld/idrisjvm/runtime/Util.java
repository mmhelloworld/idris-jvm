package io.github.mmhelloworld.idrisjvm.runtime;

import java.math.BigInteger;
import java.util.Objects;

import static io.github.mmhelloworld.idrisjvm.runtime.IdrisObject.NO_ARG_CONSTRUCTOR_0;
import static io.github.mmhelloworld.idrisjvm.runtime.IdrisObject.NO_ARG_CONSTRUCTOR_1;
import static java.lang.String.format;

public class Util {

    public static Object boolToIdrisBool(boolean b) {
        return b ? NO_ARG_CONSTRUCTOR_1 : NO_ARG_CONSTRUCTOR_0;
    }

    public static boolean idrisBoolToBool(Object idrisBool) {
        return Runtime.constructorIndex(idrisBool) == 1;
    }

    public static byte idrisBits8ToByte(Object idrisBits8) {
        return (byte) ((int) idrisBits8);
    }

    public static Object byteToIdrisBits8(byte b) {
        return (int) b;
    }

    public static short idrisBits16ToShort(Object idrisBits16) {
        return (short) ((int) idrisBits16);
    }

    public static Object shortToIdrisBits16(short s) {
        return (int) s;
    }

    public static Object throwable(Thunk computation) {
        try {
            return right(computation.call());
        } catch (Throwable t) {
            return left(t);
        }
    }

    private static IdrisObject right(Object value) {
        return new IdrisObject(1, value);
    }

    private static IdrisObject left(Object value) {
        return new IdrisObject(0, value);
    }

    private static int boolToInt(final boolean b) {
        return b ? 1 : 0;
    }

    public static int asInt(Object m) {
        if (m instanceof Integer) {
            return (Integer) m;
        } else if (m instanceof Character) {
            return (Character) m;
        } else if (m instanceof Long) {
            return ((Long) m).intValue();
        } else {
            throw new IllegalArgumentException("Not an integer: " + m);
        }
    }

    public static boolean equals(Object a, Object b) {
        if (a.getClass().isAssignableFrom(b.getClass())) {
            return Objects.equals(a, b);
        } else if (a instanceof BigInteger && b instanceof Integer) {
            return a.equals(BigInteger.valueOf((Integer) b));
        } else if (a instanceof Integer && b instanceof BigInteger) {
            return b.equals(BigInteger.valueOf((Integer)a));
        } else {
            return Objects.equals(a, b);
        }
    }

    public static int hash(Object that) {
        if (that instanceof Integer) {
            return (Integer) that;
        } else if (that instanceof Character) {
            return (Character) that;
        } else {
            throw new IllegalArgumentException(format("Non hashable value: %s (%s)", that, that.getClass()));
        }
    }

    public static BigInteger asBigInt(Object m) {
        if (m instanceof BigInteger) {
            return (BigInteger) m;
        } else if (m instanceof Integer) {
            return BigInteger.valueOf((Integer) m);
        } else if (m instanceof Long) {
            return BigInteger.valueOf((Long) m);
        } else if (m instanceof String) {
            return new BigInteger((String) m);
        } else {
            throw new RuntimeException("cannot convert " + m + " to BigInteger");
        }
    }

    public static Object objectEquals(Object s, Object t) {
        return boolToInt(Objects.equals(s, t));
    }

    public static Object charLessThan(Object m, Object n) {
        return boolToInt((char) (m) < (char) (n));
    }

    public static Object charLessThanOrEqualTo(Object m, Object n) {
        return boolToInt((char) (m) <= (char) (n));
    }

    public static Object charGreaterThan(Object m, Object n) {
        return boolToInt((char) (m) > (char) (n));
    }

    public static Object charGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt((char) (m) >= (char) (n));
    }


    public static Object intLessThan(Object m, Object n) {
        return boolToInt(asInt(m) < asInt(n));
    }

    public static Object intLessThanOrEqualTo(Object m, Object n) {
        return boolToInt(asInt(m) <= asInt(n));
    }

    public static Object intGreaterThan(Object m, Object n) {
        return boolToInt(asInt(m) > asInt(n));
    }

    public static Object intGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt(asInt(m) >= asInt(n));
    }

    public static Object stringLessThan(Object m, Object n) {
        return boolToInt(((String)m).compareTo((String)n) < 0);
    }

    public static Object uintLessThan(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) < 0);
    }

    public static Object uintLessThanOrEqualTo(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) <= 0);
    }

    public static Object uintGreaterThan(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) > 0);
    }

    public static Object uintGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) >= 0);
    }

    public static Object doubleLessThan(Object m, Object n) {
        return boolToInt((double) (m) < (double) (n));
    }

    public static Object doubleLessThanOrEqualTo(Object m, Object n) {
        return boolToInt((double) (m) <= (double) (n));
    }

    public static Object doubleGreaterThan(Object m, Object n) {
        return boolToInt((double) (m) > (double) (n));
    }

    public static Object doubleGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt((double) (m) >= (double) (n));
    }

    public static Object longLessThan(Object m, Object n) {
        return boolToInt((long) (m) < (long) (n));
    }

    public static Object longLessThanOrEqualTo(Object m, Object n) {
        return boolToInt((long) (m) <= (long) (n));
    }

    public static Object longGreaterThan(Object m, Object n) {
        return boolToInt((long) (m) > (long) (n));
    }

    public static Object longGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt((long) (m) >= (long) (n));
    }

    public static Object ulongLessThan(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) < 0);
    }

    public static Object ulongLessThanOrEqualTo(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) <= 0);
    }

    public static Object ulongGreaterThan(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) > 0);
    }

    public static Object ulongGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) >= 0);
    }

    public static Object bigIntegerLessThan(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) < 0);
    }

    public static Object bigIntegerLessThanOrEqualTo(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) <= 0);
    }

    public static Object bigIntegerGreaterThan(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) > 0);
    }

    public static Object bigIntegerGreaterThanOrEqualTo(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) >= 0);
    }

}
