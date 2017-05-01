package mmhelloworld.idrisjvmruntime;

import java.math.BigInteger;
import java.util.Objects;

import static java.lang.String.format;

public class Util {

    private static final Object[] NOTHING = new Object[]{0};

    public static Object boolToIntObject(boolean b) {
        return boolToInt(b);
    }

    public static Object boolToIdrisBool(boolean b) {
        return new Object[]{boolToInt(b)}; // Idris Bool constructor
    }

    public static boolean idrisBoolToBool(Object idrisBool) {
        return ((int) ((Object[]) idrisBool)[0]) == 1;
    }

    public static byte idrisBits8ToByte(Object idrisBits8) {
        return (byte)((int) idrisBits8);
    }

    public static Object byteToIdrisBits8(byte b) {
        return (int)b;
    }

    public static short idrisBits16ToShort(Object idrisBits16) {
        return (short)((int) idrisBits16);
    }

    public static Object shortToIdrisBits16(short s) {
        return (int)s;
    }

    public static Object nullableRefToMaybe(Object ref) {
        return ref == null ? NOTHING : new Object[] {1, ref};
    }

    public static Object maybeToNullableRef(Object maybe) {
        Object[] maybeValue = (Object[]) maybe;
        int constructor = (int) maybeValue[0];
        return constructor == 0 ? null : maybeValue[1];
    }

    private static int boolToInt(final boolean b) {
        return b ? 1 : 0;
    }

    public static int asInt(Object m) {
        if (m instanceof Character) {
            return (Character) m;
        } else {
            return (int) m;
        }
    }

    public static boolean equals(Object a, Object b) {
        if (a.getClass().isAssignableFrom(b.getClass())) {
            return Objects.equals(a, b);
        } else if (a instanceof BigInteger && b instanceof Integer) {
            return a.equals(BigInteger.valueOf((Integer) b));
        } else if (a instanceof Integer && b instanceof BigInteger) {
            return equals(b, a);
        } else {
            throw new RuntimeException(
                format("Comparing incompatible types: %s (%s) and %s (%s)", a, a.getClass(), b, b.getClass()));
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
        } else if (m instanceof String) {
            return new BigInteger((String) m);
        } else {
            throw new RuntimeException("cannot convert " + m + " to BigInteger");
        }
    }

    public static Object objectEquals(Object s, Object t) {
        return boolToIntObject(Objects.equals(s, t));
    }

    public static Object charLessThan(Object m, Object n) {
        return boolToIntObject((char) (m) < (char) (n));
    }

    public static Object charLessThanOrEqualTo(Object m, Object n) {
        return boolToIntObject((char) (m) <= (char) (n));
    }

    public static Object charGreaterThan(Object m, Object n) {
        return boolToIntObject((char) (m) > (char) (n));
    }

    public static Object charGreaterThanOrEqualTo(Object m, Object n) {
        return boolToIntObject((char) (m) >= (char) (n));
    }


    public static Object intLessThan(Object m, Object n) {
        return boolToIntObject(asInt(m) < asInt(n));
    }

    public static Object intLessThanOrEqualTo(Object m, Object n) {
        return boolToIntObject(asInt(m) <= asInt(n));
    }

    public static Object intGreaterThan(Object m, Object n) {
        return boolToIntObject(asInt(m) > asInt(n));
    }

    public static Object intGreaterThanOrEqualTo(Object m, Object n) {
        return boolToIntObject(asInt(m) >= asInt(n));
    }


    public static Object uintLessThan(Object m, Object n) {
        return boolToIntObject(Integer.compareUnsigned(asInt(m), asInt(n)) < 0);
    }

    public static Object uintLessThanOrEqualTo(Object m, Object n) {
        return boolToIntObject(Integer.compareUnsigned(asInt(m), asInt(n)) <= 0);
    }

    public static Object uintGreaterThan(Object m, Object n) {
        return boolToIntObject(Integer.compareUnsigned(asInt(m), asInt(n)) > 0);
    }

    public static Object uintGreaterThanOrEqualTo(Object m, Object n) {
        return boolToIntObject(Integer.compareUnsigned(asInt(m), asInt(n)) >= 0);
    }

    public static Object doubleLessThan(Object m, Object n) {
        return boolToIntObject((double) (m) < (double) (n));
    }

    public static Object doubleLessThanOrEqualTo(Object m, Object n) {
        return boolToIntObject((double) (m) <= (double) (n));
    }

    public static Object doubleGreaterThan(Object m, Object n) {
        return boolToIntObject((double) (m) > (double) (n));
    }

    public static Object doubleGreaterThanOrEqualTo(Object m, Object n) {
        return boolToIntObject((double) (m) >= (double) (n));
    }

    public static Object longLessThan(Object m, Object n) {
        return boolToIntObject((long) (m) < (long) (n));
    }

    public static Object longLessThanOrEqualTo(Object m, Object n) {
        return boolToIntObject((long) (m) <= (long) (n));
    }

    public static Object longGreaterThan(Object m, Object n) {
        return boolToIntObject((long) (m) > (long) (n));
    }

    public static Object longGreaterThanOrEqualTo(Object m, Object n) {
        return boolToIntObject((long) (m) >= (long) (n));
    }

    public static Object ulongLessThan(Object m, Object n) {
        return boolToIntObject(Long.compareUnsigned((long) m, (long) n) < 0);
    }

    public static Object ulongLessThanOrEqualTo(Object m, Object n) {
        return boolToIntObject(Long.compareUnsigned((long) m, (long) n) <= 0);
    }

    public static Object ulongGreaterThan(Object m, Object n) {
        return boolToIntObject(Long.compareUnsigned((long) m, (long) n) > 0);
    }

    public static Object ulongGreaterThanOrEqualTo(Object m, Object n) {
        return boolToIntObject(Long.compareUnsigned((long) m, (long) n) >= 0);
    }

    public static Object bigIntegerLessThan(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToIntObject(b1.compareTo(b2) < 0);
    }

    public static Object bigIntegerLessThanOrEqualTo(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToIntObject(b1.compareTo(b2) <= 0);
    }

    public static Object bigIntegerGreaterThan(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToIntObject(b1.compareTo(b2) > 0);
    }

    public static Object bigIntegerGreaterThanOrEqualTo(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToIntObject(b1.compareTo(b2) >= 0);
    }

}
