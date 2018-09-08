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

    public static int boolToInt(final boolean b) {
        return b ? 1 : 0;
    }

    public static int asInt(Object m) {
        if (m instanceof Integer) {
            return (Integer) m;
        } else if (m instanceof Character) {
            return (Character) m;
        } else if (m instanceof Boolean) {
            return boolToInt((Boolean) m);
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
            return b.equals(BigInteger.valueOf((Integer) a));
        } else {
            return Objects.equals(a, b);
        }
    }

    public static int hash(Object that) {
        if (that instanceof Integer) {
            return (Integer) that;
        } else if (that instanceof Character) {
            return (Character) that;
        } else if (that instanceof Boolean) {
            return ((boolean) that) ? 1 : 0;
        } else {
            throw new IllegalArgumentException(format("Non hashable value: %s (%s)", that, that.getClass()));
        }
    }

    public static int toInt(Object that) {
        if (that instanceof Integer) {
            return (int) that;
        } else if (that instanceof Character) {
            return (char) that;
        } else if (that instanceof Boolean) {
            return ((boolean) that) ? 1 : 0;
        } else if (that instanceof Byte) {
            return (byte) that;
        } else if (that instanceof Short) {
            return (short) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type (%s) to int",
                    that, that.getClass()));
        }
    }

    public static boolean toBoolean(Object that) {
        if (that == null) {
            return false;
        } else if (that instanceof Integer) {
            return ((Integer) that) != 0;
        } else if (that instanceof Boolean) {
            return (Boolean) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type (%s) to boolean",
                    that, that.getClass()));
        }
    }

    public static double toDouble(Object that) {
        if (that instanceof Double) {
            return (double) that;
        } else if (that instanceof Float) {
            return (float) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type (%s) to double",
                    that, that.getClass()));
        }
    }

    public static byte toByte(Object that) {
        if (that instanceof Integer) {
            return ((Integer) that).byteValue();
        } else if (that instanceof Byte) {
            return (byte) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type (%s) to byte",
                    that, that.getClass()));
        }
    }

    public static short toShort(Object that) {
        if (that instanceof Integer) {
            return ((Integer) that).shortValue();
        } else if (that instanceof Short) {
            return (short) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type (%s) to short",
                    that, that.getClass()));
        }
    }

    public static float toFloat(Object that) {
        if (that instanceof Double) {
            return ((Double) that).floatValue();
        } else if (that instanceof Float) {
            return (float) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type (%s) to float",
                    that, that.getClass()));
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
        if (s != null && t != null && !s.getClass().equals(t.getClass())) {
            System.out.printf("**************** comparing different type values: %s, %s\n", s, t);
        }
        return Objects.equals(s, t);
    }

    public static Object charLessThan(Object m, Object n) {
        return boolToInt((char) (m) < (char) (n));
    }

    public static boolean charLessThan(char m, char n) {
        return m < n;
    }

    public static Object charLessThanOrEqualTo(Object m, Object n) {
        return boolToInt((char) (m) <= (char) (n));
    }

    public static boolean charLessThanOrEqualTo(char m, char n) {
        return m <= n;
    }

    public static Object charGreaterThan(Object m, Object n) {
        return boolToInt((char) (m) > (char) (n));
    }

    public static boolean charGreaterThan(char m, char n) {
        return m > n;
    }

    public static Object charGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt((char) (m) >= (char) (n));
    }

    public static boolean charGreaterThanOrEqualTo(char m, char n) {
        return m >= n;
    }

    public static Object intLessThan(Object m, Object n) {
        return boolToInt(asInt(m) < asInt(n));
    }

    public static boolean intLessThan(int m, int n) {
        return m < n;
    }

    public static Object intLessThanOrEqualTo(Object m, Object n) {
        return boolToInt(asInt(m) <= asInt(n));
    }

    public static boolean intLessThanOrEqualTo(int m, int n) {
        return m <= n;
    }

    public static Object intGreaterThan(Object m, Object n) {
        return boolToInt(asInt(m) > asInt(n));
    }

    public static boolean intGreaterThan(int m, int n) {
        return m > n;
    }

    public static Object intGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt(asInt(m) >= asInt(n));
    }

    public static boolean intGreaterThanOrEqualTo(int m, int n) {
        return m >= n;
    }

    public static Object stringLessThan(Object m, Object n) {
        return boolToInt(((String) m).compareTo((String) n) < 0);
    }

    public static boolean stringLessThan(String m, String n) {
        return m.compareTo(n) < 0;
    }

    public static Object uintLessThan(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) < 0);
    }

    public static boolean uintLessThan(int m, int n) {
        return Integer.compareUnsigned(m, n) < 0;
    }

    public static Object uintLessThanOrEqualTo(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) <= 0);
    }

    public static boolean uintLessThanOrEqualTo(int m, int n) {
        return Integer.compareUnsigned(m, n) <= 0;
    }

    public static Object uintGreaterThan(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) > 0);
    }

    public static boolean uintGreaterThan(int m, int n) {
        return Integer.compareUnsigned(m, n) > 0;
    }

    public static Object uintGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt(Integer.compareUnsigned(asInt(m), asInt(n)) >= 0);
    }

    public static boolean uintGreaterThanOrEqualTo(int m, int n) {
        return Integer.compareUnsigned(m, n) >= 0;
    }

    public static boolean doubleEquals(double m, double n) {
        return m == n;
    }

    public static boolean charEquals(char m, char n) {
        return m == n;
    }

    public static boolean intEquals(int m, int n) {
        return m == n;
    }

    public static boolean longEquals(long m, long n) {
        return m == n;
    }

    public static boolean bigIntegerEquals(BigInteger m, BigInteger n) {
        return m.equals(n);
    }

    public static boolean stringEquals(String m, String n) {
        return m.equals(n);
    }

    public static Object doubleLessThan(Object m, Object n) {
        return boolToInt((double) (m) < (double) (n));
    }

    public static boolean doubleLessThan(double m, double n) {
        return m < n;
    }

    public static Object doubleLessThanOrEqualTo(Object m, Object n) {
        return boolToInt((double) (m) <= (double) (n));
    }

    public static boolean doubleLessThanOrEqualTo(double m, double n) {
        return m <= n;
    }

    public static Object doubleGreaterThan(Object m, Object n) {
        return boolToInt((double) (m) > (double) (n));
    }

    public static boolean doubleGreaterThan(double m, double n) {
        return m > n;
    }

    public static Object doubleGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt((double) (m) >= (double) (n));
    }

    public static boolean doubleGreaterThanOrEqualTo(double m, double n) {
        return m >= n;
    }

    public static Object longLessThan(Object m, Object n) {
        return boolToInt((long) (m) < (long) (n));
    }

    public static boolean longLessThan(long m, long n) {
        return m < n;
    }

    public static Object longLessThanOrEqualTo(Object m, Object n) {
        return boolToInt((long) (m) <= (long) (n));
    }

    public static boolean longLessThanOrEqualTo(long m, long n) {
        return m <= n;
    }

    public static Object longGreaterThan(Object m, Object n) {
        return boolToInt((long) (m) > (long) (n));
    }

    public static boolean longGreaterThan(long m, long n) {
        return m > n;
    }

    public static Object longGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt((long) (m) >= (long) (n));
    }

    public static boolean longGreaterThanOrEqualTo(long m, long n) {
        return m >= n;
    }

    public static Object ulongLessThan(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) < 0);
    }

    public static boolean ulongLessThan(long m, long n) {
        return Long.compareUnsigned(m, n) < 0;
    }

    public static Object ulongLessThanOrEqualTo(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) <= 0);
    }

    public static boolean ulongLessThanOrEqualTo(long m, long n) {
        return Long.compareUnsigned(m, n) <= 0;
    }

    public static Object ulongGreaterThan(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) > 0);
    }

    public static boolean ulongGreaterThan(long m, long n) {
        return Long.compareUnsigned(m, n) > 0;
    }

    public static Object ulongGreaterThanOrEqualTo(Object m, Object n) {
        return boolToInt(Long.compareUnsigned((long) m, (long) n) >= 0);
    }

    public static boolean ulongGreaterThanOrEqualTo(long m, long n) {
        return Long.compareUnsigned(m, n) >= 0;
    }

    public static Object bigIntegerLessThan(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) < 0);
    }

    public static boolean bigIntegerLessThan(BigInteger m, BigInteger n) {
        return m.compareTo(n) < 0;
    }

    public static Object bigIntegerLessThanOrEqualTo(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) <= 0);
    }

    public static boolean bigIntegerLessThanOrEqualTo(BigInteger m, BigInteger n) {
        return m.compareTo(n) <= 0;
    }

    public static Object bigIntegerGreaterThan(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) > 0);
    }

    public static boolean bigIntegerGreaterThan(BigInteger m, BigInteger n) {
        return m.compareTo(n) > 0;
    }

    public static Object bigIntegerGreaterThanOrEqualTo(Object m, Object n) {
        BigInteger b1 = (BigInteger) m;
        BigInteger b2 = (BigInteger) n;
        return boolToInt(b1.compareTo(b2) >= 0);
    }

    public static boolean bigIntegerGreaterThanOrEqualTo(BigInteger m, BigInteger n) {
        return m.compareTo(n) >= 0;
    }

}
