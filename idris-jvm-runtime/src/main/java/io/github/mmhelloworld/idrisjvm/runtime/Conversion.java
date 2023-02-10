package io.github.mmhelloworld.idrisjvm.runtime;

import java.math.BigDecimal;
import java.math.BigInteger;

import static java.lang.String.format;
import static java.math.BigInteger.ONE;
import static java.math.BigInteger.ZERO;
import static java.math.RoundingMode.DOWN;

public final class Conversion {
    private Conversion() {
    }

    public static int toInt(Object that) {
        if (that == null) {
            return 0;
        } else if (that instanceof Integer) {
            return (int) that;
        } else if (that instanceof Thunk) {
            return ((Thunk) that).getInt();
        } else if (that instanceof BigInteger) {
            return ((BigInteger) that).intValue();
        } else if (that instanceof Character) {
            return (char) that;
        } else if (that instanceof Boolean) {
            return boolToInt((boolean) that);
        } else if (that instanceof Byte) {
            return (byte) that;
        } else if (that instanceof Short) {
            return (short) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to int",
                that, that.getClass()));
        }
    }

    public static int toInt1(Object that) {
        if (that == null) {
            return 0;
        } else if (that instanceof Integer) {
            return (int) that;
        } else if (that instanceof Thunk) {
            return ((Thunk) that).getInt();
        } else if (that instanceof BigInteger) {
            return ((BigInteger) that).intValueExact();
        } else if (that instanceof Character) {
            return (char) that;
        } else if (that instanceof Boolean) {
            return boolToInt1((boolean) that);
        } else if (that instanceof Byte) {
            return (byte) that;
        } else if (that instanceof Short) {
            return (short) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to int",
                that, that.getClass()));
        }
    }

    public static char toChar(Object that) {
        if (that == null) {
            return 0;
        } else if (that instanceof Character) {
            return (char) that;
        } else if (that instanceof Thunk) {
            return (char) ((Thunk) that).getInt();
        } else if (that instanceof Integer) {
            return (char) (int) (Integer) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to char",
                that, that.getClass()));
        }
    }

    public static boolean toBoolean(Object that) {
        if (that == null) {
            return false;
        } else if (that instanceof Boolean) {
            return (Boolean) that;
        } else if (that instanceof Thunk) {
            return intToBoolean(((Thunk) that).getInt());
        } else if (that instanceof Integer) {
            return intToBoolean((Integer) that);
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to boolean",
                that, that.getClass()));
        }
    }

    public static boolean toBoolean1(Object that) {
        if (that == null) {
            return false;
        } else if (that instanceof Boolean) {
            return (Boolean) that;
        } else if (that instanceof Thunk) {
            return intToBoolean1(((Thunk) that).getInt());
        } else if (that instanceof Integer) {
            return intToBoolean1((Integer) that);
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to boolean",
                that, that.getClass()));
        }
    }

    public static double toDouble(Object that) {
        if (that instanceof Double) {
            return (double) that;
        } else if (that instanceof Thunk) {
            return ((Thunk) that).getDouble();
        } else if (that instanceof Float) {
            return (float) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to double",
                that, that.getClass()));
        }
    }

    public static byte toByte(Object that) {
        if (that instanceof Integer) {
            return ((Integer) that).byteValue();
        } else if (that instanceof Byte) {
            return (byte) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to byte",
                that, that.getClass()));
        }
    }

    public static short toShort(Object that) {
        if (that instanceof Integer) {
            return ((Integer) that).shortValue();
        } else if (that instanceof Short) {
            return (short) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to short",
                that, that.getClass()));
        }
    }

    public static float toFloat(Object that) {
        if (that instanceof Double) {
            return ((Double) that).floatValue();
        } else if (that instanceof Float) {
            return (float) that;
        } else {
            throw new IllegalArgumentException(format("Unable to convert value %s of type %s to float",
                that, that.getClass()));
        }
    }

    public static BigInteger toInteger(String value) {
        try {
            return new BigDecimal(value)
                .setScale(0, DOWN)
                .toBigIntegerExact();
        } catch (NumberFormatException exception) {
            // Conforming to scheme backend
            return ZERO;
        }
    }

    public static int toInt(String value) {
        try {
            return new BigDecimal(value)
                .setScale(0, DOWN)
                .intValueExact();
        } catch (NumberFormatException exception) {
            // Conforming to scheme backend
            return 0;
        }
    }

    public static double toDouble(String value) {
        try {
            return new BigDecimal(value).doubleValue();
        } catch (NumberFormatException exception) {
            // Conforming to scheme backend
            return 0;
        }
    }

    public static BigInteger toUnsignedBigInteger(long value) {
        if (value >= 0L) {
            return BigInteger.valueOf(value);
        } else {
            int upper = (int) (value >>> 32);
            int lower = (int) value;

            // return (upper << 32) + lower
            return (BigInteger.valueOf(Integer.toUnsignedLong(upper))).shiftLeft(32).
                add(BigInteger.valueOf(Integer.toUnsignedLong(lower)));
        }
    }

    public static int boolToInt(boolean value) {
        return value ? 0 : 1;
    }

    public static int boolToInt1(boolean value) {
        return value ? 1 : 0;
    }

    public static boolean intToBoolean(int value) {
        return value == 0;
    }

    public static boolean intToBoolean1(int value) {
        return value == 1;
    }

    public static int toUnsignedInt(int value, int numberOfBits) {
        return value % (1 << numberOfBits);
    }

    public static int toUnsignedInt(long value, int numberOfBits) {
        return (int) toUnsignedLong(value, numberOfBits);
    }

    public static long toUnsignedLong(int value, int numberOfBits) {
        long max = numberOfBits < Long.SIZE ? 1L << numberOfBits : (long) Math.pow(2, numberOfBits);
        return value % max;
    }

    public static long toUnsignedLong(long value, int numberOfBits) {
        return value % (1L << numberOfBits);
    }

    public static int toUnsignedInt(BigInteger value, int numberOfBits) {
        return (int) toUnsignedLong(value, numberOfBits);
    }

    public static long toUnsignedLong(BigInteger value, int numberOfBits) {
        return value.mod(ONE.shiftLeft(numberOfBits)).longValue();
    }
}
