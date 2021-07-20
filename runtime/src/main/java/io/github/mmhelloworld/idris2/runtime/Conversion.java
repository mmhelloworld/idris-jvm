package io.github.mmhelloworld.idris2.runtime;

import java.math.BigDecimal;
import java.math.BigInteger;

import static java.lang.String.format;

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
            return ((BigInteger) that).intValueExact();
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
            return new BigDecimal(value).toBigInteger();
        } catch (NumberFormatException exception) {
            // Conforming to scheme backend
            return BigInteger.ZERO;
        }
    }

    public static int toInt(String value) {
        try {
            return new BigDecimal(value).toBigInteger().intValueExact();
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

    public static int boolToInt(boolean value) {
        return value ? 0 : 1;
    }

    public static boolean intToBoolean(int value) {
        return value != 0;
    }
}
