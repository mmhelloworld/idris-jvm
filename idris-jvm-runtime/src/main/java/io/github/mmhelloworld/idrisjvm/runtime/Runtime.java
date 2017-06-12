package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.Scanner;

public class Runtime {
    private static Scanner inputScanner = new Scanner(System.in);

    public static String readString() {
        return inputScanner.nextLine();
    }

    public static char readChar() {
        return inputScanner.nextLine().charAt(0);
    }

    public static Integer writeString(Object s) {
        System.out.print(s);
        return 0;
    }

    public static Thunk thunk(Function fn, Object[] args) {
        return () -> fn.apply(args);
    }

    public static Object error(Object s) {
        throw new RuntimeException(s.toString());
    }

    public static Object unwrap(Object value) {
        while (value instanceof Thunk) {
            value = ((Thunk) value).call();
        }
        return value;
    }

    public static int constructorIndex(Object obj) {
        if (obj instanceof Object[]) {
            return (int) ((Object[]) obj)[0];
        } else {
            return obj == null ? 0 : (Integer)obj;
        }
    }

}
