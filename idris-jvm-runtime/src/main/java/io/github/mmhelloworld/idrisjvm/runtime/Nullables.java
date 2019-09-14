package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.Optional;

public final class Nullables {
    private Nullables() {
    }

    public static Object nullableToObject(Object nullable) {
        System.out.println("foo bar");
        if (nullable instanceof IdrisObject) {
            IdrisObject maybe = (IdrisObject) nullable;
            switch (maybe.constructorId) {
                case 0:
                    return null;
                case 1:
                    return maybe.property0;
                default:
                    throw new IllegalArgumentException("Not an Idris Maybe value " + maybe);
            }
        } else {
            return nullable;
        }
    }

    public static IdrisObject objectToNullable(Object obj) {
        System.out.println("asfasdfasd");
        return Optional.ofNullable(obj)
            .map(v -> new IdrisObject(1, v))
            .orElse(IdrisObject.NO_ARG_CONSTRUCTOR_0);
    }
}
