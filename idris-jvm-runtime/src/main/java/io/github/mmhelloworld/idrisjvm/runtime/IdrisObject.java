package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.Arrays;

public class IdrisObject {
    public static final IdrisObject NO_ARG_CONSTRUCTOR_0 = new IdrisObject(0);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_1 = new IdrisObject(1);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_2 = new IdrisObject(2);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_3 = new IdrisObject(3);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_4 = new IdrisObject(4);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_5 = new IdrisObject(5);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_6 = new IdrisObject(6);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_7 = new IdrisObject(7);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_8 = new IdrisObject(8);
    public static final IdrisObject NO_ARG_CONSTRUCTOR_9 = new IdrisObject(9);
    private static final int UNROLLED_PROPS_COUNT = 10;

    public final int constructorId;

    public final Object property0;
    public final Object property1;
    public final Object property2;
    public final Object property3;
    public final Object property4;
    public final Object property5;
    public final Object property6;
    public final Object property7;
    public final Object property8;
    public final Object property9;

    public final Object[] properties;

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3,
                       Object property4, Object property5, Object property6, Object property7, Object property8,
                       Object property9, Object[] properties) {
        this.constructorId = constructorId;
        this.property0 = property0;
        this.property1 = property1;
        this.property2 = property2;
        this.property3 = property3;
        this.property4 = property4;
        this.property5 = property5;
        this.property6 = property6;
        this.property7 = property7;
        this.property8 = property8;
        this.property9 = property9;
        this.properties = properties;
    }

    public IdrisObject(int constructorId) {
        this(constructorId, null, null, null, null, null, null, null, null, null, null, null);
    }

    public IdrisObject(int constructorId, Object property0) {
        this(constructorId, property0, null, null, null, null, null, null, null, null, null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1) {
        this(constructorId, property0, property1, null, null, null, null, null, null, null, null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2) {
        this(constructorId, property0, property1, property2, null, null, null, null, null, null, null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3) {
        this(constructorId, property0, property1, property2, property3, null, null, null, null, null, null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3,
                       Object property4) {
        this(constructorId, property0, property1, property2, property3, property4, null, null, null, null, null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3,
                       Object property4, Object property5) {
        this(constructorId, property0, property1, property2, property3, property4, property5, null, null, null, null,
                null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3,
                       Object property4, Object property5, Object property6) {
        this(constructorId, property0, property1, property2, property3, property4, property5, property6, null, null,
                null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3,
                       Object property4, Object property5, Object property6, Object property7) {
        this(constructorId, property0, property1, property2, property3, property4, property5, property6, property7,
                null, null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3,
                       Object property4, Object property5, Object property6, Object property7, Object property8) {
        this(constructorId, property0, property1, property2, property3, property4, property5, property6, property7,
                property8, null, null);
    }

    public IdrisObject(int constructorId, Object property0, Object property1, Object property2, Object property3,
                       Object property4, Object property5, Object property6, Object property7, Object property8,
                       Object property9) {
        this(constructorId, property0, property1, property2, property3, property4, property5, property6, property7,
                property8, property9, null);
    }

    @Override
    public String toString() {
        return "IdrisObject{" +
                "constructorId=" + constructorId +
                ", property0=" + property0 +
                ", property1=" + property1 +
                ", property2=" + property2 +
                ", property3=" + property3 +
                ", property4=" + property4 +
                ", property5=" + property5 +
                ", property6=" + property6 +
                ", property7=" + property7 +
                ", property8=" + property8 +
                ", property9=" + property9 +
                ", properties=" + Arrays.toString(properties) +
                '}';
    }
}
