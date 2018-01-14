package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.Arrays;

public class IdrisObject {
    public static final IdrisObject IDRIS_NO_ARG_CONSTRUCTOR_0 = new IdrisObject(0);
    public static final IdrisObject IDRIS_NO_ARG_CONSTRUCTOR_1 = new IdrisObject(1);
    private final int constructorId;
    private final Object[] properties;

    public IdrisObject(int constructorId) {
        this(constructorId, new Object[] {IDRIS_NO_ARG_CONSTRUCTOR_0});
    }

    public IdrisObject(int constructorId, Object[] properties) {
        this.constructorId = constructorId;
        this.properties = properties;
    }

    public Object[] getProperties() {
        return properties;
    }

    public int getConstructorId() {
        return constructorId;
    }

    public static Object getProperty(Object obj, int propertyIndex) {
        return ((IdrisObject)obj).getProperties()[propertyIndex];
    }

    @Override
    public String toString() {
        return "IdrisObject{" +
                "constructorId=" + constructorId +
                ", properties=" + Arrays.toString(properties) +
                '}';
    }
}
