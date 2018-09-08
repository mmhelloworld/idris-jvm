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

    public Object property0;
    public Object property1;
    public Object property2;
    public Object property3;
    public Object property4;
    public Object property5;
    public Object property6;
    public Object property7;
    public Object property8;
    public Object property9;

    public Object[] properties;

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

    public IdrisObject(int constructorId, Object[] properties) {
        this.constructorId = constructorId;
        int propertiesCount = properties.length;

        if (propertiesCount > 0) {
            this.property0 = properties[0];
        }
        if (propertiesCount > 1) {
            this.property1 = properties[1];
        }
        if (propertiesCount > 2) {
            this.property2 = properties[2];
        }
        if (propertiesCount > 3) {
            this.property3 = properties[3];
        }
        if (propertiesCount > 4) {
            this.property4 = properties[4];
        }
        if (propertiesCount > 5) {
            this.property5 = properties[5];
        }
        if (propertiesCount > 6) {
            this.property6 = properties[6];
        }
        if (propertiesCount > 7) {
            this.property7 = properties[7];
        }
        if (propertiesCount > 8) {
            this.property8 = properties[8];
        }
        if (propertiesCount > 9) {
            this.property9 = properties[9];
        }
        if (propertiesCount > 10) {
            this.properties = Arrays.copyOfRange(properties, 10, propertiesCount);
        }
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

    public static Object getProperty(Object obj, int propertyIndex) {
        if (obj instanceof IdrisObject) {
            IdrisObject idrisObject = (IdrisObject) obj;
            if (propertyIndex < UNROLLED_PROPS_COUNT) {
                switch (propertyIndex) {
                    case 0:
                        return idrisObject.property0;
                    case 1:
                        return idrisObject.property1;
                    case 2:
                        return idrisObject.property2;
                    case 3:
                        return idrisObject.property3;
                    case 4:
                        return idrisObject.property4;
                    case 5:
                        return idrisObject.property5;
                    case 6:
                        return idrisObject.property6;
                    case 7:
                        return idrisObject.property7;
                    case 8:
                        return idrisObject.property8;
                    case 9:
                        return idrisObject.property9;
                    default:
                        throw new IllegalStateException("Unexpected property index: " + propertyIndex);
                }
            } else {
                return idrisObject.properties[propertyIndex - UNROLLED_PROPS_COUNT];
            }
        } else {
            return obj;
        }
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
