package io.github.mmhelloworld.idris2.jvmassembler;

import java.util.List;

public abstract class AnnotationValue {
    private final AnnotationValueType type;

    private AnnotationValue(final AnnotationValueType type) {
        this.type = type;
    }

    public AnnotationValueType getType() {
        return type;
    }

    public enum AnnotationValueType {
        AnnBoolean,
        AnnByte,
        AnnChar,
        AnnShort,
        AnnInt,
        AnnLong,
        AnnFloat,
        AnnDouble,
        AnnString,
        AnnEnum,
        AnnClass,
        AnnArray,
        AnnAnnotation
    }

    public static class AnnString extends AnnotationValue {
        private final String value;

        public AnnString(final String value) {
            super(AnnotationValueType.AnnString);
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    public static class AnnEnum extends AnnotationValue {
        private final String enumTy;
        private final String value;

        public AnnEnum(String enumTy, final String value) {
            super(AnnotationValueType.AnnEnum);
            this.enumTy = enumTy;
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public String getEnumTy() {
            return enumTy;
        }
    }

    public static class AnnInt extends AnnotationValue {
        private final int value;

        public AnnInt(final int value) {
            super(AnnotationValueType.AnnInt);
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }

    public static class AnnByte extends AnnotationValue {
        private final byte value;

        public AnnByte(final byte value) {
            super(AnnotationValueType.AnnByte);
            this.value = value;
        }

        public byte getValue() {
            return value;
        }
    }

    public static class AnnChar extends AnnotationValue {
        private final char value;

        public AnnChar(final char value) {
            super(AnnotationValueType.AnnChar);
            this.value = value;
        }

        public char getValue() {
            return value;
        }
    }

    public static class AnnBoolean extends AnnotationValue {
        private final boolean value;

        public AnnBoolean(final boolean value) {
            super(AnnotationValueType.AnnBoolean);
            this.value = value;
        }

        public boolean getValue() {
            return value;
        }
    }

    public static class AnnShort extends AnnotationValue {
        private final short value;

        public AnnShort(final short value) {
            super(AnnotationValueType.AnnShort);
            this.value = value;
        }

        public short getValue() {
            return value;
        }
    }

    public static class AnnLong extends AnnotationValue {
        private final long value;

        public AnnLong(final long value) {
            super(AnnotationValueType.AnnLong);
            this.value = value;
        }

        public long getValue() {
            return value;
        }
    }

    public static class AnnFloat extends AnnotationValue {
        private final double value;

        public AnnFloat(final double value) {
            super(AnnotationValueType.AnnFloat);
            this.value = value;
        }

        public double getValue() {
            return value;
        }
    }

    public static class AnnDouble extends AnnotationValue {
        private final double value;

        public AnnDouble(final double value) {
            super(AnnotationValueType.AnnDouble);
            this.value = value;
        }

        public double getValue() {
            return value;
        }
    }

    public static class AnnArray extends AnnotationValue {
        private final List<AnnotationValue> values;

        public AnnArray(final List<AnnotationValue> values) {
            super(AnnotationValueType.AnnArray);
            this.values = values;
        }

        public List<AnnotationValue> getValues() {
            return values;
        }
    }

    public static class AnnClass extends AnnotationValue {
        private final String value;

        public AnnClass(final String value) {
            super(AnnotationValueType.AnnClass);
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    public static class AnnAnnotation extends AnnotationValue {
        private final Annotation value;

        public AnnAnnotation(final Annotation value) {
            super(AnnotationValueType.AnnAnnotation);
            this.value = value;
        }

        public Annotation getValue() {
            return value;
        }
    }
}
