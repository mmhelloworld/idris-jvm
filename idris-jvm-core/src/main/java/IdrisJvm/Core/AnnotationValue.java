package IdrisJvm.Core;

import lombok.ToString;

import java.util.List;

abstract class AnnotationValue {
    private final AnnotationValueType type;

    private AnnotationValue(final AnnotationValueType type) {
        this.type = type;
    }

    public AnnotationValueType getType() {
        return type;
    }

    public enum AnnotationValueType {
        AnnString,
        AnnInt,
        AnnArray,
        AnnEnum;
    }

    @ToString
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

    @ToString
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

    @ToString
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

    @ToString
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
}