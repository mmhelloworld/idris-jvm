package idrisjvm.core;

import lombok.ToString;

import java.util.List;

abstract class JAnnotationValue {
    private final AnnotationValueType type;

    private JAnnotationValue(final AnnotationValueType type) {
        this.type = type;
    }

    public AnnotationValueType getType() {
        return type;
    }

    public enum AnnotationValueType {
        AnnString,
        AnnInt,
        AnnArray
    }

    @ToString
    public static class JAnnString extends JAnnotationValue {
        private final String value;

        public JAnnString(final String value) {
            super(AnnotationValueType.AnnString);
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    @ToString
    public static class JAnnInt extends JAnnotationValue {
        private final int value;

        public JAnnInt(final int value) {
            super(AnnotationValueType.AnnInt);
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }

    @ToString
    public static class JAnnArray extends JAnnotationValue {
        private final List<JAnnotationValue> values;

        public JAnnArray(final List<JAnnotationValue> values) {
            super(AnnotationValueType.AnnArray);
            this.values = values;
        }

        public List<JAnnotationValue> getValues() {
            return values;
        }
    }
}