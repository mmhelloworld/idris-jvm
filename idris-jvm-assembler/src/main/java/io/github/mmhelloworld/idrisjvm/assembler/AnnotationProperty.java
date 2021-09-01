package io.github.mmhelloworld.idrisjvm.assembler;

public class AnnotationProperty {
    private final String name;
    private final AnnotationValue value;

    public AnnotationProperty(final String name,
                              final AnnotationValue value) {
        this.name = name;
        this.value = value;
    }

    public AnnotationValue getValue() {
        return value;
    }

    public String getName() {
        return name;
    }
}
