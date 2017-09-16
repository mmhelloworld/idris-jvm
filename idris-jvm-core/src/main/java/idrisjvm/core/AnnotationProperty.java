package IdrisJvm.Core;

class AnnotationProperty {
    private final String name;
    private final JAnnotationValue value;

    public AnnotationProperty(final String name,
                              final JAnnotationValue value) {
        this.name = name;
        this.value = value;
    }

    public JAnnotationValue getValue() {
        return value;
    }

    public String getName() {
        return name;
    }
}