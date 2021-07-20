package io.github.mmhelloworld.idris2.jvmassembler;

import java.util.List;

public class Annotation {
    private final String name;
    private final List<AnnotationProperty> properties;

    public Annotation(final String name,
                      final List<AnnotationProperty> properties) {
        this.name = name;
        this.properties = properties;
    }

    public String getName() {
        return name;
    }

    public List<AnnotationProperty> getProperties() {
        return properties;
    }
}

