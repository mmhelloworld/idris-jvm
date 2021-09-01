package io.github.mmhelloworld.idris2.runtime;

public class Ref {
    private Object value;

    public Ref(Object value) {
        this.value = value;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }
}
