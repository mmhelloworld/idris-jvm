package com.example;

// A non-JDK class, compiled into a jar the test puts on --jvm-classpath. It is NOT on the
// idris2 compiler's own classpath, so reflecting it exercises the explicit-classpath path.
public final class Greeter {
    private final String name;

    public Greeter(String name) {
        this.name = name;
    }

    public String greet(String greeting) {
        return greeting + ", " + name + "!";
    }

    public int nameLength() {
        return name.length();
    }

    public static Greeter of(String name) {
        return new Greeter(name);
    }
}
