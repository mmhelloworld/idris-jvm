package io.github.mmhelloworld.idris2.runtime;

import java.util.ArrayList;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toCollection;

public final class Arrays {
    private Arrays() {
    }

    public static <T> ArrayList<T> create(int size, T initialElement) {
        return Stream.generate(() -> initialElement)
            .limit(size)
            .collect(toCollection(() -> new ArrayList<>(size)));
    }
}
