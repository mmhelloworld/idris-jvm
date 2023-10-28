package io.github.mmhelloworld.idrisjvm.runtime;

import java.lang.reflect.Array;
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

    public static int[] toIntArray(ArrayList<Integer> arrayList) {
        return arrayList.stream().mapToInt(i -> i).toArray();
    }

    public static <T> T[] toArray(ArrayList<T> arrayList, Class<T> clazz) {
        return arrayList.toArray((T[]) Array.newInstance(clazz, arrayList.size()));
    }


}
