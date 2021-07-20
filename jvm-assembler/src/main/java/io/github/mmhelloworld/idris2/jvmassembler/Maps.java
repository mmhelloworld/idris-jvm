package io.github.mmhelloworld.idris2.jvmassembler;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.function.BinaryOperator;

import static java.util.stream.Collectors.toMap;

public final class Maps {
    private Maps() {
    }

    public static <K, V> Map<V, K> transpose(Map<K, V> map) {
        return map.entrySet().stream()
            .collect(toMap(Entry::getValue, Entry::getKey, throwDuplicateEntries(), TreeMap::new));
    }

    public static <K, V> List<Entry<K, V>> toList(Map<K, V> map) {
        return new ArrayList<>(map.entrySet());
    }

    public static <K, V> List<K> keys(Map<K, V> map) {
        return new ArrayList<>(map.keySet());
    }

    public static <K, V> List<V> values(Map<K, V> map) {
        return new ArrayList<>(map.values());
    }

    public static <K, V1, V2> Map<K, V2> getValue2(Map<K, Entry<V1, V2>> map) {
        return map.entrySet().stream()
            .map(entry -> new SimpleImmutableEntry<>(entry.getKey(), entry.getValue().getValue()))
            .collect(toMap(Entry::getKey, Entry::getValue, throwDuplicateEntries(), TreeMap::new));
    }

    private static <K> BinaryOperator<K> throwDuplicateEntries() {
        return (entry1, entry2) -> {
            throw new IllegalStateException("Duplicate entries in Map");
        };
    }
}
