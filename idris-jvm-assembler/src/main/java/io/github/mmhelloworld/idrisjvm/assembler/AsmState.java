package io.github.mmhelloworld.idrisjvm.assembler;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import static java.util.Map.Entry.comparingByValue;

public final class AsmState {

    public static List<String> getVariableNames(Map<String, Integer> indicesByName) {
        return indicesByName.entrySet().stream()
            .sorted(comparingByValue())
            .map(Entry::getKey)
            .toList();
    }
}
