package io.github.mmhelloworld.idris2.jvmassembler;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import static java.util.Map.Entry.comparingByValue;
import static java.util.stream.Collectors.toList;

public final class AsmState {

    public static void updateVariableIndices(Map<String, Integer> resultIndicesByName,
                                             Map<String, Integer> indicesByName) {
        indicesByName.entrySet().stream()
            .filter(nameAndIndex -> !resultIndicesByName.containsKey(nameAndIndex.getKey()) &&
                !resultIndicesByName.containsValue(nameAndIndex.getValue()))
            .forEach(nameAndIndex -> resultIndicesByName.put(nameAndIndex.getKey(), nameAndIndex.getValue()));
    }

    public static List<String> getVariableNames(Map<String, Integer> indicesByName) {
        return indicesByName.entrySet().stream()
            .sorted(comparingByValue())
            .map(Entry::getKey)
            .collect(toList());
    }
}
