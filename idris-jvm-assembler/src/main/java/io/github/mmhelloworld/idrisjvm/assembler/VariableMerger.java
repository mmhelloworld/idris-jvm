package io.github.mmhelloworld.idrisjvm.assembler;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import static java.util.Map.Entry.comparingByValue;

/**
 * Accumulates variable indices and types while walking a scope's parent chain
 * (current scope first, root last). Replaces the per-variable parent-chain
 * walks in Compiler.Jvm.Asm.updateScopeVariableTypes with a single walk, and
 * the O(n) Map.containsValue index check with a HashSet lookup.
 */
public final class VariableMerger {
    private final Map<String, Integer> mergedIndices = new TreeMap<>();
    private final Set<Integer> usedIndices = new HashSet<>();
    private final Map<String, Integer> firstIndexByName = new HashMap<>();
    private final Map<String, Object> firstTypeByName = new HashMap<>();

    public void mergeScope(Map<String, Integer> variableIndices, Map<String, Object> variableTypes) {
        for (Entry<String, Integer> entry : variableIndices.entrySet()) {
            String name = entry.getKey();
            Integer index = entry.getValue();
            if (!mergedIndices.containsKey(name) && !usedIndices.contains(index)) {
                mergedIndices.put(name, index);
                usedIndices.add(index);
            }
            firstIndexByName.putIfAbsent(name, index);
        }
        for (Entry<String, Object> entry : variableTypes.entrySet()) {
            firstTypeByName.putIfAbsent(entry.getKey(), entry.getValue());
        }
    }

    public Map<String, Integer> getMergedIndices() {
        return mergedIndices;
    }

    /**
     * Types keyed by the first index found per merged name walking from the
     * current scope to the root, visited in merged-index order, first index
     * wins. {@code unknownType} is the Idris IUnknown value, passed in
     * opaquely; used when no scope in the chain has a type for the name.
     */
    public Map<Integer, Object> getMergedTypesByIndex(Object unknownType) {
        Map<Integer, Object> typesByIndex = new TreeMap<>();
        mergedIndices.entrySet().stream()
            .sorted(comparingByValue())
            .forEachOrdered(entry -> {
                String name = entry.getKey();
                typesByIndex.putIfAbsent(firstIndexByName.get(name),
                    firstTypeByName.getOrDefault(name, unknownType));
            });
        return typesByIndex;
    }
}
