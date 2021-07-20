package io.github.mmhelloworld.idris2.jvmassembler;

import io.github.mmhelloworld.idris2.runtime.IdrisList;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toCollection;

public final class IdrisName {

    public static final String CLASS_NAME_FUNCTION_NAME_SEPARATOR = ",";
    private static final Map<Character, String> replacements = new HashMap<>();

    static {
        replacements.put(' ', "$spc");
        replacements.put('!', "$not");
        replacements.put('"', "$dquo");
        replacements.put('#', "$hash");
        replacements.put('%', "$mod");
        replacements.put('&', "$and");
        replacements.put('\'', "$squo");
        replacements.put('(', "$lpar");
        replacements.put(')', "$rpar");
        replacements.put('*', "$mul");
        replacements.put('+', "$add");
        replacements.put(',', "$com");
        replacements.put('-', "$hyp");
        replacements.put('.', "$dot");
        replacements.put('/', "$div");
        replacements.put('\\', "$bsl");
        replacements.put(':', "$col");
        replacements.put(';', "$scol");
        replacements.put('<', "$lt");
        replacements.put('=', "$eq");
        replacements.put('>', "$gt");
        replacements.put('?', "$ques");
        replacements.put('@', "$at");
        replacements.put('^', "$caret");
        replacements.put('`', "$grave");
        replacements.put('{', "$lbr");
        replacements.put('|', "$or");
        replacements.put('}', "$rbr");
        replacements.put('~', "$tilde");
        replacements.put('[', "$lsqr");
        replacements.put(']', "$rsqr");
    }

    private IdrisName() {
    }

    public static IdrisList getIdrisFunctionName(String programName, String idrisNamespace, String idrisFunctionName) {
        return getIdrisName(programName, idrisNamespace, idrisFunctionName);
    }

    public static String getIdrisConstructorClassName(String idrisName) {
        return String.join("/", addModulePrefix("main", asList((idrisName).split("/"))));
    }

    public static String transformCharacters(String value) {
        return value
            .chars()
            .flatMap(c -> replacements.getOrDefault((char) c, String.valueOf((char) c)).chars())
            .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
            .toString();
    }

    public static String transformCharacter(char c) {
        return replacements.getOrDefault(c, String.valueOf(c));
    }

    private static IdrisList getIdrisName(String programName, String idrisNamespace, String memberName) {
        Entry<String, String> classAndMemberName = getClassAndMemberName(programName, idrisNamespace, memberName);
        return IdrisList.fromIterable(asList(classAndMemberName.getKey(), classAndMemberName.getValue()));
    }

    private static String join(String value1, String value2) {
        return value1 + CLASS_NAME_FUNCTION_NAME_SEPARATOR + value2;
    }

    private static Entry<String, String> getClassAndMemberName(String programName, String idrisNamespace,
                                                               String memberName) {
        LinkedList<String> moduleParts = Stream.of(idrisNamespace.split("/")).collect(toCollection(LinkedList::new));
        String className = String.join("/", addModulePrefix(programName, moduleParts));
        return new SimpleImmutableEntry<>(className, memberName);
    }

    private static List<String> addModulePrefix(String programName, List<String> nameParts) {
        int size = nameParts.size();
        if (size > 1) {
            List<String> packageNameParts = nameParts.subList(1, size - 1);
            LinkedList<String> prefixedPackagedNames = packageNameParts.stream()
                .map(packageName -> "M$" + packageName)
                .collect(toCollection(LinkedList::new));
            String rootPackage = nameParts.get(0);
            prefixedPackagedNames.add(0, rootPackage.equals(programName) ? rootPackage : "M$" + rootPackage);
            String className = nameParts.get(size - 1);
            prefixedPackagedNames.add(className);
            return prefixedPackagedNames;
        } else {
            return asList(programName, nameParts.get(0));
        }
    }

    private static <T> LinkedList<T> add(LinkedList<T> items, T item) {
        items.add(item);
        return items;
    }

    private static <T> LinkedList<T> tail(LinkedList<T> items) {
        return new LinkedList<>(items.subList(1, items.size()));
    }

    private static Stream<String> getIdrisModuleNameFromFileName(String fileName) {
        Path path = Paths.get(fileName.replaceAll("^\\.+|\\.idr$", ""));
        Path module = path.normalize();
        return IntStream.range(0, module.getNameCount())
            .mapToObj(module::getName)
            .map(Path::toString);
    }

}
