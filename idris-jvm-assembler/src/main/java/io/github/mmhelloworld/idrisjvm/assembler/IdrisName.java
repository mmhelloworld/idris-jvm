package io.github.mmhelloworld.idrisjvm.assembler;

import io.github.mmhelloworld.idrisjvm.runtime.IdrisList;

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
    private static final Map<Character, String> REPLACEMENTS = new HashMap<>();

    static {
        REPLACEMENTS.put(' ', "$s");
        REPLACEMENTS.put('!', "$not");
        REPLACEMENTS.put('"', "$d");
        REPLACEMENTS.put('#', "$hash");
        REPLACEMENTS.put('%', "$mod");
        REPLACEMENTS.put('&', "$and");
        REPLACEMENTS.put('\'', "$q");
        REPLACEMENTS.put('(', "$lpar");
        REPLACEMENTS.put(')', "$rpar");
        REPLACEMENTS.put('*', "$mul");
        REPLACEMENTS.put('+', "$add");
        REPLACEMENTS.put(',', "$com");
        REPLACEMENTS.put('-', "$hyp");
        REPLACEMENTS.put('.', "$dot");
        REPLACEMENTS.put('/', "$div");
        REPLACEMENTS.put('\\', "$bsl");
        REPLACEMENTS.put(':', "$col");
        REPLACEMENTS.put(';', "$scol");
        REPLACEMENTS.put('<', "$lt");
        REPLACEMENTS.put('=', "$eq");
        REPLACEMENTS.put('>', "$gt");
        REPLACEMENTS.put('?', "$ques");
        REPLACEMENTS.put('@', "$at");
        REPLACEMENTS.put('^', "$caret");
        REPLACEMENTS.put('`', "$grave");
        REPLACEMENTS.put('{', "$lbr");
        REPLACEMENTS.put('|', "$or");
        REPLACEMENTS.put('}', "$rbr");
        REPLACEMENTS.put('~', "$tilde");
        REPLACEMENTS.put('[', "$lsqr");
        REPLACEMENTS.put(']', "$rsqr");
    }

    private IdrisName() {
    }

    public static IdrisList getIdrisFunctionName(String programName, String idrisNamespace, String idrisFunctionName) {
        return getIdrisName(programName, idrisNamespace, idrisFunctionName);
    }

    public static String getIdrisConstructorClassName(String idrisName) {
        return String.join("/", addModulePrefix("main", asList(idrisName.split("/"))));
    }

    public static String transformCharacters(String value) {
        return value
            .chars()
            .flatMap(c -> REPLACEMENTS.getOrDefault((char) c, String.valueOf((char) c)).chars())
            .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
            .toString();
    }

    public static String transformCharacter(char c) {
        return REPLACEMENTS.getOrDefault(c, String.valueOf(c));
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
        String className = getClassName(programName, idrisNamespace);
        return new SimpleImmutableEntry<>(className, memberName);
    }

    private static String getClassName(String programName, String idrisNamespace) {
        if (idrisNamespace.startsWith("io/github/mmhelloworld/idrisjvm")) {
            return idrisNamespace;
        } else if (idrisNamespace.startsWith("nomangle:")) {
            return idrisNamespace.substring("nomangle:".length());
        } else {
            LinkedList<String> moduleParts =
                Stream.of(idrisNamespace.split("/")).collect(toCollection(LinkedList::new));
            return String.join("/", addModulePrefix(programName, moduleParts));
        }
    }

    private static List<String> addModulePrefix(String programName, List<String> nameParts) {
        int size = nameParts.size();
        if (size > 1) {
            List<String> packageNameParts = nameParts.subList(1, size - 1);
            LinkedList<String> prefixedPackagedNames = packageNameParts.stream()
                .map(packageName -> "M_" + packageName)
                .collect(toCollection(LinkedList::new));
            String rootPackage = nameParts.get(0);
            prefixedPackagedNames.add(0, rootPackage.equals(programName) ? rootPackage : "M_" + rootPackage);
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
