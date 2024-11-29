package io.github.mmhelloworld.idrisjvm.assembler;

import io.github.mmhelloworld.idrisjvm.runtime.IdrisList;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import static java.lang.Character.toUpperCase;
import static java.util.Arrays.asList;

public final class IdrisName {

    private static final Map<Character, String> REPLACEMENTS = new HashMap<>();

    private IdrisName() {
    }

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

    public static IdrisList getIdrisFunctionName(String programName, String idrisNamespace, String idrisFunctionName) {
        return getIdrisName(getRootModuleName(programName), idrisNamespace, idrisFunctionName);
    }

    public static String getIdrisConstructorClassName(String programName, String idrisName) {
        return addModulePrefix(getRootModuleName(programName), idrisName);
    }

    public static String transformCharacters(String value) {
        StringBuilder builder = new StringBuilder();
        for (char c: value.toCharArray()) {
            builder.append(transformCharacter(c));
        }
        return builder.toString();
    }

    private static String transformCharacter(char c) {
        return REPLACEMENTS.getOrDefault(c, String.valueOf(c));
    }

    private static IdrisList getIdrisName(String programName, String idrisNamespace, String memberName) {
        Entry<String, String> classAndMemberName = getClassAndMemberName(programName, idrisNamespace, memberName);
        return IdrisList.fromIterable(asList(classAndMemberName.getKey(), classAndMemberName.getValue()));
    }

    private static String getRootModuleName(String programName) {
        return toUpperCase(programName.charAt(0)) + programName.substring(1);
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
            return addModulePrefix(programName, idrisNamespace);
        }
    }

    private static String addModulePrefix(String programName, String name) {
        if (name.indexOf('/') < 0) {
            return programName + "/" + name;
        } else {
            return name;
        }
    }
}
