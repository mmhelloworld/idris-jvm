package io.github.mmhelloworld.idrisjvm.runtime.ffi;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.lang.Thread.currentThread;
import static java.lang.reflect.Modifier.isFinal;
import static java.lang.reflect.Modifier.isPublic;
import static java.lang.reflect.Modifier.isStatic;
import static java.util.Collections.emptyList;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class TypeProvider {

    private static final String INSTANCE = "i";
    private static final String STATIC = "s";
    private static final String SETTER = "s";
    private static final String GETTER = "g";
    private static final String CONSTRUCTOR = "c";
    private static final String CLASS = "c";
    private static final String INTERFACE = "i";

    public static void main(String[] args) throws IOException {
        Path outputFilePath = Paths.get(".idrisjvmtypes");
        Iterable<String> lines = Files.lines(Paths.get(".idrisjvmtypesimport"))
                .flatMap(TypeProvider::importItems)::iterator;
        Files.write(outputFilePath, lines);
    }

    private static Stream<String> importItems(String importItemsStr) {
        String[] imports = importItemsStr.split(" ");
        String className = imports[0].replace('/', '.');

        try {
            Class<?> clazz = Class.forName(className, false, currentThread().getContextClassLoader());
            List<String> importItems = getImportItems(imports);
            Stream<String> constructors = importItems.contains("<init>") ? importConstructors(clazz) : Stream.empty();
            Stream<String> methods = importMethods(importItems, clazz);
            Stream<String> fields = importFields(importItems, clazz);
            return Stream.of(methods, constructors, fields).flatMap(identity());
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private static List<String> getImportItems(String[] imports) {
        return imports.length > 1 ?
                Arrays.stream(imports, 1, imports.length).collect(toList()) : emptyList();
    }

    private static Stream<String> importFields(List<String> fieldNames, Class<?> clazz) {
        Predicate<Field> fieldNamePredicate = fieldNames.isEmpty() ? m -> true :
                f -> fieldNames.contains(f.getName());
        return Arrays.stream(clazz.getFields())
                .filter(f -> isPublic(f.getModifiers()) && !f.isSynthetic() && fieldNamePredicate.test(f))
                .flatMap(TypeProvider::importField);
    }

    private static Stream<String> importConstructors(Class<?> clazz) {
        return Arrays.stream(clazz.getConstructors())
                .filter(constructor -> !constructor.isSynthetic() && isPublic(constructor.getModifiers()))
                .map(TypeProvider::importConstructor);
    }

    private static Stream<String> importMethods(List<String> methodNames, Class<?> clazz) {
        Predicate<Method> methodNamePredicate = methodNames.isEmpty() ? m -> true :
                m -> methodNames.contains(m.getName());
        return Arrays.stream(clazz.getMethods())
                .filter(m -> isPublic(m.getModifiers()) && !m.isBridge() && !m.isSynthetic() &&
                        methodNamePredicate.test(m))
                .map(TypeProvider::importMethod);
    }

    private static Stream<String> importField(Field f) {
        String getterArgs = isStatic(f.getModifiers()) ? "" : renderType(f.getDeclaringClass());
        String getter = format("%s,%s,%s,%s",
                renderFieldType(f, false),
                renderType(f.getType()),
                f.getName(),
                getterArgs);
        if (!isFinal(f.getModifiers())) {
            String setterArgs = isStatic(f.getModifiers())
                    ? renderType(f.getType())
                    : format("%s,%s", renderType(f.getDeclaringClass()), renderType(f.getType()));
            String setter = format("%s,%s,%s,%s",
                    renderFieldType(f, true),

                    // Setters always return void
                    renderType(void.class),

                    f.getName(),
                    setterArgs);
            return Stream.of(getter, setter);
        } else {
            return Stream.of(getter);
        }
    }

    private static String renderFieldType(Field field, boolean isSetter) {
        boolean isStatic = isStatic(field.getModifiers());
        String itemCategoryPrefix = format("%sf%s", isStatic ? STATIC : INSTANCE, isSetter ? SETTER : GETTER);
        return isStatic ? format("%s %s", itemCategoryPrefix, renderType(field.getDeclaringClass()))
                : itemCategoryPrefix;
    }

    private static String importMethod(Method m) {
        String args = renderArgs(m);
        return format("%s,%s,%s,%s",
                renderMethodType(m),
                renderType(m.getReturnType()),
                m.getName(),
                args);
    }

    private static String importConstructor(Constructor c) {
        String args = renderConstructorArgs(c);
        return format("%s,%s,%s",
                CONSTRUCTOR,
                renderType(c.getDeclaringClass()),
                args);
    }

    private static String renderConstructorArgs(Constructor c) {
        return Arrays.stream(c.getParameterTypes())
                .map(TypeProvider::renderType)
                .collect(joining(","));
    }

    private static String renderArgs(Method m) {
        Stream<Class<?>> parameterTypes = Arrays.stream(m.getParameterTypes());
        Stream<Class<?>> args = isStatic(m.getModifiers())
                ? parameterTypes
                : Stream.concat(Stream.of(m.getDeclaringClass()), parameterTypes);

        return args
                .map(TypeProvider::renderType)
                .collect(joining(","));
    }

    private static String renderType(Class<?> type) {
        if (type.isPrimitive()) {
            return type.getName();
        } else if (type.isArray()) {
            return format("Array %d %s", getArrayDimensions(type), renderType(getDeepComponentType(type)));
        } else {
            return renderNonPrimitiveType(type);
        }
    }

    private static Class<?> getDeepComponentType(Class<?> arrayType) {
        Class<?> componentType = arrayType.getComponentType();
        return componentType.isArray() ? getDeepComponentType(componentType) : componentType;
    }

    private static int getArrayDimensions(Class<?> arrayType) {
        Class<?> componentType = arrayType.getComponentType();
        return componentType.isArray() ? getArrayDimensions(componentType) + 1 : 1;
    }

    private static String renderNonPrimitiveType(Class<?> type) {
        String interfaceOrClass = type.isInterface() ? INTERFACE : CLASS;
        return format("%s %s", interfaceOrClass, type.getName().replace('.', '/'));
    }

    private static String renderMethodType(Method m) {
        return isStatic(m.getModifiers()) ? renderStatic(m) : INSTANCE;
    }

    private static String renderStatic(Method m) {
        return format("%s %s", STATIC, renderType(m.getDeclaringClass()));
    }


}