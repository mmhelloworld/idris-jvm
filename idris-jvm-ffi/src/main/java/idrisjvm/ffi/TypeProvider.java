package idrisjvm.ffi;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.lang.Thread.currentThread;
import static java.lang.reflect.Modifier.isPublic;
import static java.lang.reflect.Modifier.isStatic;
import static java.util.Collections.emptyList;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class TypeProvider {

    public static void main(String[] args) {
        Arrays.stream(args)
                .map(className -> className.replace('/', '.'))
                .flatMap(TypeProvider::importMethods)
                .forEach(System.out::println);
    }

    private static Stream<String> importMethods(String classAndMethodNameStr) {
        String[] classAndMethodNames = classAndMethodNameStr.split(" ");
        String className = classAndMethodNames[0];
        List<String> methodNames = classAndMethodNames.length > 1 ?
                Arrays.stream(classAndMethodNames, 1, classAndMethodNames.length).collect(toList()) : emptyList();
        Predicate<Method> methodNamePredicate = methodNames.isEmpty() ? m -> true :
                m -> methodNames.contains(m.getName());
        try {
            Class<?> clazz = Class.forName(className, false, currentThread().getContextClassLoader());
            return Arrays.stream(clazz.getMethods())
                    .filter(m -> isPublic(m.getModifiers()) && !m.isBridge() && !m.isSynthetic() &&
                        methodNamePredicate.test(m))
                    .map(TypeProvider::importMethod);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private static String importMethod(Method m) {
        String args = renderArgs(m);
        return format("%s,%s,%s,%s",
                renderMethodType(m),
                renderType(m.getReturnType()),
                m.getName(),
                args);
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
        String interfaceOrClass = type.isInterface() ? "interface" : "class";
        return format("%s %s", interfaceOrClass, type.getName().replace('.', '/'));
    }

    private static String renderMethodType(Method m) {
        return isStatic(m.getModifiers()) ? renderStatic(m) : "instance";
    }

    private static String renderStatic(Method m) {
        return format("static %s", renderType(m.getDeclaringClass()));
    }


}