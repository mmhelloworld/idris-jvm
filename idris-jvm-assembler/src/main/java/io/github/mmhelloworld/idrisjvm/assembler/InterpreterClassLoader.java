package io.github.mmhelloworld.idrisjvm.assembler;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.HashMap;
import java.util.Map;

import static java.lang.Thread.currentThread;

public class InterpreterClassLoader extends URLClassLoader {
    private final Map<String, byte[]> classes;

    public InterpreterClassLoader(Map<String, byte[]> classes) {
        this(currentThread().getContextClassLoader(), classes);
    }

    public InterpreterClassLoader(ClassLoader parent,
                                  Map<String, byte[]> classes) {
        super(new URL[0], parent);
        this.classes = new HashMap<>(classes);
    }

    @Override
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        Class<?> loadedClass = findLoadedClass(name);
        if (loadedClass == null) {
            try {
                loadedClass = findClass(name);
            } catch (ClassNotFoundException e) {
                loadedClass = super.loadClass(name, resolve);
            }
        }
        if (resolve) {
            resolveClass(loadedClass);
        }
        return loadedClass;
    }

    @Override
    protected Class<?> findClass(String className)
            throws ClassNotFoundException {
        byte[] bytecode = classes.get(className);
        if (bytecode == null) {
            bytecode = classes.get(className.replace('.', '/'));
        }

        if (bytecode != null) {
            return defineClass(className, bytecode, 0, bytecode.length);
        } else {
            throw new ClassNotFoundException(className);
        }
    }

    @Override
    public InputStream getResourceAsStream(String name) {
        InputStream contents = super.getResourceAsStream(name);
        if (contents != null) {
            return contents;
        }
        if (name.endsWith(".class")) {
            String noSuffix = name.substring(0, name.lastIndexOf('.'));
            String relativeName;
            if (name.startsWith("/")) {
                relativeName = noSuffix.substring(1);
            } else {
                relativeName = noSuffix;
            }
            String className = relativeName.replace('/', '.');
            byte[] bytecode = classes.get(className);
            if (bytecode != null) {
                return new ByteArrayInputStream(bytecode);
            }
        }
        return null;
    }
}