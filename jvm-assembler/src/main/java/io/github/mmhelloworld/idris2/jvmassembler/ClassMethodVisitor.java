package io.github.mmhelloworld.idris2.jvmassembler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;

import java.util.HashMap;
import java.util.Map;

public final class ClassMethodVisitor {
    private final String className;
    private final String methodName;
    private final ClassWriter classVisitor;
    private final MethodVisitor methodVisitor;
    private final Map<String, Object> env;

    public ClassMethodVisitor(String className, String methodName, ClassWriter classVisitor,
                              MethodVisitor methodVisitor,
                              Map<String, Object> env) {
        this.className = className;
        this.methodName = methodName;
        this.classVisitor = classVisitor;
        this.methodVisitor = methodVisitor;
        this.env = new HashMap<>(env);
    }

    public ClassWriter getClassVisitor() {
        return classVisitor;
    }

    public MethodVisitor getMethodVisitor() {
        return methodVisitor;
    }

    public Map<String, Object> getEnv() {
        return env;
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }
}
