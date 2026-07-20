package io.github.mmhelloworld.idrisjvm.assembler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public final class ClassMethodVisitor {
    private final String className;
    private final String methodName;
    private final ClassWriter classVisitor;
    private final MethodVisitor methodVisitor;
    private final Map<String, Object> env;
    private final Set<Integer> lineNumberOffsets;

    public ClassMethodVisitor(String className, String methodName, ClassWriter classVisitor,
                              MethodVisitor methodVisitor,
                              Map<String, Object> env,
                              Set<Integer> lineNumberOffsets) {
        this.className = className;
        this.methodName = methodName;
        this.classVisitor = classVisitor;
        this.methodVisitor = methodVisitor;
        this.env = new HashMap<>(env);
        this.lineNumberOffsets = new HashSet<>(lineNumberOffsets);
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

    public Set<Integer> getLineNumberOffsets() {
        return lineNumberOffsets;
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }
}
