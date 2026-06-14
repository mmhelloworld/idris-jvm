package io.github.mmhelloworld.idrisjvm.assembler;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureVisitor;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Reflects JVM class metadata off the compiler's classpath using ASM, for the FFI
 * binding generator (see {@code Compiler.Jvm.Reflection} / {@code Compiler.Jvm.FfiGen}).
 *
 * <p>Returns a flat, line-oriented "dump" rather than structured objects, so the
 * descriptor-to-{@code jvm:}-token translation can live in Idris (where it is
 * unit-testable against the hand-written bindings in {@code Asm.idr}). The dump is
 * {@code '\n'}-separated lines, fields {@code '|'}-separated:
 *
 * <pre>
 *   C|&lt;internalName&gt;|&lt;isInterface 0|1&gt;
 *   M|&lt;name&gt;|&lt;jvmMethodDescriptor&gt;|&lt;isStatic 0|1&gt;|&lt;throws 0|1&gt;
 *   F|&lt;name&gt;|&lt;jvmTypeDescriptor&gt;|&lt;isStatic 0|1&gt;
 *   X|&lt;internalName&gt;     -- a transitive supertype (superclass or interface), excl. Object
 * </pre>
 *
 * Constructors appear as {@code M} lines named {@code <init>}. On any failure a
 * single {@code ERR|<message>} line is returned so the caller can surface a clean
 * error instead of an exception crossing the FFI boundary.
 */
public final class ClasspathReflector {

    private ClasspathReflector() {
    }

    /**
     * @param className internal ({@code java/util/ArrayList}) or binary
     *                  ({@code java.util.ArrayList}) class name.
     * @return the line-protocol dump described above, or {@code ERR|...} on failure.
     */
    public static String reflect(String className) {
        try {
            byte[] bytes = readClassBytes(className);
            StringBuilder sb = new StringBuilder(dump(bytes));
            for (String supertype : transitiveSupertypes(className)) {
                sb.append("X|").append(supertype).append('\n');
            }
            return sb.toString();
        } catch (Throwable t) {
            return "ERR|" + className + ": " + t.getClass().getSimpleName() + ": " + t.getMessage();
        }
    }

    private static String dump(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        ClassReader reader = new ClassReader(bytes);
        reader.accept(new ClassVisitor(Opcodes.ASM9) {
            @Override
            public void visit(int version, int access, String name, String signature,
                              String superName, String[] interfaces) {
                boolean isInterface = (access & Opcodes.ACC_INTERFACE) != 0;
                // Trailing field = comma-separated generic type-parameter names (e.g. "K,V");
                // empty for a non-generic class. Its length is the marker type's arity.
                sb.append("C|").append(name).append('|').append(isInterface ? 1 : 0)
                    .append('|').append(formalTypeParams(signature)).append('\n');
            }

            @Override
            public MethodVisitor visitMethod(int access, String name, String descriptor,
                                             String signature, String[] exceptions) {
                if (isPublicApi(access) && !name.equals("<clinit>")) {
                    boolean isStatic = (access & Opcodes.ACC_STATIC) != 0;
                    boolean throwsChecked = exceptions != null && exceptions.length > 0;
                    // Trailing field = raw generic signature (empty when the method is non-generic),
                    // from which Idris derives shared type variables / parameterized types.
                    sb.append("M|").append(name).append('|').append(descriptor).append('|')
                        .append(isStatic ? 1 : 0).append('|').append(throwsChecked ? 1 : 0)
                        .append('|').append(signature == null ? "" : signature).append('\n');
                }
                return null;
            }

            @Override
            public FieldVisitor visitField(int access, String name, String descriptor,
                                           String signature, Object value) {
                if (isPublicApi(access)) {
                    boolean isStatic = (access & Opcodes.ACC_STATIC) != 0;
                    sb.append("F|").append(name).append('|').append(descriptor).append('|')
                        .append(isStatic ? 1 : 0).append('\n');
                }
                return null;
            }
        }, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
        return sb.toString();
    }

    // All superclasses and interfaces, transitively, excluding the class itself and
    // java/lang/Object (which is handled by a universal `Inherits a Object` instance).
    // ClassReader exposes the header directly, so ancestors need no full visit.
    private static java.util.Set<String> transitiveSupertypes(String className) {
        String start = className.replace('.', '/');
        java.util.LinkedHashSet<String> result = new java.util.LinkedHashSet<>();
        java.util.Set<String> visited = new java.util.HashSet<>();
        java.util.Deque<String> queue = new java.util.ArrayDeque<>();
        queue.add(start);
        visited.add(start);
        while (!queue.isEmpty()) {
            String current = queue.poll();
            try {
                ClassReader reader = new ClassReader(readClassBytes(current));
                java.util.List<String> parents = new java.util.ArrayList<>();
                if (reader.getSuperName() != null) {
                    parents.add(reader.getSuperName());
                }
                java.util.Collections.addAll(parents, reader.getInterfaces());
                for (String parent : parents) {
                    if (visited.add(parent)) {
                        queue.add(parent);
                        if (!parent.equals("java/lang/Object")) {
                            result.add(parent);
                        }
                    }
                }
            } catch (Exception ignored) {
                // An unreadable ancestor just truncates that branch of the hierarchy.
            }
        }
        return result;
    }

    // Comma-separated generic formal type-parameter names declared by a class signature.
    private static String formalTypeParams(String signature) {
        if (signature == null) {
            return "";
        }
        StringBuilder names = new StringBuilder();
        new SignatureReader(signature).accept(new SignatureVisitor(Opcodes.ASM9) {
            @Override
            public void visitFormalTypeParameter(String name) {
                if (names.length() > 0) {
                    names.append(',');
                }
                names.append(name);
            }
        });
        return names.toString();
    }

    private static boolean isPublicApi(int access) {
        return (access & Opcodes.ACC_PUBLIC) != 0
            && (access & Opcodes.ACC_SYNTHETIC) == 0
            && (access & Opcodes.ACC_BRIDGE) == 0;
    }

    private static byte[] readClassBytes(String className) throws IOException, ClassNotFoundException {
        String internalName = className.replace('.', '/');
        String resourcePath = internalName + ".class";

        // 1. Application / user classpath via the classloaders.
        ClassLoader contextLoader = Thread.currentThread().getContextClassLoader();
        if (contextLoader != null) {
            byte[] bytes = readResource(contextLoader, resourcePath);
            if (bytes != null) {
                return bytes;
            }
        }
        byte[] systemBytes = readResource(ClassLoader.getSystemClassLoader(), resourcePath);
        if (systemBytes != null) {
            return systemBytes;
        }

        // 2. JDK platform classes live in modules (not on the classpath on JDK 9+);
        //    read them out of the jrt: filesystem.
        String binaryName = internalName.replace('/', '.');
        Module module = Class.forName(binaryName).getModule();
        String moduleName = module.getName();
        if (moduleName != null) {
            FileSystem jrt = FileSystems.getFileSystem(URI.create("jrt:/"));
            Path path = jrt.getPath("/modules", moduleName, resourcePath);
            if (Files.exists(path)) {
                return Files.readAllBytes(path);
            }
        }

        throw new IOException("could not locate bytecode for " + className);
    }

    private static byte[] readResource(ClassLoader loader, String resourcePath) throws IOException {
        try (InputStream in = loader.getResourceAsStream(resourcePath)) {
            return in == null ? null : in.readAllBytes();
        }
    }
}
