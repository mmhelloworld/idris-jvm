package io.github.mmhelloworld.idrisjvm.assembler;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.TypePath;
import org.objectweb.asm.TypeReference;
import org.objectweb.asm.signature.SignatureReader;
import org.objectweb.asm.signature.SignatureVisitor;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Paths;

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
 *   M|&lt;name&gt;|&lt;jvmMethodDescriptor&gt;|&lt;isStatic 0|1&gt;|&lt;throws 0|1&gt;|&lt;genericSig&gt;|&lt;nullableReturn 0|1&gt;
 *   F|&lt;name&gt;|&lt;jvmTypeDescriptor&gt;|&lt;isStatic 0|1&gt;
 *   X|&lt;internalName&gt;|&lt;arity&gt;  -- a transitive supertype (superclass/interface, excl. Object) and its generic arity
 *   S|&lt;internalName&gt;|&lt;samName&gt;|&lt;samDescriptor&gt;|&lt;samGenericSignature&gt;|&lt;formalTypeParams&gt;
 *                                -- a referenced functional interface and its single abstract method
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
     * @param classpath the target classpath to resolve {@code className} (and its supertypes /
     *                  referenced types) against: a {@link File#pathSeparator}-separated list of
     *                  jar files and directories. May be empty/blank, in which case only the
     *                  compiler's own classpath and the JDK platform modules are searched (so JDK
     *                  classes always resolve, but project dependencies do not). A fresh loader is
     *                  built per call, so a long-lived process always sees the current jars on disk.
     * @param className internal ({@code java/util/ArrayList}) or binary
     *                  ({@code java.util.ArrayList}) class name.
     * @return the line-protocol dump described above, or {@code ERR|...} on failure.
     */
    public static String reflect(String classpath, String className) {
        var previous = Thread.currentThread().getContextClassLoader();
        // readClassBytes consults the context classloader first, so installing a loader over the
        // target classpath transparently routes every nested read (supertypes, SAM probing) through
        // it — no parameter threading. Parent is the platform loader, NOT the compiler's own loader,
        // so the target classpath is authoritative for application classes (decoupled from idris2's
        // classpath) while JDK platform classes still resolve.
        var loader = classpathClassLoader(classpath);
        if (loader != null) {
            Thread.currentThread().setContextClassLoader(loader);
        }
        try {
            var bytes = readClassBytes(className);
            var referenced = new java.util.LinkedHashSet<String>();
            var sb = new StringBuilder(dump(bytes, referenced));
            for (var supertype : transitiveSupertypes(className).entrySet()) {
                // X|<name>|<arity>: arity is the supertype's own generic-param count, so the
                // generated marker (e.g. AbstractList) is parameterised rather than bare.
                sb.append("X|").append(supertype.getKey()).append('|').append(supertype.getValue()).append('\n');
            }
            // S|<name>|...: any referenced reference type that is a functional interface, so the
            // generated binding can accept an Idris function (bridged via jlambda).
            for (var ref : referenced) {
                var sam = functionalInterfaceInfo(ref);
                if (sam != null) {
                    sb.append("S|").append(ref).append('|').append(sam.name).append('|')
                        .append(sam.descriptor).append('|').append(sam.genericSignature).append('|')
                        .append(sam.formalTypeParams).append('\n');
                }
            }
            return sb.toString();
        } catch (Throwable t) {
            return "ERR|" + className + ": " + t.getClass().getSimpleName() + ": " + t.getMessage();
        } finally {
            if (loader != null) {
                Thread.currentThread().setContextClassLoader(previous);
                try {
                    loader.close();
                } catch (IOException ignored) {
                    // best-effort: nothing actionable if the loader's jars fail to close
                }
            }
        }
    }

    /**
     * A loader over the given {@link File#pathSeparator}-separated classpath (jars and directories),
     * or {@code null} when the classpath is blank or has no usable entries. Parent is the platform
     * loader so only JDK classes are inherited; everything else comes from the listed entries.
     */
    private static URLClassLoader classpathClassLoader(String classpath) {
        if (classpath == null || classpath.isBlank()) {
            return null;
        }
        var urls = new java.util.ArrayList<URL>();
        for (var entry : classpath.split(java.util.regex.Pattern.quote(File.pathSeparator))) {
            if (entry.isBlank()) {
                continue;
            }
            try {
                urls.add(Paths.get(entry).toUri().toURL());
            } catch (Exception ignored) {
                // skip a malformed entry; a missing jar/dir simply won't resolve its classes
            }
        }
        return urls.isEmpty() ? null : new URLClassLoader(urls.toArray(new URL[0]),
            ClassLoader.getPlatformClassLoader());
    }

    private static String dump(byte[] bytes, java.util.Set<String> referenced) {
        var sb = new StringBuilder();
        var reader = new ClassReader(bytes);
        reader.accept(new ClassVisitor(Opcodes.ASM9) {
            @Override
            public void visit(int version, int access, String name, String signature,
                              String superName, String[] interfaces) {
                var isInterface = (access & Opcodes.ACC_INTERFACE) != 0;
                // Trailing field = comma-separated generic type-parameter names (e.g. "K,V");
                // empty for a non-generic class. Its length is the marker type's arity.
                sb.append("C|").append(name).append('|').append(isInterface ? 1 : 0)
                    .append('|').append(formalTypeParams(signature)).append('\n');
            }

            @Override
            public MethodVisitor visitMethod(int access, String name, String descriptor,
                                             String signature, String[] exceptions) {
                if (!isPublicApi(access) || name.equals("<clinit>")) {
                    return null;
                }
                var isStatic = (access & Opcodes.ACC_STATIC) != 0;
                var throwsChecked = exceptions != null && exceptions.length > 0;
                for (var arg : Type.getArgumentTypes(descriptor)) {
                    addReferencedType(referenced, arg);
                }
                var returnType = Type.getReturnType(descriptor);
                addReferencedType(referenced, returnType);
                // Only a reference-typed return can be null; primitive/void returns are never wrapped.
                var refReturn = returnType.getSort() == Type.OBJECT || returnType.getSort() == Type.ARRAY;
                // Defer emitting the M| line to visitEnd so we can fold in any @Nullable annotation
                // on the return (visited after visitMethod returns). Trailing fields: raw generic
                // signature (for shared type variables / parameterized types) then nullable-return.
                return new MethodVisitor(Opcodes.ASM9) {
                    private boolean nullableReturn = false;

                    @Override
                    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
                        // Method-level nullness annotation (JSR-305, JetBrains): @Nullable Foo m().
                        if (refReturn && isNullableAnnotation(desc)) {
                            nullableReturn = true;
                        }
                        return null;
                    }

                    @Override
                    public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath,
                                                                 String desc, boolean visible) {
                        // TYPE_USE nullness annotation on the return type (jspecify): Foo m() with
                        // @Nullable applied to the return. typePath != null targets a nested type
                        // argument (e.g. List<@Nullable T>), which does not make the return nullable.
                        if (refReturn && typePath == null
                            && new TypeReference(typeRef).getSort() == TypeReference.METHOD_RETURN
                            && isNullableAnnotation(desc)) {
                            nullableReturn = true;
                        }
                        return null;
                    }

                    @Override
                    public void visitEnd() {
                        sb.append("M|").append(name).append('|').append(descriptor).append('|')
                            .append(isStatic ? 1 : 0).append('|').append(throwsChecked ? 1 : 0)
                            .append('|').append(signature == null ? "" : signature)
                            .append('|').append(nullableReturn ? 1 : 0).append('\n');
                    }
                };
            }

            @Override
            public FieldVisitor visitField(int access, String name, String descriptor,
                                           String signature, Object value) {
                if (isPublicApi(access)) {
                    var isStatic = (access & Opcodes.ACC_STATIC) != 0;
                    sb.append("F|").append(name).append('|').append(descriptor).append('|')
                        .append(isStatic ? 1 : 0).append('\n');
                }
                return null;
            }
        }, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
        return sb.toString();
    }

    // All superclasses and interfaces, transitively (mapped to their own generic arity),
    // excluding the class itself and java/lang/Object (handled by a universal
    // `Inherits a Object` instance). Each node's bytes are read once, both to walk its
    // parents and to record its formal-type-parameter count for the parameterised marker.
    private static java.util.Map<String, Integer> transitiveSupertypes(String className) {
        var start = className.replace('.', '/');
        var result = new java.util.LinkedHashSet<String>();
        var arity = new java.util.HashMap<String, Integer>();
        var visited = new java.util.HashSet<String>();
        var queue = new java.util.ArrayDeque<String>();
        queue.add(start);
        visited.add(start);
        while (!queue.isEmpty()) {
            var current = queue.poll();
            try {
                var reader = new ClassReader(readClassBytes(current));
                arity.put(current, classArity(reader));
                var parents = new java.util.ArrayList<String>();
                if (reader.getSuperName() != null) {
                    parents.add(reader.getSuperName());
                }
                java.util.Collections.addAll(parents, reader.getInterfaces());
                for (var parent : parents) {
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
        var out = new java.util.LinkedHashMap<String, Integer>();
        for (var name : result) {
            out.put(name, arity.getOrDefault(name, 0));
        }
        return out;
    }

    // Number of generic formal type parameters a class declares (its marker arity).
    private static int classArity(ClassReader reader) {
        var formals = formalTypeParams(classSignature(reader));
        return formals.isEmpty() ? 0 : formals.split(",").length;
    }

    // The class file's generic signature header (null when the class is non-generic).
    private static String classSignature(ClassReader reader) {
        var sig = new String[]{null};
        reader.accept(new ClassVisitor(Opcodes.ASM9) {
            @Override
            public void visit(int version, int access, String name, String signature,
                              String superName, String[] interfaces) {
                sig[0] = signature;
            }
        }, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
        return sig[0];
    }

    // Record the internal name of an object (or object-array) type for later SAM probing.
    private static void addReferencedType(java.util.Set<String> referenced, Type type) {
        if (type.getSort() == Type.OBJECT) {
            referenced.add(type.getInternalName());
        } else if (type.getSort() == Type.ARRAY && type.getElementType().getSort() == Type.OBJECT) {
            referenced.add(type.getElementType().getInternalName());
        }
    }

    // SAM metadata for one functional interface.
    private static final class SamInfo {
        private final String name;
        private final String descriptor;
        private final String genericSignature;
        private final String formalTypeParams;

        SamInfo(String name, String descriptor, String genericSignature, String formalTypeParams) {
            this.name = name;
            this.descriptor = descriptor;
            this.genericSignature = genericSignature == null ? "" : genericSignature;
            this.formalTypeParams = formalTypeParams;
        }
    }

    // One abstract method discovered while walking an interface hierarchy.
    private static final class AbstractMethod {
        private final String owner;
        private final String name;
        private final String descriptor;
        private final String signature;

        AbstractMethod(String owner, String name, String descriptor, String signature) {
            this.owner = owner;
            this.name = name;
            this.descriptor = descriptor;
            this.signature = signature;
        }
    }

    // If {@code name} is a functional interface (exactly one abstract method, ignoring the
    // public methods of Object), return its SAM; otherwise null. The single abstract method may
    // be inherited from a super-interface (as for UnaryOperator extends Function).
    private static SamInfo functionalInterfaceInfo(String name) {
        byte[] bytes;
        try {
            bytes = readClassBytes(name);
        } catch (Exception ignored) {
            return null;
        }
        var reader = new ClassReader(bytes);
        if ((reader.getAccess() & Opcodes.ACC_INTERFACE) == 0) {
            return null;
        }
        var abstracts = new java.util.LinkedHashMap<String, AbstractMethod>();
        collectAbstractMethods(name, abstracts, new java.util.HashSet<>());
        if (abstracts.size() != 1) {
            return null;
        }
        var sam = abstracts.values().iterator().next();
        // Keep the SAM's generic signature only when it is declared on this very interface, so
        // its type variables line up with this interface's formal type parameters; otherwise the
        // Idris side falls back to the erased (Object-typed) callback signature.
        var genericSignature = name.equals(sam.owner) ? sam.signature : "";
        return new SamInfo(sam.name, sam.descriptor, genericSignature, formalTypeParams(classSignature(reader)));
    }

    private static void collectAbstractMethods(String current, java.util.Map<String, AbstractMethod> abstracts,
                                               java.util.Set<String> visited) {
        if (!visited.add(current)) {
            return;
        }
        byte[] bytes;
        try {
            bytes = readClassBytes(current);
        } catch (Exception ignored) {
            return;
        }
        var reader = new ClassReader(bytes);
        reader.accept(new ClassVisitor(Opcodes.ASM9) {
            @Override
            public MethodVisitor visitMethod(int access, String methodName, String descriptor,
                                             String signature, String[] exceptions) {
                var isStatic = (access & Opcodes.ACC_STATIC) != 0;
                var isAbstract = (access & Opcodes.ACC_ABSTRACT) != 0;
                if (!isStatic && isAbstract && !isObjectMethod(methodName, descriptor)) {
                    // dedup by name+descriptor: a re-declaration in a sub-interface wins (first seen)
                    abstracts.putIfAbsent(methodName + descriptor,
                        new AbstractMethod(current, methodName, descriptor, signature));
                }
                return null;
            }
        }, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);
        for (var parent : reader.getInterfaces()) {
            collectAbstractMethods(parent, abstracts, visited);
        }
    }

    // The public methods of java.lang.Object do not count toward the single-abstract-method
    // tally (an interface may legally re-declare them, e.g. Comparator.equals).
    private static boolean isObjectMethod(String name, String descriptor) {
        return ("equals".equals(name) && "(Ljava/lang/Object;)Z".equals(descriptor))
            || ("hashCode".equals(name) && "()I".equals(descriptor))
            || ("toString".equals(name) && "()Ljava/lang/String;".equals(descriptor));
    }

    // Comma-separated generic formal type-parameter names declared by a class signature.
    private static String formalTypeParams(String signature) {
        if (signature == null) {
            return "";
        }
        var names = new StringBuilder();
        new SignatureReader(signature).accept(new SignatureVisitor(Opcodes.ASM9) {
            @Override
            public void visitFormalTypeParameter(String name) {
                if (!names.isEmpty()) {
                    names.append(',');
                }
                names.append(name);
            }
        });
        return names.toString();
    }

    // Any annotation whose simple name marks nullability, regardless of package, so the common
    // conventions all work: javax.annotation (JSR-305), org.jetbrains.annotations, jspecify, etc.
    private static boolean isNullableAnnotation(String descriptor) {
        var simpleName = descriptor;
        var lastSlash = simpleName.lastIndexOf('/');
        if (lastSlash >= 0) {
            simpleName = simpleName.substring(lastSlash + 1);
        }
        if (simpleName.endsWith(";")) {
            simpleName = simpleName.substring(0, simpleName.length() - 1);
        }
        return simpleName.equals("Nullable") || simpleName.equals("CheckForNull");
    }

    private static boolean isPublicApi(int access) {
        return (access & Opcodes.ACC_PUBLIC) != 0
            && (access & Opcodes.ACC_SYNTHETIC) == 0
            && (access & Opcodes.ACC_BRIDGE) == 0;
    }

    private static byte[] readClassBytes(String className) throws IOException, ClassNotFoundException {
        var internalName = className.replace('.', '/');
        var resourcePath = internalName + ".class";

        // 1. Application / user classpath via the classloaders.
        var contextLoader = Thread.currentThread().getContextClassLoader();
        if (contextLoader != null) {
            var bytes = readResource(contextLoader, resourcePath);
            if (bytes != null) {
                return bytes;
            }
        }
        var systemBytes = readResource(ClassLoader.getSystemClassLoader(), resourcePath);
        if (systemBytes != null) {
            return systemBytes;
        }

        // 2. JDK platform classes live in modules (not on the classpath on JDK 9+);
        //    read them out of the jrt: filesystem.
        var binaryName = internalName.replace('/', '.');
        var module = Class.forName(binaryName).getModule();
        var moduleName = module.getName();
        if (moduleName != null) {
            var jrt = FileSystems.getFileSystem(URI.create("jrt:/"));
            var path = jrt.getPath("/modules", moduleName, resourcePath);
            if (Files.exists(path)) {
                return Files.readAllBytes(path);
            }
        }

        throw new IOException("could not locate bytecode for " + className);
    }

    private static byte[] readResource(ClassLoader loader, String resourcePath) throws IOException {
        try (var in = loader.getResourceAsStream(resourcePath)) {
            return in == null ? null : in.readAllBytes();
        }
    }
}
