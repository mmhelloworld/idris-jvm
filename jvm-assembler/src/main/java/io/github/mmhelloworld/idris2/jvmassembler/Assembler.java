package io.github.mmhelloworld.idris2.jvmassembler;

import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnAnnotation;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnArray;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnBoolean;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnByte;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnChar;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnClass;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnDouble;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnEnum;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnFloat;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnInt;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnLong;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnShort;
import io.github.mmhelloworld.idris2.jvmassembler.AnnotationValue.AnnString;
import io.github.mmhelloworld.idris2.jvmassembler.JBsmArg.JBsmArgGetType;
import io.github.mmhelloworld.idris2.runtime.IdrisSystem;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.AbstractMap.SimpleEntry;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.IntStream;

import static java.lang.Boolean.parseBoolean;
import static java.lang.String.format;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Collections.emptyMap;
import static java.util.Objects.requireNonNull;
import static java.util.jar.Attributes.Name.MAIN_CLASS;
import static java.util.jar.Attributes.Name.MANIFEST_VERSION;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;
import static org.objectweb.asm.ClassWriter.COMPUTE_FRAMES;
import static org.objectweb.asm.ClassWriter.COMPUTE_MAXS;
import static org.objectweb.asm.Opcodes.AALOAD;
import static org.objectweb.asm.Opcodes.AASTORE;
import static org.objectweb.asm.Opcodes.ACC_FINAL;
import static org.objectweb.asm.Opcodes.ACC_PRIVATE;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ACC_SUPER;
import static org.objectweb.asm.Opcodes.ACONST_NULL;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.ANEWARRAY;
import static org.objectweb.asm.Opcodes.ARETURN;
import static org.objectweb.asm.Opcodes.ARRAYLENGTH;
import static org.objectweb.asm.Opcodes.ASTORE;
import static org.objectweb.asm.Opcodes.ATHROW;
import static org.objectweb.asm.Opcodes.BALOAD;
import static org.objectweb.asm.Opcodes.BASTORE;
import static org.objectweb.asm.Opcodes.BIPUSH;
import static org.objectweb.asm.Opcodes.CALOAD;
import static org.objectweb.asm.Opcodes.CASTORE;
import static org.objectweb.asm.Opcodes.CHECKCAST;
import static org.objectweb.asm.Opcodes.D2F;
import static org.objectweb.asm.Opcodes.D2I;
import static org.objectweb.asm.Opcodes.DADD;
import static org.objectweb.asm.Opcodes.DALOAD;
import static org.objectweb.asm.Opcodes.DASTORE;
import static org.objectweb.asm.Opcodes.DCMPG;
import static org.objectweb.asm.Opcodes.DCMPL;
import static org.objectweb.asm.Opcodes.DCONST_0;
import static org.objectweb.asm.Opcodes.DCONST_1;
import static org.objectweb.asm.Opcodes.DDIV;
import static org.objectweb.asm.Opcodes.DLOAD;
import static org.objectweb.asm.Opcodes.DMUL;
import static org.objectweb.asm.Opcodes.DNEG;
import static org.objectweb.asm.Opcodes.DREM;
import static org.objectweb.asm.Opcodes.DRETURN;
import static org.objectweb.asm.Opcodes.DSTORE;
import static org.objectweb.asm.Opcodes.DSUB;
import static org.objectweb.asm.Opcodes.DUP;
import static org.objectweb.asm.Opcodes.F2D;
import static org.objectweb.asm.Opcodes.FALOAD;
import static org.objectweb.asm.Opcodes.FASTORE;
import static org.objectweb.asm.Opcodes.FCONST_0;
import static org.objectweb.asm.Opcodes.FCONST_1;
import static org.objectweb.asm.Opcodes.FLOAD;
import static org.objectweb.asm.Opcodes.FRETURN;
import static org.objectweb.asm.Opcodes.FSTORE;
import static org.objectweb.asm.Opcodes.GETFIELD;
import static org.objectweb.asm.Opcodes.I2B;
import static org.objectweb.asm.Opcodes.I2C;
import static org.objectweb.asm.Opcodes.I2D;
import static org.objectweb.asm.Opcodes.I2L;
import static org.objectweb.asm.Opcodes.I2S;
import static org.objectweb.asm.Opcodes.IADD;
import static org.objectweb.asm.Opcodes.IALOAD;
import static org.objectweb.asm.Opcodes.IAND;
import static org.objectweb.asm.Opcodes.IASTORE;
import static org.objectweb.asm.Opcodes.ICONST_0;
import static org.objectweb.asm.Opcodes.ICONST_1;
import static org.objectweb.asm.Opcodes.ICONST_2;
import static org.objectweb.asm.Opcodes.ICONST_3;
import static org.objectweb.asm.Opcodes.ICONST_4;
import static org.objectweb.asm.Opcodes.ICONST_5;
import static org.objectweb.asm.Opcodes.ICONST_M1;
import static org.objectweb.asm.Opcodes.IDIV;
import static org.objectweb.asm.Opcodes.IFEQ;
import static org.objectweb.asm.Opcodes.IFGE;
import static org.objectweb.asm.Opcodes.IFGT;
import static org.objectweb.asm.Opcodes.IFLE;
import static org.objectweb.asm.Opcodes.IFLT;
import static org.objectweb.asm.Opcodes.IFNE;
import static org.objectweb.asm.Opcodes.IFNONNULL;
import static org.objectweb.asm.Opcodes.IFNULL;
import static org.objectweb.asm.Opcodes.IF_ICMPEQ;
import static org.objectweb.asm.Opcodes.IF_ICMPGE;
import static org.objectweb.asm.Opcodes.IF_ICMPGT;
import static org.objectweb.asm.Opcodes.IF_ICMPLE;
import static org.objectweb.asm.Opcodes.IF_ICMPLT;
import static org.objectweb.asm.Opcodes.IF_ICMPNE;
import static org.objectweb.asm.Opcodes.ILOAD;
import static org.objectweb.asm.Opcodes.IMUL;
import static org.objectweb.asm.Opcodes.INEG;
import static org.objectweb.asm.Opcodes.INSTANCEOF;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
import static org.objectweb.asm.Opcodes.INVOKEVIRTUAL;
import static org.objectweb.asm.Opcodes.IOR;
import static org.objectweb.asm.Opcodes.IREM;
import static org.objectweb.asm.Opcodes.IRETURN;
import static org.objectweb.asm.Opcodes.ISHL;
import static org.objectweb.asm.Opcodes.ISHR;
import static org.objectweb.asm.Opcodes.ISTORE;
import static org.objectweb.asm.Opcodes.ISUB;
import static org.objectweb.asm.Opcodes.IUSHR;
import static org.objectweb.asm.Opcodes.IXOR;
import static org.objectweb.asm.Opcodes.L2I;
import static org.objectweb.asm.Opcodes.LADD;
import static org.objectweb.asm.Opcodes.LALOAD;
import static org.objectweb.asm.Opcodes.LAND;
import static org.objectweb.asm.Opcodes.LASTORE;
import static org.objectweb.asm.Opcodes.LCONST_0;
import static org.objectweb.asm.Opcodes.LCONST_1;
import static org.objectweb.asm.Opcodes.LDIV;
import static org.objectweb.asm.Opcodes.LLOAD;
import static org.objectweb.asm.Opcodes.LMUL;
import static org.objectweb.asm.Opcodes.LNEG;
import static org.objectweb.asm.Opcodes.LOR;
import static org.objectweb.asm.Opcodes.LREM;
import static org.objectweb.asm.Opcodes.LRETURN;
import static org.objectweb.asm.Opcodes.LSHL;
import static org.objectweb.asm.Opcodes.LSHR;
import static org.objectweb.asm.Opcodes.LSTORE;
import static org.objectweb.asm.Opcodes.LSUB;
import static org.objectweb.asm.Opcodes.LUSHR;
import static org.objectweb.asm.Opcodes.LXOR;
import static org.objectweb.asm.Opcodes.NEW;
import static org.objectweb.asm.Opcodes.NEWARRAY;
import static org.objectweb.asm.Opcodes.POP;
import static org.objectweb.asm.Opcodes.PUTFIELD;
import static org.objectweb.asm.Opcodes.RETURN;
import static org.objectweb.asm.Opcodes.SALOAD;
import static org.objectweb.asm.Opcodes.SASTORE;
import static org.objectweb.asm.Opcodes.SIPUSH;
import static org.objectweb.asm.Opcodes.T_BOOLEAN;
import static org.objectweb.asm.Opcodes.T_BYTE;
import static org.objectweb.asm.Opcodes.T_CHAR;
import static org.objectweb.asm.Opcodes.T_DOUBLE;
import static org.objectweb.asm.Opcodes.T_FLOAT;
import static org.objectweb.asm.Opcodes.T_INT;
import static org.objectweb.asm.Opcodes.T_LONG;
import static org.objectweb.asm.Opcodes.T_SHORT;

public final class Assembler {
    private static final boolean shouldDebug = parseBoolean(System.getProperty("IDRIS_JVM_DEBUG", "false"));
    private final Map<String, ClassWriter> cws;
    private final Deque<ClassMethodVisitor> classMethodVisitorStack = new LinkedList<>();
    private Map<String, Object> env;
    private ClassWriter cw;
    private MethodVisitor mv;
    private FieldVisitor fv;
    private String className;
    private String methodName;
    private int localVarCount;
    private boolean shouldDescribeFrame;

    public Assembler() {
        this.cws = new HashMap<>();
        this.env = new HashMap<>();
    }

    public void endMethod() {
        mv.visitEnd();
    }

    public void aaload() {
        mv.visitInsn(AALOAD);
    }

    public void aastore() {
        mv.visitInsn(AASTORE);
    }

    public void aconstnull() {
        mv.visitInsn(ACONST_NULL);
    }

    public void aload(int n) {
        mv.visitVarInsn(ALOAD, n);
    }

    public void anewarray(String desc) {
        mv.visitTypeInsn(ANEWARRAY, desc);
    }

    public void anewintarray() {
        mv.visitIntInsn(NEWARRAY, T_INT);
    }

    public void anewbytearray() {
        mv.visitIntInsn(NEWARRAY, T_BYTE);
    }

    public void anewbooleanarray() {
        mv.visitIntInsn(NEWARRAY, T_BOOLEAN);
    }

    public void anewchararray() {
        mv.visitIntInsn(NEWARRAY, T_CHAR);
    }

    public void anewshortarray() {
        mv.visitIntInsn(NEWARRAY, T_SHORT);
    }

    public void anewlongarray() {
        mv.visitIntInsn(NEWARRAY, T_LONG);
    }

    public void anewfloatarray() {
        mv.visitIntInsn(NEWARRAY, T_FLOAT);
    }

    public void anewdoublearray() {
        mv.visitIntInsn(NEWARRAY, T_DOUBLE);
    }

    public void arraylength() {
        mv.visitInsn(ARRAYLENGTH);
    }

    public void areturn() {
        mv.visitInsn(ARETURN);
    }

    public void astore(int index) {
        mv.visitVarInsn(ASTORE, index);
    }

    public void athrow() {
        mv.visitInsn(ATHROW);
    }

    public void baload() {
        mv.visitInsn(BALOAD);
    }

    public void bastore() {
        mv.visitInsn(BASTORE);
    }

    public void caload() {
        mv.visitInsn(CALOAD);
    }

    public void castore() {
        mv.visitInsn(CASTORE);
    }

    public void checkcast(String desc) {
        mv.visitTypeInsn(CHECKCAST, desc);
    }

    public void classCodeStart(int version,
                               int access,
                               String className,
                               String signature,
                               String parentClassName,
                               Object interfaces,
                               Object annotations) {
        classCodeStart(version, access, className, signature, parentClassName, (List) interfaces,
            (List) annotations);
    }

    public void classCodeStart(int version,
                               int access,
                               String className,
                               String signature,
                               String parentClassName,
                               List<String> interfaces,
                               List<Annotation> annotations) {
        this.className = className;
        handleClassCodeStart(cws, cw, version, access, className, signature, parentClassName, interfaces, annotations);
    }

    public void createClass(int flags) {
        cw = new IdrisClassWriter(flags);
    }

    public Map<String, ClassWriter> getClassWriters() {
        return cws;
    }

    public static void createJar(String directory, String fileName, String mainClass) throws IOException {
        String jarFileName = fileName + ".jar";
        File jarFile = new File(directory, jarFileName);
        jarFile.delete();
        JarOutputStream target = new JarOutputStream(new FileOutputStream(jarFile), createManifest(mainClass));
        File sourceDirectory = new File(directory);
        add(sourceDirectory, target, jarFile, sourceDirectory);
        target.close();
        deleteClassFiles(new File(directory), jarFileName);
    }

    private static void deleteClassFiles(File directory, String jarFileName) {
        for (File file : directory.listFiles())
            if (file.isFile() && !file.getName().equals(jarFileName)) {
                file.delete();
            } else if (file.isDirectory()) {
                deleteClassFiles(file, jarFileName);
                file.delete();
            }
    }

    private static Manifest createManifest(String mainClass) {
        Manifest manifest = new Manifest();
        Attributes manifestAttributes = manifest.getMainAttributes();
        manifestAttributes.put(MANIFEST_VERSION, "1.0");
        manifestAttributes.put(MAIN_CLASS, mainClass);
        return manifest;
    }

    private static void add(File source, JarOutputStream target, File jarFile, File rootDirectory) throws IOException {
        if (source.isDirectory()) {
            addDirectory(source, target, jarFile, rootDirectory);
        } else {
            addFile(source, target, jarFile, rootDirectory);
        }
    }

    private static void addFile(File source, JarOutputStream jarOutputStream, File jarFile, File rootDirectory)
        throws IOException {
        if (source.equals(jarFile)) {
            return;
        }
        JarEntry entry = new JarEntry(getJarEntryName(source, rootDirectory));
        entry.setTime(source.lastModified());
        jarOutputStream.putNextEntry(entry);
        try (BufferedInputStream in = new BufferedInputStream(new FileInputStream(source))) {
            byte[] buffer = new byte[1024];
            while (true) {
                int count = in.read(buffer);
                if (count == -1) {
                    break;
                }
                jarOutputStream.write(buffer, 0, count);
            }
            jarOutputStream.closeEntry();
        }
    }

    private static void addDirectory(File source, JarOutputStream jarOutputStream, File jarFile, File rootDirectory)
        throws IOException {
        String name = getJarEntryName(source, rootDirectory);
        if (!name.isEmpty()) {
            createDirectory(name, source.lastModified(), jarOutputStream);
        }
        File[] files = requireNonNull(source.listFiles(), "Unable to get files from directory " + source);
        for (File file : files)
            add(file, jarOutputStream, jarFile, rootDirectory);
    }

    private static String getJarEntryName(File source, File rootDirectory) {
        String name = source.getPath().replace(rootDirectory.getPath(), "").replace("\\", "/");
        return name.isEmpty() || !name.startsWith("/") ? name : name.substring(1);
    }

    private static void createDirectory(String name, long lastModified, JarOutputStream jarOutputStream)
        throws IOException {
        if (!name.endsWith("/")) {
            name += "/";
        }
        JarEntry entry = new JarEntry(name);
        entry.setTime(lastModified);
        jarOutputStream.putNextEntry(entry);
        jarOutputStream.closeEntry();
    }

    public void createField(int acc, String className, String fieldName, String desc, String sig, Object value) {
        cw = cws.computeIfAbsent(className, cname -> {
            final ClassWriter classWriter = new IdrisClassWriter(COMPUTE_MAXS + COMPUTE_FRAMES);
            classWriter.visit(52, ACC_PUBLIC, cname, null, "java/lang/Object", null);
            return classWriter;
        });
        fv = cw.visitField(acc, fieldName, desc, sig, value);
    }

    public void createLabel(String labelName) {
        env.put(labelName, new Label());
    }

    public void createMethod(int acc,
                             String sourceFile,
                             String className,
                             String methodName,
                             String desc,
                             String sig,
                             Object exceptions,
                             Object annotations,
                             Object paramAnnotations) {
        createMethod(acc, sourceFile, className, methodName, desc, sig, (List) exceptions,
            (List) annotations, (List) paramAnnotations);
    }

    public void createMethod(int acc,
                             String sourceFile,
                             String className,
                             String methodName,
                             String desc,
                             String sig,
                             List<String> exceptions,
                             List<Annotation> annotations,
                             List<List<Annotation>> paramAnnotations) {
        if (mv != null) {
            classMethodVisitorStack.addFirst(new ClassMethodVisitor(this.className, this.methodName, cw, mv,
                env));
            env = new HashMap<>();
        }
        this.className = className;
        this.methodName = methodName;
        cw = cws.computeIfAbsent(className, cname -> {
            final ClassWriter classWriter = new IdrisClassWriter(COMPUTE_MAXS + COMPUTE_FRAMES);
            classWriter.visit(52, ACC_PUBLIC + ACC_FINAL, className, null, "java/lang/Object", null);
            classWriter.visitSource(sourceFile, null);
            return classWriter;
        });
        String[] exceptionsArr = exceptions == null ? null : exceptions.toArray(new String[0]);
        mv = cw.visitMethod(
            acc,
            methodName,
            desc,
            sig,
            exceptionsArr);
        handleCreateMethod(mv, annotations, paramAnnotations);
    }

    public void createIdrisConstructorClass(String className, boolean isStringConstructor,
                                            int constructorParameterCount) {
        if (!cws.containsKey(className)) {
            ClassWriter cw = new IdrisClassWriter(COMPUTE_MAXS + COMPUTE_FRAMES);
            FieldVisitor fv;
            MethodVisitor mv;

            cw.visit(52, ACC_PUBLIC + ACC_SUPER, className, null, "java/lang/Object",
                new String[]{"io/github/mmhelloworld/idris2/runtime/IdrisObject"});

            cw.visitSource(format("IdrisGenerated$%s.idr", className.replaceAll("/", "\\$")), null);

            String constructorFieldDescriptor = isStringConstructor ? "Ljava/lang/String;" : "I";
            fv = cw.visitField(ACC_PRIVATE + ACC_FINAL, "constructorId", constructorFieldDescriptor, null, null);
            fv.visitEnd();

            for (int index = 0; index < constructorParameterCount; index++) {
                fv = cw.visitField(ACC_PRIVATE + ACC_FINAL, "property" + index, "Ljava/lang/Object;", null, null);
                fv.visitEnd();
            }

            String constructorDescriptor = format("(%s%s)V", constructorFieldDescriptor,
                IntStream.range(0, constructorParameterCount)
                    .mapToObj(index -> "Ljava/lang/Object;")
                    .collect(joining()));
            mv = cw.visitMethod(ACC_PUBLIC, "<init>", constructorDescriptor, null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(isStringConstructor ? ALOAD : ILOAD, 1);
            mv.visitFieldInsn(PUTFIELD, className, "constructorId", constructorFieldDescriptor);
            for (int index = 0; index < constructorParameterCount; index++) {
                mv.visitVarInsn(ALOAD, 0);
                mv.visitVarInsn(ALOAD, index + 2);
                mv.visitFieldInsn(PUTFIELD, className, "property" + index, "Ljava/lang/Object;");
            }
            mv.visitInsn(RETURN);
            mv.visitMaxs(-1, -1);
            mv.visitEnd();

            String constructorGetter = isStringConstructor ? "getStringConstructorId" : "getConstructorId";
            mv = cw.visitMethod(ACC_PUBLIC, constructorGetter, format("()%s", constructorFieldDescriptor), null, null);
            mv.visitCode();
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, className, "constructorId", constructorFieldDescriptor);
            mv.visitInsn(isStringConstructor ? ARETURN : IRETURN);
            mv.visitMaxs(-1, -1);
            mv.visitEnd();

            if (constructorParameterCount > 0) {
                mv = cw.visitMethod(ACC_PUBLIC, "getProperty", "(I)Ljava/lang/Object;", null, null);
                mv.visitCode();

                mv.visitVarInsn(ILOAD, 1);
                IntStream propertyIndices = IntStream.range(0, constructorParameterCount);
                List<Entry<Integer, Label>> labels = IntStream.range(0, constructorParameterCount)
                    .mapToObj(index -> new SimpleEntry<>(index, new Label()))
                    .collect(toList());
                Label switchEnd = new Label();

                mv.visitLookupSwitchInsn(switchEnd, propertyIndices.toArray(), labels.stream()
                    .map(Entry::getValue)
                    .toArray(Label[]::new));
                MethodVisitor caseMv = mv;
                labels.forEach(labelAndIndex -> {
                    caseMv.visitLabel(labelAndIndex.getValue());
                    caseMv.visitVarInsn(ALOAD, 0);
                    caseMv.visitFieldInsn(GETFIELD, className, "property" + labelAndIndex.getKey(),
                        "Ljava/lang/Object;");
                    caseMv.visitInsn(ARETURN);
                });
                mv.visitLabel(switchEnd);
                mv.visitInsn(ACONST_NULL);
                mv.visitInsn(ARETURN);
                mv.visitMaxs(-1, -1);
                mv.visitEnd();
            }

            mv = cw.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null);
            mv.visitCode();
            mv.visitTypeInsn(NEW, "java/lang/StringBuilder");
            mv.visitInsn(DUP);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/StringBuilder", "<init>", "()V", false);
            mv.visitLdcInsn(format("%s{constructorId=", className));
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append",
                "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, className, "constructorId", constructorFieldDescriptor);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append",
                format("(%s)Ljava/lang/StringBuilder;", constructorFieldDescriptor), false);

            for (int index = 0; index < constructorParameterCount; index++) {
                mv.visitLdcInsn(format(", property%d=", index));
                mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append",
                    "(Ljava/lang/String;)Ljava/lang/StringBuilder;", false);
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, className, "property" + index, "Ljava/lang/Object;");
                mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append",
                    "(Ljava/lang/Object;)Ljava/lang/StringBuilder;", false);
            }

            mv.visitIntInsn(BIPUSH, 125);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "append", "(C)Ljava/lang/StringBuilder;",
                false);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/StringBuilder", "toString", "()Ljava/lang/String;", false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(-1, -1);
            mv.visitEnd();

            cw.visitEnd();
            cws.put(className, cw);
        }
    }

    public void d2i() {
        mv.visitInsn(D2I);
    }

    public void d2f() {
        mv.visitInsn(D2F);
    }

    public void dadd() {
        mv.visitInsn(DADD);
    }

    public void daload() {
        mv.visitInsn(DALOAD);
    }

    public void dastore() {
        mv.visitInsn(DASTORE);
    }

    public void dcmpg() {
        mv.visitInsn(DCMPG);
    }

    public void dcmpl() {
        mv.visitInsn(DCMPL);
    }

    public void dconst(double n) {
        if (n == 0) {
            mv.visitInsn(DCONST_0);
        } else if (n == 1) {
            mv.visitInsn(DCONST_1);
        } else {
            mv.visitLdcInsn(n);
        }
    }

    public void ddiv() {
        mv.visitInsn(DDIV);
    }

    public void debug(String msg) {
        if (shouldDebug) {
            System.out.println(msg);
        }
    }

    public void dload(int n) {
        mv.visitVarInsn(DLOAD, n);
    }

    public void dmul() {
        mv.visitInsn(DMUL);
    }

    public void dneg() {
        mv.visitInsn(DNEG);
    }

    public void drem() {
        mv.visitInsn(DREM);
    }

    public void dreturn() {
        mv.visitInsn(DRETURN);
    }

    public void dstore(int n) {
        mv.visitVarInsn(DSTORE, n);
    }

    public void dsub() {
        mv.visitInsn(DSUB);
    }

    public void dup() {
        mv.visitInsn(DUP);
    }

    public void f2d() {
        mv.visitInsn(F2D);
    }

    public void faload() {
        mv.visitInsn(FALOAD);
    }

    public void fastore() {
        mv.visitInsn(FASTORE);
    }

    public void fconst(double n) {
        if (n == 0) {
            mv.visitInsn(FCONST_0);
        } else if (n == 1) {
            mv.visitInsn(FCONST_1);
        } else {
            mv.visitLdcInsn((float) n);
        }
    }

    public void field(int fieldType, String className, String fieldName, String desc) {
        mv.visitFieldInsn(fieldType, className, fieldName, desc);
    }

    public void fieldEnd() {
        fv.visitEnd();
    }

    public void fload(int n) {
        mv.visitVarInsn(FLOAD, n);
    }

    public void frame(int frameType, int nLocal, Object local, int nStack, Object stack) {
        frame(frameType, nLocal, (List) local, nStack, (List) stack);
    }

    public void frame(int frameType, int nLocal, List<String> local, int nStack, List<String> stack) {
        mv.visitFrame(
            frameType,
            nLocal,
            local.stream().map(this::toOpcode).toArray(),
            nStack,
            stack.stream().map(this::toOpcode).toArray()
        );
    }

    public void freturn() {
        mv.visitInsn(FRETURN);
    }

    public void fstore(int n) {
        mv.visitVarInsn(FSTORE, n);
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }

    public int getLocalVarCount() {
        return localVarCount;
    }

    public void gotoLabel(String labelName) {
        Label label = (Label) env.get(labelName);
        if (label == null) {
            throw new NullPointerException(format("Label %s cannot be null. Current method: %s, Environment: %s",
                labelName, methodName, env));
        }
        mv.visitJumpInsn(Opcodes.GOTO, label);
    }

    public void i2b() {
        mv.visitInsn(I2B);
    }

    public void i2c() {
        mv.visitInsn(I2C);
    }

    public void i2d() {
        mv.visitInsn(I2D);
    }

    public void i2l() {
        mv.visitInsn(I2L);
    }

    public void i2s() {
        mv.visitInsn(I2S);
    }

    public void iadd() {
        mv.visitInsn(IADD);
    }

    public void iaload() {
        mv.visitInsn(IALOAD);
    }

    public void iand() {
        mv.visitInsn(IAND);
    }

    public void iastore() {
        mv.visitInsn(IASTORE);
    }

    public void ior() {
        mv.visitInsn(IOR);
    }

    public void ixor() {
        mv.visitInsn(IXOR);
    }

    public void icompl() {
        mv.visitInsn(ICONST_M1);
        mv.visitInsn(IXOR);
    }

    public void iconst(int n) {
        if (n >= 0 && n <= 5) {
            final int[] opcodes = {ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5};
            mv.visitInsn(opcodes[n]);
        } else if (n == -1) {
            mv.visitInsn(ICONST_M1);
        } else if (n >= (-128) && n <= 127) {
            mv.visitIntInsn(BIPUSH, n);
        } else if (n >= (-32768) && n <= 32767) {
            mv.visitIntInsn(SIPUSH, n);
        } else {
            mv.visitLdcInsn(n);
        }
    }

    public void idiv() {
        mv.visitInsn(IDIV);
    }

    public void ifeq(String label) {
        mv.visitJumpInsn(IFEQ, (Label) env.get(label));
    }

    public void ifge(String label) {
        mv.visitJumpInsn(IFGE, (Label) env.get(label));
    }

    public void ifgt(String label) {
        mv.visitJumpInsn(IFGT, (Label) env.get(label));
    }

    public void ificmpge(String label) {
        mv.visitJumpInsn(IF_ICMPGE, (Label) env.get(label));
    }

    public void ificmpgt(String label) {
        mv.visitJumpInsn(IF_ICMPGT, (Label) env.get(label));
    }

    public void ificmple(String label) {
        mv.visitJumpInsn(IF_ICMPLE, (Label) env.get(label));
    }

    public void ificmplt(String label) {
        mv.visitJumpInsn(IF_ICMPLT, (Label) env.get(label));
    }

    public void ificmpne(String label) {
        mv.visitJumpInsn(IF_ICMPNE, (Label) env.get(label));
    }

    public void ificmpeq(String label) {
        mv.visitJumpInsn(IF_ICMPEQ, (Label) env.get(label));
    }

    public void ifle(String label) {
        mv.visitJumpInsn(IFLE, (Label) env.get(label));
    }

    public void iflt(String label) {
        mv.visitJumpInsn(IFLT, (Label) env.get(label));
    }

    public void ifne(String label) {
        mv.visitJumpInsn(IFNE, (Label) env.get(label));
    }

    public void ifnonnull(String label) {
        mv.visitJumpInsn(IFNONNULL, (Label) env.get(label));
    }

    public void ifnull(String label) {
        mv.visitJumpInsn(IFNULL, (Label) env.get(label));
    }

    public void iload(int n) {
        mv.visitVarInsn(ILOAD, n);
    }

    public void imul() {
        mv.visitInsn(IMUL);
    }

    public void ineg() {
        mv.visitInsn(INEG);
    }

    public void instanceOf(String className) {
        mv.visitTypeInsn(INSTANCEOF, className);
    }

    public void invokeMethod(int invocType, String className, String methodName, String desc, boolean isInterface) {
        mv.visitMethodInsn(
            invocType,
            className,
            methodName,
            desc,
            isInterface);
    }

    public void invokeDynamic(String methodName, String desc, Object handle, Object invokeDynamicArgs) {
        invokeDynamic(methodName, desc, (JHandle) handle, (List) invokeDynamicArgs);
    }

    public void invokeDynamic(String methodName, String desc, JHandle handle, List<JBsmArg> invokeDynamicArgs) {
        final Object[] bsmArgs = new Object[invokeDynamicArgs.size()];
        for (int index = 0; index < bsmArgs.length; index++) {
            JBsmArg arg = invokeDynamicArgs.get(index);
            switch (arg.getType()) {
                case BsmArgGetType:
                    JBsmArgGetType getType = (JBsmArgGetType) arg;
                    bsmArgs[index] = Type.getType(getType.getDesc());
                    break;
                case BsmArgHandle:
                    JBsmArg.JBsmArgHandle bsmHandle = (JBsmArg.JBsmArgHandle) arg;
                    bsmArgs[index] = getAsmHandle(bsmHandle.getHandle());
                    break;
            }
        }

        mv.visitInvokeDynamicInsn(
            methodName,
            desc,
            getAsmHandle(handle),
            bsmArgs
        );
    }

    public void irem() {
        mv.visitInsn(IREM);
    }

    public void ireturn() {
        mv.visitInsn(IRETURN);
    }

    public void ishl() {
        mv.visitInsn(ISHL);
    }

    public void ishr() {
        mv.visitInsn(ISHR);
    }

    public void istore(int n) {
        mv.visitVarInsn(ISTORE, n);
    }

    public void isub() {
        mv.visitInsn(ISUB);
    }

    public void iushr() {
        mv.visitInsn(IUSHR);
    }

    public void l2i() {
        mv.visitInsn(L2I);
    }

    public void labelStart(String label) {
        mv.visitLabel((Label) (env.get(label)));
    }

    public void ladd() {
        mv.visitInsn(LADD);
    }

    public void laload() {
        mv.visitInsn(LALOAD);
    }

    public void land() {
        mv.visitInsn(LAND);
    }

    public void lastore() {
        mv.visitInsn(LASTORE);
    }

    public void lor() {
        mv.visitInsn(LOR);
    }

    public void lxor() {
        mv.visitInsn(LXOR);
    }

    public void lcompl() {
        mv.visitLdcInsn(-1L);
        mv.visitInsn(LXOR);
    }

    public void lconst(long n) {
        if (n == 0) {
            mv.visitInsn(LCONST_0);
        } else if (n == 1) {
            mv.visitInsn(LCONST_1);
        } else {
            mv.visitLdcInsn(n);
        }
    }

    public void ldcDouble(double val) {
        mv.visitLdcInsn(val);
    }

    public void ldcInteger(int val) {
        mv.visitLdcInsn(val);
    }

    public void ldcLong(long val) {
        mv.visitLdcInsn(val);
    }

    public void ldcString(String val) {
        mv.visitLdcInsn(val);
    }

    public void ldcType(String val) {
        mv.visitLdcInsn(Type.getType(val));
    }

    public void ldc(Object value) {
        mv.visitLdcInsn(value);
    }

    public void ldiv() {
        mv.visitInsn(LDIV);
    }

    public void lload(int n) {
        mv.visitVarInsn(LLOAD, n);
    }

    public void lmul() {
        mv.visitInsn(LMUL);
    }

    public void lneg() {
        mv.visitInsn(LNEG);
    }

    public void lookupSwitch(String defaultLabel, Object caseLabels, Object cases) {
        lookupSwitch(defaultLabel, (List) caseLabels, (List) cases);
    }

    public void lookupSwitch(String defaultLabel, List<String> caseLabels, List<Integer> cases) {
        int[] casesArr = cases.stream().mapToInt(n -> n).toArray();
        mv.visitLookupSwitchInsn(
            (Label) env.get(defaultLabel),
            casesArr,
            caseLabels.stream()
                .map(s -> (Label) env.get(s))
                .toArray(Label[]::new)
        );
    }

    public void lshl() {
        mv.visitInsn(LSHL);
    }

    public void lrem() {
        mv.visitInsn(LREM);
    }

    public void lreturn() {
        mv.visitInsn(LRETURN);
    }

    public void lshr() {
        mv.visitInsn(LSHR);
    }

    public void lstore(int n) {
        mv.visitVarInsn(LSTORE, n);
    }

    public void lsub() {
        mv.visitInsn(LSUB);
    }

    public void lushr() {
        mv.visitInsn(LUSHR);
    }

    public void maxStackAndLocal(int maxStack, int maxLocal) {
        mv.visitMaxs(maxStack, maxLocal);
    }

    public void methodCodeStart() {
        mv.visitCode();
    }

    public void methodCodeEnd() {
        mv.visitEnd();
        if (!classMethodVisitorStack.isEmpty()) {
            ClassMethodVisitor classMethodVisitor = classMethodVisitorStack.removeFirst();
            cw = classMethodVisitor.getClassVisitor();
            mv = classMethodVisitor.getMethodVisitor();
            env = classMethodVisitor.getEnv();
            className = classMethodVisitor.getClassName();
            methodName = classMethodVisitor.getMethodName();
        } else {
            mv = null;
            env.clear();
        }
    }

    public void multiANewArray(String desc, int noOfDims) {
        mv.visitMultiANewArrayInsn(desc, noOfDims);
    }

    public void asmNew(String className) {
        mv.visitTypeInsn(NEW, className);
    }

    public void pop() {
        mv.visitInsn(POP);
    }

    public void voidReturn() {
        mv.visitInsn(RETURN);
    }

    public void saload() {
        mv.visitInsn(SALOAD);
    }

    public void sastore() {
        mv.visitInsn(SASTORE);
    }

    public boolean shouldDescribeFrame() {
        return shouldDescribeFrame;
    }

    public void sourceInfo(String sourceFileName) {
        cw.visitSource(sourceFileName, null);
    }

    public void updateLocalVarCount(int localVarCount) {
        this.localVarCount = localVarCount;
    }

    public void updateShouldDescribeFrame(boolean shouldDescribeFrame) {
        this.shouldDescribeFrame = shouldDescribeFrame;
    }

    public void lineNumber(int lineNumber, String label) {
        mv.visitLineNumber(lineNumber, (Label) env.get(label));
    }

    public void localVariable(String name, String typeDescriptor, String signature, String lineNumberStartLabel,
                              String lineNumberEndLabel, int index) {
        Label start = (Label) env.get(lineNumberStartLabel);
        requireNonNull(start, format("Line number start label '%s' for variable %s at index %d must not be null",
            lineNumberStartLabel, name, index));
        Label end = (Label) env.get(lineNumberEndLabel);
        requireNonNull(end, format("Line number end label '%s' for variable %s at index %d must not be null",
            lineNumberEndLabel, name, index));
        mv.visitLocalVariable(name, typeDescriptor, signature, start, end, index);
    }

    public static void copyRuntimeClasses(String outputDirectoryName) {
        String runtimePath = "/io/github/mmhelloworld/idris2/runtime";
        Path outputDirectory = Paths.get(outputDirectoryName, runtimePath);
        try {
            copyFromJar(runtimePath, outputDirectory);
        } catch (URISyntaxException | IOException e) {
            e.printStackTrace();
        }
    }

    public static void copyFromJar(String source, Path target) throws URISyntaxException, IOException {
        URI resource = IdrisSystem.class.getResource("").toURI();
        FileSystem fileSystem = FileSystems.newFileSystem(resource, emptyMap());
        Path jarPath = fileSystem.getPath(source);
        Files.walkFileTree(jarPath, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
                Path currentTarget = target.resolve(jarPath.relativize(dir).toString());
                Files.createDirectories(currentTarget);
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                Files.copy(file, target.resolve(jarPath.relativize(file).toString()), REPLACE_EXISTING);
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private void handleCreateMethod(MethodVisitor mv, List<Annotation> annotations,
                                    List<List<Annotation>> parametersAnnotations) {
        annotations.forEach(annotation -> {
            final AnnotationVisitor av = mv.visitAnnotation(annotation.getName(), true);
            annotation.getProperties().forEach(prop -> visitAnnotationProperty(av, prop.getName(), prop.getValue()));
            av.visitEnd();
        });

        for (int index = 0; index < parametersAnnotations.size(); index++) {
            int parameterIndex = index;
            List<Annotation> parameterAnnotations = parametersAnnotations.get(parameterIndex);
            parameterAnnotations.forEach(paramAnnotation -> {
                final AnnotationVisitor av =
                    mv.visitParameterAnnotation(parameterIndex, paramAnnotation.getName(), true);
                paramAnnotation.getProperties().forEach(prop -> visitAnnotationProperty(av, prop.getName(),
                    prop.getValue()));
                av.visitEnd();
            });
        }

    }

    private String getClassNameLastPart(String className) {
        return className.substring(className.lastIndexOf('/') + 1);
    }

    private Object toOpcode(String s) {
        switch (s) {
            case "INTEGER":
                return Opcodes.INTEGER;
            case "FLOAT":
                return Opcodes.FLOAT;
            case "LONG":
                return Opcodes.LONG;
            case "DOUBLE":
                return Opcodes.DOUBLE;
            case "NULL":
                return Opcodes.NULL;
            case "UNINITIALIZED_THIS":
                return Opcodes.UNINITIALIZED_THIS;
            case "TOP":
                return Opcodes.TOP;
            default:
                return s;
        }
    }

    private void handleClassCodeStart(final Map<String, ClassWriter> cws,
                                      final ClassWriter cw,
                                      final int version,
                                      final int access,
                                      final String className,
                                      final String signature,
                                      final String parentClassName,
                                      final List<String> interfaces,
                                      final List<Annotation> annotations) {
        cw.visit(version,
            access,
            className,
            signature,
            parentClassName,
            interfaces.toArray(new String[0]));
        cws.put(className, cw);

        annotations.forEach(annotation ->
            visitAnnotation(annotation, cw.visitAnnotation(annotation.getName(), true)));
    }

    private void visitAnnotation(Annotation annotation, AnnotationVisitor av) {
        annotation.getProperties()
            .forEach(prop -> visitAnnotationProperty(av, prop.getName(), prop.getValue()));
        av.visitEnd();
    }

    private void visitAnnotationProperty(AnnotationVisitor annotationVisitor, String name, AnnotationValue value) {
        switch (value.getType()) {
            case AnnString:
                AnnString annString = (AnnString) value;
                annotationVisitor.visit(name, annString.getValue());
                break;
            case AnnEnum:
                AnnEnum annEnum = (AnnEnum) value;
                annotationVisitor.visitEnum(name, annEnum.getEnumTy(), annEnum.getValue());
                break;
            case AnnInt:
                AnnInt annInt = (AnnInt) value;
                annotationVisitor.visit(name, annInt.getValue());
                break;
            case AnnBoolean:
                AnnBoolean annBoolean = (AnnBoolean) value;
                annotationVisitor.visit(name, annBoolean.getValue());
                break;
            case AnnByte:
                AnnByte annByte = (AnnByte) value;
                annotationVisitor.visit(name, annByte.getValue());
                break;
            case AnnChar:
                AnnChar annChar = (AnnChar) value;
                annotationVisitor.visit(name, annChar.getValue());
                break;
            case AnnShort:
                AnnShort annShort = (AnnShort) value;
                annotationVisitor.visit(name, annShort.getValue());
                break;
            case AnnLong:
                AnnLong annLong = (AnnLong) value;
                annotationVisitor.visit(name, annLong.getValue());
                break;
            case AnnFloat:
                AnnFloat annFloat = (AnnFloat) value;
                annotationVisitor.visit(name, (float) annFloat.getValue());
                break;
            case AnnDouble:
                AnnDouble annDouble = (AnnDouble) value;
                annotationVisitor.visit(name, annDouble.getValue());
                break;
            case AnnClass:
                AnnClass annClass = (AnnClass) value;
                String typeDescriptor = annClass.getValue();
                Type type = getType(typeDescriptor);
                annotationVisitor.visit(name, type);
                break;
            case AnnArray:
                AnnArray annArray = (AnnArray) value;
                AnnotationVisitor arrayPropertyVisitor = annotationVisitor.visitArray(name);
                annArray.getValues()
                    .forEach(propertyValue -> visitAnnotationProperty(arrayPropertyVisitor, null, propertyValue));
                arrayPropertyVisitor.visitEnd();
                break;
            case AnnAnnotation:
                AnnAnnotation annAnnotation = (AnnAnnotation) value;
                Annotation annotation = annAnnotation.getValue();
                visitAnnotation(annotation, annotationVisitor.visitAnnotation(name, annotation.getName()));
                break;
        }
    }

    private static Type getType(String typeDescriptor) {
        switch (typeDescriptor) {
            case "boolean":
                return Type.BOOLEAN_TYPE;
            case "byte":
                return Type.BYTE_TYPE;
            case "char":
                return Type.CHAR_TYPE;
            case "short":
                return Type.SHORT_TYPE;
            case "int":
                return Type.INT_TYPE;
            case "long":
                return Type.LONG_TYPE;
            case "float":
                return Type.FLOAT_TYPE;
            case "double":
                return Type.DOUBLE_TYPE;
            case "void":
                return Type.VOID_TYPE;
            default:
                if (typeDescriptor.endsWith("[]")) {
                    return Type.getObjectType(getArrayDescriptor(typeDescriptor));
                } else {
                    return Type.getObjectType(typeDescriptor);
                }
        }

    }

    private static String getArrayDescriptor(String str) {
        int stack = 0;
        int dimension = 0;
        for (int i = str.length() - 1; i >= 0; i--) {
            char ch = str.charAt(i);
            int delta = ch == ']' ? 1 : (ch == '[' ? -1 : 0);
            if (delta == 0) {
                if (stack != 0) {
                    throw new IllegalArgumentException("Invalid array descriptor " + str);
                }
                return createArrayDescriptor(str.substring(0, i + 1), dimension);
            }
            stack = stack + delta;
            if (stack == 0) {
                dimension++;
            } else if (stack != 1) {
                throw new IllegalArgumentException("Invalid array descriptor " + str);
            }
        }
        throw new IllegalArgumentException("Invalid array descriptor " + str);
    }

    private static String createArrayDescriptor(String elementTypeName, int dimension) {
        String elementTypeDesc = getType(elementTypeName).getDescriptor();
        StringBuilder arrayDesc = new StringBuilder();
        for (int i = 0; i < dimension; i++) {
            arrayDesc.append('[');
        }
        return arrayDesc + elementTypeDesc;
    }

    private Handle getAsmHandle(JHandle handle) {
        return new Handle(
            handle.getTag(),
            handle.getCname(),
            handle.getMname(),
            handle.getDesc(),
            handle.isIntf()
        );
    }
}

