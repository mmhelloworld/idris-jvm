package IdrisJvm.Core;

import IdrisJvm.Core.JBsmArg.JBsmArgGetType;
import IdrisJvm.Core.JBsmArg.JBsmArgHandle;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Handle;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.objectweb.asm.ClassWriter.COMPUTE_MAXS;
import static org.objectweb.asm.Opcodes.AALOAD;
import static org.objectweb.asm.Opcodes.AASTORE;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
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
import static org.objectweb.asm.Opcodes.DCONST_0;
import static org.objectweb.asm.Opcodes.DCONST_1;
import static org.objectweb.asm.Opcodes.DDIV;
import static org.objectweb.asm.Opcodes.DLOAD;
import static org.objectweb.asm.Opcodes.DMUL;
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
import static org.objectweb.asm.Opcodes.IFNONNULL;
import static org.objectweb.asm.Opcodes.IFNULL;
import static org.objectweb.asm.Opcodes.IF_ICMPGE;
import static org.objectweb.asm.Opcodes.IF_ICMPGT;
import static org.objectweb.asm.Opcodes.IF_ICMPLE;
import static org.objectweb.asm.Opcodes.IF_ICMPLT;
import static org.objectweb.asm.Opcodes.ILOAD;
import static org.objectweb.asm.Opcodes.IMUL;
import static org.objectweb.asm.Opcodes.INSTANCEOF;
import static org.objectweb.asm.Opcodes.INVOKESPECIAL;
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

public class Assembler {
    private static final Logger LOGGER = LogManager.getLogger(Assembler.class);

    private Map<String, ClassWriter> cws;
    private ClassWriter cw;
    private MethodVisitor mv;
    private FieldVisitor fv;
    private Map<String, Object> env;
    private String className;
    private String methodName;
    private int localVarCount;
    private int ifIndex;
    private int switchIndex;
    private Map<String, Integer> lambdaIndex;
    private boolean shouldDescribeFrame;

    public Assembler() {
        this.cws = new HashMap<>();
        this.env = new HashMap<>();
        this.lambdaIndex = new HashMap<>();
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
                               List<String> interfaces,
                               final List<Annotation> annotations) {
        this.className = className;
        handleClassCodeStart(cws, cw, version, access, className, signature, parentClassName, interfaces, annotations);
    }

    public void classCodeEnd(String outputClassFileDir) {
        cw.visitEnd();
        cws.entrySet().parallelStream().forEach(e -> {
            String className = e.getKey();
            ClassWriter classWriter = e.getValue();
            File outFile = new File(outputClassFileDir, className + ".class");
            new File(outFile.getParent()).mkdirs();
            try (OutputStream out = new FileOutputStream(outFile)) {
                out.write(classWriter.toByteArray());
            } catch (Exception exception) {
                exception.printStackTrace();
            }
        });
    }

    public void createClass(int flags) {
        cw = new ClassWriter(flags);
    }

    private MethodVisitor createDefaultConstructor(final ClassWriter cw) {
        final MethodVisitor mv;
        mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
        return mv;
    }

    public void createField(int acc, String className, String fieldName, String desc, String sig, Object value) {
        cw = cws.computeIfAbsent(className, cname -> {
            final ClassWriter classWriter = new ClassWriter(COMPUTE_MAXS);
            classWriter.visit(52, ACC_PUBLIC, cname, null, "java/lang/Object", null);
            createDefaultConstructor(classWriter);
            return classWriter;
        });
        fv = cw.visitField(acc, fieldName, desc, sig, value);
    }

    public void createLabel(String labelName) {
        Label label = new Label();
        env.put(labelName, label);
    }

    private void handleCreateMethod(final MethodVisitor mv, List<Annotation> annotations) {
        annotations.forEach(annotation -> {
            final AnnotationVisitor av = mv.visitAnnotation(annotation.getName(), true);
            annotation.getProperties().forEach(prop -> addPropertyToAnnotation(av, prop));
        });
    }


    public void createMethod(int acc,
                             String className,
                             String methodName,
                             String desc,
                             String sig,
                             List<String> exceptions,
                             List<Annotation> annotations,
                             List<List<Annotation>> paramAnnotations) {
        this.className = className;
        this.methodName = methodName;
        cw = cws.computeIfAbsent(className, cname -> {
            final ClassWriter classWriter = new ClassWriter(COMPUTE_MAXS);
            classWriter.visit(52, ACC_PUBLIC, className, null, "java/lang/Object", null);
            classWriter.visitSource(getClassNameLastPart(className) + ".idr", null);
            createDefaultConstructor(classWriter);
            return classWriter;
        });
        final String[] exceptionsArr = exceptions == null ? null : exceptions.toArray(new String[exceptions.size()]);
        mv = cw.visitMethod(
            acc,
            methodName,
            desc,
            sig,
            exceptionsArr);
        handleCreateMethod(mv, annotations);
    }

    private String getClassNameLastPart(String className) {
        return className.substring(className.lastIndexOf('/') + 1);
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
        LOGGER.debug(msg);
    }

    public void dload(int n) {
        mv.visitVarInsn(DLOAD, n);
    }

    public void dmul() {
        mv.visitInsn(DMUL);
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
            mv.visitLdcInsn((float)n);
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

    public void frame(int frameType, int nLocal, List<String> local, int nStack, List<String> stack) {
        mv.visitFrame(
            frameType,
            nLocal,
            local.stream().map(this::toOpcode).toArray(),
            nStack,
            stack.stream().map(this::toOpcode).toArray()
        );
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

    public int freshIfIndex() {
        return ifIndex++;
    }

    public int freshSwitchIndex() {
        return switchIndex++;
    }

    public int freshLambdaIndex(String className) {
        final int currentLambdaIndex = lambdaIndex.getOrDefault(className, 0);
        lambdaIndex.put(className, currentLambdaIndex + 1);
        return currentLambdaIndex;
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
        mv.visitJumpInsn(Opcodes.GOTO, (Label) env.get(labelName));
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
        mv.visitJumpInsn(Opcodes.IFEQ, (Label) env.get(label));
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
                    JBsmArgHandle bsmHandle = (JBsmArgHandle) arg;
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

    public void lookupSwitch(String defaultLabel, List<String> caseLabels, List<Integer> cases) {
        final int[] casesArr = cases.stream().mapToInt(n -> n).toArray();
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

    public void asmReturn() {
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

    public void updateFunctionName(String className, String methodName) {
        this.className = className;
        this.methodName = methodName;
    }

    public void updateLocalVarCount(int localVarCount) {
        this.localVarCount = localVarCount;
    }

    public void updateShouldDescribeFrame(boolean shouldDescribeFrame) {
        this.shouldDescribeFrame = shouldDescribeFrame;
    }

    public void updateIfIndex(int ifIndex) {
        this.ifIndex = ifIndex;
    }

    public void updateSwitchIndex(int switchIndex) {
        this.switchIndex = switchIndex;
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
            interfaces.toArray(new String[interfaces.size()]));
        cws.put(className, cw);

        annotations.forEach(annotation -> {
            AnnotationVisitor av = cw.visitAnnotation(annotation.getName(), true);
            annotation.getProperties().forEach(prop -> addPropertyToAnnotation(av, prop));
            av.visitEnd();
        });
    }

    private void addPropertyToAnnotation(final AnnotationVisitor av, final AnnotationProperty prop) {
        final JAnnotationValue.AnnotationValueType propType = prop.getValue().getType();
        switch (propType) {
            case AnnString:
                JAnnotationValue.JAnnString annStr = (JAnnotationValue.JAnnString) prop.getValue();
                av.visit(prop.getName(), annStr.getValue());
                break;
            case AnnInt:
                JAnnotationValue.JAnnInt annInt = (JAnnotationValue.JAnnInt) prop.getValue();
                av.visit(prop.getName(), annInt.getValue());
                break;
            case AnnArray:
                break;
        }
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
