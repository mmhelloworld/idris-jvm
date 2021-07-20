package io.github.mmhelloworld.idris2.jvmassembler;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;

public class IdrisClassWriter extends ClassWriter {
    public IdrisClassWriter(int flags) {
        super(flags);
    }

    public IdrisClassWriter(ClassReader classReader, int flags) {
        super(classReader, flags);
    }

    @Override
    protected String getCommonSuperClass(String type1, String type2) {
        try {
            return super.getCommonSuperClass(type1, type2);
        } catch (TypeNotPresentException exception) {
            return "java/lang/Object";
        }
    }
}
