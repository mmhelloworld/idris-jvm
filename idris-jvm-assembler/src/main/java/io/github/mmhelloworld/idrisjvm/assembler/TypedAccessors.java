package io.github.mmhelloworld.idrisjvm.assembler;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;

import java.util.List;

import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ALOAD;
import static org.objectweb.asm.Opcodes.ARETURN;
import static org.objectweb.asm.Opcodes.CHECKCAST;
import static org.objectweb.asm.Opcodes.DRETURN;
import static org.objectweb.asm.Opcodes.FRETURN;
import static org.objectweb.asm.Opcodes.GETFIELD;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;
import static org.objectweb.asm.Opcodes.IRETURN;
import static org.objectweb.asm.Opcodes.LRETURN;

/**
 * Typed per-slot accessors of specialised constructor classes and their TCon family interfaces:
 * the {@code getInt0()}/{@code getRef2()} naming, the union of accessors a family interface
 * declares across its member shapes, and the unboxing bridges a natural (Object-slotted) class
 * provides so interface-accessor reads stay carrier-safe under mixed spec/natural values.
 */
final class TypedAccessors {

  private TypedAccessors() {
  }

  // Split a concatenated field-descriptor string into per-slot descriptors.
  static java.util.List<String> splitDescriptors(String shape) {
    var slots = new java.util.ArrayList<String>();
    int i = 0;
    while (i < shape.length()) {
      int start = i;
      while (shape.charAt(i) == '[') {
        i++;
      }
      if (shape.charAt(i) == 'L') {
        i = shape.indexOf(';', i) + 1;
      } else {
        i++;
      }
      slots.add(shape.substring(start, i));
    }
    return slots;
  }

  /** A typed slot accessor of a family interface: its method name and return descriptor. */
  record Accessor(String name, String descriptor) {
  }

  // Union of slot accessors across member shapes, keyed by BOTH name and descriptor. Two members can
  // type the same slot index differently (a family-refined reference slot in one shape, plain Object
  // in another), and every constructor implements the accessor of its own shape, so each variant must
  // be declared or an interface call typed against the undeclared variant fails with NoSuchMethodError.
  static java.util.Set<Accessor> accessorUnion(List<String> memberShapes) {
    var union = new java.util.LinkedHashSet<Accessor>();
    for (String shape : memberShapes) {
      var slots = splitDescriptors(shape);
      for (int i = 0; i < slots.size(); i++) {
        union.add(new Accessor(accessorNameFor(slots.get(i), i), slots.get(i)));
      }
    }
    return union;
  }

  // Unboxing bridge from an Object-typed natural field to a typed accessor
  // return. Reference returns checkcast (family-refined slot interfaces are
  // implemented by natural carriers too, so the cast holds for both).
  static void visitNaturalAccessorBridge(ClassWriter classWriter, String className,
                                                 String accessorName, String descriptor, int slot) {
    var conversion = "io/github/mmhelloworld/idrisjvm/runtime/Conversion";
    var bridge = classWriter.visitMethod(ACC_PUBLIC, accessorName, "()" + descriptor, null, null);
    bridge.visitCode();
    bridge.visitVarInsn(ALOAD, 0);
    bridge.visitFieldInsn(GETFIELD, className, "property" + slot, "Ljava/lang/Object;");
    switch (descriptor.charAt(0)) {
      case 'I':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toInt", "(Ljava/lang/Object;)I", false);
        break;
      case 'J':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toLong", "(Ljava/lang/Object;)J", false);
        break;
      case 'D':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toDouble", "(Ljava/lang/Object;)D", false);
        break;
      case 'Z':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toBoolean", "(Ljava/lang/Object;)Z", false);
        break;
      case 'C':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toChar", "(Ljava/lang/Object;)C", false);
        break;
      case 'B':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toByte", "(Ljava/lang/Object;)B", false);
        break;
      case 'S':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toShort", "(Ljava/lang/Object;)S", false);
        break;
      case 'F':
        bridge.visitMethodInsn(INVOKESTATIC, conversion, "toFloat", "(Ljava/lang/Object;)F", false);
        break;
      default:
        if (!descriptor.equals("Ljava/lang/Object;")) {
          bridge.visitTypeInsn(CHECKCAST, descriptor.substring(1, descriptor.length() - 1));
        }
        break;
    }
    bridge.visitInsn(returnOpcodeFor(descriptor));
    bridge.visitMaxs(-1, -1);
    bridge.visitEnd();
  }

  static String accessorNameFor(String descriptor, int index) {
    switch (descriptor.charAt(0)) {
      case 'Z': return "getBool"   + index;
      case 'B': return "getByte"   + index;
      case 'C': return "getChar"   + index;
      case 'S': return "getShort"  + index;
      case 'I': return "getInt"    + index;
      case 'J': return "getLong"   + index;
      case 'F': return "getFloat"  + index;
      case 'D': return "getDouble" + index;
      default:  return "getRef"    + index;
    }
  }

  static int returnOpcodeFor(String descriptor) {
    switch (descriptor.charAt(0)) {
      case 'Z': case 'B': case 'C': case 'S': case 'I': return IRETURN;
      case 'J': return LRETURN;
      case 'F': return FRETURN;
      case 'D': return DRETURN;
      default:  return ARETURN;
    }
  }

  // Box a primitive on the stack via the matching java.lang.<Wrapper>.valueOf
  // static method; reference values are already boxed.
  static void boxPrimitive(MethodVisitor mv, String descriptor) {
    switch (descriptor.charAt(0)) {
      case 'Z':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean",   "valueOf", "(Z)Ljava/lang/Boolean;",   false);
        break;
      case 'B':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Byte",      "valueOf", "(B)Ljava/lang/Byte;",      false);
        break;
      case 'C':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false);
        break;
      case 'S':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Short",     "valueOf", "(S)Ljava/lang/Short;",     false);
        break;
      case 'I':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer",   "valueOf", "(I)Ljava/lang/Integer;",   false);
        break;
      case 'J':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long",      "valueOf", "(J)Ljava/lang/Long;",      false);
        break;
      case 'F':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Float",     "valueOf", "(F)Ljava/lang/Float;",     false);
        break;
      case 'D':
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double",    "valueOf", "(D)Ljava/lang/Double;",    false);
        break;
      default:
        // reference type: already an Object — nothing to do
        break;
    }
  }

}
