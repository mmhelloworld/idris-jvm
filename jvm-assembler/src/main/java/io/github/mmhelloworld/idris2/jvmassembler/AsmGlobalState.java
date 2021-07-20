package io.github.mmhelloworld.idris2.jvmassembler;

import org.objectweb.asm.ClassWriter;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import static io.github.mmhelloworld.idris2.jvmassembler.Assembler.copyRuntimeClasses;
import static io.github.mmhelloworld.idris2.jvmassembler.Assembler.createJar;
import static java.util.Collections.synchronizedSet;
import static java.util.stream.Collectors.toMap;

public final class AsmGlobalState {

    private final Map<String, Object> functions;
    private final Set<String> untypedFunctions;
    private final Set<String> constructors;
    private final String programName;
    private final Map<String, Assembler> assemblers;

    public AsmGlobalState(String programName) {
        this.programName = programName;
        functions = new ConcurrentHashMap<>();
        untypedFunctions = synchronizedSet(new HashSet<>());
        constructors = synchronizedSet(new HashSet<>());
        assemblers = new ConcurrentHashMap<>();
    }

    public void addFunction(String name, Object value) {
        functions.put(name, value);
    }

    public Object getFunction(String name) {
        return functions.get(name);
    }

    public void addUntypedFunction(String name) {
        untypedFunctions.add(name);
    }

    public boolean isUntypedFunction(String name) {
        return untypedFunctions.contains(name);
    }

    public Assembler getAssembler(String name) {
        return assemblers.computeIfAbsent(name, key -> new Assembler());
    }

    public String getProgramName() {
        return programName;
    }

    public boolean hasConstructor(String name) {
        return constructors.contains(name);
    }

    public void addConstructor(String name) {
        constructors.add(name);
    }

    public void classCodeEnd(String outputDirectory, String outputFile, String mainClass) throws IOException {
        if (outputDirectory.isEmpty()) {
            interpret(mainClass.replace('/', '.'));
        } else {
            assemblers.values().parallelStream()
                .flatMap(assembler -> assembler.getClassWriters().entrySet().stream())
                .forEach(classNameAndClassWriter ->
                    writeClass(classNameAndClassWriter.getKey(), classNameAndClassWriter.getValue(),
                        outputDirectory));
            copyRuntimeClasses(outputDirectory);
            createJar(outputDirectory, outputFile, mainClass.replace('/', '.'));
        }
    }

    public void interpret(String mainClass) {
        Map<String, byte[]> classes = assemblers.values().parallelStream()
            .flatMap(assembler -> assembler.getClassWriters().entrySet().stream())
            .collect(toMap(Entry::getKey, entry -> entry.getValue().toByteArray()));
        try {
            new InterpreterClassLoader(classes)
                .loadClass(mainClass)
                .getDeclaredMethod("main", String[].class)
                .invoke(null, (Object) new String[]{});
        } catch (ClassNotFoundException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

    public void writeClass(String className, ClassWriter classWriter, String outputClassFileDir) {
        File outFile = new File(outputClassFileDir, className + ".class");
        new File(outFile.getParent()).mkdirs();
        try (OutputStream out = new FileOutputStream(outFile)) {
            out.write(classWriter.toByteArray());
        } catch (Exception exception) {
            exception.printStackTrace();
        }
    }

}
