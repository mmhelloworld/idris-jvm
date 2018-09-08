package io.github.mmhelloworld.idrisjvm;

import IdrisJvm.Core.Assembler;
import IdrisJvm.Core.IdrisToJavaNameConverter;
import IdrisJvm.Core.export.Codegen;
import IdrisJvm.IR.export.ExportIFace;
import IdrisJvm.IR.export.SDecl;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.github.mmhelloworld.idrisjvm.model.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.embedded.EmbeddedServletContainerInitializedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import static com.fasterxml.jackson.core.JsonToken.END_ARRAY;
import static com.fasterxml.jackson.core.JsonToken.END_OBJECT;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ACC_STATIC;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;
import static org.objectweb.asm.Opcodes.PUTSTATIC;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

@RestController
@RequestMapping(path = "/")
public class CodegenController implements ApplicationListener<EmbeddedServletContainerInitializedEvent> {
    private static final Logger LOGGER = LoggerFactory.getLogger(CodegenController.class);

    private final String workingDir;
    private final ObjectMapper mapper;

    public CodegenController(@Value("${working.dir}") String workingDir,
                             ObjectMapper mapper) {
        this.workingDir = workingDir;
        this.mapper = mapper;
    }

    @RequestMapping(method = POST)
    public void compile(@RequestBody String[] args) {
        JsonFactory jsonfactory = new JsonFactory();
        jsonfactory.setCodec(mapper);
        List<String> argsList = stripArgs(asList(args));
        File source = getSourceFile(argsList);
        try {
            JsonParser parser = jsonfactory.createParser(source);
            Assembler assembler = new Assembler();
            codegen(parser, assembler);
            addMainMethod(assembler);
            assembler.classCodeEnd(getOutputFile(argsList).getPath());
            parser.close();
        } catch (Exception e) {
            throw new IdrisCompilationException(e);
        }
    }

    private void requireJsonObject(JsonParser parser) throws IOException {
        if (parser.nextToken() != JsonToken.START_OBJECT) {
            throw new IOException("Expected data to start with an Object");
        }
    }

    private void codegen(JsonParser parser, Assembler assembler) throws IOException {
        requireJsonObject(parser);
        while (parser.nextToken() != END_OBJECT) {
            String fieldName = parser.getCurrentName();
            if (fieldName.equals("codegen-info")) {
                processCodegenInfo(parser, assembler);
            } else {
                parser.nextToken();
                parser.skipChildren();
            }
        }
    }

    private List<String> stripArgs(List<String> args) {
        if (args.get(0).equals("--interface")) {
            return args.subList(1, args.size());
        } else {
            return args;
        }
    }

    private void processCodegenInfo(final JsonParser parser, final Assembler assembler) throws IOException {
        List<SDecl> decls = new ArrayList<>();
        List<ExportIFace> exports = new ArrayList<>();
        parser.nextToken();
        while (parser.nextToken() != END_OBJECT) {
            String codegenInfoFieldName = parser.getCurrentName();
            switch (codegenInfoFieldName) {
                case "simple-decls":
                    decls = parseSimpleDecls(parser);
                    break;
                case "exports":
                    exports = parseExports(parser);
                    break;
                default:
                    parser.nextToken();
                    parser.skipChildren();
                    break;
            }
        }
        Codegen.generateMethods(assembler, Converters.toIdrisListSDecl(decls),
                Converters.toIdrisListExportIFace(exports));
    }

    private List<ExportIFace> parseExports(final JsonParser parser) throws IOException {
        List<ExportIFace> exports = new LinkedList<>();
        parser.nextToken();
        while (parser.nextToken() != END_ARRAY) {
            ExportIFace exportIFace = mapper.readerFor(ExportIFace.class).readValue(parser);
            exports.add(exportIFace);
        }
        return exports;
    }

    private List<SDecl> parseSimpleDecls(final JsonParser parser) throws IOException {
        List<SDecl> decls = new LinkedList<>();
        parser.nextToken();
        while (parser.nextToken() != END_ARRAY) {
            final JsonNode node = parser.getCodec().readTree(parser);
            final ObjectMapper mapper = Context.getMapper();
            if (node.isArray()) {
                SDecl sDecl = mapper.readerFor(SDecl.class).readValue(node.get(1));
                decls.add(sDecl);
            } else {
                throw new RuntimeException("An array representing SimpleDecl expected");
            }
        }
        return decls;
    }

    private void addMainMethod(final Assembler assembler) {
        final String[] classAndMethodName = IdrisToJavaNameConverter.idrisClassMethodName("{runMain_0}").split(",");
        assembler.createMethod(ACC_PUBLIC + ACC_STATIC, "main/Main", "main", "([Ljava/lang/String;)V", null, null,
            emptyList(), emptyList());
        assembler.methodCodeStart();
        assembler.aload(0);
        assembler.invokeMethod(INVOKESTATIC, "java/util/Arrays", "asList", "([Ljava/lang/Object;)Ljava/util/List;", false);
        assembler.field(PUTSTATIC, "io/github/mmhelloworld/idrisjvm/runtime/Runtime", "programArgs", "Ljava/util/List;");
        assembler.invokeMethod(INVOKESTATIC, classAndMethodName[0], classAndMethodName[1], "()Lio/github/mmhelloworld/idrisjvm/runtime/Thunk;", false);
        assembler.pop();
        assembler.asmReturn();
        assembler.maxStackAndLocal(-1, -1);
        assembler.methodCodeEnd();
    }

    private File getSourceFile(final List<String> args) {
        String sourceFileName = args.get(0);
        return getAbsoluteFile(sourceFileName, getCurrentWorkingDirectory(args));
    }

    private File getOutputFile(final List<String> args) {
        String outputFileName = args.get(2);
        return getAbsoluteFile(outputFileName, getCurrentWorkingDirectory(args));
    }

    private String getCurrentWorkingDirectory(final List<String> args) {
        return args.get(args.size() - 1);
    }

    private File getAbsoluteFile(String fileName, String cwd) {
        File absolute = new File(fileName);
        if (absolute.isAbsolute()) {
            return absolute;
        } else {
            return new File(cwd, fileName);
        }
    }

    private void writePort(int port) {
        final File portInfoFile = getInfoFile();
        try (BufferedWriter out = new BufferedWriter(new FileWriter(portInfoFile))) {
            out.append(Integer.toString(port)).append('\n');
            out.flush();
        } catch (IOException e) {
            throw new RuntimeException("Cannot write port: ", e);
        }
    }

    private File getInfoFile() {
        final String workingDirWithDefault = workingDir.isEmpty() ? System.getProperty("user.home") : workingDir;
        return new File(workingDirWithDefault, ".idrisjvmport");
    }

    @Override
    public void onApplicationEvent(final EmbeddedServletContainerInitializedEvent event) {
        int port = event.getEmbeddedServletContainer().getPort();
        writePort(port);
    }
}
