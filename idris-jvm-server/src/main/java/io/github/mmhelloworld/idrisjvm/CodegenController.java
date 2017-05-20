package io.github.mmhelloworld.idrisjvm;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import idrisjvm.core.Assembler;
import idrisjvm.core.JCodegen;
import idrisjvm.ir.ExportIFace;
import idrisjvm.ir.SDecl;
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

import static java.util.Collections.emptyList;
import static org.objectweb.asm.Opcodes.ACC_PUBLIC;
import static org.objectweb.asm.Opcodes.ACC_STATIC;
import static org.objectweb.asm.Opcodes.INVOKESTATIC;
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
    public void compile(@RequestBody String[] args) throws IOException {
        JsonFactory jsonfactory = new JsonFactory();
        jsonfactory.setCodec(mapper);
        File source = getSourceFile(args);
        JsonParser parser = jsonfactory.createParser(source);
        Assembler assembler = new Assembler();
        if (parser.nextToken() != JsonToken.START_OBJECT) {
            throw new IOException("Expected data to start with an Object");
        }

        while (parser.nextToken() != JsonToken.END_OBJECT) {
            String fieldName = parser.getCurrentName();
            if (fieldName.equals("codegen-info")) {
                parser.nextToken();
                while (parser.nextToken() != JsonToken.END_OBJECT) {
                    String codegenInfoFieldName = parser.getCurrentName();
                    switch (codegenInfoFieldName) {
                        case "simple-decls":
                            parser.nextToken();
                            while (parser.nextToken() != JsonToken.END_ARRAY) {
                                final JsonNode node = parser.getCodec().readTree(parser);
                                final ObjectMapper mapper = Context.getMapper();
                                if (node.isArray()) {
                                    final String name = node.get(0).asText();
                                    SDecl sDecl = mapper.readerFor(SDecl.class).readValue(node.get(1));
                                    JCodegen.generateMethod(assembler, name, sDecl);
                                } else {
                                    throw new RuntimeException("An array representing SimpleDecl expected");
                                }
                            }
                            break;
                        case "exports":
                            parser.nextToken();
                            while (parser.nextToken() != JsonToken.END_ARRAY) {
                                ExportIFace exportIFace = mapper.readerFor(ExportIFace.class).readValue(parser);
                                JCodegen.generateExport(assembler, exportIFace);
                            }
                            break;
                        default:
                            parser.nextToken();
                            parser.skipChildren();
                            break;
                    }
                }
            } else {
                parser.nextToken();
                parser.skipChildren();
            }
        }
        addMainMethod(assembler);
        assembler.classCodeEnd(getOutputFile(args).getPath());
        parser.close();
    }

    private void addMainMethod(final Assembler assembler) {
        assembler.createMethod(ACC_PUBLIC + ACC_STATIC, "main/Main", "main", "([Ljava/lang/String;)V", null, null,
            emptyList(), emptyList());
        assembler.methodCodeStart();
        assembler.invokeMethod(INVOKESTATIC, "main/Main", "_lbrace_runMain_0_rbrace_", "()Ljava/lang/Object;", false);
        assembler.pop();
        assembler.asmReturn();
        assembler.maxStackAndLocal(-1, -1);
        assembler.methodCodeEnd();
    }

    private File getSourceFile(final String[] args) {
        return getAbsoluteFile(args[0], getCurrentWorkingDirectory(args));
    }

    private File getOutputFile(final String[] args) {
        return getAbsoluteFile(args[2], getCurrentWorkingDirectory(args));
    }

    private String getCurrentWorkingDirectory(final String[] args) {
        return args[args.length - 1];
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
