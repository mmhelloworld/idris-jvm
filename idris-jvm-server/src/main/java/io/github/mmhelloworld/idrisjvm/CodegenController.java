package io.github.mmhelloworld.idrisjvm;

import IdrisJvm.Core.Assembler;
import IdrisJvm.Core.export.Codegen;
import IdrisJvm.IR.export.ExportIFace;
import IdrisJvm.IR.export.SDecl;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
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
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static com.fasterxml.jackson.core.JsonToken.END_ARRAY;
import static com.fasterxml.jackson.core.JsonToken.END_OBJECT;
import static java.lang.String.format;
import static java.nio.file.StandardCopyOption.COPY_ATTRIBUTES;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.Arrays.asList;
import static java.util.Comparator.reverseOrder;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

@RestController
@RequestMapping(path = "/")
public class CodegenController implements ApplicationListener<EmbeddedServletContainerInitializedEvent> {
    private static final Logger LOGGER = LoggerFactory.getLogger(CodegenController.class);
    private static final String IDRIS_JVM_HOME = Optional.ofNullable(System.getProperty("IDRIS_JVM_HOME"))
        .orElseGet(() -> Optional.ofNullable(System.getenv("IDRIS_JVM_HOME"))
            .orElseGet(() -> System.getProperty("user.home")));

    private final String workingDir;
    private final Template windowsExeTemplate;
    private final ObjectMapper mapper;
    private Template unixExeTemplate;

    public CodegenController(@Value("${working.dir}") String workingDir,
                             Configuration freemarkerConfiguration, ObjectMapper mapper) throws IOException {
        this.workingDir = workingDir;
        this.unixExeTemplate = freemarkerConfiguration.getTemplate("idrisjvm");
        this.windowsExeTemplate = freemarkerConfiguration.getTemplate("idrisjvm.bat");
        this.mapper = mapper;
    }

    @RequestMapping(method = POST)
    public void compile(@RequestBody String[] args) {
        LOGGER.info("Processing with args: " + Arrays.toString(args));
        JsonFactory jsonfactory = new JsonFactory();
        jsonfactory.setCodec(mapper);
        List<String> argsList = stripArgs(asList(args));
        File source = getSourceFile(argsList);
        try {
            JsonParser parser = jsonfactory.createParser(source);
            Assembler assembler = new Assembler();
            codegen(parser, assembler);
            assembler.classCodeEnd(getOutputClassesDirectory(argsList).getPath());
            parser.close();
            generateExecutables(argsList);
            copyRuntimeJar(argsList);
        } catch (Exception e) {
            throw new IdrisCompilationException(e);
        }
    }

    private void copyRuntimeJar(List<String> args) throws IOException {
        String outputDirectory = getOutputDirectory(args);
        File runtimeJar = Paths.get(IDRIS_JVM_HOME, "idris-jvm-runtime.jar").toFile();
        File targetRuntimeJar = Paths.get(outputDirectory, "idris-jvm-runtime.jar").toFile();
        if (!targetRuntimeJar.exists() || targetRuntimeJar.lastModified() < runtimeJar.lastModified()) {
            Files.copy(runtimeJar.toPath(), targetRuntimeJar.toPath(), REPLACE_EXISTING, COPY_ATTRIBUTES);
        }
    }

    private void generateExecutables(List<String> args) throws IOException, TemplateException {
        String out = getOutput(args);
        File outputClassesDirectory = getOutputClassesDirectory(args);
        File exeDir = Optional.ofNullable(outputClassesDirectory.getParentFile())
            .orElse(outputClassesDirectory);
        Map<String, Object> templateModel = new HashMap<>();
        templateModel.put("classesDir", outputClassesDirectory.getName());
        templateModel.put("outputName", out);
        File exeFile = getExeFile(out, exeDir);
        deleteFile(exeFile);
        Template template = out.endsWith(".bat") ? windowsExeTemplate : unixExeTemplate;
        try (FileWriter exeWriter = new FileWriter(exeFile)) {
            template.process(templateModel, exeWriter);
        }
        io.github.mmhelloworld.idrisjvm.io.Files.chmod(exeFile.getPath(), 0777);
    }

    private void deleteFile(File file) throws IOException {
        if (!file.exists()) {
            return;
        }

        if (file.isFile()) {
            file.delete();
        } else {
            deleteDir(file);
        }
    }

    private void deleteDir(File directory) throws IOException {
        Files.walk(directory.toPath())
            .map(Path::toFile)
            .sorted(reverseOrder())
            .forEach(File::delete);
    }

    private File getExeFile(String out, File exeDir) {
        return Paths.get(out).isAbsolute() ? new File(out) : new File(exeDir, out);
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

    private File getSourceFile(final List<String> args) {
        String sourceFileName = args.get(0);
        return getAbsoluteFile(sourceFileName, getCurrentWorkingDirectory(args));
    }

    private File getOutputClassesDirectory(final List<String> args) {
        String outputDirectory = getOutputDirectory(args);
        return getAbsoluteFile(getClassesDirectoryName(args), outputDirectory);
    }

    private String getOutputDirectory(List<String> args) {
        String output = getOutput(args);
        return getAbsoluteFile(output, getCurrentWorkingDirectory(args)).getParent();
    }

    private String getClassesDirectoryName(List<String> args) {
        boolean hasCustomClassesDir = args.stream().anyMatch(arg -> arg.startsWith("--classes-dir="));
        return hasCustomClassesDir ? getCustomClassesDirectory(args) : format("%s-classes", getOutput(args));
    }

    private String getCustomClassesDirectory(List<String> args) {
        String directory = args.stream()
            .filter(arg -> arg.startsWith("--classes-dir="))
            .findAny()
            .orElseThrow(() -> new IllegalArgumentException(format("No classes directory provided: %s", args)));
        return directory.substring("--classes-dir=".length());
    }

    private String getOutput(List<String> args) {
        return args.get(2);
    }

    private String getCurrentWorkingDirectory(final List<String> args) {
        return args.get(args.size() - 1);
    }

    private File getAbsoluteFile(String fileName, String parent) {
        File absolute = new File(fileName);
        return absolute.isAbsolute() ? absolute : new File(parent, fileName);
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
