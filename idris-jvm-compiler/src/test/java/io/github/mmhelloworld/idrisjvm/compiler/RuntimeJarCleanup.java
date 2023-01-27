package io.github.mmhelloworld.idrisjvm.compiler;

import org.apache.maven.artifact.versioning.ComparableVersion;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

import static java.util.Comparator.comparing;

/*
 * This is a hack to remove bootstrap compiler runtime JAR under build/exec/idris2_app. Idris JVM compiler copies
 * runtime JAR into application output directories so that we don't have to setup the JAR explicitly in the classpath to
 * run programs but for compiler code itself, we want to use the runtime JAR from the current version, not of the
 * compiler that compiles the current code. This program compares the semantic versions of runtime JARs and removes
 * old JAR. This is specifically inside "test" directory so that dependencies for this class like
 * "maven-artifact" dependency wouldn't be copied into "build/exec/idris2_app" directory.
 */
public final class RuntimeJarCleanup {
    private RuntimeJarCleanup() {
    }

    public static void main(String[] args) {
        String directory = args[0];
        getRuntimeJars(directory)
            .max(comparing(RuntimeJarCleanup::getVersion))
            .ifPresent(requiredJarPath -> getRuntimeJars(directory)
                .filter(path -> !path.equals(requiredJarPath))
                .forEach(oldJarPath -> oldJarPath.toFile().delete()));
    }

    private static Stream<Path> getRuntimeJars(String directory) {
        try {
            return Files.find(Paths.get(directory), 1,
                (path, attributes) -> path.toFile().getName().startsWith("idris-jvm-runtime-"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static ComparableVersion getVersion(Path path) {
        String name = path.toFile().getName();
        String version = name.substring("idris-jvm-runtime-".length(), name.length() - ".jar".length());
        return new ComparableVersion(version);
    }
}
