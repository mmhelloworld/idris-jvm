package io.github.mmhelloworld.idris2.runtime;

import java.nio.file.Path;

public final class Paths {
    private Paths() {
    }

    public static Path createPath(String pathStr) {
        return createPath(pathStr, Directories.workingDir);
    }

    private static Path createPath(String pathStr, String workingDir) {
        if (pathStr.isEmpty()) {
            return java.nio.file.Paths.get(workingDir);
        }

        Path path = java.nio.file.Paths.get(pathStr);
        final Path resolvedPath;
        if (path.isAbsolute()) {
            resolvedPath = path;
        } else {
            if (pathStr.startsWith("../")) {
                resolvedPath = createPath(pathStr.substring("../".length()),
                    java.nio.file.Paths.get(workingDir).getParent().toString());
            } else if (pathStr.startsWith("..")) {
                resolvedPath = createPath(pathStr.substring("..".length()),
                    java.nio.file.Paths.get(workingDir).getParent().toString());
            } else if (pathStr.startsWith("./")) {
                resolvedPath = createPath(pathStr.substring("./".length()), workingDir);
            } else if (pathStr.startsWith(".")) {
                resolvedPath = createPath(pathStr.substring(".".length()), workingDir);
            } else {
                resolvedPath = java.nio.file.Paths.get(workingDir, pathStr);
            }
        }
        return resolvedPath;
    }
}
