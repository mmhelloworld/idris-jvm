package io.github.mmhelloworld.idrisjvm.runtime;

import java.nio.file.Path;

public final class Paths {
    private Paths() {
    }

    public static Path createPath(String pathStr) {
        return createPath(pathStr, Directories.getWorkingDir());
    }

    private static Path createPath(String pathStr, String workingDir) {

        Path cwd = java.nio.file.Paths.get(workingDir);

        if (pathStr == null || pathStr.isEmpty()) {
            return cwd;
        }

        Path path = java.nio.file.Paths.get(pathStr);
        return cwd.resolve(path).normalize();
    }
}
