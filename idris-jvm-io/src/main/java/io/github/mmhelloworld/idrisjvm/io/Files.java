package io.github.mmhelloworld.idrisjvm.io;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static java.lang.System.lineSeparator;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.attribute.PosixFilePermission.GROUP_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.GROUP_READ;
import static java.nio.file.attribute.PosixFilePermission.GROUP_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_READ;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_READ;
import static java.nio.file.attribute.PosixFilePermission.OWNER_WRITE;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toSet;

public final class Files {

    private static final boolean isPosix = FileSystems.getDefault().supportedFileAttributeViews().contains("posix");
    private static final Map<Integer, PosixFilePermission> modeToPermissions = new HashMap<>();
    private static String workingDir = System.getProperty("user.dir");


    static {
        modeToPermissions.put(256, OWNER_READ);
        modeToPermissions.put(128, OWNER_WRITE);
        modeToPermissions.put(64, OWNER_EXECUTE);
        modeToPermissions.put(32, GROUP_READ);
        modeToPermissions.put(16, GROUP_WRITE);
        modeToPermissions.put(8, GROUP_EXECUTE);
        modeToPermissions.put(4, OTHERS_READ);
        modeToPermissions.put(2, OTHERS_WRITE);
        modeToPermissions.put(1, OTHERS_EXECUTE);
    }

    private Files() {

    }

    public static String readFile(String pathString) throws IOException {
        Path path = createPath(pathString);
        return java.nio.file.Files.lines(path)
            .collect(joining(lineSeparator()));
    }

    public static void writeFile(String pathString, String content) throws IOException {
        Path path = createPath(pathString);
        byte[] bytes = content.getBytes(UTF_8);
        java.nio.file.Files.write(path, bytes);
    }

    public static Path createPath(String pathStr) {
        Path path = Paths.get(pathStr);
        if (path.isAbsolute()) {
            return path;
        } else {
            if (pathStr.equals(".")) {
                return Paths.get(workingDir);
            } else if (pathStr.equals("..")) {
                return Paths.get(workingDir).getParent();
            } else {
                return Paths.get(workingDir, pathStr);
            }
        }
    }

    public static void createDirectory(String pathString) throws IOException {
        java.nio.file.Files.createDirectory(createPath(pathString));
    }

    public static void createDirectories(Path dirPath) throws IOException {
        java.nio.file.Files.createDirectories(dirPath);
    }

    public static String getWorkingDir() {
        return workingDir;
    }

    public static boolean changeDir(String dir) {
        workingDir = createPath(dir).toString();
        return true;
    }

    public static String getTemporaryFileName() throws IOException {
        String prefix = "idris";
        Path tempFile = java.nio.file.Files.createTempFile(prefix, null);
        tempFile.toFile().delete();
        return tempFile.toString();
    }

    public static void chmod(String file, int mode) throws IOException {
        if (isPosix) {
            java.nio.file.Files.setPosixFilePermissions(createPath(file), createPosixFilePermissions(mode));
        }
    }


    private static Set<PosixFilePermission> createPosixFilePermissions(int mode) {
        return modeToPermissions.entrySet().stream()
            .filter(modeAndPermission -> (mode & modeAndPermission.getKey()) == modeAndPermission.getKey())
            .map(Map.Entry::getValue)
            .collect(toSet());
    }

}
