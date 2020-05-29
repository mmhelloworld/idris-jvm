package io.github.mmhelloworld.idrisjvm.io;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.nio.file.Files.newDirectoryStream;
import static java.nio.file.Files.readAllBytes;
import static java.nio.file.Files.write;
import static java.nio.file.attribute.PosixFilePermission.GROUP_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.GROUP_READ;
import static java.nio.file.attribute.PosixFilePermission.GROUP_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_READ;
import static java.nio.file.attribute.PosixFilePermission.OTHERS_WRITE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE;
import static java.nio.file.attribute.PosixFilePermission.OWNER_READ;
import static java.nio.file.attribute.PosixFilePermission.OWNER_WRITE;
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
        return new String(readAllBytes(path), UTF_8);
    }

    public static void writeFile(String pathString, String content) throws IOException {
        Path path = createPath(pathString);
        byte[] bytes = content.getBytes(UTF_8);
        createDirectories(path.getParent());
        write(path, bytes);
    }

    public static Path createPath(String pathStr) {
        return createPath(pathStr, workingDir);
    }

    private static Path createPath(String pathStr, String workingDir) {
        if (pathStr.isEmpty()) {
            return Paths.get(workingDir);
        }

        Path path = Paths.get(pathStr);
        final Path resolvedPath;
        if (path.isAbsolute()) {
            resolvedPath = path;
        } else {
            if (pathStr.startsWith("../")) {
                resolvedPath = createPath(pathStr.substring("../".length()),
                    Paths.get(workingDir).getParent().toString());
            } else if (pathStr.startsWith("..")) {
                resolvedPath = createPath(pathStr.substring("..".length()),
                    Paths.get(workingDir).getParent().toString());
            } else if (pathStr.startsWith("./")) {
                resolvedPath = createPath(pathStr.substring("./".length()), workingDir);
            } else if (pathStr.startsWith(".")) {
                resolvedPath = createPath(pathStr.substring(".".length()), workingDir);
            } else {
                resolvedPath = Paths.get(workingDir, pathStr);
            }
        }
        return resolvedPath;
    }

    public static void createDirectory(String pathString) throws IOException {
        java.nio.file.Files.createDirectory(createPath(pathString));
    }

    public static void createDirectories(Path dirPath) throws IOException {
        if (dirPath != null) {
            java.nio.file.Files.createDirectories(dirPath);
        }
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

    public static boolean deleteIfExists(Path path) {
        return path.toFile().delete();
    }

    public static Directory openDirectory(String name) throws IOException {
        Path path = Paths.get(name);
        DirectoryStream<Path> stream = newDirectoryStream(path);
        return new Directory(path, stream, stream.iterator());
    }

    public static void closeDirectory(Directory directory) throws IOException {
        directory.getStream().close();
    }

    public static String getNextDirectoryEntry(Directory directory) {
        return directory.getIterator().next().toString();
    }

    private static Set<PosixFilePermission> createPosixFilePermissions(int mode) {
        return modeToPermissions.entrySet().stream()
            .filter(modeAndPermission -> (mode & modeAndPermission.getKey()) == modeAndPermission.getKey())
            .map(Entry::getValue)
            .collect(toSet());
    }
}
