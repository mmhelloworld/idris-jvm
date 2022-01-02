package io.github.mmhelloworld.idrisjvm.runtime;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;

import static java.nio.file.Files.newDirectoryStream;

public final class Directories {
    static String workingDir = System.getProperty("user.dir");

    private Directories() {
    }

    public static int createDirectory(String pathString) {
        try {
            java.nio.file.Files.createDirectory(Paths.createPath(pathString));
            return 0;
        } catch (Exception exception) {
            handleException(exception);
            return -1;
        }
    }

    public static Object getWorkingDirectory() {
        return workingDir;
    }

    public static int changeDirectory(String dir) {
        Path path = Paths.createPath(dir);
        if (!Files.exists(path)) {
            Runtime.setErrorNumber(2);
            return -1;
        } else {
            workingDir = path.toString();
            return 0;
        }
    }

    public static String getTemporaryFileName() throws IOException {
        String prefix = "idris";
        Path tempFile = java.nio.file.Files.createTempFile(prefix, null);
        tempFile.toFile().delete();
        return tempFile.toString();
    }

    public static boolean deleteIfExists(Path path) {
        return path.toFile().delete();
    }

    public static void delete(String pathString) {
        Runtime.setErrorNumber(0);
        try {
            Files.delete(Paths.createPath(pathString));
        } catch (IOException exception) {
            handleException(exception);
        }
    }

    public static Object openDirectory(String name) {
        Runtime.setErrorNumber(0);
        Path path = Paths.createPath(name);
        try {
            DirectoryStream<Path> stream = newDirectoryStream(path);
            return new Directory(path, stream, stream.iterator());
        } catch (Exception exception) {
            handleException(exception);
            return null;
        }
    }

    public static void closeDirectory(Object directory) {
        Runtime.setErrorNumber(0);
        try {
            if (directory != null) {
                ((Directory) directory).getStream().close();
            }
        } catch (IOException exception) {
            handleException(exception);
        }
    }

    private static void handleException(Exception exception) {
        Runtime.setException(exception);
        Runtime.setErrorNumber(ChannelIo.getErrorNumber(exception));
    }

    public static Object getNextDirectoryEntry(Object directory) {
        Runtime.setErrorNumber(0);
        Iterator<Path> iterator = ((Directory) directory).getIterator();
        if (iterator.hasNext()) {
            return iterator.next().getFileName().toString();
        } else {
            return null;
        }
    }
}
