package io.github.mmhelloworld.idris2.runtime;

import java.nio.file.DirectoryStream;
import java.nio.file.Path;
import java.util.Iterator;

public final class Directory {
    private final Path directory;
    private final DirectoryStream<Path> stream;
    private final Iterator<Path> iterator;

    public Directory(Path directory, DirectoryStream<Path> stream, Iterator<Path> iterator) {
        this.directory = directory;
        this.stream = stream;
        this.iterator = iterator;
    }

    public Path getDirectory() {
        return directory;
    }

    public DirectoryStream<Path> getStream() {
        return stream;
    }

    public Iterator<Path> getIterator() {
        return iterator;
    }
}
