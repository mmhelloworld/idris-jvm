package io.github.mmhelloworld.idris2.runtime;

public interface IdrisFile<T extends IdrisFile<T>> {
    void close();
    int getErrorNumber();
    String readLine();
    String readChars(int numberOfCharacters);
    char readChar();
    int writeLine(String line);
    int isEof();
    int flush();
    int size();
    int getAccessTime();
    int getModifiedTime();
    int getStatusTime();
}
