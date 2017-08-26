package io.github.mmhelloworld.idrisjvm;

public class IdrisCompilationException extends RuntimeException {
    public IdrisCompilationException(Exception e) {
        super(e);
    }
}