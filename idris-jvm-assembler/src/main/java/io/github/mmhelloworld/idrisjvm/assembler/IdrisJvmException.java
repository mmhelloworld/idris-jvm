package io.github.mmhelloworld.idrisjvm.assembler;

public class IdrisJvmException extends RuntimeException {
    public IdrisJvmException(String message) {
        super(message);
    }

    public IdrisJvmException(String message, Throwable cause) {
        super(message, cause);
    }

    public IdrisJvmException(Throwable cause) {
        super(cause);
    }
}
