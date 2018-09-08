package io.github.mmhelloworld.idrisjvm.runtime;

public class IdrisRuntimeException extends RuntimeException {
    public IdrisRuntimeException() {
    }

    public IdrisRuntimeException(String message) {
        super(message);
    }

    public IdrisRuntimeException(String message, Throwable cause) {
        super(message, cause);
    }

    public IdrisRuntimeException(Throwable cause) {
        super(cause);
    }
}
