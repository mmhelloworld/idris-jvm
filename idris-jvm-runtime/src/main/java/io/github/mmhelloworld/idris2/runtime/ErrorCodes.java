package io.github.mmhelloworld.idris2.runtime;

public final class ErrorCodes {
    public static final int SUCCESS = 0;
    public static final int NO_SUCH_FILE = 2;
    public static final int IO_ERROR = 5;
    public static final int SOCKET_ACCESS_DENIED = 13;
    public static final int INTERRUPTED = 4;
    public static final int UNSUPPORTED_SOCKET_TYPE = 44;
    public static final int CANNOT_ASSIGN_REQUESTED_ADDRESS = 49;
    public static final int NO_ROUTE_TO_HOST = 65;
    public static final int PROTOCOL_ERROR = 86;

    private ErrorCodes() {
    }
}
