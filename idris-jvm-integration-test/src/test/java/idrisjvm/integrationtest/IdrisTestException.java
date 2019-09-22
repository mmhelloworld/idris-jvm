package idrisjvm.integrationtest;

public class IdrisTestException extends RuntimeException {
    public IdrisTestException(String s) {
        super(s);
    }

    public IdrisTestException(String s, Throwable throwable) {
        super(s, throwable);
    }

    public IdrisTestException(Throwable throwable) {
        super(throwable);
    }
}
