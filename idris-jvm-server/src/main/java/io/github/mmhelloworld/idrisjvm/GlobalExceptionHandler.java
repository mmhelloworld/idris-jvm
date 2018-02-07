package io.github.mmhelloworld.idrisjvm;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import static org.springframework.http.HttpStatus.BAD_REQUEST;

@ControllerAdvice
public class GlobalExceptionHandler {

    private static final Logger logger = LoggerFactory.getLogger(GlobalExceptionHandler.class);

    @ExceptionHandler(IdrisCompilationException.class)
    public ResponseEntity<String> compilationFailure(IdrisCompilationException exception) {
        logger.error("Compilation error", exception);
        return new ResponseEntity<>(getMessage(exception), BAD_REQUEST);
    }

    private String getMessage(Throwable throwable) {
        return throwable.getCause() == null ? throwable.getMessage() : getMessage(throwable.getCause());
    }
}