package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.stereotype.Component;

@Component
public class Context {
    private static ObjectMapper mapper;

    public Context(ObjectMapper mapper) {
        Context.mapper = mapper;
    }

    public static ObjectMapper getMapper() {
        return mapper;
    }
}
