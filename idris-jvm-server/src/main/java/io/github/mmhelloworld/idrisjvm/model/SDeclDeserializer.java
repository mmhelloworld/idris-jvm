package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import idrisjvm.ir.SDecl;
import idrisjvm.ir.SExp;
import io.github.mmhelloworld.idrisjvm.Converters;

import java.io.IOException;
import java.util.List;

import static idrisjvm.core.JCodegen.sFun;
import static java.util.Arrays.asList;

public class SDeclDeserializer extends StdDeserializer<SDecl> {

    public SDeclDeserializer() {
        this(null);
    }

    public SDeclDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public SDecl deserialize(final JsonParser jsonParser,
                             final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        final ObjectMapper mapper = Context.getMapper();

        if (node.isObject()) {
            final JsonNode sfun = node.fields().next().getValue();
            final String name = sfun.get(0).asText();
            final List<String> args = asList(mapper.readerFor(String[].class).readValue(sfun.get(1)));
            final int n = sfun.get(2).asInt();
            final SExp sexp = mapper.readerFor(SExp.class).readValue(sfun.get(3));
            return sFun(name, Converters.toIdrisListString(args), n, sexp);
        } else {
            throw new RuntimeException("An object representing SFun expected");
        }
    }
}
