package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import idrisjvm.ir.Const;
import idrisjvm.core.JCodegen;
import idrisjvm.ir.SAlt;
import idrisjvm.ir.SExp;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static idrisjvm.core.JCodegen.sDefaultCase;
import static io.github.mmhelloworld.idrisjvm.Converters.toIdrisListString;
import static java.util.Arrays.asList;

public class SAltDeserializer extends StdDeserializer<SAlt> {

    public SAltDeserializer() {
        this(null);
    }

    public SAltDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public SAlt deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        final ObjectMapper mapper = Context.getMapper();

        if (node.isObject()) {
            final Map.Entry<String, JsonNode> field = node.fields().next();
            final String constructor = field.getKey();
            final JsonNode value = field.getValue();

            switch (constructor) {
                case "SConCase":
                    return deserializeSConCase(value);
                case "SConstCase":
                    return deserializeSConstCase(value);
                case "SDefaultCase":
                    return sDefaultCase(mapper.readerFor(SExp.class).readValue(value));
                default:
                    throw new RuntimeException("A subclass of SAlt expected but found " + jsonParser.getCurrentName());
            }
        } else {
            throw new RuntimeException("An object representing SAlt expected but found " +
                jsonParser.getCurrentName());
        }
    }

    private SAlt deserializeSConstCase(final JsonNode node) throws IOException {
        final ObjectMapper mapper = Context.getMapper();

        if (node.isArray()) {
            final Const con = mapper.readerFor(Const.class).readValue(node.get(0));
            final SExp exp = mapper.readerFor(SExp.class).readValue(node.get(1));
            return JCodegen.sConstCase(con, exp);
        } else {
            throw new RuntimeException("An array representing SConstCase expected but found " + node);
        }
    }

    private SAlt deserializeSConCase(final JsonNode node) throws IOException {
        final ObjectMapper mapper = Context.getMapper();

        if (node.isArray()) {
            final int n1 = node.get(0).asInt();
            final int n2 = node.get(1).asInt();
            final String name = node.get(2).asText();
            final List<String> names = asList(mapper.readerFor(String[].class).readValue(node.get(3)));
            final SExp exp = mapper.readerFor(SExp.class).readValue(node.get(4));
            return JCodegen.sConCase(n1, n2, name, toIdrisListString(names), exp);
        } else {
            throw new RuntimeException("An array representing SConCase expected but found " +
                node);
        }
    }
}
