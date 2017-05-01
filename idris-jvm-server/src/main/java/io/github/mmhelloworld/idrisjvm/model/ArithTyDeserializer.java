package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import idrisjvm.ir.ArithTy;

import java.io.IOException;
import java.util.Map;

import static idrisjvm.core.JCodegen.atFloat;
import static idrisjvm.core.JCodegen.atInt;
import static io.github.mmhelloworld.idrisjvm.model.PrimFnDeserializer.parseIntTy;

public class ArithTyDeserializer extends StdDeserializer<ArithTy> {

    public static final ArithTy ATFloat = atFloat();

    public ArithTyDeserializer() {
        this(null);
    }

    public ArithTyDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public ArithTy deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {

        final ObjectMapper mapper = Context.getMapper();
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if (node.isObject()) {
            final Map.Entry<String, JsonNode> field = node.fields().next();
            final String constructor = field.getKey();
            final JsonNode value = field.getValue();

            switch (constructor) {
                case "ATInt":
                    return atInt(parseIntTy(value.asText()));
                case "ATFloat":
                    return ATFloat;
                default:
                    throw new RuntimeException("An ArithTy value expected but found " +
                        jsonParser.getCurrentName());
            }
        } else {
            throw new RuntimeException("An object representing ArithTy expected but found " +
                jsonParser.getCurrentName());
        }
    }
}
