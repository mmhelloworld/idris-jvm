package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import IdrisJvm.IR.export.LVar;

import java.io.IOException;
import java.util.Map;

import static IdrisJvm.Core.export.Codegen.glob;
import static IdrisJvm.Core.export.Codegen.loc;

public class LVarDeserializer extends StdDeserializer<LVar> {

    public LVarDeserializer() {
        this(null);
    }

    public LVarDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public LVar deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {

        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if (node.isObject()) {
            final Map.Entry<String, JsonNode> field = node.fields().next();
            final String constructor = field.getKey();
            final JsonNode value = field.getValue();
            switch (constructor) {
                case "Loc":
                    int locVar = value.asInt();
                    return loc(locVar);
                case "Glob":
                    String glob = value.asText();
                    return glob(glob);
                default:
                    throw new RuntimeException("A Loc or Glob expected but was " + jsonParser.getCurrentName());
            }
        } else {
            throw new RuntimeException("An object representing LVar expected but was " +
                jsonParser.getCurrentName());
        }
    }
}
