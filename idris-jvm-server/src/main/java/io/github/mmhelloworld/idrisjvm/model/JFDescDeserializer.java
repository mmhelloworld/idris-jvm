package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import IdrisJvm.IR.export.FDesc;
import IdrisJvm.Core.export.Codegen;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static IdrisJvm.Core.export.Codegen.fcon;
import static IdrisJvm.Core.export.Codegen.fio;
import static IdrisJvm.Core.export.Codegen.fstr;
import static IdrisJvm.Core.export.Codegen.funknown;
import static io.github.mmhelloworld.idrisjvm.Converters.toIdrisListFDesc;
import static java.util.Arrays.asList;

public class JFDescDeserializer extends StdDeserializer<FDesc> {

    public JFDescDeserializer() {
        this(null);
    }

    public JFDescDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public FDesc deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {

        final ObjectMapper mapper = Context.getMapper();
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if (node.isObject()) {
            final Map.Entry<String, JsonNode> field = node.fields().next();
            final String constructor = field.getKey();
            final JsonNode value = field.getValue();

            switch (constructor) {
                case "FCon":
                    return fcon(value.asText());
                case "FStr":
                    return fstr(value.asText());
                case "FIO":
                    return fio(mapper.readerFor(FDesc.class).readValue(value));
                case "FUnknown":
                    return funknown();
                case "FApp":
                    return deserializeJFApp(value);
                default:
                    throw new RuntimeException("An FDesc value expected but found " +
                        jsonParser.getCurrentName());
            }
        } else {
            throw new RuntimeException("An object representing FDesc expected but found " +
                jsonParser.getCurrentName());
        }
    }

    private static FDesc deserializeJFApp(final JsonNode node) throws IOException {
        final String name = node.get(0).asText();
        final List<FDesc> fdescs = asList(Context.getMapper().readerFor(FDesc[].class).readValue(node.get(1)));
        return Codegen.fapp(name, toIdrisListFDesc(fdescs));
    }

}
