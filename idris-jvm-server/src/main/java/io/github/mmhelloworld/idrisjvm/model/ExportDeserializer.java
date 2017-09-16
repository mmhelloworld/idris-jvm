package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import IdrisJvm.IR.export.Export;
import IdrisJvm.IR.export.FDesc;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static IdrisJvm.Core.export.Codegen.exportData;
import static IdrisJvm.Core.export.Codegen.exportFun;
import static io.github.mmhelloworld.idrisjvm.Converters.toIdrisListFDesc;
import static java.util.Arrays.asList;

public class ExportDeserializer extends StdDeserializer<Export> {

    public ExportDeserializer() {
        this(null);
    }

    public ExportDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public Export deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException {

        final ObjectMapper mapper = Context.getMapper();
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if (node.isObject()) {
            final Map.Entry<String, JsonNode> field = node.fields().next();
            final String constructor = field.getKey();
            final JsonNode value = field.getValue();

            switch (constructor) {
                case "ExportData":
                    return exportData(mapper.readerFor(FDesc.class).readValue(value));
                case "ExportFun":
                    return deserializeExportFun(value);
                default:
                    throw new RuntimeException("An Export value expected but found " +
                        jsonParser.getCurrentName());
            }
        }
        throw new RuntimeException("An object representing Export expected but found " + jsonParser.getCurrentName());
    }

    public Export deserializeExportFun(JsonNode exportFun)
        throws IOException {

        final ObjectMapper mapper = Context.getMapper();
        if (exportFun.isArray()) {
            final String name = exportFun.get(0).asText();
            final FDesc fdesc = mapper.readerFor(FDesc.class).readValue(exportFun.get(1));
            final FDesc retDesc = mapper.readerFor(FDesc.class).readValue(exportFun.get(2));
            final List<FDesc> args = asList(mapper.readerFor(FDesc[].class).readValue(exportFun.get(3)));
            return exportFun(name, fdesc, retDesc, toIdrisListFDesc(args));
        } else {
            throw new RuntimeException("An array representing ExportFun expected but found " + exportFun);
        }
    }
}
