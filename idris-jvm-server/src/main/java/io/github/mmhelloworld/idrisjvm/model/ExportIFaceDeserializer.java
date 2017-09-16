package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import IdrisJvm.IR.export.Export;
import IdrisJvm.IR.export.ExportIFace;
import io.github.mmhelloworld.idrisjvm.Converters;

import java.io.IOException;
import java.util.List;

import static IdrisJvm.Core.export.Codegen.mkExportIFace;
import static java.util.Arrays.asList;

public class ExportIFaceDeserializer extends StdDeserializer<IdrisJvm.IR.export.ExportIFace> {

    public ExportIFaceDeserializer() {
        this(null);
    }

    public ExportIFaceDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public ExportIFace deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        final ObjectMapper mapper = Context.getMapper();

        final String ffiDesc = node.get("ffi-desc").asText();
        final String interfaceFile = node.get("interface-file").asText();
        final List<Export> exports = asList(mapper.readerFor(Export[].class).readValue(node.get("exports")));
        return mkExportIFace(ffiDesc, interfaceFile, Converters.toIdrisListExport(exports));
    }
}
