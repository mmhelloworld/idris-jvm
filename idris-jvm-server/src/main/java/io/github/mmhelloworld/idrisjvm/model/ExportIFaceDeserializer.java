package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import idrisjvm.ir.Export;
import idrisjvm.ir.ExportIFace;
import io.github.mmhelloworld.idrisjvm.Converters;

import java.io.IOException;
import java.util.List;

import static idrisjvm.core.JCodegen.mkExportIFace;
import static java.util.Arrays.asList;

public class ExportIFaceDeserializer extends StdDeserializer<idrisjvm.ir.ExportIFace> {

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
