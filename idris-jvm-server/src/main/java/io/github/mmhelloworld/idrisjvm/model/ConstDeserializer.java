package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import idrisjvm.core.JCodegen;
import idrisjvm.ir.ArithTy;

import java.io.IOException;

import static idrisjvm.core.JCodegen.aType;
import static idrisjvm.core.JCodegen.constB16;
import static idrisjvm.core.JCodegen.constB32;
import static idrisjvm.core.JCodegen.constB64;
import static idrisjvm.core.JCodegen.constB8;
import static idrisjvm.core.JCodegen.constBI;
import static idrisjvm.core.JCodegen.constFl;
import static idrisjvm.core.JCodegen.constI;
import static idrisjvm.core.JCodegen.constStr;
import static idrisjvm.core.JCodegen.forgot;
import static idrisjvm.core.JCodegen.strType;
import static idrisjvm.core.JCodegen.theWorld;
import static idrisjvm.core.JCodegen.voidType;
import static idrisjvm.core.JCodegen.worldType;
import static java.lang.String.format;

public class ConstDeserializer extends StdDeserializer<idrisjvm.ir.Const> {

    public static final idrisjvm.ir.Const STR_TYPE = strType();
    public static final idrisjvm.ir.Const WORLD_TYPE = worldType();
    public static final idrisjvm.ir.Const THE_WORLD = theWorld();
    public static final idrisjvm.ir.Const VOID_TYPE = voidType();
    public static final idrisjvm.ir.Const FORGOT = forgot();

    public ConstDeserializer() {
        this(null);
    }

    public ConstDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public idrisjvm.ir.Const deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        final ObjectMapper mapper = Context.getMapper();
        if (node.isObject()) {
            final String constructor = node.fields().next().getKey();
            final JsonNode value = node.fields().next().getValue();

            switch (constructor) {
                case "int":
                    return constI(value.asInt());
                case "bigint":
                    return constBI(value.asText());
                case "double":
                    return constFl(value.asDouble());
                case "char":
                    return deserializeChar(value);
                case "string":
                    return constStr(value.asText());
                case "bits8":
                    return constB8((byte) value.asInt());
                case "bits16":
                    return constB16((short) value.asInt());
                case "bits32":
                    return constB32(value.asInt());
                case "bits64":
                    return constB64(value.asLong());
                case "atype":
                    return aType(mapper.readerFor(ArithTy.class).readValue(value));
                case "strtype":
                    return STR_TYPE;
                case "worldtype":
                    return WORLD_TYPE;
                case "theworld":
                    return THE_WORLD;
                case "voidtype":
                    return VOID_TYPE;
                case "forgot":
                    return FORGOT;
                default:
                    throw new RuntimeException("A Const value expected but was: " +
                        jsonParser.getCurrentName());
            }
        }
        throw new RuntimeException("An object representing Const expected but was " + jsonParser.getCurrentName());
    }

    private idrisjvm.ir.Const deserializeChar(final JsonNode node) {
        final String strValue = node.asText();
        final char value;

        if (!strValue.isEmpty() && strValue.charAt(0) == '\'') {
            value = unescape(strValue);
        } else {
            value = strValue.charAt(0);
        }
        return JCodegen.constCh(value);
    }

    private char unescape(final String strValue) {
        return parseAsciiAbbr(strValue.substring(1, strValue.length() - 1));
    }

    private static char parseAsciiAbbr(final String str) {
        switch (str) {
            case "\\0":
                return 0;
            case "\\a":
                return 7;
            case "\\b":
                return '\b';
            case "\\f":
                return '\f';
            case "\\n":
                return '\n';
            case "\\r":
                return '\r';
            case "\\t":
                return '\t';
            case "\\v":
                return 11;
            case "\\\"":
                return '"';
            case "\\&":
                return 0;
            case "\\'":
                return '\'';
            case "\\\\":
                return '\\';
            case "\\NUL":
                return 0;
            case "\\SOH":
                return 1;
            case "\\STX":
                return 2;
            case "\\ETX":
                return 3;
            case "\\EOT":
                return 4;
            case "\\ENQ":
                return 5;
            case "\\ACK":
                return 6;
            case "\\BEL":
                return 7;
            case "\\BS":
                return '\b';
            case "\\HT":
                return '\t';
            case "\\LF":
                return '\n';
            case "\\VT":
                return 11;
            case "\\FF":
                return 12;
            case "\\CR":
                return '\r';
            case "\\SO":
                return 14;
            case "\\SI":
                return 15;
            case "\\DLE":
                return 16;
            case "\\DC1":
                return 17;
            case "\\DC2":
                return 18;
            case "\\DC3":
                return 19;
            case "\\DC4":
                return 20;
            case "\\NAK":
                return 21;
            case "\\SYN":
                return 22;
            case "\\ETB":
                return 23;
            case "\\CAN":
                return 24;
            case "\\EM":
                return 25;
            case "\\SUB":
                return 26;
            case "\\ESC":
                return 27;
            case "\\FS":
                return 28;
            case "\\GS":
                return 29;
            case "\\RS":
                return 30;
            case "\\US":
                return 31;
            case "\\SP":
                return ' ';
            case "\\DEL":
                return 127;
            default:
                if (str.length() == 1) {
                    return str.charAt(0);
                } else if (str.startsWith("\\")) {
                    return (char) Integer.parseInt(str.substring(1));
                } else {
                    throw new RuntimeException(format("Unable to parse '%s' into a character", str));
                }
        }
    }
}
