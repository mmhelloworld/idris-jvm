package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import IdrisJvm.IR.export.CaseType;
import IdrisJvm.IR.export.Const;
import IdrisJvm.IR.export.FDesc;
import IdrisJvm.Core.export.Codegen;
import IdrisJvm.IR.export.LVar;
import IdrisJvm.IR.export.PrimFn;
import IdrisJvm.IR.export.SAlt;
import IdrisJvm.IR.export.SExp;
import IdrisJvm.IR.export.SForeignArg;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static IdrisJvm.Core.export.Codegen.mkSForeignArg;
import static IdrisJvm.Core.export.Codegen.sCase;
import static IdrisJvm.Core.export.Codegen.sChkCase;
import static IdrisJvm.Core.export.Codegen.sCon;
import static IdrisJvm.Core.export.Codegen.sConst;
import static IdrisJvm.Core.export.Codegen.sError;
import static IdrisJvm.Core.export.Codegen.sForeign;
import static IdrisJvm.Core.export.Codegen.sNothing;
import static IdrisJvm.Core.export.Codegen.sV;
import static IdrisJvm.Core.export.Codegen.shared;
import static IdrisJvm.Core.export.Codegen.updatable;
import static io.github.mmhelloworld.idrisjvm.Converters.nullableToMaybeLVar;
import static io.github.mmhelloworld.idrisjvm.Converters.toIdrisListLVar;
import static io.github.mmhelloworld.idrisjvm.Converters.toIdrisListSAlt;
import static io.github.mmhelloworld.idrisjvm.Converters.toIdrisListSForeignArgs;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.StreamSupport.stream;

public class SExpDeserializer extends StdDeserializer<SExp> {

    public static final SExp S_NOTHING = sNothing();

    public SExpDeserializer() {
        this(null);
    }

    public SExpDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public SExp deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {

        final ObjectMapper mapper = Context.getMapper();
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if (node.isObject()) {
            final Map.Entry<String, JsonNode> field = node.fields().next();
            final String constructor = field.getKey();
            final JsonNode value = field.getValue();

            switch (constructor) {
                case "SV":
                    return sV(mapper.readerFor(LVar.class).readValue(value));
                case "SApp":
                    return deserializeSApp(value);
                case "SLet":
                    return deserializeSLet(value);
                case "SUpdate":
                    return deserializeSUpdate(value);
                case "SCon":
                    return deserializeSCon(value);
                case "SCase":
                    return deserializeSCase(value);
                case "SChkCase":
                case "DChkCase": // This is needed due to a bug in the current idris version
                    return deserializeSChkCase(value);
                case "SProj":
                case "DProj": // This is needed due to a bug in the current idris version
                    return deserializeSProj(value);
                case "SConst":
                    return deserializeSConst(value);
                case "SForeign":
                    return deserializeSForeign(value);
                case "SOp":
                case "DOp": // This is needed due to a bug in the current idris version
                    return deserializeSOp(value);
                case "SNothing":
                    return S_NOTHING;
                case "SError":
                    return sError(value.asText());
                default:
                    throw new RuntimeException("An SExp value expected but found " + jsonParser.getCurrentName());
            }
        }
        throw new RuntimeException("An object representing SExp expected but found " + jsonParser.getCurrentName());
    }

    private SExp deserializeSOp(final JsonNode node) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (node.isArray()) {
            final PrimFn primFn = mapper.readerFor(PrimFn.class).readValue(node.get(0));
            final List<LVar> lvars = asList(mapper.readerFor(LVar[].class).readValue(node.get(1)));
            return Codegen.sOp(primFn, toIdrisListLVar(lvars));
        } else {
            throw new RuntimeException("An array representing SOp expected but found " + node);
        }
    }

    private SExp deserializeSForeign(final JsonNode node) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (node.isArray()) {
            final FDesc fd = mapper.readerFor(FDesc.class).readValue(node.get(0));
            final FDesc ret = mapper.readerFor(FDesc.class).readValue(node.get(1));
            final JsonNode args = node.get(2);
            final List<SForeignArg> exprs = stream(args.spliterator(), false)
                .map(arg -> {
                    try {
                        final FDesc expDesc = mapper.readerFor(FDesc.class).readValue(arg.get(0));
                        final LVar expVar = mapper.readerFor(LVar.class).readValue(arg.get(1));
                        return mkSForeignArg(expDesc, expVar);
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }).collect(toList());
            return sForeign(fd, ret, toIdrisListSForeignArgs(exprs));
        } else {
            throw new RuntimeException("An array representing SForeign expected but found " + node);
        }
    }

    private SExp deserializeSConst(final JsonNode node) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (node.isObject()) {
            final Const constValue = mapper.readerFor(Const.class).readValue(node);
            return sConst(constValue);
        } else {
            throw new RuntimeException("An object representing SConst expected but found " + node);
        }
    }

    private SExp deserializeSProj(final JsonNode sProj) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (sProj.isArray()) {
            final LVar lvar = mapper.readerFor(LVar.class).readValue(sProj.get(0));
            final int n = sProj.get(1).asInt();
            return Codegen.sProj(lvar, n);
        } else {
            throw new RuntimeException("An object representing SProj expected but found " + sProj);
        }
    }

    private SExp deserializeSChkCase(final JsonNode sChkCase) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (sChkCase.isArray()) {
            final LVar lvar = mapper.readerFor(LVar.class).readValue(sChkCase.get(0));
            final List<SAlt> salts = asList(mapper.readerFor(SAlt[].class).readValue(sChkCase.get(1)));
            return sChkCase(lvar, toIdrisListSAlt(salts));
        } else {
            throw new RuntimeException("An object representing SCon expected but found " + sChkCase);
        }
    }

    private SExp deserializeSCase(final JsonNode scase) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (scase.isArray()) {
            final CaseType caseType = deserializeCaseType(scase.get(0));
            final LVar lvar = mapper.readerFor(LVar.class).readValue(scase.get(1));
            final List<SAlt> salts = asList(mapper.readerFor(SAlt[].class).readValue(scase.get(2)));
            return sCase(caseType, lvar, toIdrisListSAlt(salts));
        } else {
            throw new RuntimeException("An object representing SCon expected but found " + scase);
        }
    }

    private CaseType deserializeCaseType(final JsonNode node) {
        final String caseTypeString = node.asText();
        switch (caseTypeString) {
            case "Updatable":
                return updatable();
            case "Shared":
                return shared();
            default:
                throw new IllegalArgumentException("Invalid CaseType " + node);
        }
    }

    private SExp deserializeSCon(final JsonNode scon) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (scon.isArray()) {
            final LVar lvar = mapper.readerFor(LVar.class).readValue(scon.get(0));
            final int n = scon.get(1).asInt();
            final String name = scon.get(2).asText();
            final List<LVar> lVars = asList(mapper.readerFor(LVar[].class).readValue(scon.get(3)));
            return sCon(nullableToMaybeLVar(lvar), n, name, toIdrisListLVar(lVars));
        } else {
            throw new RuntimeException("An object representing SCon expected but found " + scon);
        }
    }

    private SExp deserializeSUpdate(final JsonNode node) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (node.isArray()) {
            final LVar lvar = mapper.readerFor(LVar.class).readValue(node.get(0));
            final SExp sexp = mapper.readerFor(SExp.class).readValue(node.get(1));
            return Codegen.sUpdate(lvar, sexp);
        } else {
            throw new RuntimeException("An object representing SUpdate expected but found " + node);
        }
    }

    private SExp deserializeSLet(final JsonNode slet) throws IOException {
        final ObjectMapper mapper = Context.getMapper();

        if (slet.isArray()) {
            final LVar lvar = mapper.readerFor(LVar.class).readValue(slet.get(0));
            final SExp lsexp = mapper.readerFor(SExp.class).readValue(slet.get(1));
            final SExp rsexp = mapper.readerFor(SExp.class).readValue(slet.get(2));
            return Codegen.sLet(lvar, lsexp, rsexp);
        } else {
            throw new RuntimeException("An object representing SLet expected but found " + slet);
        }
    }

    private SExp deserializeSApp(final JsonNode sapp) throws IOException {
        final ObjectMapper mapper = Context.getMapper();
        if (sapp.isArray()) {
            final boolean isTailRecursive = sapp.get(0).asBoolean();
            final String functionName = sapp.get(1).asText();
            final List<LVar> lvars = asList(mapper.readerFor(LVar[].class).readValue(sapp.get(2)));
            return Codegen.sApp(isTailRecursive, functionName, toIdrisListLVar(lvars));
        } else {
            throw new RuntimeException("An object representing SApp expected but found " + sapp);
        }
    }

}
