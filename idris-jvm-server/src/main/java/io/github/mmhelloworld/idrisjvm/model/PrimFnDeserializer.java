package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import IdrisJvm.IR.export.ArithTy;
import IdrisJvm.IR.export.IntTy;
import IdrisJvm.IR.export.PrimFn;

import java.io.IOException;
import java.util.Map;

import static IdrisJvm.Core.export.Codegen.it16;
import static IdrisJvm.Core.export.Codegen.it32;
import static IdrisJvm.Core.export.Codegen.it64;
import static IdrisJvm.Core.export.Codegen.it8;
import static IdrisJvm.Core.export.Codegen.itBig;
import static IdrisJvm.Core.export.Codegen.itChar;
import static IdrisJvm.Core.export.Codegen.itFixed;
import static IdrisJvm.Core.export.Codegen.itNative;
import static IdrisJvm.Core.export.Codegen.lASHR;
import static IdrisJvm.Core.export.Codegen.lAnd;
import static IdrisJvm.Core.export.Codegen.lBitCast;
import static IdrisJvm.Core.export.Codegen.lChInt;
import static IdrisJvm.Core.export.Codegen.lCompl;
import static IdrisJvm.Core.export.Codegen.lCrash;
import static IdrisJvm.Core.export.Codegen.lEq;
import static IdrisJvm.Core.export.Codegen.lExternal;
import static IdrisJvm.Core.export.Codegen.lFACos;
import static IdrisJvm.Core.export.Codegen.lFASin;
import static IdrisJvm.Core.export.Codegen.lFATan;
import static IdrisJvm.Core.export.Codegen.lFCeil;
import static IdrisJvm.Core.export.Codegen.lFCos;
import static IdrisJvm.Core.export.Codegen.lFExp;
import static IdrisJvm.Core.export.Codegen.lFFloor;
import static IdrisJvm.Core.export.Codegen.lFLog;
import static IdrisJvm.Core.export.Codegen.lFNegate;
import static IdrisJvm.Core.export.Codegen.lFSin;
import static IdrisJvm.Core.export.Codegen.lFSqrt;
import static IdrisJvm.Core.export.Codegen.lFTan;
import static IdrisJvm.Core.export.Codegen.lFloatInt;
import static IdrisJvm.Core.export.Codegen.lFloatStr;
import static IdrisJvm.Core.export.Codegen.lFork;
import static IdrisJvm.Core.export.Codegen.lGe;
import static IdrisJvm.Core.export.Codegen.lGt;
import static IdrisJvm.Core.export.Codegen.lIntCh;
import static IdrisJvm.Core.export.Codegen.lIntFloat;
import static IdrisJvm.Core.export.Codegen.lIntStr;
import static IdrisJvm.Core.export.Codegen.lLSHR;
import static IdrisJvm.Core.export.Codegen.lLe;
import static IdrisJvm.Core.export.Codegen.lLt;
import static IdrisJvm.Core.export.Codegen.lMinus;
import static IdrisJvm.Core.export.Codegen.lNoOp;
import static IdrisJvm.Core.export.Codegen.lOr;
import static IdrisJvm.Core.export.Codegen.lPar;
import static IdrisJvm.Core.export.Codegen.lPlus;
import static IdrisJvm.Core.export.Codegen.lReadStr;
import static IdrisJvm.Core.export.Codegen.lSDiv;
import static IdrisJvm.Core.export.Codegen.lSExt;
import static IdrisJvm.Core.export.Codegen.lSGe;
import static IdrisJvm.Core.export.Codegen.lSGt;
import static IdrisJvm.Core.export.Codegen.lSHL;
import static IdrisJvm.Core.export.Codegen.lSLe;
import static IdrisJvm.Core.export.Codegen.lSLt;
import static IdrisJvm.Core.export.Codegen.lSRem;
import static IdrisJvm.Core.export.Codegen.lStrConcat;
import static IdrisJvm.Core.export.Codegen.lStrCons;
import static IdrisJvm.Core.export.Codegen.lStrEq;
import static IdrisJvm.Core.export.Codegen.lStrFloat;
import static IdrisJvm.Core.export.Codegen.lStrHead;
import static IdrisJvm.Core.export.Codegen.lStrIndex;
import static IdrisJvm.Core.export.Codegen.lStrInt;
import static IdrisJvm.Core.export.Codegen.lStrLen;
import static IdrisJvm.Core.export.Codegen.lStrLt;
import static IdrisJvm.Core.export.Codegen.lStrRev;
import static IdrisJvm.Core.export.Codegen.lStrSubstr;
import static IdrisJvm.Core.export.Codegen.lStrTail;
import static IdrisJvm.Core.export.Codegen.lSystemInfo;
import static IdrisJvm.Core.export.Codegen.lTimes;
import static IdrisJvm.Core.export.Codegen.lTrunc;
import static IdrisJvm.Core.export.Codegen.lUDiv;
import static IdrisJvm.Core.export.Codegen.lURem;
import static IdrisJvm.Core.export.Codegen.lWriteStr;
import static IdrisJvm.Core.export.Codegen.lXOr;
import static IdrisJvm.Core.export.Codegen.lZExt;
import static java.lang.String.format;

public class PrimFnDeserializer extends StdDeserializer<PrimFn> {

    public static final IntTy ITNATIVE = itNative();
    public static final IntTy ITBig = itBig();
    public static final IntTy IT8 = itFixed(it8());
    public static final IntTy IT16 = itFixed(it16());
    public static final IntTy IT32 = itFixed(it32());
    public static final IntTy IT64 = itFixed(it64());
    public static final IntTy ITChar = itChar();
    public static final PrimFn LFExp = lFExp();
    public static final PrimFn LFLog = lFLog();
    public static final PrimFn LFSin = lFSin();
    public static final PrimFn LFCos = lFCos();
    public static final PrimFn LFTan = lFTan();
    public static final PrimFn LFASin = lFASin();
    public static final PrimFn LFACos = lFACos();
    public static final PrimFn LFATan = lFATan();
    public static final PrimFn LFSqrt = lFSqrt();
    public static final PrimFn LFFloor = lFFloor();
    public static final PrimFn LFCeil = lFCeil();
    public static final PrimFn LFNegate = lFNegate();
    public static final PrimFn LStrHead = lStrHead();
    public static final PrimFn LStrTail = lStrTail();
    public static final PrimFn LStrCons = lStrCons();
    public static final PrimFn LStrIndex = lStrIndex();
    public static final PrimFn LStrRev = lStrRev();
    public static final PrimFn LStrSubstr = lStrSubstr();
    public static final PrimFn LReadStr = lReadStr();
    public static final PrimFn LWriteStr = lWriteStr();
    public static final PrimFn LSystemInfo = lSystemInfo();
    public static final PrimFn LFork = lFork();
    public static final PrimFn LPar = lPar();
    public static final PrimFn LCrash = lCrash();
    public static final PrimFn LNoOp = lNoOp();
    public static final PrimFn LStrConcat = lStrConcat();
    public static final PrimFn LStrLt = lStrLt();
    public static final PrimFn LStrEq = lStrEq();
    public static final PrimFn LStrLen = lStrLen();
    public static final PrimFn LFloatStr = lFloatStr();
    public static final PrimFn LStrFloat = lStrFloat();

    public PrimFnDeserializer() {
        this(null);
    }

    public PrimFnDeserializer(final Class<?> vc) {
        super(vc);
    }

    @Override
    public PrimFn deserialize(final JsonParser jsonParser, final DeserializationContext deserializationContext)
        throws IOException, JsonProcessingException {

        final ObjectMapper mapper = Context.getMapper();
        final JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if (node.isObject()) {
            final Map.Entry<String, JsonNode> field = node.fields().next();
            final String constructor = field.getKey();
            final JsonNode value = field.getValue();

            switch (constructor) {
                case "LPlus":
                    return lPlus(parseArithTy(mapper, value));
                case "LMinus":
                    return lMinus(parseArithTy(mapper, value));
                case "LTimes":
                    return lTimes(parseArithTy(mapper, value));
                case "LUDiv":
                    return lUDiv(parseIntTy(value.asText()));
                case "LURem":
                    return lURem(parseIntTy(value.asText()));
                case "LSDiv":
                    return lSDiv(parseArithTy(mapper, value));
                case "LSRem":
                    return lSRem(parseArithTy(mapper, value));
                case "LEq":
                    return lEq(parseArithTy(mapper, value));
                case "LSLt":
                    return lSLt(parseArithTy(mapper, value));
                case "LSLe":
                    return lSLe(parseArithTy(mapper, value));
                case "LSGt":
                    return lSGt(parseArithTy(mapper, value));
                case "LSGe":
                    return lSGe(parseArithTy(mapper, value));
                case "LAnd":
                    return lAnd(parseIntTy(value.asText()));
                case "LOr":
                    return lOr(parseIntTy(value.asText()));
                case "LXOr":
                    return lXOr(parseIntTy(value.asText()));
                case "LCompl":
                    return lCompl(parseIntTy(value.asText()));
                case "LSHL":
                    return lSHL(parseIntTy(value.asText()));
                case "LLSHR":
                    return lLSHR(parseIntTy(value.asText()));
                case "LASHR":
                    return lASHR(parseIntTy(value.asText()));
                case "LLt":
                    return lLt(parseIntTy(value.asText()));
                case "LLe":
                    return lLe(parseIntTy(value.asText()));
                case "LGt":
                    return lGt(parseIntTy(value.asText()));
                case "LGe":
                    return lGe(parseIntTy(value.asText()));
                case "LIntFloat":
                    return lIntFloat(parseIntTy(value.asText()));
                case "LFloatInt":
                    return lFloatInt(parseIntTy(value.asText()));
                case "LIntStr":
                    return lIntStr(parseIntTy(value.asText()));
                case "LStrInt":
                    return lStrInt(parseIntTy(value.asText()));
                case "LChInt":
                    return lChInt(parseIntTy(value.asText()));
                case "LIntCh":
                    return lIntCh(parseIntTy(value.asText()));
                case "LFExp":
                    return LFExp;
                case "LFLog":
                    return LFLog;
                case "LFSin":
                    return LFSin;
                case "LFCos":
                    return LFCos;
                case "LFTan":
                    return LFTan;
                case "LFASin":
                    return LFASin;
                case "LFACos":
                    return LFACos;
                case "LFATan":
                    return LFATan;
                case "LFSqrt":
                    return LFSqrt;
                case "LFFloor":
                    return LFFloor;
                case "LFCeil":
                    return LFCeil;
                case "LFNegate":
                    return LFNegate;
                case "LStrHead":
                    return LStrHead;
                case "LStrTail":
                    return LStrTail;
                case "LStrCons":
                    return LStrCons;
                case "LStrIndex":
                    return LStrIndex;
                case "LStrRev":
                    return LStrRev;
                case "LStrSubstr":
                    return LStrSubstr;
                case "LReadStr":
                    return LReadStr;
                case "LWriteStr":
                    return LWriteStr;
                case "LSystemInfo":
                    return LSystemInfo;
                case "LFork":
                    return LFork;
                case "LPar":
                    return LPar;
                case "LCrash":
                    return LCrash;
                case "LNoOp":
                    return LNoOp;
                case "LStrConcat":
                    return LStrConcat;
                case "LStrLt":
                    return LStrLt;
                case "LStrEq":
                    return LStrEq;
                case "LStrLen":
                    return LStrLen;
                case "LFloatStr":
                    return LFloatStr;
                case "LStrFloat":
                    return LStrFloat;
                case "LSExt":
                    return lSExt(parseIntTy(value.get(0).asText()), parseIntTy(value.get(1).asText()));
                case "LZExt":
                    return lZExt(parseIntTy(value.get(0).asText()), parseIntTy(value.get(1).asText()));
                case "LTrunc":
                    return lTrunc(parseIntTy(value.get(0).asText()), parseIntTy(value.get(1).asText()));
                case "LBitCast":
                    return lBitCast(parseArithTy(mapper, value.get(0)), parseArithTy(mapper, value.get(1)));
                case "LExternal":
                    return lExternal(value.asText());
                default:
                    throw new RuntimeException(format("A PrimFn value expected but found %s, %s",
                        constructor, jsonParser.getCurrentName()));
            }
        } else {
            throw new RuntimeException("An object representing PrimFn expected but found " +
                jsonParser.getCurrentName());
        }
    }

    private ArithTy parseArithTy(final ObjectMapper mapper, final JsonNode value) throws IOException {
        return mapper.readerFor(ArithTy.class).readValue(value);
    }

    public static IntTy parseIntTy(String intTyStr) {
        switch (intTyStr) {
            case "Int":
                return ITNATIVE;
            case "BigInt":
                return ITBig;
            case "B8":
                return IT8;
            case "B16":
                return IT16;
            case "B32":
                return IT32;
            case "B64":
                return IT64;
            case "Char":
                return ITChar;
        }
        throw new IllegalArgumentException("Not an IntTy: " + intTyStr);
    }
}
