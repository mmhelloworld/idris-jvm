package io.github.mmhelloworld.idrisjvm.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import idrisjvm.ir.ArithTy;
import idrisjvm.ir.IntTy;
import idrisjvm.ir.PrimFn;

import java.io.IOException;
import java.util.Map;

import static idrisjvm.core.JCodegen.it16;
import static idrisjvm.core.JCodegen.it32;
import static idrisjvm.core.JCodegen.it64;
import static idrisjvm.core.JCodegen.it8;
import static idrisjvm.core.JCodegen.itBig;
import static idrisjvm.core.JCodegen.itChar;
import static idrisjvm.core.JCodegen.itFixed;
import static idrisjvm.core.JCodegen.itNative;
import static idrisjvm.core.JCodegen.lASHR;
import static idrisjvm.core.JCodegen.lAnd;
import static idrisjvm.core.JCodegen.lBitCast;
import static idrisjvm.core.JCodegen.lChInt;
import static idrisjvm.core.JCodegen.lCompl;
import static idrisjvm.core.JCodegen.lCrash;
import static idrisjvm.core.JCodegen.lEq;
import static idrisjvm.core.JCodegen.lExternal;
import static idrisjvm.core.JCodegen.lFACos;
import static idrisjvm.core.JCodegen.lFASin;
import static idrisjvm.core.JCodegen.lFATan;
import static idrisjvm.core.JCodegen.lFCeil;
import static idrisjvm.core.JCodegen.lFCos;
import static idrisjvm.core.JCodegen.lFExp;
import static idrisjvm.core.JCodegen.lFFloor;
import static idrisjvm.core.JCodegen.lFLog;
import static idrisjvm.core.JCodegen.lFNegate;
import static idrisjvm.core.JCodegen.lFSin;
import static idrisjvm.core.JCodegen.lFSqrt;
import static idrisjvm.core.JCodegen.lFTan;
import static idrisjvm.core.JCodegen.lFloatInt;
import static idrisjvm.core.JCodegen.lFloatStr;
import static idrisjvm.core.JCodegen.lFork;
import static idrisjvm.core.JCodegen.lGe;
import static idrisjvm.core.JCodegen.lGt;
import static idrisjvm.core.JCodegen.lIntCh;
import static idrisjvm.core.JCodegen.lIntFloat;
import static idrisjvm.core.JCodegen.lIntStr;
import static idrisjvm.core.JCodegen.lLSHR;
import static idrisjvm.core.JCodegen.lLe;
import static idrisjvm.core.JCodegen.lLt;
import static idrisjvm.core.JCodegen.lMinus;
import static idrisjvm.core.JCodegen.lNoOp;
import static idrisjvm.core.JCodegen.lOr;
import static idrisjvm.core.JCodegen.lPar;
import static idrisjvm.core.JCodegen.lPlus;
import static idrisjvm.core.JCodegen.lReadStr;
import static idrisjvm.core.JCodegen.lSDiv;
import static idrisjvm.core.JCodegen.lSExt;
import static idrisjvm.core.JCodegen.lSGe;
import static idrisjvm.core.JCodegen.lSGt;
import static idrisjvm.core.JCodegen.lSHL;
import static idrisjvm.core.JCodegen.lSLe;
import static idrisjvm.core.JCodegen.lSLt;
import static idrisjvm.core.JCodegen.lSRem;
import static idrisjvm.core.JCodegen.lStrConcat;
import static idrisjvm.core.JCodegen.lStrCons;
import static idrisjvm.core.JCodegen.lStrEq;
import static idrisjvm.core.JCodegen.lStrFloat;
import static idrisjvm.core.JCodegen.lStrHead;
import static idrisjvm.core.JCodegen.lStrIndex;
import static idrisjvm.core.JCodegen.lStrInt;
import static idrisjvm.core.JCodegen.lStrLen;
import static idrisjvm.core.JCodegen.lStrLt;
import static idrisjvm.core.JCodegen.lStrRev;
import static idrisjvm.core.JCodegen.lStrSubstr;
import static idrisjvm.core.JCodegen.lStrTail;
import static idrisjvm.core.JCodegen.lSystemInfo;
import static idrisjvm.core.JCodegen.lTimes;
import static idrisjvm.core.JCodegen.lTrunc;
import static idrisjvm.core.JCodegen.lUDiv;
import static idrisjvm.core.JCodegen.lURem;
import static idrisjvm.core.JCodegen.lWriteStr;
import static idrisjvm.core.JCodegen.lXOr;
import static idrisjvm.core.JCodegen.lZExt;

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
                    throw new RuntimeException("A PrimFn value expected but found " +
                        jsonParser.getCurrentName());
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
