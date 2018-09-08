module IdrisJvm.IR.Exports

import IdrisJvm.IR.Types
import IdrisJvm.IO

%access public export

funknown : FDesc
fcon : String -> FDesc
fstr : String -> FDesc
fio : FDesc -> FDesc
fapp : String -> List FDesc -> FDesc

consFDesc : FDesc -> List FDesc -> List FDesc
emptyFDesc : List FDesc

consSDecl : SDecl -> List SDecl -> List SDecl
consSDecl = (::)

emptySDecl : List SDecl
emptySDecl = []

consExportIFace : ExportIFace -> List ExportIFace -> List ExportIFace
consExportIFace = (::)

emptyExportIFace : List ExportIFace
emptyExportIFace = []

consString : String -> List String -> List String
emptyListString : List String

consExport : Export -> List Export -> List Export
emptyExport : List Export

loc : Int -> LVar
glob : String -> LVar
it8 : NativeTy
it16 : NativeTy
it32 : NativeTy
it64 : NativeTy
itFixed : NativeTy -> IntTy
itNative : IntTy
itBig : IntTy
itChar : IntTy
atInt : IntTy -> ArithTy
atFloat : ArithTy
lPlus : ArithTy -> PrimFn
lMinus : ArithTy -> PrimFn
lTimes : ArithTy -> PrimFn
lSDiv : ArithTy -> PrimFn
lSRem : ArithTy -> PrimFn
lEq : ArithTy -> PrimFn
lSLt : ArithTy -> PrimFn
lSLe : ArithTy -> PrimFn
lSGt : ArithTy -> PrimFn
lSGe : ArithTy -> PrimFn
lUDiv : IntTy -> PrimFn
lURem : IntTy -> PrimFn
lAnd : IntTy -> PrimFn
lOr : IntTy -> PrimFn
lXOr : IntTy -> PrimFn
lCompl : IntTy -> PrimFn
lSHL : IntTy -> PrimFn
lLSHR : IntTy -> PrimFn
lASHR : IntTy -> PrimFn
lLt : IntTy -> PrimFn
lLe : IntTy -> PrimFn
lGt : IntTy -> PrimFn
lGe : IntTy -> PrimFn
lIntFloat : IntTy -> PrimFn
lFloatInt : IntTy -> PrimFn
lIntStr : IntTy -> PrimFn
lStrInt : IntTy -> PrimFn
lChInt : IntTy -> PrimFn
lIntCh : IntTy -> PrimFn
lFExp : PrimFn
lFLog : PrimFn
lFSin : PrimFn
lFCos : PrimFn
lFTan : PrimFn
lFASin : PrimFn
lFACos : PrimFn
lFATan : PrimFn
lFSqrt : PrimFn
lFFloor : PrimFn
lFCeil : PrimFn
lFNegate : PrimFn
lStrHead : PrimFn
lStrTail : PrimFn
lStrCons : PrimFn
lStrIndex : PrimFn
lStrRev : PrimFn
lStrSubstr : PrimFn
lReadStr : PrimFn
lWriteStr : PrimFn
lSystemInfo : PrimFn
lFork : PrimFn
lPar : PrimFn
lCrash : PrimFn
lNoOp : PrimFn
lStrConcat : PrimFn
lStrLt : PrimFn
lStrEq : PrimFn
lStrLen : PrimFn
lFloatStr : PrimFn
lStrFloat : PrimFn
lSExt : IntTy -> IntTy -> PrimFn
lZExt : IntTy -> IntTy -> PrimFn
lTrunc : IntTy -> IntTy -> PrimFn
lBitCast : ArithTy -> ArithTy -> PrimFn
lExternal : String -> PrimFn
exportData : FDesc -> Export
exportFun : String -> FDesc -> FDesc -> List FDesc -> Export
mkExportIFace : String -> String -> List Export -> ExportIFace
sV : LVar -> SExp
sApp : Bool -> String -> List LVar -> SExp
sLet : LVar -> SExp -> SExp -> SExp
sUpdate : LVar -> SExp -> SExp
sCon : (Maybe LVar) -> Int -> String -> List LVar -> SExp
sCase : CaseType -> LVar -> List SAlt -> SExp
sChkCase : LVar -> List SAlt -> SExp
sProj : LVar -> Int -> SExp
sConst : Const -> SExp
sForeign : FDesc -> FDesc -> List (FDesc, LVar) -> SExp
sOp : PrimFn -> List LVar -> SExp
sNothing : SExp
sError : String -> SExp
mkSForeignArg : FDesc -> LVar -> (FDesc, LVar)
consSAlt : SAlt -> List SAlt -> List SAlt
emptySAlt : List SAlt
consSForeignArg : (FDesc, LVar) -> List (FDesc, LVar) -> List (FDesc, LVar)
emptySForeignArg : List (FDesc, LVar)
consLVar : LVar -> List LVar -> List LVar
emptyLVar : List LVar
constI : Int -> Const
constBI : String -> Const
constFl : Double -> Const
constCh : Char -> Const
constStr : String -> Const
constB8 : Bits8 -> Const
constB16 : Bits16 -> Const
constB32 : Int -> Const
constB64 : Bits64 -> Const
aType : ArithTy -> Const
strType : Const
worldType : Const
theWorld : Const
voidType : Const
forgot : Const
sFun : String -> List String -> Int -> SExp -> SDecl
sConCase : Int -> Int -> String -> List String -> SExp -> SAlt
sConstCase : Const -> SExp -> SAlt
sDefaultCase : SExp -> SAlt
nothingLVar : Maybe LVar
justLVar : LVar -> Maybe LVar
updatable : CaseType
shared : CaseType

funknown = FUnknown

fcon = FCon

fstr = FStr

fio = FIO

fapp = FApp

consFDesc = (::)

emptyFDesc = []

consString = (::)

emptyListString  = []

consExport = (::)

emptyExport = []

loc = Loc

glob = Glob

it8 = IT8

it16 = IT16

it32 = IT32

it64 = IT64

itFixed = ITFixed

itNative = ITNative

itBig = ITBig

itChar = ITChar

atInt = ATInt

atFloat = ATFloat

{- PrimFn exports -}

lPlus = LPlus

lMinus = LMinus

lTimes = LTimes

lSDiv = LSDiv

lSRem = LSRem

lEq = LEq

lSLt = LSLt

lSLe = LSLe

lSGt = LSGt

lSGe = LSGe

lUDiv = LUDiv

lURem = LURem

lAnd = LAnd

lOr = LOr

lXOr = LXOr

lCompl = LCompl

lSHL = LSHL

lLSHR = LLSHR

lASHR = LASHR

lLt = LLt

lLe = LLe

lGt = LGt

lGe = LGe

lIntFloat = LIntFloat

lFloatInt = LFloatInt

lIntStr = LIntStr

lStrInt = LStrInt

lChInt = LChInt

lIntCh = LIntCh

lFExp = LFExp

lFLog = LFLog

lFSin = LFSin

lFCos = LFCos

lFTan = LFTan

lFASin = LFASin

lFACos = LFACos

lFATan = LFATan

lFSqrt = LFSqrt

lFFloor = LFFloor

lFCeil = LFCeil

lFNegate = LFNegate

lStrHead = LStrHead

lStrTail = LStrTail

lStrCons = LStrCons

lStrIndex = LStrIndex

lStrRev = LStrRev

lStrSubstr = LStrSubstr

lReadStr = LReadStr

lWriteStr = LWriteStr

lSystemInfo = LSystemInfo

lFork = LFork

lPar = LPar

lCrash = LCrash

lNoOp = LNoOp

lStrConcat = LStrConcat

lStrLt = LStrLt

lStrEq = LStrEq

lStrLen = LStrLen

lFloatStr = LFloatStr

lStrFloat = LStrFloat

lSExt = LSExt

lZExt = LZExt

lTrunc = LTrunc

lBitCast = lBitCast

lExternal = LExternal

exportData = ExportData

exportFun = ExportFun

mkExportIFace = MkExportIFace

sV = SV

sApp = SApp

sLet = SLet

sUpdate = SUpdate

sCon = SCon

sCase = SCase

sChkCase = SChkCase

sProj = SProj

sConst = SConst

sForeign = SForeign

sOp = SOp

sNothing = SNothing

sError = SError

mkSForeignArg fdesc lvar = (fdesc, lvar)

consSAlt = (::)

emptySAlt = []

consSForeignArg = (::)

emptySForeignArg = []

consLVar = (::)

emptyLVar = []

constI = I

constBI = BI

constFl = Fl

constCh = Ch

constStr = Str

constB8 = B8

constB16 = B16

constB32 = B32

constB64 = B64

aType = AType

strType = StrType

worldType = WorldType

theWorld = TheWorld

voidType = VoidType

forgot = Forgot

sFun n args locs exp = SFun n args locs exp

sConCase = SConCase

sConstCase = SConstCase

sDefaultCase = SDefaultCase

nothingLVar = Nothing

justLVar = Just

updatable = Updatable

shared = Shared
