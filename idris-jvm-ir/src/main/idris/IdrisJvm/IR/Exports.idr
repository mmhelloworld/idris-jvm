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

{-
exports : FFI_Export FFI_JVM "idrisjvm/ir/JTypes" []
exports =
  Data FDesc "idrisjvm/ir/FDesc" $
  Data SExp "idrisjvm/ir/SExp" $
  Data Const "idrisjvm/ir/Const" $
  Data LVar "idrisjvm/ir/LVar" $
  Data CaseType "idrisjvm/ir/CaseType" $
  Data Export "idrisjvm/ir/Export" $
  Data NativeTy "idrisjvm/ir/NativeTy" $
  Data ExportIFace "idrisjvm/ir/ExportIFace" $
  Data IntTy ("idrisjvm/ir/IntTy") $
  Data ArithTy ("idrisjvm/ir/ArithTy") $
  Data PrimFn ("idrisjvm/ir/PrimFn") $
  Data SAlt "idrisjvm/ir/SAlt" $
  Data SDecl "idrisjvm/ir/SDecl" $
  Data (FDesc, LVar) "idrisjvm/ir/SForeignArg" $
  Data (List SAlt) "idris/prelude/list/ListSAlt" $
  Data (List (FDesc, LVar)) "idrisjvm/ir/SForeignArgs" $
  Data (Maybe LVar) "idrisjvm/ir/MaybeLVar" $
  Data (List String) "idris/prelude/list/ListString" $
  Data (List FDesc) "idris/prelude/list/ListFDesc" $
  Data (List LVar) "idris/prelude/list/ListLVar" $
  Data (List Export) "idris/prelude/list/ListExport" $

  Fun consFDesc (ExportStatic "consFDesc") $
  Fun emptyFDesc (ExportStatic "emptyFDesc") $

  Fun consString (ExportStatic "consString") $
  Fun emptyListString (ExportStatic "emptyListString") $

  Fun fcon (ExportStatic "fcon") $
  Fun fstr (ExportStatic "fstr") $
  Fun fapp (ExportStatic "fapp") $
  Fun fio (ExportStatic "fio") $
  Fun funknown (ExportStatic "funknown") $

  Fun loc (ExportStatic "loc") $
  Fun glob (ExportStatic "glob") $

  Fun consLVar (ExportStatic "consLVar") $
  Fun emptyLVar (ExportStatic "emptyLVar") $

  Fun consExport (ExportStatic "consExport") $
  Fun emptyExport (ExportStatic "emptyExport") $
  Fun exportData (ExportStatic "exportData") $
  Fun exportFun (ExportStatic "exportFun") $

  Fun mkExportIFace (ExportStatic "mkExportIFace") $

  Fun it8 (ExportStatic "it8") $
  Fun it16 (ExportStatic "it16") $
  Fun it32 (ExportStatic "it32") $
  Fun it64 (ExportStatic "it64") $

  Fun itFixed (ExportStatic "itFixed") $
  Fun itNative (ExportStatic "itNative") $
  Fun itBig (ExportStatic "itBig") $
  Fun itChar (ExportStatic "itChar") $

  Fun atInt (ExportStatic "atInt") $
  Fun atFloat (ExportStatic "atFloat") $

  Fun lFExp (ExportStatic "lFExp") $
  Fun lFLog (ExportStatic "lFLog") $
  Fun lFSin (ExportStatic "lFSin") $
  Fun lFCos (ExportStatic "lFCos") $
  Fun lFTan (ExportStatic "lFTan") $
  Fun lFASin (ExportStatic "lFASin") $
  Fun lFACos (ExportStatic "lFACos") $
  Fun lFATan (ExportStatic "lFATan") $
  Fun lFSqrt (ExportStatic "lFSqrt") $
  Fun lFFloor (ExportStatic "lFFloor") $
  Fun lFCeil (ExportStatic "lFCeil") $
  Fun lFNegate (ExportStatic "lFNegate") $
  Fun lStrHead (ExportStatic "lStrHead") $
  Fun lStrTail (ExportStatic "lStrTail") $
  Fun lStrCons (ExportStatic "lStrCons") $
  Fun lStrIndex (ExportStatic "lStrIndex") $
  Fun lStrRev (ExportStatic "lStrRev") $
  Fun lStrSubstr (ExportStatic "lStrSubstr") $
  Fun lReadStr (ExportStatic "lReadStr") $
  Fun lWriteStr (ExportStatic "lWriteStr") $
  Fun lSystemInfo (ExportStatic "lSystemInfo") $
  Fun lFork (ExportStatic "lFork") $
  Fun lPar (ExportStatic "lPar") $
  Fun lCrash (ExportStatic "lCrash") $
  Fun lNoOp (ExportStatic "lNoOp") $
  Fun lStrConcat (ExportStatic "lStrConcat") $
  Fun lStrLt (ExportStatic "lStrLt") $
  Fun lStrEq (ExportStatic "lStrEq") $
  Fun lStrLen (ExportStatic "lStrLen") $
  Fun lFloatStr (ExportStatic "lFloatStr") $
  Fun lStrFloat (ExportStatic "lStrFloat") $
  Fun lPlus (ExportStatic "lPlus") $
  Fun lMinus (ExportStatic "lMinus") $
  Fun lTimes (ExportStatic "lTimes") $
  Fun lSDiv (ExportStatic "lSDiv") $
  Fun lSRem (ExportStatic "lSRem") $
  Fun lEq (ExportStatic "lEq") $
  Fun lSLt (ExportStatic "lSLt") $
  Fun lSLe (ExportStatic "lSLe") $
  Fun lSGt (ExportStatic "lSGt") $
  Fun lSGe (ExportStatic "lSGe") $
  Fun lUDiv (ExportStatic "lUDiv") $
  Fun lURem (ExportStatic "lURem") $
  Fun lAnd (ExportStatic "lAnd") $
  Fun lOr (ExportStatic "lOr") $
  Fun lXOr (ExportStatic "lXOr") $
  Fun lCompl (ExportStatic "lCompl") $
  Fun lSHL (ExportStatic "lSHL") $
  Fun lLSHR (ExportStatic "lLSHR") $
  Fun lASHR (ExportStatic "lASHR") $
  Fun lLt (ExportStatic "lLt") $
  Fun lLe (ExportStatic "lLe") $
  Fun lGt (ExportStatic "lGt") $
  Fun lGe (ExportStatic "lGe") $
  Fun lIntFloat (ExportStatic "lIntFloat") $
  Fun lFloatInt (ExportStatic "lFloatInt") $
  Fun lIntStr (ExportStatic "lIntStr") $
  Fun lStrInt (ExportStatic "lStrInt") $
  Fun lChInt (ExportStatic "lChInt") $
  Fun lIntCh (ExportStatic "lIntCh") $
  Fun lSExt (ExportStatic "lSExt") $
  Fun lZExt (ExportStatic "lZExt") $
  Fun lTrunc (ExportStatic "lTrunc") $
  Fun lBitCast (ExportStatic "lBitCast") $
  Fun lExternal (ExportStatic "lExternal") $

  Fun sConCase (ExportStatic "sConCase") $
  Fun sConstCase (ExportStatic "sConstCase") $
  Fun sDefaultCase (ExportStatic "sDefaultCase") $
  Fun consSAlt (ExportStatic "consSAlt") $
  Fun emptySAlt (ExportStatic "emptySAlt") $

  Fun mkSForeignArg (ExportStatic "mkSForeignArg") $

  Fun consSForeignArg (ExportStatic "consSForeignArg") $
  Fun emptySForeignArg (ExportStatic "emptySForeignArg") $

  Fun nothingLVar (ExportStatic "nothingLVar") $
  Fun justLVar (ExportStatic "justLVar") $

  Fun constI (ExportStatic "constI") $
  Fun constBI (ExportStatic "constBI") $
  Fun constFl (ExportStatic "constFl") $
  Fun constCh (ExportStatic "constCh") $
  Fun constStr (ExportStatic "constStr") $
  Fun constB8 (ExportStatic "constB8") $
  Fun constB16 (ExportStatic "constB16") $
  Fun constB32 (ExportStatic "constB32") $
  Fun constB64 (ExportStatic "constB64") $
  Fun aType (ExportStatic "aType") $
  Fun strType (ExportStatic "strType") $
  Fun worldType (ExportStatic "worldType") $
  Fun theWorld (ExportStatic "theWorld") $
  Fun voidType (ExportStatic "voidType") $
  Fun forgot (ExportStatic "forgot") $

  Fun sV (ExportStatic "sV") $
  Fun sApp (ExportStatic "sApp") $
  Fun sLet (ExportStatic "sLet") $
  Fun sUpdate (ExportStatic "sUpdate") $
  Fun sCon (ExportStatic "sCon") $
  Fun sCase (ExportStatic "sCase") $
  Fun sChkCase (ExportStatic "sChkCase") $
  Fun sProj (ExportStatic "sProj") $
  Fun sConst (ExportStatic "sConst") $
  Fun sForeign (ExportStatic "sForeign") $
  Fun sOp (ExportStatic "sOp") $
  Fun sNothing (ExportStatic "sNothing") $
  Fun sError (ExportStatic "sError") $

  Fun sFun (ExportStatic "sFun") $

  Fun updatable (ExportStatic "updatable") $
  Fun shared (ExportStatic "shared") $

  End
-}