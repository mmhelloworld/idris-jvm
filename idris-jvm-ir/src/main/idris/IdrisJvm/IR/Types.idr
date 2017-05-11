module IdrisJvm.IR.Types

%access public export

%hide Language.Reflection.ArithTy
%hide Language.Reflection.IntTy
%hide Language.Reflection.NativeTy
%hide Language.Reflection.Const

data LVar = Loc Int | Glob String

Show LVar where
  show (Loc loc) = "Loc " ++ show loc
  show (Glob s) = "Glob " ++ s

data CaseType = Updatable | Shared

Show CaseType where
  show Updatable = "Updatable"
  show Shared = "Shared"

data FDesc = FCon String
           | FStr String
           | FUnknown
           | FIO FDesc
           | FApp String (List FDesc)

Show FDesc where
  show (FCon s) = "FCon(" ++ s ++ ")"
  show (FStr s) = "FStr(" ++ s ++ ")"
  show FUnknown = "FUnknown"
  show (FIO fdesc) = "FIO(" ++ show fdesc ++ ")"
  show (FApp f descs) = concat $ the (List String) ["FApp(", f, ", ", show descs, ")"]

mutual
  data SExp = SV LVar
            | SApp Bool String (List LVar)
            | SLet LVar SExp SExp
            | SUpdate LVar SExp
            | SCon (Maybe LVar) -- location to reallocate, if available
                   Int String (List LVar)
            | SCase CaseType LVar (List SAlt)
            | SChkCase LVar (List SAlt)
            | SProj LVar Int
            | SConst Const
            -- Keep DExps for describing foreign things, because they get
            -- translated differently
            | SForeign FDesc FDesc (List (FDesc, LVar))
            | SOp PrimFn (List LVar)
            | SNothing -- erased value, will never be inspected
            | SError String

  data PrimFn = LPlus ArithTy | LMinus ArithTy | LTimes ArithTy
              | LUDiv IntTy | LSDiv ArithTy | LURem IntTy | LSRem ArithTy
              | LAnd IntTy | LOr IntTy | LXOr IntTy | LCompl IntTy
              | LSHL IntTy | LLSHR IntTy | LASHR IntTy
              | LEq ArithTy | LLt IntTy | LLe IntTy | LGt IntTy | LGe IntTy
              | LSLt ArithTy | LSLe ArithTy | LSGt ArithTy | LSGe ArithTy
              | LSExt IntTy IntTy | LZExt IntTy IntTy | LTrunc IntTy IntTy
              | LStrConcat | LStrLt | LStrEq | LStrLen
              | LIntFloat IntTy | LFloatInt IntTy | LIntStr IntTy | LStrInt IntTy
              | LFloatStr | LStrFloat | LChInt IntTy | LIntCh IntTy
              | LBitCast ArithTy ArithTy -- Only for values of equal width

              | LFExp | LFLog | LFSin | LFCos | LFTan | LFASin | LFACos | LFATan
              | LFSqrt | LFFloor | LFCeil | LFNegate

              | LStrHead | LStrTail | LStrCons | LStrIndex | LStrRev | LStrSubstr
              | LReadStr | LWriteStr

              -- system info
              | LSystemInfo

              | LFork
              | LPar -- evaluate argument anywhere, possibly on another
                     -- core or another machine. 'id' is a valid implementation
              | LExternal String
              | LCrash

              | LNoOp

  data SDecl = SFun String (List String) Int SExp

  data SAlt = SConCase Int Int String (List String) SExp
            | SConstCase Const SExp
            | SDefaultCase SExp

  data NativeTy = IT8 | IT16 | IT32 | IT64

  data IntTy = ITFixed NativeTy | ITNative | ITBig | ITChar

  data ArithTy = ATInt IntTy | ATFloat

  data Const = I Int | BI String | Fl Double | Ch Char | Str String
             | B8 Bits8 | B16 Bits16 | B32 Int | B64 Bits64
             | AType ArithTy | StrType
             | WorldType | TheWorld
             | VoidType | Forgot

itBitsName : NativeTy -> String
itBitsName IT8 = "Bits8"
itBitsName IT16 = "Bits16"
itBitsName IT32 = "Bits32"
itBitsName IT64 = "Bits64"

Show NativeTy where
  show IT8 = "IT8"
  show IT16 = "IT16"
  show IT32 = "IT32"
  show IT64 = "IT64"

Show IntTy where
  show (ITFixed nativeTy) = "ITFixed " ++ show nativeTy
  show ITNative = "ITNative"
  show ITBig = "ITBig"
  show ITChar = "ITChar"

Show ArithTy where
  show (ATInt intTy) = "ATInt " ++ show intTy
  show ATFloat = "ATFloat"

Show Const where
  show (I i) = show i
  show (BI i) = show i
  show (Fl f) = show f
  show (Ch c) = show c
  show (Str s) = show s
  show (B8 x) = show x
  show (B16 x) = show x
  show (B32 x) = show x
  show (B64 x) = show x
  show (AType ATFloat) = "Double"
  show (AType (ATInt ITBig)) = "Integer"
  show (AType (ATInt ITNative)) = "Int"
  show (AType (ATInt ITChar)) = "Char"
  show (AType (ATInt (ITFixed it))) = itBitsName it
  show TheWorld = "prim__TheWorld"
  show WorldType = "prim__WorldType"
  show StrType = "String"
  show VoidType = "Void"
  show Forgot = "Forgot"

data Export = ExportData FDesc -- Exported data descriptor (usually string)
            | ExportFun String -- Idris name
                        FDesc -- Exported function descriptor
                        FDesc -- Return type descriptor
                        (List FDesc) -- Argument types

data ExportIFace = MkExportIFace String -- FFI descriptor
                                 String -- interface file
                                 (List Export)

Show PrimFn where
  show (LPlus a) = "LPlus " ++ show a
  show (LMinus a) = "LPlus " ++ show a
  show (LTimes a) = "LPlus " ++ show a
  show (LUDiv a) = "LUDiv " ++ show a
  show (LSDiv a) = "LSDiv " ++ show a
  show (LURem a) = "LURem " ++ show a
  show (LSRem a) = "LSRem " ++ show a
  show (LAnd a) = "LAnd " ++ show a
  show (LOr a) = "LOr " ++ show a
  show (LXOr a) = "LXOr " ++ show a
  show (LCompl a) = "LCompl " ++ show a
  show (LSHL a) = "LSHL " ++ show a
  show (LLSHR a) = "LLSHR " ++ show a
  show (LASHR a) = "LASHR " ++ show a
  show (LEq a) = "LEq " ++ show a
  show (LLt a) = "LLt " ++ show a
  show (LLe a) = "LLe " ++ show a
  show (LGt a) = "LGt " ++ show a
  show (LGe a) = "LGe " ++ show a
  show (LSLt a) = "LSLt " ++ show a
  show (LSLe a) = "LSLe " ++ show a
  show (LSGt a) = "LSGt " ++ show a
  show (LSGe a) = "LSGe " ++ show a
  show (LSExt a b) = "LSExt (" ++ show a ++ ", " ++ show b ++ ")"
  show (LZExt a b) = "LZExt (" ++ show a ++ ", " ++ show b ++ ")"
  show (LTrunc a b) = "LTrunc (" ++ show a ++ ", " ++ show b ++ ")"
  show LStrConcat = "LStrConcat"
  show LStrLt = "LStrLt"
  show LStrEq = "LStrEq"
  show LStrLen = "LStrLen"
  show (LIntFloat a) = "LIntFloat " ++ show a
  show (LFloatInt a) = "LFloatInt " ++ show a
  show (LIntStr a) = "LIntStr " ++ show a
  show (LStrInt a) = "LStrInt " ++ show a
  show LFloatStr = "LFloatStr"
  show LStrFloat = "LStrFloat"
  show (LChInt a) = "LChInt " ++ show a
  show (LIntCh a) = "LIntCh " ++ show a
  show (LBitCast a b) = "LBitCast (" ++ show a ++ ", " ++ show b ++ ")"
  show LFExp = "LFExp"
  show LFLog = "LFLog"
  show LFSin = "LFSin"
  show LFCos = "LFCos"
  show LFTan = "LFTan"
  show LFASin = "LFASin"
  show LFACos = "LFACos"
  show LFATan = "LFATan"
  show LFSqrt = "LFSqrt"
  show LFFloor = "LFFloor"
  show LFCeil = "LFCeil"
  show LFNegate = "LFNegate"
  show LStrHead = "LStrHead"
  show LStrTail = "LStrTail"
  show LStrCons = "LStrCons"
  show LStrIndex = "LStrIndex"
  show LStrRev = "LStrRev"
  show LStrSubstr = "LStrSubstr"
  show LReadStr = "LReadStr"
  show LWriteStr = "LWriteStr"
  show LSystemInfo = "LSystemInfo"
  show LFork = "LFork"
  show LPar = "LPar"
  show (LExternal s) = "LExternal " ++ s
  show LCrash = "LCrash"
  show LNoOp = "LNoOp"

Show Export where
  show (ExportData fdesc) = "ExportData(" ++ show fdesc ++ ")"
  show (ExportFun f desc retDesc argDescs)
    = "ExportFun(" ++ f ++ ", " ++ show desc ++ ", " ++ show retDesc ++ ", " ++ show argDescs ++ ")"

Show ExportIFace where
  show (MkExportIFace desc f exports) = "MKExportIFace(" ++ desc ++ ", " ++ f ++ ", " ++ show exports ++ ")"

isTypeConst : Const -> Bool
isTypeConst (AType _) = True
isTypeConst StrType = True
isTypeConst WorldType = True
isTypeConst VoidType = True
isTypeConst _ = False

