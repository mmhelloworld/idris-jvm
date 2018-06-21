module IdrisJvm.Core.Inference

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.Core.Constant
import IdrisJvm.Core.ControlFlow
import IdrisJvm.Core.Foreign
import IdrisJvm.Core.Operator
import IdrisJvm.IR.Types
import Data.SortedMap

%access public export

data InferredType = IBool | IChar | IInt | ILong | IFloat | IDouble | Ref String | IUnknown

InferredTypeStore : Type
InferredTypeStore = SortedMap Int InferredType

record InferredFunctionType where
    constructor MkInferredFunctionType
    functionName : JMethodName
    returnType : InferredType
    argsType : InferredTypeStore

inferredObjectType : InferredType
inferredObjectType = Ref $ classSig "java/lang/Object"

inferredIdrisObjectType : InferredType
inferredIdrisObjectType = Ref $ classSig idrisObjectType

record InferenceConfig where
  constructor MkInferenceConfig
  functionName : JMethodName
  functionTypes : SortedMap JMethodName (InferredType, InferredTypeStore)

Eq InferredType where
  IBool == IBool = True
  IChar == IChar = True
  IInt == IInt = True
  ILong == ILong = True
  IFloat == IFloat = True
  IDouble == IDouble = True
  (Ref ty1) == (Ref ty2) = ty1 == ty2
  IUnknown == IUnknown = True
  _ == _ = False

Show InferredType where
    show IBool = "boolean"
    show IChar = "char"
    show IInt = "int"
    show ILong = "long"
    show IFloat = "float"
    show IDouble = "double"
    show (Ref clsName) = clsName
    show IUnknown = "unknown"

Semigroup InferredType where
  IUnknown <+> known = known
  known <+> IUnknown = known
  ty1 <+> ty2 = if ty1 == ty2 then ty1 else inferredObjectType

Monoid InferredType where
  neutral = IUnknown

addType : InferredTypeStore -> Int -> InferredType -> InferredTypeStore
addType types var ty = maybe addNew checkExisting $ lookup var types where
  addNew : InferredTypeStore
  addNew = insert var ty types

  checkExisting : InferredType -> InferredTypeStore
  checkExisting existingTy = insert var (existingTy <+> ty) types

inferBinaryOp : InferredTypeStore -> InferredType -> LVar -> LVar -> InferredTypeStore
inferBinaryOp types ty (Loc op1) (Loc op2) = let typesWithFirstOp = addType types op1 ty
                                             in addType typesWithFirstOp op2 ty

inferUnaryOp : InferredTypeStore -> InferredType  -> LVar -> InferredTypeStore
inferUnaryOp types ty (Loc op) = addType types op ty

inferOp2 : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOp2 types (LIntStr (ITFixed IT64)) [x] = (Ref "Ljava/lang/String;", inferUnaryOp types ILong x)
inferOp2 types (LIntStr (ITFixed _)) [x] = (Ref "Ljava/lang/String;", inferUnaryOp types IInt x)
inferOp2 types (LIntStr ITNative) [x] = (Ref "Ljava/lang/String;", inferUnaryOp types IInt x)
inferOp2 types (LIntStr ITBig) [x] = (Ref "Ljava/lang/String;", inferUnaryOp types (Ref "Ljava/math/BigInteger;") x)

inferOp2 types LFloatStr [x] = (Ref "Ljava/lang/String;", inferUnaryOp types IDouble x)

inferOp2 types (LChInt ITBig) [x] = (Ref "Ljava/math/BigInteger;", inferUnaryOp types IChar x)

inferOp2 types (LChInt (ITFixed IT64)) [x] = (ILong, inferUnaryOp types IChar x)

inferOp2 types (LChInt _) [x] = (IInt, inferUnaryOp types IChar x)

inferOp2 types (LIntCh ITBig) [x] = (IChar, inferUnaryOp types (Ref "Ljava/math/BigInteger;") x)

inferOp2 types (LIntCh (ITFixed IT64)) [x] = (IChar, inferUnaryOp types ILong x)

inferOp2 types (LIntCh _) [x] = (IChar, inferUnaryOp types IInt x)

inferOp2 types (LSExt ITNative ITBig) [x] = (Ref "Ljava/math/BigInteger;", inferUnaryOp types IInt x)

inferOp2 types (LSExt (ITFixed IT64) ITBig) [x] = (Ref "Ljava/math/BigInteger;", inferUnaryOp types ILong x)

inferOp2 types (LSExt (ITFixed _) ITBig) [x] = (Ref "Ljava/math/BigInteger;", inferUnaryOp types IInt x)

inferOp2 types (LTrunc ITNative (ITFixed IT64)) [x] = (ILong, inferUnaryOp types IInt x)

inferOp2 types (LTrunc (ITFixed IT64) (ITFixed IT32)) [x] = (IInt, inferUnaryOp types ILong x)
inferOp2 types (LTrunc (ITFixed IT64) (ITFixed IT16)) [x] = (IInt, inferUnaryOp types ILong x)
inferOp2 types (LTrunc (ITFixed IT64) (ITFixed IT8)) [x] = (IInt, inferUnaryOp types ILong x)

inferOp2 types (LTrunc ITBig ITNative) [x] = (IInt, inferUnaryOp types (Ref "Ljava/math/BigInteger;") x)

inferOp2 types (LTrunc (ITFixed _) (ITFixed _)) [x] = (IInt, inferUnaryOp types IInt x)

inferOp2 types LWriteStr [_, s] = (IInt, inferUnaryOp types (Ref "Ljava/lang/String;") s)

inferOp2 types LReadStr [_] = (Ref "Ljava/lang/String;", types)

inferOp2 types LStrConcat [l,r] = (Ref "Ljava/lang/String;", inferBinaryOp types (Ref "Ljava/lang/String;") l r)

inferOp2 types LStrCons [l,r] =
    let typesWithLeft =  inferUnaryOp types IChar l
    in  (Ref "Ljava/lang/String;", inferUnaryOp typesWithLeft (Ref "Ljava/lang/String;") r)

inferOp2 types LStrSubstr [offset, len, str] =
    let typesWithOffset = inferUnaryOp types IInt offset
        typesWithLen = inferUnaryOp typesWithOffset IInt len
    in (Ref "Ljava/lang/String;", inferUnaryOp typesWithLen (Ref "Ljava/lang/String;") str)

inferOp2 types (LStrInt ITBig) [x] = (Ref "Ljava/math/BigInteger;", inferUnaryOp types (Ref "Ljava/lang/String;") x)

inferOp2 types (LStrInt (ITFixed IT8)) [x] = (IInt, inferUnaryOp types (Ref "Ljava/lang/String;") x)

inferOp2 types (LStrInt _) [x] = (IInt, inferUnaryOp types (Ref "Ljava/lang/String;") x)

inferOp2 types LStrFloat [x] = (IDouble, inferUnaryOp types (Ref "Ljava/lang/String;") x)

inferOp2 types (LSHL (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp2 types (LSHL (ITFixed IT8)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types (LSHL (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types (LLSHR (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp2 types (LLSHR (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types (LASHR (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp2 types (LASHR (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types LFork [x] = (inferredObjectType, types)

inferOp2 types LPar [x] = (inferredObjectType, types)

inferOp2 types (LIntFloat ITBig) [x] = (IDouble, inferUnaryOp types (Ref "Ljava/math/BigInteger;") x)

inferOp2 types (LIntFloat ITNative) [x] = (IDouble, inferUnaryOp types IInt x)

inferOp2 types (LFloatInt ITBig) [x] = (Ref "Ljava/math/BigInteger;", inferUnaryOp types IDouble x)

inferOp2 types (LFloatInt ITNative) [x] = (IInt, inferUnaryOp types IDouble x)

inferOp2 types op _ = (IUnknown, types)

inferOpLLe : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOpLLe types (LLe ITBig) [l, r] = (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOpLLe types (LLe ITNative) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOpLLe types (LLe (ITFixed IT64)) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOpLLe types (LLe (ITFixed _)) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOpLLt : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOpLLt types (LLt ITBig) [l, r] = (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOpLLt types (LLt ITNative) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOpLLt types (LLt (ITFixed IT64)) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOpLLt types (LLt (ITFixed _)) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOpLGt : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOpLGt types (LGt ITBig) [l, r] = (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOpLGt types (LGt ITNative) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOpLGt types (LGt (ITFixed IT64)) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOpLGt types (LGt (ITFixed _)) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOp types (LAnd (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LAnd (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LAnd ITNative) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LOr (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LOr (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LOr ITNative) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LXOr (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LXOr (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LXOr ITNative) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LCompl (ITFixed IT64)) [x] = (ILong, inferUnaryOp types ILong x)
inferOp types (LCompl (ITFixed _)) [x] = (IInt, inferUnaryOp types IInt x)
inferOp types (LCompl ITNative) [x] = (IInt, inferUnaryOp types IInt x)

inferOp types llt@(LLt _) args = inferOpLLt types llt args

inferOp types lle@(LLe _) args = inferOpLLe types lle args

inferOp types lgt@(LGt _) args = inferOpLGt types lgt args

inferOp types (LGe ITBig) [l, r] = inferOp types (LSGe (ATInt ITBig)) [l, r]
inferOp types (LGe ITNative) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LGe (ITFixed IT64)) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LGe (ITFixed _)) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LPlus ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LPlus (ATInt ITBig)) [l, r] =
    (Ref "Ljava/math/BigInteger;", inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)

inferOp types (LPlus (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LPlus (ATInt ITChar)) [l, r] = (IChar, inferBinaryOp types IChar l r)

inferOp types (LPlus (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LPlus (ATInt (ITFixed IT8))) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LPlus (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LMinus ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LMinus (ATInt ITBig)) [l, r] =
    (Ref "Ljava/math/BigInteger;", inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)

inferOp types (LMinus (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LMinus (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LMinus (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LTimes ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LTimes (ATInt ITBig)) [l, r] =
    (Ref "Ljava/math/BigInteger;", inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)

inferOp types (LTimes (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LTimes (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LTimes (ATInt (ITFixed IT8))) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LTimes (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LSDiv ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LSDiv (ATInt ITBig)) [l, r] =
    (Ref "Ljava/math/BigInteger;", inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)

inferOp types (LSDiv (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LSDiv (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LSDiv (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LUDiv ITNative) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LUDiv (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp types (LUDiv ITBig) [l, r] =
    (Ref "Ljava/math/BigInteger;", inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOp types (LUDiv (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LSRem ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LSRem (ATInt ITBig)) [l, r] =
    (Ref "Ljava/math/BigInteger;", inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)

inferOp types (LSRem (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LSRem (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LSRem (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LURem (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LURem ITNative) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LURem (ITFixed IT32)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LURem ITBig) [l, r] =
    (Ref "Ljava/math/BigInteger;", inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOp types (LURem (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LEq ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LEq (ATInt ITBig)) [l, r] = (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOp types (LEq (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LEq (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LEq (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LEq (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSLt ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSLt (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSLt (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOp types (LSLt (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSLt (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSLt (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSLe ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSLe (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSLe (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOp types (LSLe (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSLe (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSLe (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSGt ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSGt (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSGt (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOp types (LSGt (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSGt (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSGt (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSGe ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSGe (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSGe (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (Ref "Ljava/math/BigInteger;") l r)
inferOp types (LSGe (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSGe (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSGe (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types LStrEq [l,r] = (IBool, inferBinaryOp types (Ref "Ljava/lang/String;") l r)

inferOp types LStrRev [var] = (Ref "Ljava/lang/String;", inferUnaryOp types (Ref "Ljava/lang/String;") var)

inferOp types LStrLen [var] = (IInt, inferUnaryOp types (Ref "Ljava/lang/String;") var)

inferOp types LStrLt [l, r] = (IBool, inferBinaryOp types (Ref "Ljava/lang/String;") l r)

inferOp types LStrHead [var] = (IChar, inferUnaryOp types (Ref "Ljava/lang/String;") var)

inferOp types LStrIndex [string, index] =
    let stringTyped = inferUnaryOp types (Ref "Ljava/lang/String;") string
        stringAndIndexTyped = inferUnaryOp stringTyped IInt index
    in (IChar, stringAndIndexTyped)

inferOp types LStrTail [var] = (Ref "Ljava/lang/String;", inferUnaryOp types (Ref "Ljava/lang/String;") var)

inferOp types (LZExt ITNative ITBig) [var] =
    (Ref "Ljava/math/BigInteger;", inferUnaryOp types IInt var)

inferOp types (LZExt (ITFixed IT8) ITNative) [x] = (IInt, inferUnaryOp types IInt x)
inferOp types (LZExt (ITFixed IT16) ITNative) [x] = (IInt, inferUnaryOp types IInt x)
inferOp types (LZExt (ITFixed IT32) ITNative) [x] = (IInt, inferUnaryOp types IInt x)

inferOp types op args = inferOp2 types op args

inferForeignReturnDesc : TypeDescriptor -> InferredType
inferForeignReturnDesc (ThrowableDescriptor _) = inferredObjectType
inferForeignReturnDesc (FieldDescriptor FieldTyDescBoolean) = IBool
inferForeignReturnDesc (FieldDescriptor FieldTyDescByte) = IInt
inferForeignReturnDesc (FieldDescriptor FieldTyDescShort) = IInt
inferForeignReturnDesc (FieldDescriptor FieldTyDescInt)     = IInt
inferForeignReturnDesc (FieldDescriptor FieldTyDescChar)    = IChar
inferForeignReturnDesc (FieldDescriptor FieldTyDescLong)    = ILong
inferForeignReturnDesc (FieldDescriptor FieldTyDescFloat)   = IDouble
inferForeignReturnDesc (FieldDescriptor FieldTyDescDouble)  = IDouble
inferForeignReturnDesc (FieldDescriptor (FieldTyDescReference f)) = Ref (asmRefTyDesc f)
inferForeignReturnDesc VoidDescriptor                       = IUnknown

inferForeignArgDescs : InferredTypeStore -> List (FieldTypeDescriptor, LVar) -> InferredTypeStore
inferForeignArgDescs types [] = types
inferForeignArgDescs types ((FieldTyDescBoolean, x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types IBool x) rest
inferForeignArgDescs types ((FieldTyDescByte, x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types IInt x) rest
inferForeignArgDescs types ((FieldTyDescShort, x) :: rest) =
  inferForeignArgDescs (inferUnaryOp types IInt x) rest
inferForeignArgDescs types ((FieldTyDescInt, x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types IInt x) rest
inferForeignArgDescs types ((FieldTyDescChar, x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types IChar x) rest
inferForeignArgDescs types ((FieldTyDescLong, x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types ILong x) rest
inferForeignArgDescs types ((FieldTyDescFloat, x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types IDouble x) rest
inferForeignArgDescs types ((FieldTyDescDouble, x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types IDouble x) rest
inferForeignArgDescs types (((FieldTyDescReference (IdrisExportDesc _)), x) :: rest) =
    inferForeignArgDescs types rest
inferForeignArgDescs types (((FieldTyDescReference NullableStrDesc), x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types (Ref "Ljava/lang/String;") x) rest
inferForeignArgDescs types (((FieldTyDescReference (NullableRefDesc cname)), x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types (Ref ("L" ++ cname ++ ";")) x) rest
inferForeignArgDescs types (((FieldTyDescReference refTy), x) :: rest) =
    inferForeignArgDescs (inferUnaryOp types (Ref (asmRefTyDesc refTy)) x) rest

inferConstExp : InferredTypeStore -> Const -> (InferredType, InferredTypeStore)
inferConstExp acc (B8 i) = (IInt, acc)
inferConstExp acc (B16 i) = (IInt, acc)
inferConstExp acc (B32 i) = (IInt, acc)
inferConstExp acc (B64 i) = (ILong, acc)
inferConstExp acc (I i) = (IInt, acc)
inferConstExp acc (Fl d) = (IDouble, acc)
inferConstExp acc (Ch c) = (IChar, acc)
inferConstExp acc (BI i) = (Ref "Ljava/math/BigInteger;", acc)
inferConstExp acc (Str s) = (Ref "Ljava/lang/String;", acc)
inferConstExp acc TheWorld = (IInt, acc)
inferConstExp acc x = if isTypeConst x
              then (IInt, acc)
              else jerror $ "Constant " ++ show x ++ " not compilable yet"

inferFunctionAppArgs : InferredTypeStore -> InferredTypeStore -> List LVar -> InferredTypeStore
inferFunctionAppArgs calledFunctionArgTypes types args =
    let lookupType = \argPos => fromMaybe IUnknown $ SortedMap.lookup (cast $ the Nat argPos) calledFunctionArgTypes
    in foldl
         (\acc, (pos, arg) => addType acc (locIndex arg) (lookupType pos))
         types
         (List.zip [0 .. length args] args)

combineTypes : InferredType -> List InferredType -> InferredType
combineTypes ty [] = ty
combineTypes IUnknown (IUnknown :: rest) = combineTypes IUnknown rest
combineTypes IUnknown (ty :: rest) = combineTypes ty rest
combineTypes ty (IUnknown :: rest) = combineTypes ty rest
combineTypes prevTy (currTy :: rest) =
    if prevTy == currTy then
        combineTypes currTy rest
    else
        inferredObjectType

mutual
    inferIfElseTy : InferenceConfig -> InferredTypeStore -> LVar -> SExp -> SExp -> (InferredType, InferredTypeStore)
    inferIfElseTy config acc (Loc i) trueAlt falseAlt =
        let ifVarInferred = addType acc i IBool
            (falseRetTy, falseAltTypes) = inferExp config ifVarInferred falseAlt
            (trueRetTy, trueAltTypes) = inferExp config ifVarInferred trueAlt
        in (falseRetTy <+> trueRetTy, merge falseAltTypes trueAltTypes)

    inferNullableIfElseTy : InferenceConfig -> InferredTypeStore -> LVar -> SExp -> SExp -> (InferredType, InferredTypeStore)
    inferNullableIfElseTy config acc (Loc i) trueAlt falseAlt =
        let ifVarInferred = addType acc i IUnknown
            (falseRetTy, falseAltTypes) = inferExp config ifVarInferred falseAlt
            (trueRetTy, trueAltTypes) = inferExp config ifVarInferred trueAlt
        in (falseRetTy <+> trueRetTy, merge falseAltTypes trueAltTypes)

    inferAlt :InferenceConfig -> InferredTypeStore -> SAlt -> (InferredType, InferredTypeStore)
    inferAlt config types (SConstCase _ expr) = inferExp config types expr
    inferAlt config types (SDefaultCase expr) = inferExp config types expr
    inferAlt config types (SConCase lv _ _ args expr) = inferExp config types expr

    inferSwitch : InferenceConfig -> InferredTypeStore -> LVar -> List SAlt -> (InferredType, InferredTypeStore)
    inferSwitch config types (Loc e) alts =
        let switchExprType =
                if isConstructorSwitchCases alts then
                    inferredIdrisObjectType
                else if isIntSwitchCases alts then
                    IInt
                else if isCharSwitchCases alts then
                    IChar
                else
                    IUnknown
            altTypes = inferAlt config (addType types e switchExprType) <$> alts

        in (combineTypes IUnknown $ fst <$> altTypes, concat $ snd <$> altTypes)

    inferExp : InferenceConfig -> InferredTypeStore -> SExp -> (InferredType, InferredTypeStore)
    inferExp config acc (SV (Glob _)) = (inferredObjectType, acc)
    inferExp config acc (SV (Loc i)) = (fromMaybe IUnknown $ lookup i acc, acc)
    inferExp config acc (SApp False f args) =
      maybe (IUnknown, acc) inferArgs $ lookup (jname f) (functionTypes config) where
        inferArgs : (InferredType, InferredTypeStore) -> (InferredType, InferredTypeStore)
        inferArgs (retTy, argTys) = (retTy, inferFunctionAppArgs argTys acc args)

    inferExp config acc (SApp True f args) =
        let calledFunctionName = jname f
        in
          if calledFunctionName == functionName config then
            (IUnknown, acc)
          else maybe (Ref rtThunkSig, acc) inferArgs $ lookup calledFunctionName (functionTypes config) where
             inferArgs : (InferredType, InferredTypeStore) -> (InferredType, InferredTypeStore)
             inferArgs (_, argTys) = (Ref rtThunkSig, inferFunctionAppArgs argTys acc args)

    inferExp config acc (SLet (Loc i) v sc) =
        let (vty, nacc) = inferExp config acc v
            naccWithI = addType nacc i vty
        in inferExp config naccWithI sc
    inferExp config acc (SUpdate _ e) = inferExp config acc e
    inferExp config acc (SProj (Loc v) i) = (inferredObjectType, addType acc v inferredIdrisObjectType)

    inferExp config acc (SCon _ 0 "Prelude.Bool.False" []) = (IBool, acc)
    inferExp config acc (SCon _ 1 "Prelude.Bool.True" []) = (IBool, acc)
    inferExp config acc (SCon _ t _ args) = (inferredIdrisObjectType, acc)
    inferExp config acc (SCase _ e ((SConCase _ 0 "Prelude.Bool.False" [] falseAlt) ::
                             (SConCase _ 1 "Prelude.Bool.True" [] trueAlt) ::
                             _)) = inferIfElseTy config acc e trueAlt falseAlt

    inferExp config acc (SCase _ e ((SConCase _ 1 "Prelude.Bool.True" [] trueAlt) ::
                            (SDefaultCase falseAlt) ::
                            _))
        = inferIfElseTy config acc e trueAlt falseAlt

    inferExp config acc (SCase _ e ((SConCase _ 0 "Prelude.Bool.False" [] falseAlt) ::
                        (SDefaultCase trueAlt) ::
                        _))
        = inferIfElseTy config acc e trueAlt falseAlt

    inferExp config acc (SCase _ e ((SConCase justValueStore 1 "Prelude.Maybe.Just" [_] justExpr) ::
                         (SConCase _ 0 "Prelude.Maybe.Nothing" [] nothingExpr) ::
                         _))
        = inferNullableIfElseTy config (addType acc justValueStore inferredObjectType) e justExpr nothingExpr

    inferExp config acc (SCase _ e ((SConCase justValueStore 1 "Prelude.Maybe.Just" [_] justExpr) ::
                             (SDefaultCase defaultExpr) ::
                             _))
        = inferNullableIfElseTy config (addType acc justValueStore inferredObjectType) e justExpr defaultExpr

    inferExp config acc (SCase _ e ((SConCase _ 0 "Prelude.Maybe.Nothing" [] nothingExpr) ::
                             (SDefaultCase defaultExpr) ::
                             _))
        = inferNullableIfElseTy config acc e nothingExpr defaultExpr

    inferExp config acc (SCase _ e alts) = inferSwitch config acc e alts

    inferExp config acc (SChkCase e alts) = inferSwitch config acc e alts

    inferExp config acc (SConst c) = inferConstExp acc c

    inferExp config acc (SOp op args) = inferOp acc op args

    inferExp config acc SNothing = (IInt, acc)

    inferExp config acc (SError x) = (IUnknown, acc)

    inferExp config acc (SForeign returns fdesc args) = case parseDescriptor returns fdesc args of
        JLambda clazz _ => (Ref ("L" ++ clazz ++ ";"), acc)
        _ =>
            let argsWithTypes = (\(fdesc, lvar) => (fdescFieldDescriptor fdesc, lvar)) <$> args
                returnDesc = fdescTypeDescriptor returns
            in (inferForeignReturnDesc returnDesc, inferForeignArgDescs acc argsWithTypes)

inferFun : SortedMap JMethodName (InferredType, InferredTypeStore) -> SDecl -> InferredFunctionType
inferFun types (SFun name args locs def) =
  let jmethodName = jname name
      config = MkInferenceConfig jmethodName types
      initialArgsTypes = fromMaybe SortedMap.empty $ snd <$> lookup jmethodName types
      (returnType, argsType) = inferExp config initialArgsTypes def
  in MkInferredFunctionType jmethodName returnType argsType