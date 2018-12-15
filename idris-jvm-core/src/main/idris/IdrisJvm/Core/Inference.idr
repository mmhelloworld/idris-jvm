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
inferOp2 types (LIntStr (ITFixed IT64)) [x] = (inferredStringType, inferUnaryOp types ILong x)
inferOp2 types (LIntStr (ITFixed _)) [x] = (inferredStringType, inferUnaryOp types IInt x)
inferOp2 types (LIntStr ITNative) [x] = (inferredStringType, inferUnaryOp types IInt x)
inferOp2 types (LIntStr ITBig) [x] = (inferredStringType, inferUnaryOp types (inferredBigIntegerType) x)

inferOp2 types LFloatStr [x] = (inferredStringType, inferUnaryOp types IDouble x)

inferOp2 types (LChInt ITBig) [x] = (inferredBigIntegerType, inferUnaryOp types IChar x)

inferOp2 types (LChInt (ITFixed IT64)) [x] = (ILong, inferUnaryOp types IChar x)

inferOp2 types (LChInt _) [x] = (IInt, inferUnaryOp types IChar x)

inferOp2 types (LIntCh ITBig) [x] = (IChar, inferUnaryOp types (inferredBigIntegerType) x)

inferOp2 types (LIntCh (ITFixed IT64)) [x] = (IChar, inferUnaryOp types ILong x)

inferOp2 types (LIntCh _) [x] = (IChar, inferUnaryOp types IInt x)

inferOp2 types (LSExt ITNative ITBig) [x] = (inferredBigIntegerType, inferUnaryOp types IInt x)

inferOp2 types (LSExt (ITFixed IT64) ITBig) [x] = (inferredBigIntegerType, inferUnaryOp types ILong x)

inferOp2 types (LSExt (ITFixed _) ITBig) [x] = (inferredBigIntegerType, inferUnaryOp types IInt x)

inferOp2 types (LTrunc ITNative (ITFixed IT64)) [x] = (ILong, inferUnaryOp types IInt x)

inferOp2 types (LTrunc (ITFixed IT64) (ITFixed IT32)) [x] = (IInt, inferUnaryOp types ILong x)
inferOp2 types (LTrunc (ITFixed IT64) (ITFixed IT16)) [x] = (IInt, inferUnaryOp types ILong x)
inferOp2 types (LTrunc (ITFixed IT64) (ITFixed IT8)) [x] = (IInt, inferUnaryOp types ILong x)

inferOp2 types (LTrunc ITBig (ITFixed IT64)) [x] = (ILong, inferUnaryOp types inferredBigIntegerType x)
inferOp2 types (LTrunc ITBig (ITFixed _)) [x] = (IInt, inferUnaryOp types inferredBigIntegerType x)
inferOp2 types (LTrunc ITBig ITNative) [x] = (IInt, inferUnaryOp types inferredBigIntegerType x)

inferOp2 types (LTrunc (ITFixed _) (ITFixed _)) [x] = (IInt, inferUnaryOp types IInt x)

inferOp2 types LWriteStr [_, s] = (IInt, inferUnaryOp types inferredStringType s)

inferOp2 types LReadStr [_] = (inferredStringType, types)

inferOp2 types LStrConcat [l,r] = (inferredStringType, inferBinaryOp types (inferredStringType) l r)

inferOp2 types LStrCons [l,r] =
    let typesWithLeft =  inferUnaryOp types IChar l
    in  (inferredStringType, inferUnaryOp typesWithLeft (inferredStringType) r)

inferOp2 types LStrSubstr [offset, len, str] =
    let typesWithOffset = inferUnaryOp types IInt offset
        typesWithLen = inferUnaryOp typesWithOffset IInt len
    in (inferredStringType, inferUnaryOp typesWithLen (inferredStringType) str)

inferOp2 types (LStrInt ITBig) [x] = (inferredBigIntegerType, inferUnaryOp types (inferredStringType) x)

inferOp2 types (LStrInt (ITFixed IT8)) [x] = (IInt, inferUnaryOp types (inferredStringType) x)

inferOp2 types (LStrInt _) [x] = (IInt, inferUnaryOp types (inferredStringType) x)

inferOp2 types LStrFloat [x] = (IDouble, inferUnaryOp types (inferredStringType) x)

inferOp2 types (LSHL (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp2 types (LSHL (ITFixed IT8)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types (LSHL (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types (LLSHR (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp2 types (LLSHR (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types (LASHR (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp2 types (LASHR (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp2 types LFork [x] = (Ref futureClass, types)

inferOp2 types LPar [x] = (Ref objectClass, types)

inferOp2 types (LIntFloat ITBig) [x] = (IDouble, inferUnaryOp types (inferredBigIntegerType) x)

inferOp2 types (LIntFloat ITNative) [x] = (IDouble, inferUnaryOp types IInt x)

inferOp2 types (LFloatInt ITBig) [x] = (inferredBigIntegerType, inferUnaryOp types IDouble x)

inferOp2 types (LFloatInt ITNative) [x] = (IInt, inferUnaryOp types IDouble x)

inferOp2 types LSystemInfo [x] = (inferredStringType, inferUnaryOp types IInt x)

inferOp2 types LCrash [x] = (IUnknown, inferUnaryOp types inferredStringType x)

inferOp2 types op _ = (IUnknown, types)

inferOpLLe : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOpLLe types (LLe ITBig) [l, r] = (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
inferOpLLe types (LLe ITNative) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOpLLe types (LLe (ITFixed IT64)) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOpLLe types (LLe (ITFixed _)) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOpLLt : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOpLLt types (LLt ITBig) [l, r] = (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
inferOpLLt types (LLt ITNative) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOpLLt types (LLt (ITFixed IT64)) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOpLLt types (LLt (ITFixed _)) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOpLGt : InferredTypeStore -> PrimFn -> List LVar -> (InferredType, InferredTypeStore)
inferOpLGt types (LGt ITBig) [l, r] = (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
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
    (inferredBigIntegerType, inferBinaryOp types (inferredBigIntegerType) l r)

inferOp types (LPlus (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LPlus (ATInt ITChar)) [l, r] = (IChar, inferBinaryOp types IChar l r)

inferOp types (LPlus (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LPlus (ATInt (ITFixed IT8))) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LPlus (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LMinus ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LMinus (ATInt ITBig)) [l, r] =
    (inferredBigIntegerType, inferBinaryOp types (inferredBigIntegerType) l r)

inferOp types (LMinus (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LMinus (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LMinus (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LTimes ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LTimes (ATInt ITBig)) [l, r] =
    (inferredBigIntegerType, inferBinaryOp types (inferredBigIntegerType) l r)

inferOp types (LTimes (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LTimes (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LTimes (ATInt (ITFixed IT8))) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LTimes (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LSDiv ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LSDiv (ATInt ITBig)) [l, r] =
    (inferredBigIntegerType, inferBinaryOp types (inferredBigIntegerType) l r)

inferOp types (LSDiv (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LSDiv (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LSDiv (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LUDiv ITNative) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LUDiv (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)

inferOp types (LUDiv ITBig) [l, r] =
    (inferredBigIntegerType, inferBinaryOp types (inferredBigIntegerType) l r)
inferOp types (LUDiv (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LSRem ATFloat) [l, r] = (IDouble, inferBinaryOp types IDouble l r)
inferOp types (LSRem (ATInt ITBig)) [l, r] =
    (inferredBigIntegerType, inferBinaryOp types (inferredBigIntegerType) l r)

inferOp types (LSRem (ATInt ITNative)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LSRem (ATInt (ITFixed IT64))) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LSRem (ATInt (ITFixed _))) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LURem (ITFixed IT64)) [l, r] = (ILong, inferBinaryOp types ILong l r)
inferOp types (LURem ITNative) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LURem (ITFixed IT32)) [l, r] = (IInt, inferBinaryOp types IInt l r)
inferOp types (LURem ITBig) [l, r] =
    (inferredBigIntegerType, inferBinaryOp types (inferredBigIntegerType) l r)
inferOp types (LURem (ITFixed _)) [l, r] = (IInt, inferBinaryOp types IInt l r)

inferOp types (LEq ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LEq (ATInt ITBig)) [l, r] = (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
inferOp types (LEq (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LEq (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LEq (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LEq (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSLt ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSLt (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSLt (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
inferOp types (LSLt (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSLt (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSLt (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSLe ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSLe (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSLe (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
inferOp types (LSLe (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSLe (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSLe (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSGt ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSGt (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSGt (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
inferOp types (LSGt (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSGt (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSGt (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types (LSGe ATFloat) [l, r] = (IBool, inferBinaryOp types IDouble l r)
inferOp types (LSGe (ATInt ITNative)) [l, r] = (IBool, inferBinaryOp types IInt l r)
inferOp types (LSGe (ATInt ITBig)) [l, r] =
    (IBool, inferBinaryOp types (inferredBigIntegerType) l r)
inferOp types (LSGe (ATInt ITChar)) [l, r] = (IBool, inferBinaryOp types IChar l r)
inferOp types (LSGe (ATInt (ITFixed IT64))) [l, r] = (IBool, inferBinaryOp types ILong l r)
inferOp types (LSGe (ATInt (ITFixed _))) [l, r] = (IBool, inferBinaryOp types IInt l r)

inferOp types LStrEq [l,r] = (IBool, inferBinaryOp types inferredStringType l r)

inferOp types LStrRev [var] = (inferredStringType, inferUnaryOp types inferredStringType var)

inferOp types LStrLen [var] = (IInt, inferUnaryOp types inferredStringType var)

inferOp types LStrLt [l, r] = (IBool, inferBinaryOp types inferredStringType l r)

inferOp types LStrHead [var] = (IChar, inferUnaryOp types inferredStringType var)

inferOp types LStrIndex [string, index] =
    let stringTyped = inferUnaryOp types (inferredStringType) string
        stringAndIndexTyped = inferUnaryOp stringTyped IInt index
    in (IChar, stringAndIndexTyped)

inferOp types LStrTail [var] = (inferredStringType, inferUnaryOp types (inferredStringType) var)

inferOp types (LZExt ITNative ITBig) [var] =
    (inferredBigIntegerType, inferUnaryOp types IInt var)

inferOp types (LZExt (ITFixed IT8) ITNative) [x] = (IInt, inferUnaryOp types IInt x)
inferOp types (LZExt (ITFixed IT16) ITNative) [x] = (IInt, inferUnaryOp types IInt x)
inferOp types (LZExt (ITFixed IT32) ITNative) [x] = (IInt, inferUnaryOp types IInt x)

inferOp types op args = inferOp2 types op args

inferForeignArgDescs : InferredTypeStore -> List (FieldTypeDescriptor, LVar) -> InferredTypeStore
inferForeignArgDescs types [] = types
inferForeignArgDescs types ((fieldTyDesc, x) :: rest) =
    let inferredTy = fieldTypeDescriptorToInferredType fieldTyDesc
    in inferForeignArgDescs (inferUnaryOp types inferredTy x) rest

inferConstExp : InferredTypeStore -> Const -> (InferredType, InferredTypeStore)
inferConstExp acc (B8 i) = (IInt, acc)
inferConstExp acc (B16 i) = (IInt, acc)
inferConstExp acc (B32 i) = (IInt, acc)
inferConstExp acc (B64 i) = (ILong, acc)
inferConstExp acc (I i) = (IInt, acc)
inferConstExp acc (Fl d) = (IDouble, acc)
inferConstExp acc (Ch c) = (IChar, acc)
inferConstExp acc (BI i) = (inferredBigIntegerType, acc)
inferConstExp acc (Str s) = (inferredStringType, acc)
inferConstExp acc TheWorld = (IInt, acc)
inferConstExp acc x = if isTypeConst x
              then (IInt, acc)
              else jerror $ "Constant " ++ show x ++ " not compilable yet"

inferFunctionAppArgs : InferredTypeStore -> InferredTypeStore -> List LVar -> InferredTypeStore
inferFunctionAppArgs calledFunctionArgTypes types args =
    let lookupType = \argPos => fromMaybe inferredObjectType $ SortedMap.lookup (cast $ the Nat argPos) calledFunctionArgTypes
        argsLength = length args
        argIndices = if argsLength > 0 then [0 .. (Nat.pred argsLength)] else []
    in foldl
         (\acc, (pos, arg) => addType acc (locIndex arg) (lookupType pos))
         types
         (List.zip argIndices args)

combineTypes : List InferredType -> InferredType
combineTypes [] = IUnknown
combineTypes (ty :: rest) = go ty rest where
  go : InferredType -> List InferredType -> InferredType
  go prevTy [] = prevTy
  go prevTy (currTy :: rest) =
    if prevTy == currTy then
        go currTy rest
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
        let ifVarInferred = addType acc i inferredObjectType
            (falseRetTy, falseAltTypes) = inferExp config ifVarInferred falseAlt
            (trueRetTy, trueAltTypes) = inferExp config ifVarInferred trueAlt
        in (falseRetTy <+> trueRetTy, merge falseAltTypes trueAltTypes)

    inferAltConCase : InferenceConfig -> InferredTypeStore -> Int -> List String -> SExp -> (InferredType, InferredTypeStore)
    inferAltConCase config types lv args expr = inferExp config typesWithPropTypes expr where
      argsLength : Int
      argsLength = cast $ length args

      typesWithPropTypes : InferredTypeStore
      typesWithPropTypes = if argsLength == 0 then types else go types lv (pred argsLength) where
        go : InferredTypeStore -> Int -> Int -> InferredTypeStore
        go types lv 0 = addType types lv inferredObjectType
        go types lv n = go (addType types (lv + n) inferredObjectType) lv (pred n)

    inferAlt : InferenceConfig -> InferredTypeStore -> SAlt -> (InferredType, InferredTypeStore)
    inferAlt config types (SConstCase _ expr) = inferExp config types expr
    inferAlt config types (SDefaultCase expr) = inferExp config types expr
    inferAlt config types (SConCase lv _ _ args expr) = inferAltConCase config types lv args expr

    inferSwitch : InferenceConfig -> InferredTypeStore -> LVar -> List SAlt -> (InferredType, InferredTypeStore)
    inferSwitch config types (Loc e) alts =
        let switchExprType =
                if isConstructorSwitchCases alts then
                    inferredObjectType
                else if isIntSwitchCases alts then
                    IInt
                else if isCharSwitchCases alts then
                    IChar
                else
                    IUnknown
            altTypes = inferAlt config (addType types e switchExprType) <$> alts

        in (combineTypes $ fst <$> altTypes, concat $ snd <$> altTypes)

    inferExp : InferenceConfig -> InferredTypeStore -> SExp -> (InferredType, InferredTypeStore)
    inferExp config acc (SV (Glob _)) = (inferredObjectType, acc)
    inferExp config acc (SV (Loc i)) = (fromMaybe IUnknown $ lookup i acc, acc)

    inferExp config acc (SApp True f args) =
        let calledFunctionName = jname f
        in
          if calledFunctionName == functionName config then
            (IUnknown, acc)
          else maybe (Ref thunkClass, acc) inferArgs $ lookup calledFunctionName (functionTypes config) where
             inferArgs : InferredFunctionType -> (InferredType, InferredTypeStore)
             inferArgs fty = (Ref thunkClass, inferFunctionAppArgs (locsType fty) acc args)

    inferExp config acc (SApp False f args) =
      maybe (inferredObjectType, acc) inferArgs $ lookup (jname f) (functionTypes config) where
        inferArgs : InferredFunctionType -> (InferredType, InferredTypeStore)
        inferArgs fty = (returnType fty, inferFunctionAppArgs (locsType fty) acc args)

    inferExp config acc (SLet (Loc i) v sc) =
        let (vty, nacc) = inferExp config acc v
            naccWithI = addType nacc i vty
        in inferExp config naccWithI sc

    inferExp config acc (SUpdate _ e) = inferExp config acc e
    inferExp config acc (SProj (Loc v) i) = (inferredObjectType, addType acc v inferredIdrisObjectType)

    inferExp config acc (SCon _ 0 "Prelude.Bool.False" []) = (IBool, acc)
    inferExp config acc (SCon _ 1 "Prelude.Bool.True" []) = (IBool, acc)

    inferExp config acc (SCon _ 0 "Prelude.Maybe.Nothing" []) = (inferredObjectType, acc)
    inferExp config acc (SCon _ 1 "Prelude.Maybe.Just" [(Loc v)]) = (inferredObjectType, acc)

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

    inferExp config acc (SCase _ e [SConCase lv _ _ args expr]) = inferAltConCase config acc lv args expr

    inferExp config acc (SCase _ e [SDefaultCase defaultExpr]) = inferExp config acc defaultExpr

    inferExp config acc (SCase _ e alts) =
        if any defaultCase alts
            then inferSwitch config acc e alts
            else inferSwitch config acc e (alts ++ [SDefaultCase SNothing])

    inferExp config acc (SChkCase e [SDefaultCase defaultExpr]) = inferExp config acc defaultExpr
    inferExp config acc (SChkCase e alts) = inferSwitch config acc e alts

    inferExp config acc (SConst c) = inferConstExp acc c

    inferExp config acc (SOp op args) = inferOp acc op args

    inferExp config acc SNothing = (inferredObjectType, acc)

    inferExp config acc (SError x) = (inferredObjectType, acc)

    inferExp config acc (SForeign returns fdesc args) = case parseDescriptor returns fdesc args of
        JLambda clazz _ => (Ref clazz, acc)
        _ =>
            let argsWithTypes = (\(fdesc, lvar) => (fdescFieldDescriptor fdesc, lvar)) <$> args
                returnDesc = fdescTypeDescriptor returns
            in (typeDescriptorToInferredType returnDesc, inferForeignArgDescs acc argsWithTypes)

inferFun : SortedMap JMethodName InferredFunctionType -> SDecl -> InferredFunctionType
inferFun types (SFun name args locs def) =
  let jmethodName = jname name
      config = MkInferenceConfig jmethodName types
      initialArgsTypes = fromMaybe SortedMap.empty $ locsType <$> lookup jmethodName types
      (returnType, locsType) = inferExp config initialArgsTypes def
  in MkInferredFunctionType jmethodName returnType locsType (length args)
