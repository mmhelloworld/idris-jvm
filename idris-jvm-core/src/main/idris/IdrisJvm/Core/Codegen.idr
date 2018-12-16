module IdrisJvm.Core.Codegen

import IdrisJvm.IO
import IdrisJvm.Core.Asm
import IdrisJvm.IR.Types
import IdrisJvm.Core.Common
import IdrisJvm.Core.Constant
import IdrisJvm.Core.Function
import IdrisJvm.Core.JAsm
import IdrisJvm.Core.Inference
import IdrisJvm.IR.Exports
import Data.SortedMap

%access public export

generateDependencyMethods : Assembler -> AsmState -> JVM_IO (SortedMap JMethodName InferredFunctionType)
generateDependencyMethods assembler state@(MkAsmState [] _ _ _) = pure (functionTypes state)
generateDependencyMethods assembler state@(MkAsmState (subroutine :: restSubroutines) _ _ _) = do
  let asmState = record { subroutines = [] } state
  (_, newState) <- runAsm asmState assembler subroutine
  let nextState = record { subroutines $= (restSubroutines ++) } newState
  generateDependencyMethods assembler nextState

generateMethod : Assembler -> SortedMap JMethodName InferredFunctionType -> SDecl
              -> JVM_IO (SortedMap JMethodName InferredFunctionType)
generateMethod assembler functionTypes (SFun name args locs def) = do
  let jmethodName = jname name
  let fname = jmethName jmethodName
  let clsName = jmethClsName jmethodName
  let asmState = MkAsmState [] functionTypes SortedMap.empty IUnknown
  (_, newState) <- runAsm asmState assembler $ cgFun [Public, Static] name clsName fname args locs def
  generateDependencyMethods assembler newState

group : Nat -> List a -> List (List a)
group _ [] = []
group n xs =
    let (ys, zs) = splitAt n xs
    in  ys :: group n zs

splitApplyFunction : String -> List String -> Int -> LVar -> List SAlt -> List SDecl
splitApplyFunction fname args n var cases =
  let cs = group 100 cases
      len = List.length cs
  in map (callNextFn fname args n var len) . List.zip [0 .. pred len] $ cs where

    callNextFn : String -> List String -> Int -> LVar -> Nat -> (Nat, List SAlt) -> SDecl
    callNextFn fname args n caseVar last (index, cs) = SFun currFn args n body where
      nextFn : String
      nextFn = fname ++ "$" ++ show (index + 1)

      currFn : String
      currFn = if index == 0 then fname else (fname ++ "$" ++ show index)

      body = SChkCase caseVar $ if index + 1 == last then cs else (cs ++ [SDefaultCase (SApp True nextFn [Loc 0, Loc 1])])

splitLargeFunctions : List SDecl -> List SDecl
splitLargeFunctions decls = decls >>= f where
  f : SDecl -> List SDecl
  f decl@(SFun "{APPLY_0}" args n (SChkCase var cases)) = splitApplyFunction "{APPLY_0}" args n var cases
  f decl = [decl]

inferFuns : SortedMap JMethodName InferredFunctionType
         -> List SDecl
         -> SortedMap JMethodName InferredFunctionType
inferFuns initial decls = go SortedMap.empty decls where
    go : SortedMap JMethodName InferredFunctionType
      -> List SDecl
      -> SortedMap JMethodName InferredFunctionType
    go acc [] = acc
    go acc (decl :: rest) =
        let ftype = inferFun initial decl
            newAcc = SortedMap.insert (functionName ftype) ftype acc
        in go newAcc rest

createMainMethod : Asm ()
createMainMethod = do
    let signature = "()Lio/github/mmhelloworld/idrisjvm/runtime/Thunk;"
    let MkJMethodName className methodName = jname "{runMain_0}"
    CreateMethod [Public, Static] className "main" "([Ljava/lang/String;)V" Nothing Nothing [] []
    MethodCodeStart
    Aload 0
    InvokeMethod InvokeStatic (rtClass "Runtime") "setProgramArgs" "([Ljava/lang/String;)V" False
    InvokeMethod InvokeStatic className methodName signature False
    Pop
    Return
    MaxStackAndLocal (-1) (-1)
    MethodCodeEnd

generateMethods : Assembler -> List SDecl -> List ExportIFace -> JVM_IO ()
generateMethods assembler decls exports = do
    let smallDecls = splitLargeFunctions decls
    let functionTypesByNameStage1 = inferFuns SortedMap.empty smallDecls
    let functionTypesByName = inferFuns functionTypesByNameStage1 smallDecls
    types <- generateMethodsWithTypes functionTypesByName smallDecls
    generateExportsWithTypes types exports
    let asmState = MkAsmState [] types SortedMap.empty IUnknown
    _ <- runAsm asmState assembler createMainMethod
    pure ()
  where
    generateMethodsWithTypes : SortedMap JMethodName InferredFunctionType -> List SDecl
                            -> JVM_IO (SortedMap JMethodName InferredFunctionType)
    generateMethodsWithTypes types [] = pure types
    generateMethodsWithTypes types (decl :: rest) = do
        newTypes <- generateMethod assembler types decl
        generateMethodsWithTypes newTypes rest

    generateExportsWithTypes : SortedMap JMethodName InferredFunctionType -> List ExportIFace -> JVM_IO ()
    generateExportsWithTypes types [] = pure ()
    generateExportsWithTypes types (exportIface :: exports) = do
        let asmState = MkAsmState [] types SortedMap.empty IUnknown
        (_, _) <- runAsm asmState assembler $ exportCode types exportIface
        generateExportsWithTypes types exports

exports : FFI_Export FFI_JVM "IdrisJvm/Core/export/Codegen" []
exports =
  Data SDecl "IdrisJvm/IR/export/SDecl" $
  Data FDesc "IdrisJvm/IR/export/FDesc" $
  Data SExp "IdrisJvm/IR/export/SExp" $
  Data Const "IdrisJvm/IR/export/Const" $
  Data LVar "IdrisJvm/IR/export/LVar" $
  Data CaseType "IdrisJvm/IR/export/CaseType" $
  Data Export "IdrisJvm/IR/export/Export" $
  Data NativeTy "IdrisJvm/IR/export/NativeTy" $
  Data ExportIFace "IdrisJvm/IR/export/ExportIFace" $
  Data IntTy ("IdrisJvm/IR/export/IntTy") $
  Data ArithTy ("IdrisJvm/IR/export/ArithTy") $
  Data PrimFn ("IdrisJvm/IR/export/PrimFn") $
  Data SAlt "IdrisJvm/IR/export/SAlt" $
  Data (FDesc, LVar) "IdrisJvm/IR/export/SForeignArg" $
  Data (List SAlt) "idris/prelude/list/ListSAlt" $
  Data (List (FDesc, LVar)) "IdrisJvm/IR/export/SForeignArgs" $
  Data (Maybe LVar) "IdrisJvm/IR/export/MaybeLVar" $
  Data (List String) "idris/prelude/list/ListString" $
  Data (List FDesc) "idris/prelude/list/ListFDesc" $
  Data (List LVar) "idris/prelude/list/ListLVar" $
  Data (List Export) "idris/prelude/list/ListExport" $
  Data (List SDecl) "IdrisJvm/IR/export/ListSDecl" $
  Data (List ExportIFace) "IdrisJvm/IR/export/ListExportIFace" $

  Fun consSDecl (ExportStatic "consSDecl") $
  Fun emptySDecl (ExportStatic "emptySDecl") $

  Fun consExportIFace (ExportStatic "consExportIFace") $
  Fun emptyExportIFace (ExportStatic "emptyExportIFace") $

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
  Fun Exports.exportFun (ExportStatic "exportFun") $

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

  Fun generateMethods (ExportStatic "generateMethods") $
  End


