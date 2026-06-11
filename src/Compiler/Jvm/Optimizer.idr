module Compiler.Jvm.Optimizer

import Algebra
import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.TailRec

import Control.Monad.Reader
import Control.Monad.State

import Compiler.Common
import Core.CompileExpr
import Core.Context
import Core.Core
import Core.Name
import Core.Reflect
import Core.TT
import Core.TT.Primitive
import Core.Env

import Idris.Pretty.Annotations
import Idris.Syntax
import Idris.Resugar
import Idris.Doc.String

import Libraries.Data.SortedMap
import Libraries.Data.SortedSet
import Data.List
import Data.List.Lazy
import Data.Maybe
import Data.String
import Data.Vect
import Debug.Trace

import Compiler.Jvm.Asm
import Compiler.Jvm.ExtPrim
import Compiler.Jvm.Foreign
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.ShowUtil

%hide Core.Name.Scoped.Scope
%hide Compiler.TailRec.Function.fc
%hide Compiler.TailRec.TcFunction.fc
%hide Libraries.Text.PrettyPrint.Prettyprinter.Util.words

namespace InferredPrimType
  export
  getInferredType : PrimType -> InferredType
  getInferredType Bits64Type = ILong
  getInferredType Int64Type = ILong
  getInferredType IntegerType = inferredBigIntegerType
  getInferredType DoubleType = IDouble
  getInferredType StringType = inferredStringType
  getInferredType CharType = IChar
  getInferredType _ = IInt

export
getFArgs : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core (List (NamedCExp, NamedCExp))
getFArgs (NmCon fc _ _ (Just 0) _) = pure []
getFArgs (NmCon fc _ _ (Just 1) [ty, val, rest]) = pure $ (ty, val) :: !(getFArgs rest)
getFArgs arg = throw (GenericMsg (getFC arg) ("Badly formed jvm call argument list " ++ show arg))

getLineNumbers : FilePos -> FilePos -> (Int, Int)
getLineNumbers (lineStart, _) (lineEnd, colEnd) =
    (lineStart + 1, if colEnd == 1 then lineEnd else lineEnd + 1)

getFileName : OriginDesc -> String
getFileName (PhysicalIdrSrc moduleIdent) = case unsafeUnfoldModuleIdent moduleIdent of
  (moduleName :: _) => moduleName ++ ".idr"
  _ => "(unknown-source)"
getFileName (PhysicalPkgSrc fname) = fname
getFileName (Virtual Interactive) = "(Interactive)"

getSourceLocationFromOriginDesc : OriginDesc -> FilePos -> FilePos -> (String, Int, Int)
getSourceLocationFromOriginDesc originDesc startPos endPos = (getFileName originDesc, getLineNumbers startPos endPos)

export
getSourceLocation : NamedCExp -> (String, Int, Int)
getSourceLocation expr = case getFC expr of
    EmptyFC => case expr of
        (NmExtPrim _ _ (arg :: args)) => getSourceLocation arg
        (NmOp _ _ (arg :: _)) => getSourceLocation arg
        _ => ("Main.idr", 1, 1)
    (MkVirtualFC originDesc startPos endPos) => getSourceLocationFromOriginDesc originDesc startPos endPos
    (MkFC originDesc startPos endPos) => getSourceLocationFromOriginDesc originDesc startPos endPos

export
getSourceLocationFromFc : FC -> (String, Int, Int)
getSourceLocationFromFc EmptyFC = ("Main.idr", 1, 1)
getSourceLocationFromFc (MkVirtualFC originDesc startPos endPos) =
    getSourceLocationFromOriginDesc originDesc startPos endPos
getSourceLocationFromFc (MkFC originDesc startPos endPos) =
    getSourceLocationFromOriginDesc originDesc startPos endPos

mutual
    export
    used : String -> NamedCExp -> Bool
    used n (NmLocal fc var) = n == jvmSimpleName var
    used n (NmRef _ _) = False
    used n (NmLam _ param sc) = n == jvmSimpleName param || used n sc
    used n (NmLet _ var value sc) = n == jvmSimpleName var || used n value || used n sc
    used n (NmApp _ f args) = used n f || any (used n) args
    used n (NmCon _ _ _ _ args) = any (used n) args
    used n (NmOp _ _ args) = any (used n) $ toList args
    used n (NmExtPrim _ _ args) = any (used n) args
    used n (NmForce _ _ t) = used n t
    used n (NmDelay _ _ t) = used n t
    used n (NmConCase _ sc alts def)
        = used n sc || any (usedCon n) alts
              || maybe False (used n) def
    used n (NmConstCase _ sc alts def)
        = used n sc || any (usedConst n) alts
              || maybe False (used n) def
    used n _ = False

    usedCon : String -> NamedConAlt -> Bool
    usedCon n (MkNConAlt _ _ _ _ sc) = used n sc

    usedConst : String -> NamedConstAlt -> Bool
    usedConst n (MkNConstAlt _ sc) = used n sc

export
extractedMethodArgumentName : Name
extractedMethodArgumentName = UN (Basic "$jvm$arg")

%inline
maxCasesInMethod : Int
maxCasesInMethod = 5

appliedLambdaSwitchIndicator : FC
appliedLambdaSwitchIndicator = MkFC (PhysicalPkgSrc "$jvmAppliedLambdaSwitch$") (0, 0) (0, 0)

appliedLambdaLetIndicator : FC
appliedLambdaLetIndicator = MkFC (PhysicalPkgSrc "$jvmAppliedLambdaLet$") (0, 0) (0, 0)

data AppliedLambdaType = AppliedLambdaSwitch | AppliedLambdaLet | AppliedLambdaUnknown

Eq AppliedLambdaType where
    AppliedLambdaSwitch == AppliedLambdaSwitch = True
    AppliedLambdaLet == AppliedLambdaLet = True
    AppliedLambdaUnknown == AppliedLambdaUnknown = True
    _ == _ = False

getAppliedLambdaType : FC -> AppliedLambdaType
getAppliedLambdaType fc =
    if fc == appliedLambdaSwitchIndicator then AppliedLambdaSwitch
    else if fc == appliedLambdaLetIndicator then AppliedLambdaLet
    else AppliedLambdaUnknown

export
appendToJvmName : String -> Jname -> Name
appendToJvmName suffix jname =
  let className = replace (className jname) '/' '.'
  in NS (mkNamespace className) (UN $ Basic (methodName jname ++ suffix))

export
extractedFunctionLabel : String
extractedFunctionLabel = "$idrisjvm$extr"

record SplitFunctionState where
  constructor MkSplitFunctionState
  caseCount: Int
  functionName: Jname
  functionIndex: Int
  variables : List Name
  functions: List TailRec.Function

mutual
    -- This is to extract `switch` cases and `let` expressions in non-tail positions into a separate function
    -- as they cannot be compiled as expressions with JVM switch or variable assignment.
    -- The extracted function will have the switch cases `return` values directly.
    -- This will also extract cases into new function if there are more than 5 cases to avoid JVM's method being too
    -- large error.
    goSplitFunction : (isTailPosition: Bool) -> NamedCExp -> State SplitFunctionState NamedCExp
    goSplitFunction False (NmLet fc var value sc) = do
        let extractedMethodArgumentVarName = extractedMethodArgumentName
        let extractedMethodArgumentVar = NmLocal fc extractedMethodArgumentVarName
        liftedValue <- goSplitFunction False value
        let vars = variables !get
        modify {variables $= (var::)}
        liftedSc <- goSplitFunction True sc
        let body = NmLet fc var extractedMethodArgumentVar liftedSc
        modify {variables := vars} -- Reset variables as we exit scope
        pure $ NmApp appliedLambdaLetIndicator (NmLam fc extractedMethodArgumentVarName body) [liftedValue]
    goSplitFunction True (NmLet fc var value sc) = do
      splitValue <- goSplitFunction False value
      modify {variables $= (var::)}
      pure $ NmLet fc var splitValue !(goSplitFunction True sc)
    goSplitFunction False (NmConCase fc sc alts def) = do
        modify { caseCount $= succ }
        let var = extractedMethodArgumentName
        liftedSc <- goSplitFunction False sc
        liftedAlts <- traverse goSplitFunctionCon alts
        liftedDef <- traverse goSplitFunctionDefault def
        pure $ NmApp appliedLambdaSwitchIndicator
            (NmLam fc var (NmConCase fc (NmLocal fc var) liftedAlts liftedDef)) [liftedSc]
    goSplitFunction True expr@(NmConCase fc sc alts def) = do
        let cases = caseCount !get
        modify { caseCount $= succ }
        if cases > maxCasesInMethod
            then extract fc expr
            else do
                liftedSc <- goSplitFunction False sc
                liftedAlts <- traverse goSplitFunctionCon alts
                liftedDef <- traverse goSplitFunctionDefault def
                pure $ NmConCase fc liftedSc liftedAlts liftedDef
    goSplitFunction False (NmConstCase fc sc alts def) = do
        modify { caseCount $= succ }
        let var = extractedMethodArgumentName
        liftedAlts <- traverse goSplitFunctionConst alts
        liftedDef <- traverse goSplitFunctionDefault def
        liftedSc <- goSplitFunction False sc
        pure $ NmApp appliedLambdaSwitchIndicator
            (NmLam fc var $ NmConstCase fc (NmLocal fc var) liftedAlts liftedDef) [liftedSc]
    goSplitFunction True expr@(NmConstCase fc sc alts def) = do
        let cases = caseCount !get
        modify { caseCount $= succ }
        if cases > maxCasesInMethod
            then extract fc expr
            else do
                liftedSc <- goSplitFunction False sc
                liftedAlts <- traverse goSplitFunctionConst alts
                liftedDef <- traverse goSplitFunctionDefault def
                pure $ NmConstCase fc liftedSc liftedAlts liftedDef
    goSplitFunction _ (NmLam fc param sc) = do
      let vars = variables !get
      let oldCaseCount = caseCount !get
      modify {variables $= (param::), caseCount := 0}
      let splitExpr = NmLam fc param !(goSplitFunction True sc)
      modify {variables := vars, caseCount := oldCaseCount}
      pure splitExpr
    goSplitFunction _ (NmApp fc f args) =
        pure $ NmApp fc !(goSplitFunction False f) !(traverse (goSplitFunction False) args)
    goSplitFunction _ expr@(NmCon fc name conInfo tag args) =
        pure $ NmCon fc name conInfo tag !(traverse (goSplitFunction False) args)
    goSplitFunction _ (NmOp fc f args) = pure $ NmOp fc f !(traverse (goSplitFunction False) args)
    goSplitFunction _ (NmExtPrim fc f args) = pure $ NmExtPrim fc f !(traverse (goSplitFunction False) args)
    goSplitFunction _ (NmForce fc reason t) = pure $ NmForce fc reason !(goSplitFunction False t)
    goSplitFunction _ (NmDelay fc reason t) = pure $ NmDelay fc reason !(goSplitFunction True t)
    goSplitFunction _ expr = pure expr

    goSplitFunctionDefault : NamedCExp -> State SplitFunctionState NamedCExp
    goSplitFunctionDefault body = goSplitFunction True body

    goSplitFunctionCon : NamedConAlt -> State SplitFunctionState NamedConAlt
    goSplitFunctionCon (MkNConAlt n conInfo tag args body) = do
      let usedVars = filter (flip used body . jvmSimpleName) args
      let vars = variables !get
      modify {variables $= (usedVars ++)}
      let splitExpr = MkNConAlt n conInfo tag args !(goSplitFunction True body)
      modify {variables := vars} -- Reset variables as we exit scope
      pure splitExpr

    goSplitFunctionConst : NamedConstAlt -> State SplitFunctionState NamedConstAlt
    goSplitFunctionConst (MkNConstAlt constant body) = do
      let vars = variables !get
      let splitExpr = MkNConstAlt constant !(goSplitFunction True body)
      modify {variables := vars} -- Reset variables as we exit scope
      pure splitExpr

    extract : FC -> NamedCExp -> State SplitFunctionState NamedCExp
    extract fc expr = do
      let extractedFunctionIndex = functionIndex !get
      let oldCaseCount = caseCount !get
      modify { functionIndex $= succ, caseCount := 0 }
      let vars = variables !get
      let functionName = functionName !get
      let extractedFunctionName = appendToJvmName (extractedFunctionLabel ++ show extractedFunctionIndex) functionName
      body <- goSplitFunction True expr
      modify { caseCount := oldCaseCount }
      let usedVars = filter (flip used body . jvmSimpleName) vars
      let newFunction = MkFunction extractedFunctionName fc usedVars body
      modify {functions $= (newFunction ::)}
      pure $ NmApp fc (NmRef fc extractedFunctionName) (NmLocal fc <$> usedVars)

splitFunction : Jname -> List Name -> NamedCExp -> (NamedCExp, List TailRec.Function)
splitFunction functionName args expr =
  let initialState = MkSplitFunctionState 0 functionName 0 args []
      (state, expr) = runState initialState (goSplitFunction True expr)
  in (expr, state.functions)

mutual
    doGetFreeVariables : SortedSet String -> SortedSet String -> NamedCExp -> SortedSet String
    doGetFreeVariables freeVariables boundVariables (NmLocal fc var) =
        let varName = jvmSimpleName var
        in
            if SortedSet.contains varName boundVariables
                then freeVariables
                else SortedSet.insert varName freeVariables
    doGetFreeVariables freeVariables boundVariables (NmLam _ parameterName body) =
        doGetFreeVariables freeVariables (SortedSet.insert (jvmSimpleName parameterName) boundVariables) body
    doGetFreeVariables freeVariables boundVariables (NmLet _ var value body) =
        let newFreeVariables = doGetFreeVariables freeVariables boundVariables value
            newBoundVariables = SortedSet.insert (jvmSimpleName var) boundVariables
        in doGetFreeVariables newFreeVariables newBoundVariables body
    doGetFreeVariables freeVariables boundVariables (NmApp _ (NmRef _ _) args) =
        getExpressionsFreeVariables freeVariables boundVariables args
    doGetFreeVariables freeVariables boundVariables (NmApp _ f args) =
        getExpressionsFreeVariables freeVariables boundVariables (f :: args)
    doGetFreeVariables freeVariables boundVariables (NmCon _ _ _ _ args) =
        getExpressionsFreeVariables freeVariables boundVariables args
    doGetFreeVariables freeVariables boundVariables (NmOp _ _ args) =
        getExpressionsFreeVariables freeVariables boundVariables $ toList args
    doGetFreeVariables freeVariables boundVariables (NmExtPrim _ _ args) =
        getExpressionsFreeVariables freeVariables boundVariables args
    doGetFreeVariables freeVariables boundVariables (NmForce _ _ t) =
        doGetFreeVariables freeVariables boundVariables t
    doGetFreeVariables freeVariables boundVariables (NmDelay _ _ t) =
        doGetFreeVariables freeVariables boundVariables t
    doGetFreeVariables freeVariables boundVariables (NmConCase _ sc alts def) =
        let switchExprFreeVariables = doGetFreeVariables freeVariables boundVariables sc
            altsFreeVariables = doGetFreeVariablesCon switchExprFreeVariables boundVariables alts
        in maybe altsFreeVariables (doGetFreeVariables altsFreeVariables boundVariables) def
    doGetFreeVariables freeVariables boundVariables (NmConstCase _ sc alts def) =
        let switchExprFreeVariables = doGetFreeVariables freeVariables boundVariables sc
            altsFreeVariables = doGetFreeVariablesConst switchExprFreeVariables boundVariables alts
        in maybe altsFreeVariables (doGetFreeVariables altsFreeVariables boundVariables) def
    doGetFreeVariables freeVariables _ _ = freeVariables

    getExpressionsFreeVariables : SortedSet String -> SortedSet String -> List NamedCExp -> SortedSet String
    getExpressionsFreeVariables freeVariables boundVariables expressions = go freeVariables expressions where
        go : SortedSet String -> List NamedCExp -> SortedSet String
        go freeVariables [] = freeVariables
        go freeVariables (arg :: args) = go (doGetFreeVariables freeVariables boundVariables arg) args

    doGetFreeVariablesCon : SortedSet String -> SortedSet String -> List NamedConAlt -> SortedSet String
    doGetFreeVariablesCon freeVariables _ [] = freeVariables
    doGetFreeVariablesCon freeVariables boundVariables ((MkNConAlt _ _ _ properties sc) :: rest) =
        let newBoundVariables = SortedSet.union boundVariables (SortedSet.fromList (jvmSimpleName <$> properties))
        in doGetFreeVariablesCon (doGetFreeVariables freeVariables newBoundVariables sc) boundVariables rest

    doGetFreeVariablesConst : SortedSet String -> SortedSet String -> List NamedConstAlt -> SortedSet String
    doGetFreeVariablesConst freeVariables _ [] = freeVariables
    doGetFreeVariablesConst freeVariables boundVariables ((MkNConstAlt _ sc) :: rest) =
        doGetFreeVariablesConst (doGetFreeVariables freeVariables boundVariables sc) boundVariables rest

getFreeVariables : SortedSet String -> NamedCExp -> SortedSet String
getFreeVariables boundVariables expr = doGetFreeVariables SortedSet.empty boundVariables expr

mutual
    markTailRecursion : NamedCExp -> Reader (Jname, String) NamedCExp
    markTailRecursion expr@(NmApp fc (NmRef nameFc idrisName) args) =
        let functionName = jvmName (snd !ask) idrisName
        in if functionName == fst !ask
               then pure $ NmApp fc (NmRef nameFc idrisTailRecName) args
               else pure expr
    markTailRecursion expr@(NmLet fc var value body) =
        NmLet fc var value <$> markTailRecursion body
    markTailRecursion expr@(NmConCase fc sc alts def) = do
        tailRecursionMarkedAlts <- traverse markTailRecursionConAlt alts
        tailRecursionMarkedDefault <- traverse markTailRecursion def
        pure (NmConCase fc sc tailRecursionMarkedAlts tailRecursionMarkedDefault)
    markTailRecursion (NmConstCase fc sc alts def) = do
        tailRecursionMarkedAlts <- traverse markTailRecursionConstAlt alts
        tailRecursionMarkedDefault <- traverse markTailRecursion def
        pure (NmConstCase fc sc tailRecursionMarkedAlts tailRecursionMarkedDefault)
    markTailRecursion expr = pure expr

    markTailRecursionConAlt : NamedConAlt -> Reader (Jname, String) NamedConAlt
    markTailRecursionConAlt (MkNConAlt name conInfo tag args caseBody) =
        MkNConAlt name conInfo tag args <$> markTailRecursion caseBody

    markTailRecursionConstAlt : NamedConstAlt -> Reader (Jname, String) NamedConstAlt
    markTailRecursionConstAlt (MkNConstAlt constant caseBody) = MkNConstAlt constant <$> markTailRecursion caseBody

exitInferenceScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Core ()
exitInferenceScope scopeIndex = updateCurrentScopeIndex scopeIndex

enterInferenceScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Int -> Core ()
enterInferenceScope lineNumberStart lineNumberEnd = do
    parentScopeIndex <- getCurrentScopeIndex
    scopeIndex <- newScopeIndex
    parentScope <- getScope parentScopeIndex
    variableTypes <- coreLift $ Map.newTreeMap {key=String} {value=InferredType}
    allVariableTypes <- coreLift $ Map.newTreeMap {key=Int} {value=InferredType}
    variableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
    allVariableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
    let newScope =
        MkScope scopeIndex (Just parentScopeIndex) variableTypes allVariableTypes variableIndices
            allVariableIndices IUnknown (nextVariableIndex parentScope) (lineNumberStart, lineNumberEnd) ("", "") []
    addScopeChild parentScopeIndex scopeIndex
    saveScope newScope
    updateCurrentScopeIndex scopeIndex

createLambdaClosureScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Int -> List String -> Scope -> Core Scope
createLambdaClosureScope scopeIndex childScopeIndex closureVariables parentScope = do
    lambdaClosureVariableIndices <- coreLift $ Map.fromList $ getLambdaClosureVariableIndices [] 0 closureVariables
    variableTypes <- coreLift $ Map.newTreeMap {key=String} {value=InferredType}
    allVariableTypes <- coreLift $ Map.newTreeMap {key=Int} {value=InferredType}
    allVariableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
    pure $ MkScope scopeIndex (Just $ index parentScope) variableTypes allVariableTypes
        lambdaClosureVariableIndices allVariableIndices IUnknown (cast $ length closureVariables)
        (lineNumbers parentScope) ("", "") [childScopeIndex]
  where
    getLambdaClosureVariableIndices : List (String, Int) -> Int -> List String -> List (String, Int)
    getLambdaClosureVariableIndices acc _ [] = acc
    getLambdaClosureVariableIndices acc index (var :: vars) =
        getLambdaClosureVariableIndices ((var, index) :: acc) (index + 1) vars

enterInferenceLambdaScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Int -> Maybe Name -> NamedCExp -> Core ()
enterInferenceLambdaScope lineNumberStart lineNumberEnd parameterName expr = do
        parentScopeIndex <- getCurrentScopeIndex
        scopeIndex <- newScopeIndex
        let boundVariables = maybe SortedSet.empty (flip SortedSet.insert SortedSet.empty . jvmSimpleName) parameterName
        let freeVariables = getFreeVariables boundVariables expr
        let usedVariables = filter (flip SortedSet.contains freeVariables) !(retrieveVariables parentScopeIndex)
        variableTypes <- coreLift $ Map.newTreeMap {key=String} {value=InferredType}
        allVariableTypes <- coreLift $ Map.newTreeMap {key=Int} {value=InferredType}
        variableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
        allVariableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
        newScope <- case usedVariables  of
            nonEmptyUsedVariables@(_ :: _) => do
                parentScope <- getScope parentScopeIndex
                lambdaParentScopeIndex <- newScopeIndex
                closureScope <- createLambdaClosureScope lambdaParentScopeIndex scopeIndex nonEmptyUsedVariables
                    parentScope
                saveScope closureScope
                let closureVariableCount = nextVariableIndex closureScope
                pure $ MkScope scopeIndex (Just lambdaParentScopeIndex) variableTypes allVariableTypes
                    variableIndices allVariableIndices IUnknown closureVariableCount (lineNumberStart, lineNumberEnd)
                    ("", "") []
            [] => pure $ MkScope scopeIndex Nothing variableTypes allVariableTypes variableIndices allVariableIndices
                IUnknown 0 (lineNumberStart, lineNumberEnd) ("", "") []
        saveScope newScope
        updateCurrentScopeIndex scopeIndex

withInferenceScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Int -> Core result -> Core result
withInferenceScope lineNumberStart lineNumberEnd op = do
    scopeIndex <- getCurrentScopeIndex
    enterInferenceScope lineNumberStart lineNumberEnd
    result <- op
    exitInferenceScope scopeIndex
    pure result

withInferenceLambdaScope : {auto stateRef: Ref AsmState AsmState} -> Int -> Int -> Maybe Name -> NamedCExp
                         -> Core result -> Core result
withInferenceLambdaScope lineNumberStart lineNumberEnd parameterName expr op = do
    scopeIndex <- getCurrentScopeIndex
    enterInferenceLambdaScope lineNumberStart lineNumberEnd parameterName expr
    result <- op
    exitInferenceScope scopeIndex
    pure result

public export
data LambdaType = DelayedLambda | FunctionLambda | Function2Lambda | Function3Lambda | Function4Lambda |
                    Function5Lambda

export
Eq LambdaType where
    DelayedLambda == DelayedLambda = True
    FunctionLambda == FunctionLambda = True
    Function2Lambda == Function2Lambda = True
    Function3Lambda == Function3Lambda = True
    Function4Lambda == Function4Lambda = True
    Function5Lambda == Function5Lambda = True
    _ == _ = False

export
Show LambdaType where
  show DelayedLambda = "DelayedLambda"
  show FunctionLambda = "FunctionLambda"
  show Function2Lambda = "Function2Lambda"
  show Function3Lambda = "Function3Lambda"
  show Function4Lambda = "Function4Lambda"
  show Function5Lambda = "Function5Lambda"

export
getLambdaTypeByParameter : (parameterName: Maybe Name) -> LambdaType
getLambdaTypeByParameter Nothing = DelayedLambda
getLambdaTypeByParameter _ = FunctionLambda

export
getLambdaInterfaceMethodName : LambdaType -> String
getLambdaInterfaceMethodName DelayedLambda = "evaluate"
getLambdaInterfaceMethodName _ = "apply"

export
getSamDesc : LambdaType -> String
getSamDesc DelayedLambda = "()Ljava/lang/Object;"
getSamDesc FunctionLambda = getMethodDescriptor $ MkInferredFunctionType inferredObjectType [inferredObjectType]
getSamDesc Function2Lambda =
  getMethodDescriptor $ MkInferredFunctionType inferredObjectType $ replicate 2 inferredObjectType
getSamDesc Function3Lambda =
  getMethodDescriptor $ MkInferredFunctionType inferredObjectType $ replicate 3 inferredObjectType
getSamDesc Function4Lambda =
  getMethodDescriptor $ MkInferredFunctionType inferredObjectType $ replicate 4 inferredObjectType
getSamDesc Function5Lambda =
  getMethodDescriptor $ MkInferredFunctionType inferredObjectType $ replicate 5 inferredObjectType

export
getLambdaInterfaceType : LambdaType -> InferredType
getLambdaInterfaceType DelayedLambda = delayedType
getLambdaInterfaceType _ = inferredLambdaType

export
getLambdaImplementationMethodReturnType : LambdaType -> InferredType
getLambdaImplementationMethodReturnType _ = inferredObjectType

export
getConstantType : {auto stateRef: Ref AsmState AsmState} -> List NamedConstAlt -> Core InferredType
getConstantType [] = throw $ GenericMsg emptyFC "Unknown constant switch type"
getConstantType ((MkNConstAlt constant _) :: _) = case constant of
    I _ => pure IInt
    I8 _ => pure IInt
    I16 _ => pure IInt
    I32 _ => pure IInt
    I64 _ => pure ILong
    B8 _ => pure IInt
    B16 _ => pure IInt
    B32 _ => pure IInt
    B64 _ => pure ILong
    Ch _ => pure IInt
    Str _ => pure inferredStringType
    BI _ => pure inferredBigIntegerType
    unsupportedConstant => throw $ GenericMsg emptyFC ("Unsupported constant switch " ++ show unsupportedConstant)

export
getIntConstantValue : {auto stateRef: Ref AsmState AsmState} -> FC -> Primitive.Constant -> Core Int
getIntConstantValue _ (I i) = pure i
getIntConstantValue _ (I8 i) = pure (cast i)
getIntConstantValue _ (I16 i) = pure (cast i)
getIntConstantValue _ (I32 i) = pure (cast i)
getIntConstantValue _ (B8 i) = pure (cast i)
getIntConstantValue _ (B16 i) = pure (cast i)
getIntConstantValue _ (B32 i) = pure (cast i)
getIntConstantValue _ (Ch c) = pure $ ord c
getIntConstantValue _ WorldVal = pure 0
getIntConstantValue _ (PrT _) = pure 0
getIntConstantValue fc x = throw $ GenericMsg fc ("Constant " ++ show x ++ " cannot be converted to integer.")

getConstructorTag : ConInfo -> Maybe Int -> Int
getConstructorTag conInfo tag = case conInfo of
  NOTHING => 0
  NIL => 0
  JUST => 1
  CONS => 1
  _ => fromMaybe 0 tag

export
getConstructorType : String -> Name -> ConInfo -> InferredType
getConstructorType programName name _ =
  let constructorClassName = getIdrisConstructorClassName programName (jvmSimpleName name)
  in IRef constructorClassName Class []

sortConCases : List NamedConAlt -> List NamedConAlt
sortConCases alts = sortBy (comparing getTag) alts where
    getTag : NamedConAlt -> Int
    getTag (MkNConAlt _ conInfo tag _ _) = getConstructorTag conInfo tag

export
isTypeCase : NamedConAlt -> Bool
isTypeCase (MkNConAlt _ _ Nothing _ _) = True
isTypeCase _ = False

export
substituteVariableSubMethodBody : NamedCExp -> NamedCExp -> NamedCExp
substituteVariableSubMethodBody variable (NmConCase fc _ alts def) = NmConCase fc variable alts def
substituteVariableSubMethodBody variable (NmConstCase fc _ alts def) = NmConstCase fc variable alts def
substituteVariableSubMethodBody value (NmLet fc var _ sc) = NmLet fc var value sc
substituteVariableSubMethodBody _ expr = expr

combineSwitchTypes : Maybe InferredType -> List InferredType -> InferredType
combineSwitchTypes defaultTy [] = fromMaybe IUnknown defaultTy
combineSwitchTypes defaultTy altTypes@(altTy :: rest) = maybe (go altTy rest) (flip go altTypes) defaultTy where
  go : InferredType -> List InferredType -> InferredType
  go prevTy [] = prevTy
  go prevTy (currTy :: rest) = if prevTy == currTy then go currTy rest else inferredObjectType

createNewVariable : {auto stateRef: Ref AsmState AsmState} -> (variablePrefix: String) -> InferredType -> Core ()
createNewVariable variablePrefix ty = do
    variable <- generateVariable variablePrefix
    addVariableType variable ty

export
canUseMethodReference : {auto stateRef: Ref AsmState AsmState} -> Name -> List Name -> Maybe Name -> List Name -> Core Bool
canUseMethodReference _ _ Nothing _ = pure False
canUseMethodReference functionName args (Just p0) params = do
  Just (MkInferredFunctionType returnType parameterTypes) <- findFunctionType (jvmName !getProgramName functionName)
    | Nothing => throw (GenericMsg emptyFC ("Unable to find function \{show functionName}"))
  pure $ not (any isPrimitive (returnType :: parameterTypes)) && all (uncurry (==)) (zip args (p0 :: params))

export
isIoAction : NamedCExp -> Bool
isIoAction (NmCon _ (UN (Basic "->")) _ _ [argumentType, returnType]) = isIoAction returnType
isIoAction (NmApp _ (NmRef _ name) _) = name == primio "PrimIO"
isIoAction (NmCon _ name _ _ _) = name == primio "IORes"
isIoAction (NmLam fc arg expr) = isIoAction expr
isIoAction expr = False

voidTypeExpr : NamedCExp
voidTypeExpr = NmCon emptyFC (UN (Basic "void")) TYCON Nothing []

tailRecLoopFunctionName : Name
tailRecLoopFunctionName =
  NS (mkNamespace "io.github.mmhelloworld.idrisjvm.runtime.Runtime") (UN $ Basic "tailRec")

-- Derive a typed callback signature from a literal lambda's own body for
-- POLYMORPHIC callback slots (type arguments are erased in NamedCExp, so
-- the declaration cannot pin them — e.g. the `a -> b` slot of `map`).
-- The parameter is pinned to a primitive only when every primitive-typed
-- observation of it (as an argument of a primitive operation) agrees; the
-- return type is read off the body's root expression shape.  Purely
-- syntactic — shared verbatim by inference and emission, and stable under
-- rewriteSpecCalls (which only renames NmRef heads).  Anything unclear
-- (no primitive anywhere, conflicting observations, shadowed binders)
-- yields Nothing and the lambda stays on the natural path.
export
findLambdaCallbackSig : Name -> NamedCExp -> Maybe InferredFunctionType
findLambdaCallbackSig paramName body =
    case nub (filter isPrimitive (observe body)) of
      []   => mkCallbackSig (MkInferredFunctionType (rootReturnType body) [inferredObjectType])
      [ty] => mkCallbackSig (MkInferredFunctionType (rootReturnType body) [ty])
      _    => Nothing
  where
    isParam : NamedCExp -> Bool
    isParam (NmLocal _ n) = n == paramName
    isParam _ = False

    -- Expected argument types of the primitive operations that can pin
    -- the parameter.  Mirrors inferExprOp's typing of the same ops.
    opArgTypes : {arity : _} -> PrimFn arity -> List InferredType
    opArgTypes (Add ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (Sub ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (Mul ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (Div ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (Mod ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (Neg ty) = [getInferredType ty]
    opArgTypes (ShiftL ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (ShiftR ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (BAnd ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (BOr ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (BXOr ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (LT ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (LTE ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (EQ ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (GT ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (GTE ty) = [getInferredType ty, getInferredType ty]
    opArgTypes (Cast ty1 _) = [getInferredType ty1]
    opArgTypes DoubleExp = [IDouble]
    opArgTypes DoubleLog = [IDouble]
    opArgTypes DoublePow = [IDouble, IDouble]
    opArgTypes DoubleSin = [IDouble]
    opArgTypes DoubleCos = [IDouble]
    opArgTypes DoubleTan = [IDouble]
    opArgTypes DoubleASin = [IDouble]
    opArgTypes DoubleACos = [IDouble]
    opArgTypes DoubleATan = [IDouble]
    opArgTypes DoubleSqrt = [IDouble]
    opArgTypes DoubleFloor = [IDouble]
    opArgTypes DoubleCeiling = [IDouble]
    opArgTypes _ = []

    opReturnType : {arity : _} -> PrimFn arity -> InferredType
    opReturnType (Add ty) = getInferredType ty
    opReturnType (Sub ty) = getInferredType ty
    opReturnType (Mul ty) = getInferredType ty
    opReturnType (Div ty) = getInferredType ty
    opReturnType (Mod ty) = getInferredType ty
    opReturnType (Neg ty) = getInferredType ty
    opReturnType (ShiftL ty) = getInferredType ty
    opReturnType (ShiftR ty) = getInferredType ty
    opReturnType (BAnd ty) = getInferredType ty
    opReturnType (BOr ty) = getInferredType ty
    opReturnType (BXOr ty) = getInferredType ty
    opReturnType (LT _) = IBool
    opReturnType (LTE _) = IBool
    opReturnType (EQ _) = IBool
    opReturnType (GT _) = IBool
    opReturnType (GTE _) = IBool
    opReturnType (Cast _ ty2) = getInferredType ty2
    opReturnType DoubleExp = IDouble
    opReturnType DoubleLog = IDouble
    opReturnType DoublePow = IDouble
    opReturnType DoubleSin = IDouble
    opReturnType DoubleCos = IDouble
    opReturnType DoubleTan = IDouble
    opReturnType DoubleASin = IDouble
    opReturnType DoubleACos = IDouble
    opReturnType DoubleATan = IDouble
    opReturnType DoubleSqrt = IDouble
    opReturnType DoubleFloor = IDouble
    opReturnType DoubleCeiling = IDouble
    opReturnType StrLength = IInt
    opReturnType StrHead = IChar
    opReturnType StrIndex = IChar
    opReturnType _ = inferredObjectType

    constReturnType : Primitive.Constant -> InferredType
    constReturnType (I _) = IInt
    constReturnType (I8 _) = IInt
    constReturnType (I16 _) = IInt
    constReturnType (I32 _) = IInt
    constReturnType (I64 _) = ILong
    constReturnType (B8 _) = IInt
    constReturnType (B16 _) = IInt
    constReturnType (B32 _) = IInt
    constReturnType (B64 _) = ILong
    constReturnType (Ch _) = IInt
    constReturnType (Db _) = IDouble
    constReturnType _ = inferredObjectType

    rootReturnType : NamedCExp -> InferredType
    rootReturnType (NmOp _ op _) = opReturnType op
    rootReturnType (NmPrimVal _ c) = constReturnType c
    rootReturnType (NmLet _ _ _ e) = rootReturnType e
    rootReturnType _ = inferredObjectType

    -- A binder that rebinds the parameter name shadows it; stop
    -- descending that branch (conservative).
    mutual
      observeList : List NamedCExp -> List InferredType
      observeList [] = []
      observeList (e :: es) = observe e ++ observeList es

      observeConAlt : NamedConAlt -> List InferredType
      observeConAlt (MkNConAlt _ _ _ argNames e) =
        if any (== paramName) argNames then [] else observe e

      observeConstAlt : NamedConstAlt -> List InferredType
      observeConstAlt (MkNConstAlt _ e) = observe e

      observe : NamedCExp -> List InferredType
      observe (NmLam _ p e) = if p == paramName then [] else observe e
      observe (NmLet _ p value e) =
        observe value ++ (if p == paramName then [] else observe e)
      observe (NmApp _ f args) = observe f ++ observeList args
      observe (NmCon _ _ _ _ args) = observeList args
      observe (NmOp _ op args) =
        let argList = toList args
            here = mapMaybe (\(arg, ty) => if isParam arg then Just ty else Nothing)
                     (zip argList (opArgTypes op))
        in here ++ observeList argList
      observe (NmExtPrim _ _ args) = observeList args
      observe (NmForce _ _ e) = observe e
      observe (NmDelay _ _ e) = observe e
      observe (NmConCase _ sc alts def) =
        observe sc ++ concatMap observeConAlt alts ++ maybe [] observe def
      observe (NmConstCase _ sc alts def) =
        observe sc ++ concatMap observeConstAlt alts ++ maybe [] observe def
      observe _ = []

-- The positional rule shared by inference (inferAppArgs) and emission
-- (assembleExpr's direct-call case): the typed signature for a literal
-- lambda argument at a callback slot.  Concrete decl slots use the
-- declared signature; polymorphic function slots derive one from the
-- lambda body; everything else (including non-lambda arguments) stays
-- natural.
export
callbackSigForArg : CallbackSlot -> NamedCExp -> Maybe InferredFunctionType
callbackSigForArg (CallbackConcrete sig) (NmLam _ _ _) = Just sig
callbackSigForArg CallbackPoly (NmLam _ param body) = findLambdaCallbackSig param body
callbackSigForArg _ _ = Nothing

-- Emission counterpart of inferAppArgs: per-argument EXPECTED types for a
-- direct call.  A literal lambda at a typed callback slot is assembled
-- expecting the typed interface — in both the rewritten (spec callee,
-- slot already the interface) and natural cases, which is sound because
-- the interface extends java.util.function.Function.  Only the expected
-- types are adjusted; the call's method descriptor must keep the callee's
-- actual parameter types.
export
adjustCallbackSlotTypes : (programName : String) -> List CallbackSlot
                       -> List NamedCExp -> List InferredType -> List InferredType
adjustCallbackSlotTypes programName declSlots (arg :: args) (slot :: slots) =
  let adjusted =
        if isJust (parseCallbackIfaceType slot)
          then slot
          else case callbackSigForArg (fromMaybe CallbackNone (head' declSlots)) arg of
                 Just sig => callbackIfaceType programName sig
                 Nothing => slot
  in adjusted :: adjustCallbackSlotTypes programName (drop 1 declSlots) args slots
adjustCallbackSlotTypes _ _ _ slots = slots

export
getJavaLambdaType : {auto stateRef: Ref AsmState AsmState} -> FC -> List NamedCExp -> Core JavaLambdaType
getJavaLambdaType fc [functionType, javaInterfaceType, _] =
    do
      implementationType <- parseFunctionType functionType
      (interfaceTy, methodName, methodType) <- parseJavaInterfaceType javaInterfaceType
      pure $ MkJavaLambdaType interfaceTy methodName methodType implementationType
  where
    parseFunctionType : NamedCExp -> Core InferredFunctionType
    parseFunctionType functionType = do
        types <- go [] functionType
        case types of
          [] => asmCrash ("Invalid Java lambda at " ++ show fc ++ ": " ++ show functionType)
          (returnType :: argTypes) => pure $ MkInferredFunctionType returnType (reverse argTypes)
      where
        go : List InferredType -> NamedCExp -> Core (List InferredType)
        go acc (NmCon _ (UN (Basic "->")) _ _ [argTy, lambdaTy]) = do
          argInferredTy <- tySpec argTy
          restInferredTypes <- go acc lambdaTy
          pure (restInferredTypes ++ (argInferredTy :: acc))
        go acc (NmLam fc arg expr) = go acc expr
        go acc expr@(NmApp _ (NmRef _ name) [arg]) = go (IInt :: acc) (if name == primio "PrimIO" then arg else expr)
        go acc expr = pure (!(tySpec expr) :: acc)

    throwExpectedStructAtPos : Core a
    throwExpectedStructAtPos =
      asmCrash ("Expected a struct containing interface name and method separated by space at " ++ show fc)

    throwExpectedStruct : String -> Core a
    throwExpectedStruct name =
      asmCrash ("Expected a struct containing interface name and method separated by space at " ++
         show fc ++ " but found " ++ name)

    parseJavaInterfaceType : NamedCExp -> Core (InferredType, String, InferredFunctionType)
    parseJavaInterfaceType expr@(NmCon _ name _ _ [interfaceType, methodTypeExp]) =
        if name == builtin "Pair" then
          case interfaceType of
            NmCon _ name _ _ (NmPrimVal _ (Str namePartsStr) :: _) =>
              if name == structName
                then case words namePartsStr of
                  (interfaceName :: methodName :: _) => do
                    methodType <- parseFunctionType methodTypeExp
                    pure (IRef interfaceName Interface [], methodName, methodType)
                  _ => asmCrash ("Expected interface name and method separated by space at " ++ show fc ++ ": " ++
                        namePartsStr)
                else throwExpectedStruct namePartsStr
            _ => throwExpectedStructAtPos
        else asmCrash ("Expected a tuple containing interface type and method type but found: " ++ showNamedCExp 0 expr)
    parseJavaInterfaceType (NmApp _ (NmRef _ name) _) = do
        (_, MkNmFun _ def) <- getFcAndDefinition (jvmSimpleName name)
          | _ => asmCrash ("Expected a function returning a tuple containing interface type and method type at " ++
                   show fc)
        parseJavaInterfaceType def
    parseJavaInterfaceType (NmDelay _ _ expr) = parseJavaInterfaceType expr
    parseJavaInterfaceType expr = asmCrash ("Expected a tuple containing interface type and method type but found: " ++ showNamedCExp 0 expr)

getJavaLambdaType fc exprs = asmCrash ("Invalid Java lambda at " ++ show fc ++ ": " ++ show exprs)

mutual
    inferExpr : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core InferredType
    inferExpr (NmDelay _ _ expr) = inferExprLam AppliedLambdaUnknown Nothing Nothing expr
    inferExpr expr@(NmLocal _ var) = retrieveVariableType (jvmSimpleName var)
    inferExpr (NmRef _ name) = pure IUnknown
    inferExpr app@(NmApp _ (NmRef _ name) args) = inferExprApp app
    inferExpr (NmApp fc (NmLam _ var body) [expr]) =
        inferExprLam (getAppliedLambdaType fc) (Just expr) (Just var) body
    inferExpr (NmLam _ var body) = inferExprLam AppliedLambdaUnknown Nothing (Just var) body
    inferExpr (NmLet fc var value expr) = inferExprLet fc var value expr
    inferExpr app@(NmApp _ _ _) = inferExprApp app
    inferExpr expr@(NmCon fc name conInfo tag args) = inferExprCon fc name conInfo tag args
    inferExpr (NmOp _ fn args) = inferExprOp fn args
    inferExpr (NmExtPrim fc fn args) = inferExtPrim fc (toPrim fn) args
    inferExpr (NmForce _ _ expr) = do
        ignore $ inferExpr expr
        pure inferredObjectType

    inferExpr (NmConCase _ sc [] Nothing) = pure IUnknown
    inferExpr (NmConCase _ sc [] (Just def)) = do
        ignore $ inferConstructorSwitchExpr sc
        inferExpr def
    inferExpr (NmConCase _ sc [MkNConAlt name conInfo _ args expr] Nothing) = do
        discriminantVar <- inferConstructorSwitchExpr sc
        inferConCaseExpr discriminantVar name conInfo args expr
    inferExpr (NmConCase _ sc alts def) = do
        discriminantVar <- inferConstructorSwitchExpr sc
        let hasTypeCase = any isTypeCase alts
        when hasTypeCase $ do
            createNewVariable "constructorCaseExpr" inferredStringType
            createNewVariable "hashCodePosition" IInt
        let sortedAlts = if hasTypeCase then alts else sortConCases alts
        altTypes <- traverse (inferExprConAlt discriminantVar) sortedAlts
        defaultTy <- traverseOpt inferExprWithNewScope def
        pure $ combineSwitchTypes defaultTy altTypes

    inferExpr (NmConstCase fc sc [] Nothing) = pure IUnknown
    inferExpr (NmConstCase fc sc [] (Just expr)) = inferExpr expr
    inferExpr (NmConstCase fc sc alts def) = do
        constantType <- getConstantType alts
        ignore $ inferExpr sc
        when (constantType /= IInt) $ do
            constantExprVariable <- generateVariable "constantCaseExpr"
            addVariableType constantExprVariable constantType
            hashCodePositionVariable <- generateVariable "hashCodePosition"
            addVariableType hashCodePositionVariable IInt
        sortedAlts <- sortConstCases constantType alts
        altTypes <- traverse inferExprConstAlt sortedAlts
        defaultTy <- traverseOpt inferExprWithNewScope def
        pure $ combineSwitchTypes defaultTy altTypes
      where
        getConstant : NamedConstAlt -> Primitive.Constant
        getConstant (MkNConstAlt constant _) = constant

        sortConstCases : InferredType -> List NamedConstAlt -> Core (List NamedConstAlt)
        sortConstCases IInt alts = do
            constValues <- traverse (getIntConstantValue fc . getConstant) alts
            pure $ fst <$> (sortBy (comparing snd) $ zip alts constValues)
        sortConstCases _ alts = pure alts

    inferExpr (NmPrimVal fc (I _)) = pure IInt
    inferExpr (NmPrimVal fc (I8 _)) = pure IInt
    inferExpr (NmPrimVal fc (I16 _)) = pure IInt
    inferExpr (NmPrimVal fc (I32 _)) = pure IInt
    inferExpr (NmPrimVal fc (I64 _)) = pure ILong
    inferExpr (NmPrimVal fc (B8 _)) = pure IInt
    inferExpr (NmPrimVal fc (B16 _)) = pure IInt
    inferExpr (NmPrimVal fc (B32 _)) = pure IInt
    inferExpr (NmPrimVal fc (B64 _)) = pure ILong
    inferExpr (NmPrimVal fc (BI _)) = pure inferredBigIntegerType
    inferExpr (NmPrimVal fc (Str _)) = pure inferredStringType
    inferExpr (NmPrimVal fc (Ch _)) = pure IChar
    inferExpr (NmPrimVal fc (Db _)) = pure IDouble
    inferExpr (NmPrimVal fc _) = pure IInt
    inferExpr (NmErased fc) = pure IInt
    inferExpr (NmCrash fc msg) = pure IUnknown

    -- Process the discriminant of an `NmConCase` and return its variable
    -- name.  When the discriminant is a local (typically a function
    -- parameter), the existing variable type is preserved — `addVariableType`
    -- is a no-op for parameter-bound names, so a spec'd function's
    -- `IRef SpecClass` parameter type survives this step and can be observed
    -- by `inferConCaseExpr` (which uses it to seed bound-variable types
    -- from the spec's slot types).
    inferConstructorSwitchExpr : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core String
    inferConstructorSwitchExpr (NmLocal _ var) = do
        let idrisObjectVariable = jvmSimpleName var
        addVariableType idrisObjectVariable idrisObjectType
        pure idrisObjectVariable
    inferConstructorSwitchExpr sc = do
        idrisObjectVariable <- generateVariable "constructorSwitchValue"
        ignore $ inferExpr sc
        addVariableType idrisObjectVariable idrisObjectType
        pure idrisObjectVariable

    inferExprConstAlt : {auto stateRef: Ref AsmState AsmState} -> NamedConstAlt -> Core InferredType
    inferExprConstAlt (MkNConstAlt _ expr) = inferExprWithNewScope expr

    inferExprWithNewScope : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core InferredType
    inferExprWithNewScope expr = do
         let fc = getFC expr
         let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
         withInferenceScope lineStart lineEnd $ inferExpr expr

    inferConCaseExpr : {auto stateRef: Ref AsmState AsmState} -> String -> Name -> ConInfo -> List Name -> NamedCExp -> Core InferredType
    inferConCaseExpr discriminantVar name conInfo args expr = do
            -- Two paths seed the bound variable types from a spec's slot
            -- types here during inference (`updateScopeVariableTypes` then
            -- propagates them into every scope's `allVariableTypes` so the
            -- body's `NmLocal` loads use the right instruction):
            --
            -- (a) `findByClass`: the discriminant variable is already typed
            --     as an `IRef SpecClass` for the constructor — typical of a
            --     spec'd function whose param type was narrowed.  No
            --     soundness check needed: the JVM already verified the
            --     discriminant is that class at function entry.
            --
            -- (b) `uniqueSafe`: the discriminant has a general type, but
            --     the constructor belongs to a single-constructor data type
            --     (`RECORD`/`UNIT`), has exactly one spec entry, and the
            --     natural class is dead.  The single-constructor guard
            --     matters for soundness in this path: extracting a
            --     primitive-typed field from one variant of a sum type
            --     would otherwise let that variable flow into a slot
            --     another caller fills with a sibling variant's field.
            conPlan <- getConSpecialisationPlan
            natLive <- getNaturalConsLive
            discriminantTy <- retrieveVariableType discriminantVar
            let entries = fromMaybe [] (SortedMap.lookup name conPlan)
            let mByClass : Maybe (List InferredType)
                mByClass = case discriminantTy of
                  IRef varClass _ _ =>
                    (\sc => sc.fieldTypes) <$> find (\sc => sc.specClassName == varClass) entries
                  _ => Nothing
            -- (c) `mByFamily`: the discriminant is typed as a TCon
            --     family-instantiation interface (e.g. `Maybe$I`, `Run$F`).
            --     Mirrors `mSpecByFamily` in Codegen's
            --     `assembleConCaseExpr` via the shared
            --     `findUniqueFamilyMember` (uniqueness-checked) — sound
            --     only when the natural class is also dead, because live
            --     natural siblings `implements` every active family
            --     instantiation and could therefore inhabit the
            --     interface-typed discriminant.
            let mByFamily : Maybe (List InferredType)
                mByFamily = case discriminantTy of
                  IRef varClass _ _ =>
                    if SortedSet.contains name natLive
                      then Nothing
                      else (\sc => sc.fieldTypes) <$> findUniqueFamilyMember conPlan name varClass
                  _ => Nothing
            let mUniqueSafe : Maybe (List InferredType)
                mUniqueSafe = if not (isSingleConstructor conInfo)
                  then Nothing
                  else case entries of
                    [sc] => if SortedSet.contains name natLive
                              then Nothing
                              else Just sc.fieldTypes
                    _ => Nothing
            let mSlotTypes = mByClass <|> mByFamily <|> mUniqueSafe
            traverse_ (inferArg mSlotTypes) (zip args [0 .. length args])
            inferExpr expr
        where
            isSingleConstructor : ConInfo -> Bool
            isSingleConstructor RECORD = True
            isSingleConstructor UNIT   = True
            isSingleConstructor _      = False

            -- Seed primitive slot types AND refined reference slot types
            -- (recursive slots typed as the family interface, e.g.
            -- `Node$I$L$L`'s subtrees as `Tree$I$L$L`).  Emission's `bindArg`
            -- stores the typed accessor's result at the slot type, so the
            -- variable type here must match — and a family-typed loop
            -- variable is what lets `$idrisTailRec` reassignment skip the
            -- per-iteration checkcast.  Plain Object slots stay unseeded
            -- (the variable keeps its default Object type).
            shouldSeed : InferredType -> Bool
            shouldSeed slotTy =
              isPrimitive slotTy ||
                (case slotTy of
                   IRef cls _ _ => cls /= "java/lang/Object"
                   _ => False)

            inferArg : Maybe (List InferredType) -> (Name, Nat) -> Core ()
            inferArg mSlotTypes (var, index) =
                let variableName = jvmSimpleName var
                in when (used variableName expr) $ do
                     createVariable variableName
                     case mSlotTypes of
                       Just slots => case drop index slots of
                                       (slotTy :: _) =>
                                         when (shouldSeed slotTy) $
                                           addVariableType variableName slotTy
                                       [] => pure ()
                       Nothing => pure ()

    inferExprConAlt : {auto stateRef: Ref AsmState AsmState} -> String -> NamedConAlt -> Core InferredType
    inferExprConAlt discriminantVar (MkNConAlt name conInfo _ args expr) = do
      let fc = getFC expr
      let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
      withInferenceScope lineStart lineEnd $ inferConCaseExpr discriminantVar name conInfo args expr

    inferBinaryOp : {auto stateRef: Ref AsmState AsmState} -> InferredType -> NamedCExp -> NamedCExp -> Core InferredType
    inferBinaryOp ty x y = do
        ignore $ inferExpr x
        ignore $ inferExpr y
        pure ty

    inferBoolOp : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> NamedCExp -> Core InferredType
    inferBoolOp x y = do
        ignore $ inferExpr x
        ignore $ inferExpr y
        pure IBool

    inferUnaryOp : {auto stateRef: Ref AsmState AsmState} -> InferredType -> NamedCExp -> Core InferredType
    inferUnaryOp ty x = do
      ignore $ inferExpr x
      pure ty

    inferExtPrim : {auto stateRef: Ref AsmState AsmState} -> FC -> ExtPrim -> List NamedCExp -> Core InferredType
    inferExtPrim fc GetStaticField descriptors = inferExtPrim fc JvmStaticMethodCall descriptors
    inferExtPrim fc SetStaticField descriptors = inferExtPrim fc JvmStaticMethodCall descriptors
    inferExtPrim fc GetInstanceField descriptors = inferExtPrim fc JvmStaticMethodCall descriptors
    inferExtPrim fc SetInstanceField descriptors = inferExtPrim fc JvmStaticMethodCall descriptors
    inferExtPrim fc JvmInstanceMethodCall descriptors =
      inferExtPrim fc JvmStaticMethodCall descriptors
    inferExtPrim fc JvmStaticMethodCall [ret, NmApp _ _ [functionNamePrimVal], fargs, world] =
      inferExtPrim fc JvmStaticMethodCall [ret, functionNamePrimVal, fargs, world]
    inferExtPrim _ JvmStaticMethodCall [ret, _, fargs, _]
      = do args <- getFArgs fargs
           methodReturnType <- tySpec ret
           traverse_ inferExpr $ map snd args
           pure $ if methodReturnType == IVoid then inferredObjectType else methodReturnType
    inferExtPrim fc JvmSuper [clazz, fargs, world] = do
      rootMethodName <- getRootMethodName
      if (endsWith (methodName rootMethodName) "$ltinit$gt")
        then inferExtPrim fc JvmStaticMethodCall [voidTypeExpr, NmErased fc, fargs, world]
        else pure IUnknown
    inferExtPrim _ NewArray [_, size, val, _] = do
        ignore $ inferExpr size
        ignore $ inferExpr val
        pure arrayListType
    inferExtPrim _ ArrayGet [_, arr, pos, _] = do
        ignore $ inferExpr arr
        ignore $ inferExpr pos
        pure IUnknown
    inferExtPrim _ ArraySet [_, arr, pos, val, _] = do
        ignore $ inferExpr arr
        ignore $ inferExpr pos
        ignore $ inferExpr val
        pure inferredObjectType
    inferExtPrim _ JvmNewArray [tyExpr, size, _] = do
        ignore $ inferExpr size
        elemTy <- tySpec tyExpr
        pure $ IArray elemTy
    inferExtPrim _ JvmSetArray [tyExpr, index, val, arr, _] = do
        elemTy <- tySpec tyExpr
        ignore $ inferExpr arr
        ignore $ inferExpr index
        ignore $ inferExpr val
        pure inferredObjectType
    inferExtPrim _ JvmGetArray [tyExpr, index, arr, _] = do
        elemTy <- tySpec tyExpr
        ignore $ inferExpr arr
        ignore $ inferExpr index
        pure elemTy
    inferExtPrim _ JvmArrayLength [tyExpr, arr] = do
        elemTy <- tySpec tyExpr
        ignore $ inferExpr arr
        pure IInt
    inferExtPrim _ NewIORef [_, val, world] = do
        ignore $ inferExpr val
        pure refType
    inferExtPrim _ ReadIORef [_, ref, world] = do
        ignore $ inferExpr ref
        pure IUnknown
    inferExtPrim _ WriteIORef [_, ref, val, world] = do
        ignore $ inferExpr ref
        ignore $ inferExpr val
        pure inferredObjectType
    inferExtPrim _ SysOS [] = pure inferredStringType
    inferExtPrim _ SysCodegen [] = pure inferredStringType
    inferExtPrim _ VoidElim _ = pure inferredObjectType
    inferExtPrim _ JvmClassLiteral [_] = pure $ IRef "java/lang/Class" Class []
    inferExtPrim _ JvmInstanceOf [_, obj, _] = do
      ignore $ inferExpr obj
      pure IBool
    inferExtPrim _ JvmRefEq [_, _, x, y] = inferBoolOp x y
    inferExtPrim fc JavaLambda [functionType, javaInterfaceType, lambda] = do
      ignore $ inferExpr lambda
      IFunction <$> getJavaLambdaType fc [functionType, javaInterfaceType, lambda]
    inferExtPrim _ MakeFuture [_, action] = do
        ignore $ inferExpr action
        pure inferredForkJoinTaskType
    inferExtPrim _ (Unknown name) _ = asmCrash $ "Can't compile unknown external directive " ++ show name
    inferExtPrim fc prim args = throw $ GenericMsg fc $ "Unsupported external function " ++ show prim ++ "(" ++
        (show $ showNamedCExp 0 <$> args) ++ ")"

    inferExprLamWithParameterType : {auto stateRef: Ref AsmState AsmState} -> Maybe Name
                                  -> (parameterValueExpr: Maybe (Core InferredType)) -> NamedCExp -> Core InferredType
    inferExprLamWithParameterType parameterName parameterValueExpr expr = do
        let hasParameterValue = isJust parameterValueExpr
        let (_, lineStart, lineEnd) = getSourceLocation expr
        let jvmParameterName = jvmSimpleName <$> parameterName
        let lambdaType = getLambdaTypeByParameter parameterName
        lambdaBodyReturnType <- withInferenceLambdaScope lineStart lineEnd parameterName expr $ do
            jname <- getRootMethodName
            scopeIndex <- getCurrentScopeIndex
            ignore $ traverseOpt createVariable jvmParameterName
            valueType <- maybe (pure IUnknown) id parameterValueExpr
            ignore $ traverseOpt (flip addVariableType valueType) jvmParameterName
            lambdaBodyReturnType <- inferExpr expr
            currentScope <- getScope !getCurrentScopeIndex
            saveScope $ { returnType := lambdaBodyReturnType } currentScope
            pure lambdaBodyReturnType
        pure $ if hasParameterValue
            then lambdaBodyReturnType
            else getLambdaInterfaceType lambdaType

    inferExprLamWithParameterType1 : {auto stateRef: Ref AsmState AsmState} -> (isMethodReference : Bool) -> Maybe Name
                                   -> NamedCExp -> Core InferredType
    inferExprLamWithParameterType1 True _ _ = pure inferredLambdaType
    inferExprLamWithParameterType1 False parameterName expr = inferExprLamWithParameterType parameterName Nothing expr

    inferExprLam : {auto stateRef: Ref AsmState AsmState} -> AppliedLambdaType -> (parameterValue: Maybe NamedCExp)
                 -> (parameterName : Maybe Name) -> NamedCExp -> Core InferredType
    inferExprLam appliedLambdaType parameterValue@(Just value) (Just parameterName) lambdaBody = do
        let shouldGenerateVariable = parameterName == extractedMethodArgumentName
        generatedJvmVariableName <-
            if shouldGenerateVariable
                then pure $ jvmSimpleName parameterName ++ show !newDynamicVariableIndex
                else pure $ jvmSimpleName parameterName
        let generatedVariableName =
            if shouldGenerateVariable
                then UN $ Basic generatedJvmVariableName
                else parameterName
        let valueExpr = NmLocal (getFC lambdaBody) generatedVariableName
        parentScope <- getScope !getCurrentScopeIndex
        inferExprLamWithParameterType (Just generatedVariableName)
            (Just (inferValue parentScope shouldGenerateVariable generatedJvmVariableName))
            (if appliedLambdaType == AppliedLambdaSwitch || appliedLambdaType == AppliedLambdaLet
                then substituteVariableSubMethodBody valueExpr lambdaBody
                else lambdaBody)
      where
        inferValue : Scope -> Bool -> String -> Core InferredType
        inferValue enclosingScope shouldGenerateVariable variableName = do
            lambdaScopeIndex <- getCurrentScopeIndex
            updateCurrentScopeIndex (index enclosingScope)
            valueType <- inferExpr value
            updateCurrentScopeIndex lambdaScopeIndex
            pure valueType
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmLam _ p4 (NmApp _ (NmRef _ name)
      [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2, NmLocal _ arg3, NmLocal _ arg4]))))) = do
        isMethodReference <- canUseMethodReference name [arg0, arg1, arg2, arg3, arg4] p0 [p1, p2, p3, p4]
        inferExprLamWithParameterType1 isMethodReference p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmApp _ (NmRef _ name)
      [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2, NmLocal _ arg3])))) = do
        isMethodReference <- canUseMethodReference name [arg0, arg1, arg2, arg3] p0 [p1, p2, p3]
        inferExprLamWithParameterType1 isMethodReference p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmLam _ p2 (NmApp _ (NmRef _ name)
      [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2]))) = do
        isMethodReference <- canUseMethodReference name [arg0, arg1, arg2] p0 [p1, p2]
        inferExprLamWithParameterType1 isMethodReference p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1])) = do
      isMethodReference <- canUseMethodReference name [arg0, arg1] p0 [p1]
      inferExprLamWithParameterType1 isMethodReference p0 expr
    inferExprLam _ _ p0 expr@(NmApp _ (NmRef _ name) [NmLocal _ arg0]) = do
      isMethodReference <- canUseMethodReference name [arg0] p0 []
      inferExprLamWithParameterType1 isMethodReference p0 expr
    inferExprLam _ _ p0 expr = inferExprLamWithParameterType1 False p0 expr

    inferExprLet : {auto stateRef: Ref AsmState AsmState} -> FC -> (x : Name) -> NamedCExp -> NamedCExp -> Core InferredType
    inferExprLet fc var value expr = do
        let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
        let varName = jvmSimpleName var
        createVariable varName
        let (_, lineStart, lineEnd) = getSourceLocation value
        valueTy <- withInferenceScope lineStart lineEnd $ inferExpr value
        addVariableType varName valueTy
        let (_, lineStart, lineEnd) = getSourceLocation expr
        withInferenceScope lineStart lineEnd $ inferExpr expr

    -- Infer a literal lambda argument destined for a typed callback slot
    -- (higher-order specialisation).  Mirrors inferExprLamWithParameterType
    -- but seeds the parameter variable with the callback signature's
    -- parameter type and types the lambda value as the typed interface.
    -- Seeding is only valid when the call site will be emitted against the
    -- typed interface — i.e. when the resolved callee's slot already
    -- carries the interface type.  assembleSubMethod keys typed lambda
    -- creation off the same condition (the expected parameter type parses
    -- as a callback interface), which is what keeps inference and emission
    -- agreeing on the implementation method descriptors.
    inferCallbackLambda : {auto stateRef: Ref AsmState AsmState} -> InferredFunctionType
                        -> (parameterName : Name) -> NamedCExp -> Core InferredType
    inferCallbackLambda sig parameterName expr = do
        let (_, lineStart, lineEnd) = getSourceLocation expr
        let jvmParameterName = jvmSimpleName parameterName
        let paramType = fromMaybe inferredObjectType (head' sig.parameterTypes)
        ignore $ withInferenceLambdaScope lineStart lineEnd (Just parameterName) expr $ do
            createVariable jvmParameterName
            addVariableType jvmParameterName paramType
            lambdaBodyReturnType <- inferExpr expr
            currentScope <- getScope !getCurrentScopeIndex
            saveScope $ { returnType := sig.returnType } currentScope
            pure lambdaBodyReturnType
        pure $ callbackIfaceType !getProgramName sig

    -- Per-argument inference for a direct call.  A literal lambda whose
    -- slot expects a typed callback — either because the callee slot
    -- already carries the interface type or because the callee's
    -- declaration pins a concrete primitive callback type — is inferred
    -- SEEDED and logged as the typed interface; emission applies the same
    -- positional rule (see adjustCallbackSlotTypes) and always creates
    -- such a lambda against the interface, which is sound even when the
    -- site is not rewritten to a spec: the interface extends Function.
    -- Non-literal function arguments (variables, partial applications)
    -- deliberately log their natural types and stay on the boxed path.
    inferAppArgs : {auto stateRef: Ref AsmState AsmState} -> List InferredType
                 -> List CallbackSlot -> List NamedCExp -> Core (List InferredType)
    inferAppArgs calleeSlots declSlots [] = pure []
    inferAppArgs calleeSlots declSlots (arg :: rest) = do
        argType <- inferAppArg (head' calleeSlots) (fromMaybe CallbackNone (head' declSlots)) arg
        restTypes <- inferAppArgs (drop 1 calleeSlots) (drop 1 declSlots) rest
        pure (argType :: restTypes)
      where
        inferAppArg : Maybe InferredType -> CallbackSlot -> NamedCExp -> Core InferredType
        inferAppArg mSlotType declSlot lam@(NmLam _ param body) =
          case the (Maybe InferredFunctionType)
                 ((mSlotType >>= parseCallbackIfaceType) <|> callbackSigForArg declSlot lam) of
            Just sig => inferCallbackLambda sig param body
            Nothing => inferExpr lam
        inferAppArg _ _ arg = inferExpr arg

    inferSelfTailCallParameter : {auto stateRef: Ref AsmState AsmState} -> Map Int String
                              -> (NamedCExp, Int) -> Core ()
    inferSelfTailCallParameter argumentNameByIndices (arg, index) = do
        -- Higher-order specialisation: a fresh literal lambda passed in a
        -- self tail call must be seeded against the function's own typed
        -- callback slot — an unseeded inference here would both build the
        -- wrong implementation descriptor and merge `Function` into the
        -- parameter variable's interface type, corrupting the typed apply
        -- in the rest of the body.
        fnParamTypes <- (.inferredFunctionType.parameterTypes) . currentIdrisFunction <$> getState
        let mCallbackSig = the (Maybe InferredFunctionType)
                             (getAt (cast index) fnParamTypes >>= parseCallbackIfaceType)
        ty <- case (mCallbackSig, arg) of
                (Just sig, NmLam _ param body) => inferCallbackLambda sig param body
                _ => inferExpr arg
        optName <- coreLift $ Map.get {value=String} argumentNameByIndices index
        maybe (pure ()) (doAddVariableType ty) $ nullableToMaybe optName
      where
        doAddVariableType : InferredType -> String -> Core ()
        doAddVariableType ty name = do
            addVariableType name ty
            case arg of
                NmLocal _ loc => do
                    let valueVariableName = jvmSimpleName loc
                    valueVariableIndex <- retrieveVariableIndex valueVariableName
                    when (index /= valueVariableIndex) $ createNewVariable "tailRecArg" ty
                _ => createNewVariable "tailRecArg" ty

    inferExprApp : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core InferredType
    inferExprApp app@(NmApp _ (NmRef _ (UN (Basic "$idrisTailRec"))) args) =
        case args of
            [] => pure IUnknown -- The type will not be used as it is in tail call position
            args@(_ :: argsTail) => do
                argumentNameByIndices <- coreLift $ Map.transpose $ variableIndices !(getScope 0)
                traverse_ (inferSelfTailCallParameter argumentNameByIndices) $
                  zip args [0 .. the Int $ cast $ length argsTail]
                pure IUnknown -- The type will not be used as it is in tail call position
    inferExprApp (NmApp fc (NmRef _ idrisName) args) = do
        let functionName = jvmName !getProgramName idrisName
        mFunctionType <- findFunctionType functionName
        retType <- case mFunctionType of
            Just functionType => pure $ returnType functionType
            Nothing => if idrisName == tailRecLoopFunctionName
                         then pure inferredObjectType
                         else throw $ GenericMsg fc "Unknown type for function \{show functionName}"
        let calleeSlots = maybe [] parameterTypes mFunctionType
        declSigs <- fromMaybe [] . SortedMap.lookup (jvmSimpleName idrisName) <$> getCallbackSlotSigs
        inferredArgTypes <- inferAppArgs calleeSlots declSigs args
        updateState { callSiteLog $= ((fc, idrisName, MkInferredFunctionType retType inferredArgTypes) :: ) }
        pure retType
    inferExprApp (NmApp _ lambdaVariable@(NmLocal _ var) [arg]) = do
        -- Inference mirror of the typed-apply emission: applying a
        -- variable statically typed as a callback interface yields the
        -- typed return; emission invokes the typed `apply` with the
        -- argument cast to the typed parameter.  Everything else stays on
        -- the boxed `Function.apply` path.
        varType <- inferExpr lambdaVariable
        case parseCallbackIfaceType varType of
          Just sig => do
            ignore $ inferExpr arg
            pure sig.returnType
          Nothing => do
            ignore $ inferExpr arg
            pure IUnknown
    inferExprApp (NmApp _ lambdaVariable args) = do
        ignore $ inferExpr lambdaVariable
        traverse_ inferExpr args
        pure IUnknown
    inferExprApp _ = throw $ GenericMsg emptyFC "Not a function application"

    inferExprCon : {auto stateRef: Ref AsmState AsmState} -> FC -> Name -> ConInfo -> Maybe Int -> List NamedCExp -> Core InferredType
    inferExprCon fc name conInfo tag args = do
      inferredArgTypes <- traverse inferExpr args
      -- Normalize reference-typed slots to `inferredObjectType` before
      -- logging: `mkConSpecClassName` collapses every ref slot to "L", so
      -- two sites that differ only in their ref classes (e.g.
      -- `[int, IRef CONS]` vs `[int, IRef CONS$I$L]`) would yield the same
      -- spec class name but conflicting per-iteration `paramTypes`.  That
      -- breaks `preRegisterConstructorSpecs` (which builds the class
      -- descriptor from the first-seen paramTypes) and triggers a
      -- VerifyError when a later caller pushes the spec instance into a
      -- slot typed against the older descriptor.  Normalizing makes the
      -- spec's paramTypes stable across iterations.
      let normalizedArgTypes = map normalizeRef inferredArgTypes
      updateState { conSiteLog $= ((fc, name, conInfo, tag, normalizedArgTypes) :: ) }
      -- Return the IRef of the JVM class that `assembleCon` will actually
      -- emit at this site.  This is what drives function-spec discovery:
      -- a caller of `sumNumT (MkT 6 7 8)` should see `IRef MkT$I$I$I` as
      -- the arg type so a `sumNumT$sp(_, MkT$I$I$I)` variant can be
      -- registered.  Only return a class IRef when a spec matches (so the
      -- class is guaranteed live).  When no spec matches we fall back to
      -- `idrisObjectType` rather than the natural class IRef — the natural
      -- class may not be emitted (every other site might be specialised)
      -- and leaking its name into call-site types would cause
      -- NoClassDefFoundError on classes that don't exist at runtime.
      programName <- getProgramName
      conPlan <- getConSpecialisationPlan
      -- When a matching spec exists, prefer the TCon family interface
      -- (e.g. `Maybe$I`) over the concrete DCon spec class (`JUST$I`):
      -- it lets a call site's logged arg type cover both `Just`-spec
      -- and `Nothing`-natural instances at the family level, so
      -- function-spec routing through `rewriteSpecCalls` matches
      -- whichever sibling is emitted at runtime.  When the spec carries
      -- no `tconClassName` (Phase-1 lone-DCon case for RECORD types),
      -- fall back to the spec class itself.
      --
      -- Note: sibling DCons WITHOUT a matching spec (e.g. `Nil`, which
      -- has no slots) are NOT typed as the family interface here.  A
      -- polymorphic `Nil` can flow into many `List`-family contexts (Int,
      -- Char, …) and picking one family for it would falsely commit the
      -- caller to a specific instantiation — that's how cross-family
      -- ClassCast errors showed up during the v2 bring-up.  Instead, the
      -- sibling's natural class still `implements` every active family
      -- (via `computeNaturalToTConIfaces`), so once a call site is
      -- statically typed at the family (e.g. by the parent `Cons$I$L`'s
      -- recursive slot — Phase 2d), the sibling instance is acceptable
      -- without committing the construction site itself to a family.
      pure $ case lookupConSpec name normalizedArgTypes conPlan of
        Just spec => if spec.tconClassName /= ""
                       then IRef spec.tconClassName Interface []
                       else IRef spec.specClassName Class []
        Nothing => idrisObjectType
      where
        normalizeRef : InferredType -> InferredType
        normalizeRef ty = if isPrimitive ty then ty else inferredObjectType

        slotMatch : InferredType -> InferredType -> Bool
        slotMatch spec arg =
          if isPrimitive spec then spec == arg
          else not (isPrimitive arg)

        lookupConSpec : Name -> List InferredType -> ConSpecialisationPlan
                     -> Maybe SpecialisedConstructor
        lookupConSpec n argTypes plan = do
          entries <- SortedMap.lookup n plan
          find (\sc => length sc.paramTypes == length argTypes
                       && all (uncurry slotMatch) (zip sc.paramTypes argTypes))
               entries

    inferExprCast : {auto stateRef: Ref AsmState AsmState} -> InferredType -> NamedCExp -> Core InferredType
    inferExprCast targetType expr = do
        ignore $ inferExpr expr
        pure targetType

    inferExprOp : {auto stateRef: Ref AsmState AsmState} -> PrimFn arity -> Vect arity NamedCExp -> Core InferredType
    inferExprOp (Add ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (Sub ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (Mul ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (Div ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (Mod ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (Neg ty) [x] = inferUnaryOp (getInferredType ty) x
    inferExprOp (ShiftL ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (ShiftR ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (BAnd ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (BOr ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (BXOr ty) [x, y] = inferBinaryOp (getInferredType ty) x y
    inferExprOp (LT ty) [x, y] = inferBoolOp x y
    inferExprOp (LTE ty) [x, y] = inferBoolOp x y
    inferExprOp (EQ ty) [x, y] = inferBoolOp x y
    inferExprOp (GT ty) [x, y] = inferBoolOp x y
    inferExprOp (GTE ty) [x, y] = inferBoolOp x y

    inferExprOp StrLength [x] = do
        ignore $ inferExpr x
        pure IInt
    inferExprOp StrHead [x] = do
        ignore $ inferExpr x
        pure IChar
    inferExprOp StrTail [x] = do
        ignore $ inferExpr x
        pure inferredStringType
    inferExprOp StrIndex [x, i] = do
        ignore $ inferExpr x
        ignore $ inferExpr i
        pure IChar
    inferExprOp StrCons [x, y] = do
        ignore $ inferExpr x
        ignore $ inferExpr y
        pure inferredStringType
    inferExprOp StrAppend [x, y] = inferBinaryOp inferredStringType x y
    inferExprOp StrReverse [x] = do
        ignore $ inferExpr x
        pure inferredStringType
    inferExprOp StrSubstr [offset, len, str] = do
        ignore $ inferExpr offset
        ignore $ inferExpr len
        ignore $ inferExpr str
        pure inferredStringType
    inferExprOp DoubleExp [x] = inferUnaryOp IDouble x
    inferExprOp DoubleLog [x] = inferUnaryOp IDouble x
    inferExprOp DoublePow [x, y] = inferBinaryOp IDouble x y
    inferExprOp DoubleSin [x] = inferUnaryOp IDouble x
    inferExprOp DoubleCos [x] = inferUnaryOp IDouble x
    inferExprOp DoubleTan [x] = inferUnaryOp IDouble x
    inferExprOp DoubleASin [x] = inferUnaryOp IDouble x
    inferExprOp DoubleACos [x] = inferUnaryOp IDouble x
    inferExprOp DoubleATan [x] = inferUnaryOp IDouble x
    inferExprOp DoubleSqrt [x] = inferUnaryOp IDouble x
    inferExprOp DoubleFloor [x] = inferUnaryOp IDouble x
    inferExprOp DoubleCeiling [x] = inferUnaryOp IDouble x

    inferExprOp (Cast ty1 ty2) [x] = inferExprCast (getInferredType ty2) x

    inferExprOp BelieveMe [a, b, x] = do
      ignore $ inferExpr a
      ignore $ inferExpr b
      ignore $ inferExpr x
      pure IUnknown
    inferExprOp Crash [_, msg] = do
      ignore $ inferExpr msg
      pure IUnknown
    inferExprOp op _ = throw $ GenericMsg emptyFC ("Unsupported primitive function " ++ show op)

export
%inline
emptyFunction : NamedCExp
emptyFunction = NmCrash emptyFC "uninitialized function"

showScopes : {auto stateRef: Ref AsmState AsmState} -> Int -> Core ()
showScopes n = do
    scope <- getScope n
    logAsm $ show scope
    when (n > 0) $ showScopes (n - 1)

delayNilArityExpr : FC -> (args: List Name) -> NamedCExp -> NamedCExp
delayNilArityExpr fc [] expr = NmDelay fc LLazy expr
delayNilArityExpr _ _ expr = expr

toNameFcDef : TailRec.Function -> (Name, FC, NamedDef)
toNameFcDef (MkFunction name fc args def) = (name, fc, MkNmFun args def)

logFunction : String -> Jname -> List Name -> NamedCExp -> (result: a) -> a
logFunction logPrefix name args expr result =
  if shouldDebugFunction name
    then log (logPrefix ++ " " ++ show name ++ ": " ++ show args ++ "\n" ++ showNamedCExp 0 expr) result
    else result

namespace TermType
  export
  getConstantType : Primitive.Constant -> InferredType
  getConstantType (I _) = IInt
  getConstantType WorldVal = IInt
  getConstantType (I8 _) = IInt
  getConstantType (I16 _) = IInt
  getConstantType (I32 _) = IInt
  getConstantType (I64 _) = ILong
  getConstantType (B8 _) = IInt
  getConstantType (B16 _) = IInt
  getConstantType (B32 _) = IInt
  getConstantType (B64 _) = ILong
  getConstantType (Ch _ ) = IInt
  getConstantType (Str _) = inferredStringType
  getConstantType (BI _) = inferredBigIntegerType
  getConstantType (Db _) = IDouble
  getConstantType (PrT IntType    ) = IInt
  getConstantType (PrT IntegerType) = inferredBigIntegerType
  getConstantType (PrT Int8Type   ) = IInt
  getConstantType (PrT Int16Type  ) = IInt
  getConstantType (PrT Int32Type  ) = IInt
  getConstantType (PrT Int64Type  ) = ILong
  getConstantType (PrT Bits8Type  ) = IInt
  getConstantType (PrT Bits16Type ) = IInt
  getConstantType (PrT Bits32Type ) = IInt
  getConstantType (PrT Bits64Type ) = ILong
  getConstantType (PrT StringType ) = inferredStringType
  getConstantType (PrT CharType   ) = IInt
  getConstantType (PrT DoubleType ) = IDouble
  getConstantType (PrT WorldType  ) = IInt

  getTypeTerm : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> Name -> Core (Maybe (Term []))
  getTypeTerm name = do
    defs <- get Ctxt
    Just gdef <- lookupCtxtExact name (gamma defs)
      | Nothing => pure Nothing
    ty <- normaliseSizeLimit defs 50 [] gdef.type
    Just <$> toFullNames ty

  showConType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> ConInfo -> Name -> Core ()
  showConType conInfo name = do
    Just ty <- getTypeTerm name
      | Nothing => coreLift $ printLn $ "Missing type for " ++ show name
    coreLift $ printLn $ show name ++ "(" ++ show conInfo ++ ")" ++ ": " ++ show ty

  getConstructorType : {auto c : Ref Ctxt Defs} -> Name -> Core InferredType
  getConstructorType name =
    if isBoolTySpec name then pure IInt
    else if name == preludetypes "Nat" then pure inferredBigIntegerType
    else pure inferredObjectType

  mutual
    getFnType' : {auto c : Ref Ctxt Defs} -> {vars : _} -> List InferredType -> Term vars -> List (Term vars) -> Core (List1 InferredType)
    getFnType' types (Ref _ _ tyName) args = do
      constructorType <- getConstructorType tyName
      pure (constructorType ::: types)
    getFnType' types (PrimVal _ c) [] = pure (getConstantType c ::: types)
    getFnType' types (Bind _ x (Pi _ count _ ty) sc) [] =
      if count == plusNeutral then getFnType (toList types) sc
      else do
        lambdaTypes <- getFnType [] ty
        case lambdaTypes of
          (jvmType ::: []) => getFnType (toList (jvmType ::: types)) sc
          xs => getFnType (inferredLambdaType :: toList types) sc
    getFnType' types term _ = pure (inferredObjectType ::: types)

    getFnType : {auto c : Ref Ctxt Defs} -> {vars : _} -> List InferredType -> Term vars -> Core (List1 InferredType)
    getFnType types term =
      let (fn, args) = getFnArgs term
      in getFnType' types fn args

  export
  getTermJvmType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> Name -> Core (Maybe InferredFunctionType)
  getTermJvmType name = do
      Just term <- getTypeTerm name
        | Nothing => pure Nothing
      (ret ::: args) <- getFnType [] term
      pure $ Just $ MkInferredFunctionType ret (reverse args)

  -- Declaration-driven callback-slot classification for higher-order
  -- specialisation: one CallbackSlot per unerased top-level parameter
  -- slot.  A concrete arity-1 function type with a primitive (the
  -- `(Int -> Int)` slot of `applyN`) is CallbackConcrete; an arity-1
  -- function type that doesn't pin a primitive — type variables like
  -- `a -> b` of `map`, or all-reference types — is CallbackPoly (a typed
  -- signature may still be derived per-lambda by findLambdaCallbackSig);
  -- anything else is CallbackNone.
  export
  getTermCallbackSigs : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> Name
                      -> Core (List CallbackSlot)
  getTermCallbackSigs name = do
      Just term <- getTypeTerm name
        | Nothing => pure []
      walk term
    where
      -- An erased Pi binder inside a first-class function type still
      -- occupies a runtime application stage: a rank-2 argument like
      -- `(forall vs . Term vs -> Bool)` is coerced via a lambda that
      -- takes the erased argument FIRST and returns the real predicate
      -- (the caller applies it to an erased placeholder before the real
      -- argument).  Such a slot is therefore NOT an arity-1 callback —
      -- typing its outer stage as `Fn$…` would emit a primitive cast on
      -- a function value.  getFnType skips erased binders, so check for
      -- them separately and refuse the slot.
      hasErasedBinder : {vars : _} -> Term vars -> Bool
      hasErasedBinder (Bind _ _ (Pi _ count _ _) sc) =
        count == plusNeutral || hasErasedBinder sc
      hasErasedBinder _ = False

      walk : {vars : _} -> Term vars -> Core (List CallbackSlot)
      walk (Bind _ x (Pi _ count _ ty) sc) =
        if count == plusNeutral then walk sc
        else do
          slotTypes <- getFnType [] ty
          let slot = if hasErasedBinder ty
                       then CallbackNone
                       else case slotTypes of
                              (ret ::: params@(_ :: [])) =>
                                maybe CallbackPoly CallbackConcrete
                                  (mkCallbackSig (MkInferredFunctionType ret params))
                              _ => CallbackNone
          rest <- walk sc
          pure (slot :: rest)
      walk _ = pure []

  export
  showType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> Name -> Core ()
  showType name = do
    let nameString = show name
    Just ty <- getTypeTerm name
      | Nothing => coreLift $ printLn $ "Missing type for " ++ nameString
    coreLift $ printLn $ "\{nameString}: \{show ty}"
    Just termJvmType <- getTermJvmType name
      | Nothing => coreLift $ printLn $ "Missing JVM type for " ++ nameString
    coreLift $ printLn $ "\{nameString}: \{show termJvmType}"

  -- Walk the binders of a DCon's type term to its result type, then return
  -- the head `Ref` — that's the parent type constructor.  Returns `Nothing`
  -- for non-DCon names or when the type is missing/unusual (e.g. erased).
  export
  findTConForDCon : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
                  -> Name -> Core (Maybe Name)
  findTConForDCon dconName = do
      Just term <- getTypeTerm dconName
        | Nothing => pure Nothing
      pure $ extractTCon term
    where
      extractTCon : {vars : _} -> Term vars -> Maybe Name
      extractTCon (Bind _ _ (Pi _ _ _ _) sc) = extractTCon sc
      extractTCon term =
        let (fn, _) = getFnArgs term
        in case fn of
          Ref _ _ tyName => Just tyName
          _ => Nothing

  -- Look up all data constructors of a type constructor.  Returns the
  -- empty list when the name isn't a TCon or has no recorded datacons.
  export
  findDConsForTCon : {auto c : Ref Ctxt Defs} -> Name -> Core (List Name)
  findDConsForTCon tconName = do
      defs <- get Ctxt
      case !(lookupDefExact tconName (gamma defs)) of
        Just (TCon _ _ _ _ _ (Just dcons) _) => pure dcons
        _ => pure []

  -- Role of one (non-erased) DCon argument slot relative to its parent
  -- TCon:
  --   * `SlotRecursive` — the argument's type is the parent TCon applied
  --     to exactly the same telescope variables as the DCon's return
  --     type (`Node`'s subtrees; the instantiation check matters:
  --     `data T a = MkT a (T Int)` must NOT mark the second slot — its
  --     runtime value belongs to a different family instantiation).
  --   * `SlotBare ks` — the argument's type is a bare telescope variable
  --     that occupies the return-type spine positions `ks` (≥1 position;
  --     a list because GADT-style `MkD : a -> D a a` repeats a variable
  --     in the spine).  Observed slot types at such slots witness the
  --     TCon's type-argument instantiation.
  --   * `SlotOther` — anything else (concrete types, applications, …).
  public export
  data ConSlotRole = SlotRecursive | SlotBare (List Nat) | SlotOther

  -- Classify every non-erased argument slot of a DCon and report the
  -- parent TCon's parameter count (= return-type spine length).  Locals
  -- are compared by telescope binder position (depth - 1 -
  -- de-Bruijn-index), so the comparison is scope-independent.  Erased
  -- (0-multiplicity) binders are skipped to stay aligned with compiled
  -- `NmCon` argument lists, but still count toward binder depth.
  -- `Nothing` on any unusual shape: missing type, non-`Ref` head,
  -- non-`Local` spine arguments (indexed types like `Vect (S n) a`).
  export
  findConSlotRoles : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
                   -> Name -> Core (Maybe (Nat, List ConSlotRole))
  findConSlotRoles dconName = do
      Just term <- getTypeTerm dconName
        | Nothing => pure Nothing
      pure $ (\(_, retSpine, roles) => (length retSpine, roles)) <$> walk 0 term
    where
      -- Application-spine arguments as telescope binder positions; every
      -- argument must be a plain Local.
      spineBinders : {vars : _} -> (depth : Nat) -> List (Term vars) -> Maybe (List Nat)
      spineBinders depth [] = Just []
      spineBinders depth (Local _ _ idx _ :: rest) = do
        restBinders <- spineBinders depth rest
        if idx < depth
          then Just (minus depth (S idx) :: restBinders)
          else Nothing
      spineBinders _ _ = Nothing

      slotRole : {vars : _} -> (depth : Nat) -> Name -> List Nat -> Term vars -> ConSlotRole
      slotRole depth tcon retSpine ty =
        case getFnArgs ty of
          (Ref _ _ tyName, args) =>
            if tyName == tcon && spineBinders depth args == Just retSpine
              then SlotRecursive
              else SlotOther
          (Local _ _ idx _, []) =>
            if idx < depth
              then let binder = minus depth (S idx)
                       ks = findIndices (== binder) retSpine
                   in if isNil ks then SlotOther else SlotBare ks
              else SlotOther
          _ => SlotOther

      walk : {vars : _} -> (depth : Nat) -> Term vars
           -> Maybe (Name, List Nat, List ConSlotRole)
      walk depth (Bind _ _ (Pi _ count _ ty) sc) = do
        (tcon, retSpine, roles) <- walk (S depth) sc
        if count == plusNeutral
          then Just (tcon, retSpine, roles)
          else Just (tcon, retSpine, slotRole depth tcon retSpine ty :: roles)
      walk depth term =
        case getFnArgs term of
          (Ref _ _ tyName, args) => do
            spine <- spineBinders depth args
            Just (tyName, spine, [])
          _ => Nothing

  -- Per-slot recursion flags, kept for Phase 2d (`computeFieldTypes`).
  export
  findRecursiveConSlots : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo}
                        -> Name -> Core (Maybe (List Bool))
  findRecursiveConSlots dconName = do
      mRoles <- findConSlotRoles dconName
      pure $ (\(_, roles) => map isRecursive roles) <$> mRoles
    where
      isRecursive : ConSlotRole -> Bool
      isRecursive SlotRecursive = True
      isRecursive _ = False

optimizeTailRecursion : String -> (Name, FC, NamedDef) -> List (Name, FC, NamedDef)
optimizeTailRecursion programName (name, fc, (MkNmFun args body)) =
  let jname = jvmName programName name
      nilArityHandledExpr = delayNilArityExpr fc args body
      (splitExpr, extractedFunctions) = splitFunction jname args nilArityHandledExpr
      tailRecOptimizedExpr = runReader (jname, programName) $ markTailRecursion splitExpr
      tailRecOptimizedDef = (name, fc, MkNmFun args tailRecOptimizedExpr)
      extractedFunctionDefs = toNameFcDef <$> extractedFunctions
      optimizedDefs = tailRecOptimizedDef :: extractedFunctionDefs
  in logFunction "Unoptimized" jname args body optimizedDefs
optimizeTailRecursion _ nameFcDef = [nameFcDef]

-- After frontend inlining, distinct construction and call sites often share
-- one FC: every inlined `pure x` carries the FC of the `Right x` inside
-- `pure`'s definition.  The specialisation site logs (`conSiteLog`,
-- `callSiteLog`) are keyed by FC, so colliding sites can pick up each
-- other's inferred types — e.g. `pure $ partitionOpts opts` matching the
-- `(fc, Right, [int])` entry logged by a sibling `pure True` and getting
-- emitted as the int-slot `Right$I` spec, crashing in `Conversion.toInt`
-- at runtime.  Stamp a per-definition counter into the start column of
-- every NmCon/NmApp FC to make the keys site-unique: `getLineNumbers`
-- ignores start columns, so line-number debug info is unaffected.
-- Synthetic EmptyFC sites (e.g. from tail-recursion optimisation) are
-- lifted to virtual FCs carrying the counter.
markUniqueFc : Int -> FC -> (Int, FC)
markUniqueFc counter (MkFC origin (startLine, _) end) =
  (counter + 1, MkFC origin (startLine, counter) end)
markUniqueFc counter (MkVirtualFC origin (startLine, _) end) =
  (counter + 1, MkVirtualFC origin (startLine, counter) end)
markUniqueFc counter EmptyFC =
  (counter + 1, MkVirtualFC (Virtual Interactive) (0, counter) (0, 0))

mutual
  uniquifySiteFcs : Int -> NamedCExp -> (Int, NamedCExp)
  uniquifySiteFcs counter (NmLam fc x body) =
    let (c1, body') = uniquifySiteFcs counter body
    in (c1, NmLam fc x body')
  uniquifySiteFcs counter (NmLet fc x value body) =
    let (c1, value') = uniquifySiteFcs counter value
        (c2, body') = uniquifySiteFcs c1 body
    in (c2, NmLet fc x value' body')
  uniquifySiteFcs counter (NmApp fc f args) =
    -- Applied-lambda sentinel FCs from `splitFunction` are matched by
    -- equality in `getAppliedLambdaType` during inference — leave them as is.
    let (c1, fc') = if fc == appliedLambdaSwitchIndicator || fc == appliedLambdaLetIndicator
                      then (counter, fc)
                      else markUniqueFc counter fc
        (c2, f') = uniquifySiteFcs c1 f
        (c3, args') = uniquifySiteFcsList c2 args
    in (c3, NmApp fc' f' args')
  uniquifySiteFcs counter (NmCon fc name conInfo tag args) =
    let (c1, fc') = markUniqueFc counter fc
        (c2, args') = uniquifySiteFcsList c1 args
    in (c2, NmCon fc' name conInfo tag args')
  uniquifySiteFcs counter (NmOp fc op args) =
    let (c1, args') = uniquifySiteFcsVect counter args
    in (c1, NmOp fc op args')
  uniquifySiteFcs counter (NmExtPrim fc name args) =
    let (c1, args') = uniquifySiteFcsList counter args
    in (c1, NmExtPrim fc name args')
  uniquifySiteFcs counter (NmForce fc reason expr) =
    let (c1, expr') = uniquifySiteFcs counter expr
    in (c1, NmForce fc reason expr')
  uniquifySiteFcs counter (NmDelay fc reason expr) =
    let (c1, expr') = uniquifySiteFcs counter expr
    in (c1, NmDelay fc reason expr')
  uniquifySiteFcs counter (NmConCase fc sc alts def) =
    let (c1, sc') = uniquifySiteFcs counter sc
        (c2, alts') = uniquifySiteFcsConAlts c1 alts
        (c3, def') = uniquifySiteFcsMaybe c2 def
    in (c3, NmConCase fc sc' alts' def')
  uniquifySiteFcs counter (NmConstCase fc sc alts def) =
    let (c1, sc') = uniquifySiteFcs counter sc
        (c2, alts') = uniquifySiteFcsConstAlts c1 alts
        (c3, def') = uniquifySiteFcsMaybe c2 def
    in (c3, NmConstCase fc sc' alts' def')
  uniquifySiteFcs counter expr = (counter, expr)

  uniquifySiteFcsList : Int -> List NamedCExp -> (Int, List NamedCExp)
  uniquifySiteFcsList counter [] = (counter, [])
  uniquifySiteFcsList counter (x :: xs) =
    let (c1, x') = uniquifySiteFcs counter x
        (c2, xs') = uniquifySiteFcsList c1 xs
    in (c2, x' :: xs')

  uniquifySiteFcsVect : Int -> Vect n NamedCExp -> (Int, Vect n NamedCExp)
  uniquifySiteFcsVect counter [] = (counter, [])
  uniquifySiteFcsVect counter (x :: xs) =
    let (c1, x') = uniquifySiteFcs counter x
        (c2, xs') = uniquifySiteFcsVect c1 xs
    in (c2, x' :: xs')

  uniquifySiteFcsMaybe : Int -> Maybe NamedCExp -> (Int, Maybe NamedCExp)
  uniquifySiteFcsMaybe counter Nothing = (counter, Nothing)
  uniquifySiteFcsMaybe counter (Just x) =
    let (c1, x') = uniquifySiteFcs counter x
    in (c1, Just x')

  uniquifySiteFcsConAlts : Int -> List NamedConAlt -> (Int, List NamedConAlt)
  uniquifySiteFcsConAlts counter [] = (counter, [])
  uniquifySiteFcsConAlts counter (MkNConAlt name conInfo tag args expr :: rest) =
    let (c1, expr') = uniquifySiteFcs counter expr
        (c2, rest') = uniquifySiteFcsConAlts c1 rest
    in (c2, MkNConAlt name conInfo tag args expr' :: rest')

  uniquifySiteFcsConstAlts : Int -> List NamedConstAlt -> (Int, List NamedConstAlt)
  uniquifySiteFcsConstAlts counter [] = (counter, [])
  uniquifySiteFcsConstAlts counter (MkNConstAlt constant expr :: rest) =
    let (c1, expr') = uniquifySiteFcs counter expr
        (c2, rest') = uniquifySiteFcsConstAlts c1 rest
    in (c2, MkNConstAlt constant expr' :: rest')

uniquifyDefSiteFcs : (Name, FC, NamedDef) -> (Name, FC, NamedDef)
uniquifyDefSiteFcs (name, fc, MkNmFun args body) =
  (name, fc, MkNmFun args (snd $ uniquifySiteFcs 0 body))
uniquifyDefSiteFcs (name, fc, MkNmError expr) =
  (name, fc, MkNmError (snd $ uniquifySiteFcs 0 expr))
uniquifyDefSiteFcs def = def

export
optimize : String -> LazyList (Name, FC, NamedDef) -> LazyList (Name, FC, NamedDef)
optimize programName allDefs =
  let tailRecOptimizedDefs = concatMap (Lazy.fromList . optimizeTailRecursion programName) allDefs
      tailCallOptimizedDefs = TailRec.functions tailRecLoopFunctionName $ toList tailRecOptimizedDefs
  in uniquifyDefSiteFcs . toNameFcDef <$> fromList tailCallOptimizedDefs

getArity : InferredFunctionType -> Nat
getArity (MkInferredFunctionType _ args) = length args

export
padParams : Nat -> List InferredType -> List InferredType
padParams arity paramTypes = paramTypes ++ replicate (arity `minus` length paramTypes) inferredObjectType

export
getInitialFunctionType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState} -> Name -> NamedDef -> Core InferredFunctionType
getInitialFunctionType idrisName (MkNmFun args expr) = do
  let arity = length args
  let runtimeType = MkInferredFunctionType inferredObjectType (replicate arity inferredObjectType)
  maybeTermType <- TermType.getTermJvmType idrisName
  pure $ case maybeTermType of
    Just termType =>
      if arity == getArity termType then termType else runtimeType
    Nothing => runtimeType
getInitialFunctionType _ _ = do
  name <- getRootMethodName
  throw $ GenericMsg emptyFC "Not a function \{show name}"

{-initialFunctionType could be either initial generic types based on arity or it was provided during specialization-}
export
inferFunctionType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState} -> Maybe InferredFunctionType -> NamedDef -> Core ()
inferFunctionType Nothing (MkNmFun _ _) = do
  name <- getRootMethodName
  throw $ GenericMsg emptyFC ("Missing function type for \{show name}")
inferFunctionType (Just initialFunctionType) (MkNmFun args expr) = do
  let arity = length args
  scopes <- coreLift $ ArrayList.new {elemTy=Scope}
  jname <- getRootMethodName
  let function = MkFunction jname initialFunctionType (subtyping scopes) 0 expr
  setCurrentFunction function
  coreLift $ addFunction jname function
  resetScope
  resetCallSiteLog
  scopeIndex <- newScopeIndex
  let (_, lineStart, lineEnd) = getSourceLocation expr
  allVariableTypes <- coreLift $ Map.newTreeMap {key=Int} {value=InferredType}
  allVariableIndices <- coreLift $ Map.newTreeMap {key=String} {value=Int}
  let arity = length args
  let arityInt = the Int $ cast arity
  let argumentNames = jvmSimpleName <$> args
  argIndices <- coreLift $ getArgumentIndices arityInt argumentNames
  argumentTypesByName <- coreLift $ Map.fromList $ zip argumentNames (parameterTypes initialFunctionType)
  let retType = returnType initialFunctionType
  let functionScope =
      MkScope scopeIndex Nothing argumentTypesByName allVariableTypes argIndices
          allVariableIndices retType arityInt (lineStart, lineEnd) ("", "") []
  saveScope functionScope
  when (shouldDebugFunction jname) $ showScopes 0

inferFunctionType _ (MkNmForeign foreignDescriptors argumentTypes returnType) =
  inferForeign emptyFC foreignDescriptors argumentTypes returnType
inferFunctionType _ (MkNmError expr) = inferFunctionType (Just $ MkInferredFunctionType inferredObjectType []) (MkNmFun [] expr)
inferFunctionType _ _ = pure ()

-- Rewrite each `NmApp (NmRef _ orig) args` in the body whose FC has a logged
-- inferred type and whose `orig` has a matching $sp variant in the plan to
-- `NmApp (NmRef _ spec) args'`.  Pure function: walks the tree, recursing into
-- every subexpression and rebuilding it.  See callers in `inferDef`.
rewriteSpecCalls : List (FC, Name, InferredFunctionType) -> SpecialisationPlan
                -> NamedCExp -> NamedCExp
rewriteSpecCalls log plan = go where
  lookupSiteType : FC -> Name -> Maybe InferredFunctionType
  lookupSiteType targetFc targetName =
    (\(_, _, ty) => ty) <$> find (\(fc, n, _) => fc == targetFc && n == targetName) log

  mutual
    go : NamedCExp -> NamedCExp
    go (NmApp fc (NmRef nfc orig) args) =
      let args'  = go <$> args
          chosen = fromMaybe orig $ do
                     fnType <- lookupSiteType fc orig
                     specs  <- SortedMap.lookup orig plan
                     spec   <- find (\s => s.type.parameterTypes == fnType.parameterTypes) specs
                     pure spec.name
      in NmApp fc (NmRef nfc chosen) args'
    go (NmApp fc f args) = NmApp fc (go f) (go <$> args)
    go (NmLam fc x body) = NmLam fc x (go body)
    go (NmLet fc x val body) = NmLet fc x (go val) (go body)
    go (NmCon fc n ci tag args) = NmCon fc n ci tag (go <$> args)
    go (NmOp fc op args) = NmOp fc op (map go args)
    go (NmExtPrim fc n args) = NmExtPrim fc n (go <$> args)
    go (NmForce fc reason t) = NmForce fc reason (go t)
    go (NmDelay fc reason t) = NmDelay fc reason (go t)
    go (NmConCase fc sc alts def) =
      NmConCase fc (go sc) (goConAlt <$> alts) (go <$> def)
    go (NmConstCase fc sc alts def) =
      NmConstCase fc (go sc) (goConstAlt <$> alts) (go <$> def)
    go e = e

    goConAlt : NamedConAlt -> NamedConAlt
    goConAlt (MkNConAlt n ci tag args body) = MkNConAlt n ci tag args (go body)

    goConstAlt : NamedConstAlt -> NamedConstAlt
    goConstAlt (MkNConstAlt c body) = MkNConstAlt c (go body)

export
inferDef : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState} -> Core ()
inferDef = do
    jname <- getRootMethodName
    Just function <- findFunction jname
      | Nothing => throw (GenericMsg emptyFC ("Unable to find function \{show jname}"))
    let expr = optimizedBody function
    if isForeign expr
      then pure ()
      else do
        when (shouldDebugFunction jname) $ coreLift $ printLn "Optimized: \{showNamedCExp 0 expr}"
        setCurrentFunction function
        resetScope
        size <- coreLift $ Collection.size {elemTy=Scope, obj=Collection Scope} $ believe_me (scopes function)
        setScopeCounter size
        ignore $ inferExpr expr
        updateScopeVariableTypes
        -- Rewire any call site whose inferred argument types match a $sp
        -- variant from the plan.  Persists the rewritten body BOTH on the
        -- current AsmState AND in the global function map — assembleDefinition
        -- calls `loadFunction` which re-reads from the global map, so updating
        -- only the AsmState would be silently discarded.
        log  <- callSiteLog <$> getState
        plan <- getSpecialisationPlan
        let rewritten = rewriteSpecCalls log plan expr
        let function' = { optimizedBody := rewritten } function
        setCurrentFunction function'
        coreLift $ addFunction jname function'
        when (shouldDebugFunction jname) $ showScopes (scopeCounter !getState - 1)
