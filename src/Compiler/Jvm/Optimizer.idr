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
%hide Core.Context.Context.Constructor.arity
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
  in NS (mkNamespace className) (UN $ Basic (show (methodName jname) ++ suffix))

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
        let jname = jvmName idrisName
            functionName = getIdrisFunctionName (snd !ask) (className jname) (methodName jname)
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
canUseMethodReference : {auto stateRef: Ref AsmState AsmState} -> Name -> List Name -> Maybe Name -> List Name
                      -> Core Bool
canUseMethodReference _ _ Nothing _ = pure False
canUseMethodReference functionName args (Just p0) params = do
  Just (MkInferredFunctionType returnType parameterTypes) <- findFunctionType (jvmName functionName)
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
    inferExpr expr@(NmCon fc name _ tag args) =
        inferExprCon (fst $ getSourceLocation expr) name args
    inferExpr (NmOp _ fn args) = inferExprOp fn args
    inferExpr (NmExtPrim fc fn args) = inferExtPrim fc (toPrim fn) args
    inferExpr (NmForce _ _ expr) = do
        ignore $ inferExpr expr
        pure inferredObjectType

    inferExpr (NmConCase _ sc [] Nothing) = pure IUnknown
    inferExpr (NmConCase _ sc [] (Just def)) = do
        inferConstructorSwitchExpr sc
        inferExpr def
    inferExpr (NmConCase _ sc [MkNConAlt _ _ _ args expr] Nothing) = do
        inferConstructorSwitchExpr sc
        inferConCaseExpr args expr
    inferExpr (NmConCase _ sc alts def) = do
        inferConstructorSwitchExpr sc
        let hasTypeCase = any isTypeCase alts
        when hasTypeCase $ do
            createNewVariable "constructorCaseExpr" inferredStringType
            createNewVariable "hashCodePosition" IInt
        let sortedAlts = if hasTypeCase then alts else sortConCases alts
        altTypes <- traverse inferExprConAlt sortedAlts
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

    inferConstructorSwitchExpr : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core ()
    inferConstructorSwitchExpr (NmLocal _ var) = do
        let idrisObjectVariable = jvmSimpleName var
        addVariableType idrisObjectVariable idrisObjectType
    inferConstructorSwitchExpr sc = do
        idrisObjectVariable <- generateVariable "constructorSwitchValue"
        ignore $ inferExpr sc
        addVariableType idrisObjectVariable idrisObjectType

    inferExprConstAlt : {auto stateRef: Ref AsmState AsmState} -> NamedConstAlt -> Core InferredType
    inferExprConstAlt (MkNConstAlt _ expr) = inferExprWithNewScope expr

    inferExprWithNewScope : {auto stateRef: Ref AsmState AsmState} -> NamedCExp -> Core InferredType
    inferExprWithNewScope expr = do
         let fc = getFC expr
         let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
         withInferenceScope lineStart lineEnd $ inferExpr expr

    inferConCaseExpr : {auto stateRef: Ref AsmState AsmState} -> List Name -> NamedCExp -> Core InferredType
    inferConCaseExpr args expr = do
            traverse_ inferArg args
            inferExpr expr
        where
            inferArg : Name -> Core ()
            inferArg var =
                let variableName = jvmSimpleName var
                in when (used variableName expr) $ createVariable variableName

    inferExprConAlt : {auto stateRef: Ref AsmState AsmState} -> NamedConAlt -> Core InferredType
    inferExprConAlt (MkNConAlt _ _ _ args expr) = do
      let fc = getFC expr
      let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
      withInferenceScope lineStart lineEnd $ inferConCaseExpr args expr

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

    inferSelfTailCallParameter : {auto stateRef: Ref AsmState AsmState} -> Map Int String
                              -> (NamedCExp, Int) -> Core ()
    inferSelfTailCallParameter argumentNameByIndices (arg, index) = do
        ty <- inferExpr arg
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
        let functionName = jvmName idrisName
        retType <- case !(findFunctionType functionName) of
            Just functionType => pure $ returnType functionType
            Nothing => if idrisName == tailRecLoopFunctionName
                         then pure inferredObjectType
                         else throw $ GenericMsg fc "Unknown type for function \{show functionName}"
        traverse_ inferExpr args
        pure retType
    inferExprApp (NmApp _ lambdaVariable args) = do
        ignore $ inferExpr lambdaVariable
        traverse_ inferExpr args
        pure IUnknown
    inferExprApp _ = throw $ GenericMsg emptyFC "Not a function application"

    inferExprCon : {auto stateRef: Ref AsmState AsmState} -> String -> Name -> List NamedCExp -> Core InferredType
    inferExprCon fileName name args = do
        traverse_ inferExpr args
        pure idrisObjectType

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

  export
  getTermJvmType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> Name -> Core (Maybe InferredFunctionType)
  getTermJvmType name = do
      Just term <- getTypeTerm name
        | Nothing => pure Nothing
      let (ret ::: args) = go [] term
      pure $ Just $ MkInferredFunctionType ret (reverse args)
    where

      log : Show a => Lazy a -> b -> b
      log value b = if show name == "Prelude.Show.protectEsc" then trace (show value) b else b

      getPrimVal : List (Term vars) -> Maybe Primitive.Constant
      getPrimVal [PrimVal _ c] = Just c
      getPrimVal args = Nothing

      getConstructorType : Name -> InferredType
      getConstructorType name =
        if isBoolTySpec name then IInt
        else if name == basics "List" then idrisListType
        else if name == preludetypes "Maybe" then idrisMaybeType
        else if name == preludetypes "Nat" then inferredBigIntegerType
        else inferredObjectType

      mutual
        getFnType : {vars : _} -> List InferredType -> Term vars -> List (Term vars) -> List1 InferredType
        getFnType types (Ref _ _ tyName) args = getConstructorType tyName ::: types
        getFnType types (PrimVal _ c) [] = getConstantType c ::: types
        getFnType types (Bind _ x (Pi _ count _ ty) sc) [] =
          if count == plusNeutral then go (toList types) sc
          else case go [] ty of
            (jvmType ::: []) => go (toList (jvmType ::: types)) sc
            xs => go (inferredLambdaType :: toList types) sc
        getFnType types term _ = (inferredObjectType ::: types)

        go : {vars : _} -> List InferredType -> Term vars -> List1 InferredType
        go types term =
          let (fn, args) = getFnArgs term
          in getFnType types fn args

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

optimizeTailRecursion : String -> (Name, FC, NamedDef) -> List (Name, FC, NamedDef)
optimizeTailRecursion programName (name, fc, (MkNmFun args body)) =
  let jname = jvmName name
      nilArityHandledExpr = delayNilArityExpr fc args body
      functionName = getIdrisFunctionName programName (className jname) (methodName jname)
      (splitExpr, extractedFunctions) = splitFunction jname args nilArityHandledExpr
      tailRecOptimizedExpr = runReader (functionName, programName) $ markTailRecursion splitExpr
      tailRecOptimizedDef = (name, fc, MkNmFun args tailRecOptimizedExpr)
      extractedFunctionDefs = toNameFcDef <$> extractedFunctions
      optimizedDefs = tailRecOptimizedDef :: extractedFunctionDefs
  in logFunction "Unoptimized" jname args body optimizedDefs
optimizeTailRecursion _ nameFcDef = [nameFcDef]

export
optimize : String -> LazyList (Name, FC, NamedDef) -> LazyList (Name, FC, NamedDef)
optimize programName allDefs =
  let tailRecOptimizedDefs = concatMap (Lazy.fromList . optimizeTailRecursion programName) allDefs
      tailCallOptimizedDefs = TailRec.functions tailRecLoopFunctionName $ toList tailRecOptimizedDefs
  in toNameFcDef <$> fromList tailCallOptimizedDefs

getArity : InferredFunctionType -> Nat
getArity (MkInferredFunctionType _ args) = length args

export
inferFunctionType : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState}
                  -> (Name, FC, NamedDef) -> Core ()
inferFunctionType (idrisName, _, MkNmFun args expr) = do
  let jname = jvmName idrisName
  let arity = length args
  let runtimeInitialFunctionType = MkInferredFunctionType inferredObjectType (replicate arity inferredObjectType)
  maybeFunctionType <- getTermJvmType idrisName
  let termInitialFunctionType = fromMaybe runtimeInitialFunctionType maybeFunctionType
  let termTypeArity = getArity termInitialFunctionType
  let initialFunctionType = if arity == termTypeArity then termInitialFunctionType else runtimeInitialFunctionType
  let jvmClassAndMethodName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
  scopes <- coreLift $ ArrayList.new {elemTy=Scope}
  let function = MkFunction jname initialFunctionType (subtyping scopes) 0 jvmClassAndMethodName expr
  setCurrentFunction function
  coreLift $ addFunction !getGlobalState jname function
  resetScope
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

inferFunctionType (idrisName, fc, MkNmForeign foreignDescriptors argumentTypes returnType) =
  inferForeign idrisName fc foreignDescriptors argumentTypes returnType
inferFunctionType (idrisName, fc, MkNmError expr) = inferFunctionType (idrisName, fc, MkNmFun [] expr)
inferFunctionType _ = pure ()

export
inferDef : {auto c : Ref Ctxt Defs} -> {auto s : Ref Syn SyntaxInfo} -> {auto stateRef: Ref AsmState AsmState}
         -> Name -> FC -> Core ()
inferDef idrisName fc = do
    let jname = jvmName idrisName
    globalState <- getGlobalState
    Just function <- coreLift $ findFunction globalState jname
      | Nothing => throw (GenericMsg fc ("Unable to find function \{show jname}"))
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
        when (shouldDebugFunction jname) $ showScopes (scopeCounter !getState - 1)
