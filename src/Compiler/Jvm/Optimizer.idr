module Compiler.Jvm.Optimizer

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline
import Compiler.TailRec

import Control.Monad.Reader
import Control.Monad.State

import Core.Context
import Core.Name
import Core.Reflect
import Core.TT

import Libraries.Data.SortedMap
import Libraries.Data.SortedSet
import Data.List
import Data.Maybe
import Data.String
import Data.Vect
import Debug.Trace

import Compiler.Jvm.Asm
import Compiler.Jvm.ExtPrim
import Compiler.Jvm.Foreign
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.MockAsm
import Compiler.Jvm.ShowUtil

%hide Core.Context.Context.Constructor.arity
%hide Compiler.TailRec.Function.fc
%hide Compiler.TailRec.TcFunction.fc

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
getFArgs : NamedCExp -> Asm (List (NamedCExp, NamedCExp))
getFArgs (NmCon fc _ _ (Just 0) _) = pure []
getFArgs (NmCon fc _ _ (Just 1) [ty, val, rest]) = pure $ (ty, val) :: !(getFArgs rest)
getFArgs arg = Throw (getFC arg) ("Badly formed jvm call argument list " ++ show arg)

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

exitInferenceScope : Int -> Asm ()
exitInferenceScope scopeIndex = updateCurrentScopeIndex scopeIndex

enterInferenceScope : Int -> Int -> Asm ()
enterInferenceScope lineNumberStart lineNumberEnd = do
    parentScopeIndex <- getCurrentScopeIndex
    scopeIndex <- newScopeIndex
    parentScope <- getScope parentScopeIndex
    variableTypes <- LiftIo $ Map.newTreeMap {key=String} {value=InferredType}
    allVariableTypes <- LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
    variableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
    allVariableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
    let newScope =
        MkScope scopeIndex (Just parentScopeIndex) variableTypes allVariableTypes variableIndices
            allVariableIndices IUnknown (nextVariableIndex parentScope) (lineNumberStart, lineNumberEnd) ("", "") []
    addScopeChild parentScopeIndex scopeIndex
    saveScope newScope
    updateCurrentScopeIndex scopeIndex

createLambdaClosureScope : Int -> Int -> List String -> Scope -> Asm Scope
createLambdaClosureScope scopeIndex childScopeIndex closureVariables parentScope = do
    lambdaClosureVariableIndices <- LiftIo $ Map.fromList $ getLambdaClosureVariableIndices [] 0 closureVariables
    variableTypes <- LiftIo $ Map.newTreeMap {key=String} {value=InferredType}
    allVariableTypes <- LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
    allVariableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
    Pure $ MkScope scopeIndex (Just $ index parentScope) variableTypes allVariableTypes
        lambdaClosureVariableIndices allVariableIndices IUnknown (cast $ length closureVariables)
        (lineNumbers parentScope) ("", "") [childScopeIndex]
  where
    getLambdaClosureVariableIndices : List (String, Int) -> Int -> List String -> List (String, Int)
    getLambdaClosureVariableIndices acc _ [] = acc
    getLambdaClosureVariableIndices acc index (var :: vars) =
        getLambdaClosureVariableIndices ((var, index) :: acc) (index + 1) vars

enterInferenceLambdaScope : Int -> Int -> Maybe Name -> NamedCExp -> Asm ()
enterInferenceLambdaScope lineNumberStart lineNumberEnd parameterName expr = do
        parentScopeIndex <- getCurrentScopeIndex
        scopeIndex <- newScopeIndex
        let boundVariables = maybe SortedSet.empty (flip SortedSet.insert SortedSet.empty . jvmSimpleName) parameterName
        let freeVariables = getFreeVariables boundVariables expr
        let usedVariables = filter (flip SortedSet.contains freeVariables) !(retrieveVariables parentScopeIndex)
        variableTypes <- LiftIo $ Map.newTreeMap {key=String} {value=InferredType}
        allVariableTypes <- LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
        variableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
        allVariableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
        newScope <- case usedVariables  of
            nonEmptyUsedVariables@(_ :: _) => do
                parentScope <- getScope parentScopeIndex
                lambdaParentScopeIndex <- newScopeIndex
                closureScope <- createLambdaClosureScope lambdaParentScopeIndex scopeIndex nonEmptyUsedVariables
                    parentScope
                saveScope closureScope
                let closureVariableCount = nextVariableIndex closureScope
                Pure $ MkScope scopeIndex (Just lambdaParentScopeIndex) variableTypes allVariableTypes
                    variableIndices allVariableIndices IUnknown closureVariableCount (lineNumberStart, lineNumberEnd)
                    ("", "") []
            [] => Pure $ MkScope scopeIndex Nothing variableTypes allVariableTypes variableIndices allVariableIndices
                IUnknown 0 (lineNumberStart, lineNumberEnd) ("", "") []
        saveScope newScope
        updateCurrentScopeIndex scopeIndex

withInferenceScope : Int -> Int -> Asm result -> Asm result
withInferenceScope lineNumberStart lineNumberEnd op = do
    scopeIndex <- getCurrentScopeIndex
    enterInferenceScope lineNumberStart lineNumberEnd
    result <- op
    exitInferenceScope scopeIndex
    Pure result

withInferenceLambdaScope : Int -> Int -> Maybe Name -> NamedCExp -> Asm result -> Asm result
withInferenceLambdaScope lineNumberStart lineNumberEnd parameterName expr op = do
    scopeIndex <- getCurrentScopeIndex
    enterInferenceLambdaScope lineNumberStart lineNumberEnd parameterName expr
    result <- op
    exitInferenceScope scopeIndex
    Pure result

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
getConstantType : List NamedConstAlt -> Asm InferredType
getConstantType [] = Throw emptyFC "Unknown constant switch type"
getConstantType ((MkNConstAlt constant _) :: _) = case constant of
    I _ => Pure IInt
    I8 _ => Pure IInt
    I16 _ => Pure IInt
    I32 _ => Pure IInt
    I64 _ => Pure ILong
    B8 _ => Pure IInt
    B16 _ => Pure IInt
    B32 _ => Pure IInt
    B64 _ => Pure ILong
    Ch _ => Pure IInt
    Str _ => Pure inferredStringType
    BI _ => Pure inferredBigIntegerType
    unsupportedConstant => Throw emptyFC $ "Unsupported constant switch " ++ show unsupportedConstant

export
getIntConstantValue : FC -> TT.Constant -> Asm Int
getIntConstantValue _ (I i) = Pure i
getIntConstantValue _ (I8 i) = Pure (cast i)
getIntConstantValue _ (I16 i) = Pure (cast i)
getIntConstantValue _ (I32 i) = Pure (cast i)
getIntConstantValue _ (B8 i) = Pure (cast i)
getIntConstantValue _ (B16 i) = Pure (cast i)
getIntConstantValue _ (B32 i) = Pure (cast i)
getIntConstantValue _ (Ch c) = Pure $ ord c
getIntConstantValue _ WorldVal = Pure 0
getIntConstantValue _ (PrT _) = Pure 0
getIntConstantValue fc x = Throw fc ("Constant " ++ show x ++ " cannot be converted to integer.")

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

createNewVariable : (variablePrefix: String) -> InferredType -> Asm ()
createNewVariable variablePrefix ty = do
    variable <- generateVariable variablePrefix
    ignore $ addVariableType variable ty

export
isIoAction : NamedCExp -> Bool
isIoAction (NmCon _ (UN (Basic "->")) _ _ [argumentType, returnType]) = isIoAction returnType
isIoAction (NmApp _ (NmRef _ name) _) = name == primio "PrimIO"
isIoAction (NmCon _ name _ _ _) = name == primio "IORes"
isIoAction (NmLam fc arg expr) = isIoAction expr
isIoAction expr = False

voidTypeExpr : NamedCExp
voidTypeExpr = NmCon emptyFC (UN (Basic "void")) TYCON Nothing []

export
getJavaLambdaType : FC -> List NamedCExp -> Asm JavaLambdaType
getJavaLambdaType fc [functionType, javaInterfaceType, _] =
    do
      implementationType <- parseFunctionType functionType
      (interfaceTy, methodName, methodType) <- parseJavaInterfaceType javaInterfaceType
      Pure $ MkJavaLambdaType interfaceTy methodName methodType implementationType
  where
    parseFunctionType: NamedCExp -> Asm InferredFunctionType
    parseFunctionType functionType = do
        types <- go [] functionType
        case types of
          [] => asmCrash ("Invalid Java lambda at " ++ show fc ++ ": " ++ show functionType)
          (returnType :: argTypes) => Pure $ MkInferredFunctionType returnType (reverse argTypes)
      where
        go : List InferredType -> NamedCExp -> Asm (List InferredType)
        go acc (NmCon _ (UN (Basic "->")) _ _ [argTy, lambdaTy]) = do
          argInferredTy <- tySpec argTy
          restInferredTypes <- go acc lambdaTy
          Pure (restInferredTypes ++ (argInferredTy :: acc))
        go acc (NmLam fc arg expr) = go acc expr
        go acc expr@(NmApp _ (NmRef _ name) [arg]) = go (IInt :: acc) (if name == primio "PrimIO" then arg else expr)
        go acc expr = Pure (!(tySpec expr) :: acc)

    throwExpectedStructAtPos : Asm a
    throwExpectedStructAtPos =
      asmCrash ("Expected a struct containing interface name and method separated by space at " ++ show fc)

    throwExpectedStruct : String -> Asm a
    throwExpectedStruct name =
      asmCrash ("Expected a struct containing interface name and method separated by space at " ++
         show fc ++ " but found " ++ name)

    parseJavaInterfaceType : NamedCExp -> Asm (InferredType, String, InferredFunctionType)
    parseJavaInterfaceType expr@(NmCon _ name _ _ [interfaceType, methodTypeExp]) =
        if name == builtin "Pair" then
          case interfaceType of
            NmCon _ name _ _ (NmPrimVal _ (Str namePartsStr) :: _) =>
              if name == structName
                then case words namePartsStr of
                  (interfaceName :: methodName :: _) => do
                    methodType <- parseFunctionType methodTypeExp
                    Pure (IRef interfaceName Interface [], methodName, methodType)
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
    inferExpr : InferredType -> NamedCExp -> Asm InferredType
    inferExpr exprTy (NmDelay _ _ expr) = inferExprLam AppliedLambdaUnknown Nothing Nothing expr
    inferExpr exprTy expr@(NmLocal _ var) = addVariableType (jvmSimpleName var) exprTy
    inferExpr exprTy (NmRef _ name) = pure exprTy
    inferExpr exprTy app@(NmApp _ (NmRef _ name) args) = inferExprApp exprTy app
    inferExpr _ (NmApp fc (NmLam _ var body) [expr]) =
        inferExprLam (getAppliedLambdaType fc) (Just expr) (Just var) body
    inferExpr _ (NmLam _ var body) = inferExprLam AppliedLambdaUnknown Nothing (Just var) body
    inferExpr exprTy (NmLet fc var value expr) = inferExprLet fc exprTy var value expr
    inferExpr exprTy app@(NmApp _ _ _) = inferExprApp exprTy app
    inferExpr exprTy expr@(NmCon fc name _ tag args) =
        inferExprCon exprTy (fst $ getSourceLocation expr) name args
    inferExpr exprTy (NmOp _ fn args) = inferExprOp fn args
    inferExpr exprTy (NmExtPrim fc fn args) = inferExtPrim fc exprTy (toPrim fn) args
    inferExpr exprTy (NmForce _ _ expr) = do
        ignore $ inferExpr delayedType expr
        Pure inferredObjectType

    inferExpr exprTy (NmConCase _ sc [] Nothing) = Pure IUnknown
    inferExpr exprTy (NmConCase _ sc [] (Just def)) = do
        inferConstructorSwitchExpr sc
        inferExpr exprTy def
    inferExpr exprTy (NmConCase _ sc [MkNConAlt _ _ _ args expr] Nothing) = do
        inferConstructorSwitchExpr sc
        inferConCaseExpr exprTy args expr
    inferExpr exprTy (NmConCase _ sc alts def) = do
        inferConstructorSwitchExpr sc
        let hasTypeCase = any isTypeCase alts
        when hasTypeCase $ do
            createNewVariable "constructorCaseExpr" inferredStringType
            createNewVariable "hashCodePosition" IInt
        let sortedAlts = if hasTypeCase then alts else sortConCases alts
        altTypes <- traverse (inferExprConAlt exprTy) sortedAlts
        defaultTy <- traverse (inferExprWithNewScope exprTy) def
        Pure $ combineSwitchTypes defaultTy altTypes

    inferExpr exprTy (NmConstCase fc sc [] Nothing) = Pure IUnknown
    inferExpr exprTy (NmConstCase fc sc [] (Just expr)) = inferExpr exprTy expr
    inferExpr exprTy (NmConstCase fc sc alts def) = do
        constantType <- getConstantType alts
        ignore $ inferExpr constantType sc
        when (constantType /= IInt) $ do
            constantExprVariable <- generateVariable "constantCaseExpr"
            ignore $ addVariableType constantExprVariable constantType
            hashCodePositionVariable <- generateVariable "hashCodePosition"
            ignore $ addVariableType hashCodePositionVariable IInt
        sortedAlts <- sortConstCases constantType alts
        altTypes <- traverse (inferExprConstAlt exprTy) sortedAlts
        defaultTy <- traverse (inferExprWithNewScope exprTy) def
        Pure $ combineSwitchTypes defaultTy altTypes
      where
        getConstant : NamedConstAlt -> TT.Constant
        getConstant (MkNConstAlt constant _) = constant

        sortConstCases : InferredType -> List NamedConstAlt -> Asm (List NamedConstAlt)
        sortConstCases IInt alts = do
            constValues <- traverse (getIntConstantValue fc . getConstant) alts
            Pure $ fst <$> (sortBy (comparing snd) $ zip alts constValues)
        sortConstCases _ alts = Pure alts

    inferExpr _ (NmPrimVal fc (I _)) = pure IInt
    inferExpr _ (NmPrimVal fc (I8 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (I16 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (I32 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (I64 _)) = pure ILong
    inferExpr _ (NmPrimVal fc (B8 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (B16 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (B32 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (B64 _)) = pure ILong
    inferExpr _ (NmPrimVal fc (BI _)) = pure inferredBigIntegerType
    inferExpr _ (NmPrimVal fc (Str _)) = pure inferredStringType
    inferExpr _ (NmPrimVal fc (Ch _)) = pure IChar
    inferExpr _ (NmPrimVal fc (Db _)) = pure IDouble
    inferExpr _ (NmPrimVal fc _) = pure IInt
    inferExpr exprTy (NmErased fc) = pure exprTy
    inferExpr exprTy (NmCrash fc msg) = pure exprTy

    inferConstructorSwitchExpr : NamedCExp -> Asm ()
    inferConstructorSwitchExpr (NmLocal _ var) = do
        let idrisObjectVariable = jvmSimpleName var
        ignore $ addVariableType idrisObjectVariable idrisObjectType
    inferConstructorSwitchExpr sc = do
        idrisObjectVariable <- generateVariable "constructorSwitchValue"
        ignore $ inferExpr idrisObjectType sc
        ignore $ addVariableType idrisObjectVariable idrisObjectType

    inferExprConstAlt : InferredType -> NamedConstAlt -> Asm InferredType
    inferExprConstAlt returnType (MkNConstAlt _ expr) = inferExprWithNewScope returnType expr

    inferExprWithNewScope : InferredType -> NamedCExp -> Asm InferredType
    inferExprWithNewScope returnType expr = do
         let fc = getFC expr
         let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
         withInferenceScope lineStart lineEnd $ inferExpr returnType expr

    inferConCaseExpr : InferredType -> List Name -> NamedCExp -> Asm InferredType
    inferConCaseExpr exprTy args expr = do
            traverse_ inferArg args
            inferExpr exprTy expr
        where
            inferArg : Name -> Asm ()
            inferArg var =
                let variableName = jvmSimpleName var
                in when (used variableName expr) $ createVariable variableName

    inferExprConAlt : InferredType -> NamedConAlt -> Asm InferredType
    inferExprConAlt exprTy (MkNConAlt _ _ _ args expr) = do
            let fc = getFC expr
            let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
            withInferenceScope lineStart lineEnd $ inferConCaseExpr exprTy args expr

    inferParameter : (NamedCExp, InferredType) -> Asm InferredType
    inferParameter (param, ty) = inferExpr ty param

    inferBinaryOp : InferredType -> NamedCExp -> NamedCExp -> Asm InferredType
    inferBinaryOp ty x y = do
        ignore $ inferExpr ty x
        ignore $ inferExpr ty y
        pure ty

    inferBoolOp : InferredType -> NamedCExp -> NamedCExp -> Asm InferredType
    inferBoolOp ty x y = do
        ignore $ inferExpr ty x
        ignore $ inferExpr ty y
        pure IBool

    inferUnaryOp : InferredType -> NamedCExp -> Asm InferredType
    inferUnaryOp ty x = do
      ignore $ inferExpr ty x
      Pure ty

    inferExtPrimArg : (NamedCExp, InferredType) -> Asm InferredType
    inferExtPrimArg (arg, ty) = inferExpr ty arg

    inferExtPrim : FC -> InferredType -> ExtPrim -> List NamedCExp -> Asm InferredType
    inferExtPrim fc returnType GetStaticField descriptors = inferExtPrim fc returnType JvmStaticMethodCall descriptors
    inferExtPrim fc returnType SetStaticField descriptors = inferExtPrim fc returnType JvmStaticMethodCall descriptors
    inferExtPrim fc returnType GetInstanceField descriptors = inferExtPrim fc returnType JvmStaticMethodCall descriptors
    inferExtPrim fc returnType SetInstanceField descriptors = inferExtPrim fc returnType JvmStaticMethodCall descriptors
    inferExtPrim fc returnType JvmInstanceMethodCall descriptors =
      inferExtPrim fc returnType JvmStaticMethodCall descriptors
    inferExtPrim fc returnType JvmStaticMethodCall [ret, NmApp _ _ [functionNamePrimVal], fargs, world] =
      inferExtPrim fc returnType JvmStaticMethodCall [ret, functionNamePrimVal, fargs, world]
    inferExtPrim _ returnType JvmStaticMethodCall [ret, _, fargs, _]
      = do args <- getFArgs fargs
           argTypes <- traverse tySpec (map fst args)
           methodReturnType <- tySpec ret
           traverse_ inferExtPrimArg $ zip (map snd args) argTypes
           pure $ if methodReturnType == IVoid then inferredObjectType else methodReturnType
    inferExtPrim fc returnType JvmSuper [clazz, fargs, world] = do
      rootMethodName <- getRootMethodName
      if (endsWith (methodName rootMethodName) "$ltinit$gt")
        then inferExtPrim fc returnType JvmStaticMethodCall [voidTypeExpr, NmErased fc, fargs, world]
        else pure IUnknown
    inferExtPrim _ returnType NewArray [_, size, val, world] = do
        ignore $ inferExpr IInt size
        ignore $ inferExpr IUnknown val
        pure arrayListType
    inferExtPrim _ returnType ArrayGet [_, arr, pos, world] = do
        ignore $ inferExpr arrayListType arr
        ignore $ inferExpr IInt pos
        pure IUnknown
    inferExtPrim _ returnType ArraySet [_, arr, pos, val, world] = do
        ignore $ inferExpr arrayListType arr
        ignore $ inferExpr IInt pos
        ignore $ inferExpr IUnknown val
        pure inferredObjectType
    inferExtPrim _ returnType JvmNewArray [tyExpr, size, world] = do
        ignore $ inferExpr IInt size
        elemTy <- tySpec tyExpr
        pure $ IArray elemTy
    inferExtPrim _ returnType JvmSetArray [tyExpr, index, val, arr, world] = do
        elemTy <- tySpec tyExpr
        ignore $ inferExpr (IArray elemTy) arr
        ignore $ inferExpr IInt index
        ignore $ inferExpr elemTy val
        pure inferredObjectType
    inferExtPrim _ returnType JvmGetArray [tyExpr, index, arr, world] = do
        elemTy <- tySpec tyExpr
        ignore $ inferExpr (IArray elemTy) arr
        ignore $ inferExpr IInt index
        pure elemTy
    inferExtPrim _ returnType JvmArrayLength [tyExpr, arr] = do
        elemTy <- tySpec tyExpr
        ignore $ inferExpr (IArray elemTy) arr
        pure IInt
    inferExtPrim _ returnType NewIORef [_, val, world] = do
        ignore $ inferExpr IUnknown val
        pure refType
    inferExtPrim _ returnType ReadIORef [_, ref, world] = do
        ignore $ inferExpr refType ref
        pure IUnknown
    inferExtPrim _ returnType WriteIORef [_, ref, val, world] = do
        ignore $ inferExpr refType ref
        ignore $ inferExpr IUnknown val
        pure inferredObjectType
    inferExtPrim _ returnType SysOS [] = pure inferredStringType
    inferExtPrim _ returnType SysCodegen [] = pure inferredStringType
    inferExtPrim _ returnType VoidElim _ = pure inferredObjectType
    inferExtPrim _ returnType JvmClassLiteral [_] = pure $ IRef "java/lang/Class" Class []
    inferExtPrim _ returnType JvmInstanceOf [_, obj, _] = do
      ignore $ inferExpr IUnknown obj
      pure IBool
    inferExtPrim _ returnType JvmRefEq [_, _, x, y] = inferBoolOp IUnknown x y
    inferExtPrim fc returnType JavaLambda [functionType, javaInterfaceType, lambda] = do
      ignore $ inferExpr IUnknown lambda
      IFunction <$> getJavaLambdaType fc [functionType, javaInterfaceType, lambda]
    inferExtPrim _ returnType MakeFuture [_, action] = do
        ignore $ inferExpr delayedType action
        pure inferredForkJoinTaskType
    inferExtPrim _ returnType (Unknown name) _ = asmCrash $ "Can't compile unknown external directive " ++ show name
    inferExtPrim fc _ prim args = Throw fc $ "Unsupported external function " ++ show prim ++ "(" ++
        (show $ showNamedCExp 0 <$> args) ++ ")"

    inferExprLamWithParameterType : Maybe (Name, InferredType) -> (parameterValueExpr: Maybe (Asm ())) ->
        NamedCExp -> Asm InferredType
    inferExprLamWithParameterType parameterNameAndType parameterValueExpr expr = do
        let hasParameterValue = isJust parameterValueExpr
        let (_, lineStart, lineEnd) = getSourceLocation expr
        let jvmParameterNameAndType = (\(name, ty) => (jvmSimpleName name, ty)) <$> parameterNameAndType
        let lambdaType = getLambdaTypeByParameter (fst <$> parameterNameAndType)
        lambdaBodyReturnType <- withInferenceLambdaScope lineStart lineEnd (fst <$> parameterNameAndType) expr $ do
            traverse_ createAndAddVariable jvmParameterNameAndType
            maybe (Pure ()) id parameterValueExpr
            lambdaBodyReturnType <- inferExpr IUnknown expr
            currentScope <- getScope !getCurrentScopeIndex
            saveScope $ { returnType := lambdaBodyReturnType } currentScope
            Pure lambdaBodyReturnType
        Pure $ if hasParameterValue
            then lambdaBodyReturnType
            else getLambdaInterfaceType lambdaType
      where
        createAndAddVariable : (String, InferredType) -> Asm ()
        createAndAddVariable (name, ty) = do
            createVariable name
            ignore $ addVariableType name ty

    inferExprLamWithParameterType1 : (isCached : Bool) -> Maybe Name -> NamedCExp -> Asm InferredType
    inferExprLamWithParameterType1 True _ _ = Pure inferredLambdaType
    inferExprLamWithParameterType1 False parameterName expr =
      inferExprLamWithParameterType ((\name => (name, inferredObjectType)) <$> parameterName) Nothing expr

    inferExprLam : AppliedLambdaType -> (parameterValue: Maybe NamedCExp) -> (parameterName : Maybe Name) ->
                    NamedCExp -> Asm InferredType
    inferExprLam appliedLambdaType parameterValue@(Just value) (Just parameterName) lambdaBody = do
        valueType <-
            if appliedLambdaType == AppliedLambdaSwitch
                then case lambdaBody of
                    (NmConstCase _ _ alts _) => getConstantType alts
                    (NmConCase _ _ _ _) => Pure idrisObjectType
                    _ => Pure IUnknown
                else if appliedLambdaType == AppliedLambdaLet
                        then Pure inferredObjectType
                        else Pure IUnknown
        let shouldGenerateVariable = parameterName == extractedMethodArgumentName
        generatedJvmVariableName <-
            if shouldGenerateVariable
                then Pure $ jvmSimpleName parameterName ++ show !newDynamicVariableIndex
                else Pure $ jvmSimpleName parameterName
        let generatedVariableName =
            if shouldGenerateVariable
                then UN $ Basic generatedJvmVariableName
                else parameterName
        let valueExpr = NmLocal (getFC lambdaBody) generatedVariableName
        parentScope <- getScope !getCurrentScopeIndex
        inferExprLamWithParameterType (Just (generatedVariableName, valueType))
            (Just (inferValue parentScope shouldGenerateVariable generatedJvmVariableName valueType))
            (if appliedLambdaType == AppliedLambdaSwitch || appliedLambdaType == AppliedLambdaLet
                then substituteVariableSubMethodBody valueExpr lambdaBody
                else lambdaBody)
      where
        inferValue : Scope -> Bool -> String -> InferredType -> Asm ()
        inferValue enclosingScope shouldGenerateVariable variableName valueType = do
            lambdaScopeIndex <- getCurrentScopeIndex
            updateCurrentScopeIndex (index enclosingScope)
            when shouldGenerateVariable $ createVariable variableName
            ignore $ inferExpr valueType value
            ignore $ addVariableType variableName valueType
            updateCurrentScopeIndex lambdaScopeIndex
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmLam _ p4 (NmApp _ (NmRef _ name)
      [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2, NmLocal _ arg3, NmLocal _ arg4]))))) =
        inferExprLamWithParameterType1
          (maybe False ((==) arg0) p0 && p1 == arg1 && p2 == arg2 && p3 == arg3 && p4 == arg4) p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmLam _ p2 (NmLam _ p3 (NmApp _ (NmRef _ name)
      [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2, NmLocal _ arg3])))) =
        inferExprLamWithParameterType1
          (maybe False ((==) arg0) p0 && p1 == arg1 && p2 == arg2 && p3 == arg3) p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmLam _ p2 (NmApp _ (NmRef _ name)
      [NmLocal _ arg0, NmLocal _ arg1, NmLocal _ arg2]))) =
        inferExprLamWithParameterType1 (maybe False ((==) arg0) p0 && p1 == arg1 && p2 == arg2) p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ p1 (NmApp _ (NmRef _ name) [NmLocal _ arg0, NmLocal _ arg1])) =
        inferExprLamWithParameterType1 (maybe False ((==) arg0) p0 && p1 == arg1) p0 expr
    inferExprLam _ _ p0 expr@(NmApp _ (NmRef _ _) [NmLocal _ b]) =
      inferExprLamWithParameterType1 (maybe False ((==) b) p0) p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ c (NmLam _ a (NmLocal _ b))) =
      inferExprLamWithParameterType1 (isJust p0 && (c == b || a == b)) p0 expr
    inferExprLam _ _ p0 expr@(NmLam _ a (NmLocal _ b)) =
      inferExprLamWithParameterType1 (maybe False ((==) b) p0 || (isJust p0 && a == b)) p0 expr
    inferExprLam _ _ p0 expr@(NmLocal _ b) =
      inferExprLamWithParameterType1 (maybe False ((==) b) p0) p0 expr
    inferExprLam _ _ p0 expr = inferExprLamWithParameterType1 False p0 expr

    inferExprLet : FC -> InferredType -> (x : Name) -> NamedCExp -> NamedCExp -> Asm InferredType
    inferExprLet fc exprTy var value expr = do
        let (lineStart, lineEnd) = getLineNumbers (startPos (toNonEmptyFC fc)) (endPos (toNonEmptyFC fc))
        let varName = jvmSimpleName var
        createVariable varName
        let (_, lineStart, lineEnd) = getSourceLocation value
        valueTy <- withInferenceScope lineStart lineEnd $ inferExpr IUnknown value
        ignore $ addVariableType varName valueTy
        let (_, lineStart, lineEnd) = getSourceLocation expr
        withInferenceScope lineStart lineEnd $ inferExpr exprTy expr

    inferSelfTailCallParameter : Map Int InferredType -> Map Int String -> (NamedCExp, Int) -> Asm ()
    inferSelfTailCallParameter types argumentNameByIndices (arg, index) = do
        optTy <- LiftIo $ Map.get types index
        let variableType = fromMaybe IUnknown $ nullableToMaybe optTy
        ty <- inferExpr variableType arg
        optName <- LiftIo $ Map.get {value=String} argumentNameByIndices index
        maybe (Pure ()) (doAddVariableType ty) $ nullableToMaybe optName
      where
        doAddVariableType : InferredType -> String -> Asm ()
        doAddVariableType ty name = do
            ignore $ addVariableType name ty
            case arg of
                NmLocal _ loc => do
                    let valueVariableName = jvmSimpleName loc
                    valueVariableIndex <- retrieveVariableIndex valueVariableName
                    when (index /= valueVariableIndex) $ createNewVariable "tailRecArg" ty
                _ => createNewVariable "tailRecArg" ty

    inferExprApp : InferredType -> NamedCExp -> Asm InferredType
    inferExprApp exprTy app@(NmApp _ (NmRef _ (UN (Basic "$idrisTailRec"))) args) =
        case args of
            [] => Pure exprTy
            args@(_ :: argsTail) => do
                types <- retrieveVariableTypesAtScope !getCurrentScopeIndex
                argumentNameByIndices <- LiftIo $ Map.transpose $ variableIndices !(getScope 0)
                traverse_ (inferSelfTailCallParameter types argumentNameByIndices) $
                    zip args [0 .. the Int $ cast $ length argsTail]
                Pure exprTy
    inferExprApp exprTy (NmApp _ (NmRef _ idrisName) args) = do
        let functionName = jvmName idrisName
        functionType <- case !(findFunctionType functionName) of
            Just ty => Pure ty
            Nothing => Pure $ MkInferredFunctionType inferredObjectType $ replicate (length args) inferredObjectType
        let argsWithTypes = zip args (parameterTypes functionType)
        traverse_ inferParameter argsWithTypes
        Pure $ returnType functionType
    inferExprApp exprTy (NmApp _ lambdaVariable args) = do
        ignore $ inferExpr inferredLambdaType lambdaVariable
        let argsWithTypes = zip args (replicate (length args) IUnknown)
        traverse_ inferParameter argsWithTypes
        pure IUnknown
    inferExprApp _ _ = Throw emptyFC "Not a function application"

    inferExprCon : InferredType -> String -> Name -> List NamedCExp -> Asm InferredType
    inferExprCon exprTy fileName name args = do
        let argsWithTypes = zip args (replicate (length args) inferredObjectType)
        traverse_ inferParameter argsWithTypes
        pure idrisObjectType

    inferExprCast : InferredType -> InferredType -> NamedCExp -> Asm InferredType
    inferExprCast sourceType targetType expr = do
        ignore $ inferExpr sourceType expr
        pure targetType

    inferExprOp : PrimFn arity -> Vect arity NamedCExp -> Asm InferredType
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
    inferExprOp (LT ty) [x, y] = inferBoolOp (getInferredType ty) x y
    inferExprOp (LTE ty) [x, y] = inferBoolOp (getInferredType ty) x y
    inferExprOp (EQ ty) [x, y] = inferBoolOp (getInferredType ty) x y
    inferExprOp (GT ty) [x, y] = inferBoolOp (getInferredType ty) x y
    inferExprOp (GTE ty) [x, y] = inferBoolOp (getInferredType ty) x y

    inferExprOp StrLength [x] = do
        ignore $ inferExpr inferredStringType x
        pure IInt
    inferExprOp StrHead [x] = do
        ignore $ inferExpr inferredStringType x
        pure IChar
    inferExprOp StrTail [x] = do
        ignore $ inferExpr inferredStringType x
        pure inferredStringType
    inferExprOp StrIndex [x, i] = do
        ignore $ inferExpr inferredStringType x
        ignore $ inferExpr IInt i
        pure IChar
    inferExprOp StrCons [x, y] = do
        ignore $ inferExpr IChar x
        ignore $ inferExpr inferredStringType y
        pure inferredStringType
    inferExprOp StrAppend [x, y] = inferBinaryOp inferredStringType x y
    inferExprOp StrReverse [x] = do
        ignore $ inferExpr inferredStringType x
        pure inferredStringType
    inferExprOp StrSubstr [offset, len, str] = do
        ignore $ inferExpr IInt offset
        ignore $ inferExpr IInt len
        ignore $ inferExpr inferredStringType str
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

    inferExprOp (Cast ty1 ty2) [x] = inferExprCast (getInferredType ty1) (getInferredType ty2) x

    inferExprOp BelieveMe [a, b, x] = do
      ignore $ inferExpr IUnknown a
      ignore $ inferExpr IUnknown b
      ignore $ inferExpr IUnknown x
      Pure IUnknown
    inferExprOp Crash [_, msg] = do
      ignore $ inferExpr inferredStringType msg
      Pure IUnknown
    inferExprOp op _ = Throw emptyFC ("Unsupported primitive function " ++ show op)

export
%inline
emptyFunction : NamedCExp
emptyFunction = NmCrash emptyFC "uninitialized function"

showScopes : Int -> Asm ()
showScopes n = do
    scope <- getScope n
    logAsm $ show scope
    when (n > 0) $ showScopes (n - 1)

tailRecLoopFunctionName : Name
tailRecLoopFunctionName =
  NS (mkNamespace "io.github.mmhelloworld.idrisjvm.runtime.Runtime") (UN $ Basic "tailRec")

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
optimize : String -> List (Name, FC, NamedDef) -> List (Name, FC, NamedDef)
optimize programName allDefs =
  let tailRecOptimizedDefs = concatMap (optimizeTailRecursion programName) allDefs
      tailCallOptimizedDefs = TailRec.functions tailRecLoopFunctionName tailRecOptimizedDefs
  in toNameFcDef <$> tailCallOptimizedDefs

export
inferDef : String -> Name -> FC -> NamedDef -> Asm ()
inferDef programName idrisName fc (MkNmFun args expr) = do
    let jname = jvmName idrisName
    let jvmClassAndMethodName = getIdrisFunctionName programName (className jname) (methodName jname)
    let argumentNames = jvmSimpleName <$> args
    let arity = length args
    let arityInt = the Int $ cast arity
    argIndices <- LiftIo $ getArgumentIndices arityInt argumentNames
    let initialArgumentTypes = replicate arity inferredObjectType
    let inferredFunctionType = MkInferredFunctionType inferredObjectType initialArgumentTypes
    argumentTypesByName <- LiftIo $ Map.fromList $ zip argumentNames initialArgumentTypes
    scopes <- LiftIo $ ArrayList.new {elemTy=Scope}
    let function = MkFunction jname inferredFunctionType (subtyping scopes) 0 jvmClassAndMethodName emptyFunction
    setCurrentFunction function
    LiftIo $ AsmGlobalState.addFunction !getGlobalState jname function
    updateCurrentFunction $ { optimizedBody := expr }

    resetScope
    scopeIndex <- newScopeIndex
    let (_, lineStart, lineEnd) = getSourceLocation expr
    allVariableTypes <- LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
    allVariableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
    let functionScope =
        MkScope scopeIndex Nothing argumentTypesByName allVariableTypes argIndices
            allVariableIndices IUnknown arityInt (lineStart, lineEnd) ("", "") []

    saveScope functionScope
    retTy <- inferExpr IUnknown expr
    updateScopeVariableTypes arity
    updateCurrentFunction $ { inferredFunctionType := inferredFunctionType }
    when (shouldDebugFunction jname) $ showScopes (scopeCounter !GetState - 1)
  where
    getArgumentTypes : List String -> Asm (List InferredType)
    getArgumentTypes argumentNames = do
        argumentIndicesByName <- getVariableIndicesByName 0
        argumentTypesByIndex <- getVariableTypesAtScope 0
        LiftIo $ go argumentIndicesByName argumentTypesByIndex argumentNames
      where
        go : Map String Int -> Map Int InferredType -> List String -> IO (List InferredType)
        go argumentIndicesByName argumentTypesByIndex argumentNames = do
            types <- go1 [] argumentNames
            pure $ reverse types
          where
            go1 : List InferredType -> List String -> IO (List InferredType)
            go1 acc [] = pure acc
            go1 acc (arg :: args) = do
                optIndex <- Map.get {value=Int} argumentIndicesByName arg
                ty <- case nullableToMaybe optIndex of
                    Just index => do
                        optTy <- Map.get argumentTypesByIndex index
                        pure $ fromMaybe IUnknown $ nullableToMaybe optTy
                    Nothing => pure IUnknown
                go1 (ty :: acc) args

inferDef programName n fc (MkNmError expr) = inferDef programName n fc (MkNmFun [] expr)

inferDef programName idrisName fc def@(MkNmForeign foreignDescriptors argumentTypes returnType) =
    inferForeign programName idrisName fc foreignDescriptors argumentTypes returnType

inferDef _ _ _ _ = Pure ()

export
asm : AsmState -> Asm a -> IO (a, AsmState)
asm = if shouldDebugAsm then mockRunAsm else runAsm
