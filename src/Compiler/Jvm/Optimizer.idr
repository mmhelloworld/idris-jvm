module Compiler.Jvm.Optimizer

import Compiler.Common
import Compiler.CompileExpr
import Compiler.Inline

import Control.Monad.State

import Core.Context
import Core.Name
import Core.TT

import Libraries.Data.SortedMap
import Libraries.Data.SortedSet
import Data.List
import Data.Maybe
import Data.Strings
import Data.Vect

import Compiler.Jvm.Asm
import Compiler.Jvm.ExtPrim
import Compiler.Jvm.Foreign
import Compiler.Jvm.InferredType
import Compiler.Jvm.Jname
import Compiler.Jvm.ShowUtil

%hide Core.Context.Context.Constructor.arity

mutual
    hasTailCall : (predicate: Name -> Bool) -> NamedCExp -> Bool
    hasTailCall predicate (NmLet _ _ _ expr) = hasTailCall predicate expr
    hasTailCall predicate (NmApp _ (NmRef _ name) _) = predicate name
    hasTailCall predicate (NmApp _ lambdaVariable _) = predicate (UN $ Basic "")
    hasTailCall predicate (NmExtPrim fc p args) = predicate (UN $ Basic "")
    hasTailCall predicate (NmConCase _ _ conAlts def) =
        maybe False (\defExp => hasTailCall predicate defExp) def || hasTailCallConAlt predicate conAlts
    hasTailCall predicate (NmConstCase _ _ constAlts def) =
        maybe False (\defExp => hasTailCall predicate defExp) def || hasTailCallConstAlt predicate constAlts
    hasTailCall _ _ = False

    hasTailCallConAlt : (predicate: Name -> Bool) -> List NamedConAlt -> Bool
    hasTailCallConAlt predicate [] = False
    hasTailCallConAlt predicate ((MkNConAlt _ _ _ _ expr) :: alts) =
        hasTailCall predicate expr || hasTailCallConAlt predicate alts

    hasTailCallConstAlt : (predicate: Name -> Bool) -> List NamedConstAlt -> Bool
    hasTailCallConstAlt predicate [] = False
    hasTailCallConstAlt predicate ((MkNConstAlt _ expr) :: alts) =
        hasTailCall predicate expr || hasTailCallConstAlt predicate alts

export
thunkParamName : Name
thunkParamName = UN $ Basic "$jvm$thunk"

thunkExpr : NamedCExp -> NamedCExp
thunkExpr expr = NmLam (getFC expr) thunkParamName expr

isBoolTySpec : String -> Name -> Bool
isBoolTySpec "Prelude" (UN (Basic "Bool")) = True
isBoolTySpec "Prelude.Basics" (UN (Basic "Bool")) = True
isBoolTySpec _ _ = False

export
tySpec : NamedCExp -> Asm InferredType
tySpec (NmCon fc (UN (Basic "Int")) _ _ []) = pure IInt
tySpec (NmCon fc (UN (Basic "Integer")) _ _ []) = pure inferredBigIntegerType
tySpec (NmCon fc (UN (Basic "String")) _ _ []) = pure inferredStringType
tySpec (NmCon fc (UN (Basic "Double")) _ _ []) = pure IDouble
tySpec (NmCon fc (UN (Basic "Char")) _ _ []) = pure IChar
tySpec (NmCon fc (UN (Basic "Bool")) _ _ []) = pure IBool
tySpec (NmCon fc (UN (Basic "long")) _ _ []) = pure ILong
tySpec (NmCon fc (UN (Basic "void")) _ _ []) = pure IVoid
tySpec (NmCon fc (UN (Basic ty)) _ _ []) = pure $ IRef ty
tySpec (NmCon fc (NS namespaces n) _ _ []) = cond
    [(n == UN (Basic "Unit"), pure IVoid),
      (isBoolTySpec (show namespaces) n, pure IBool)] (pure inferredObjectType)
tySpec ty = pure inferredObjectType

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

mutual
    goLiftToLambda : (isTailPosition: Bool) -> NamedCExp -> State Int NamedCExp
    goLiftToLambda False (NmLet fc var value sc) = do
        let extractedMethodArgumentVarName = extractedMethodArgumentName
        let extractedMethodArgumentVar = NmLocal fc extractedMethodArgumentVarName
        liftedValue <- goLiftToLambda False value
        liftedSc <- goLiftToLambda True sc
        let body = NmLet fc var extractedMethodArgumentVar liftedSc
        pure $ NmApp appliedLambdaLetIndicator (NmLam fc extractedMethodArgumentVarName body) [liftedValue]
    goLiftToLambda True (NmLet fc var value sc) =
        pure $ NmLet fc var !(goLiftToLambda False value) !(goLiftToLambda True sc)
    goLiftToLambda False (NmConCase fc sc alts def) = do
        put $ succ !get
        let var = extractedMethodArgumentName
        liftedSc <- goLiftToLambda False sc
        liftedAlts <- traverse liftToLambdaCon alts
        liftedDef <- traverse liftToLambdaDefault def
        pure $ NmApp appliedLambdaSwitchIndicator
            (NmLam fc var (NmConCase fc (NmLocal fc var) liftedAlts liftedDef)) [liftedSc]
    goLiftToLambda True expr@(NmConCase fc sc alts def) = do
        cases <- get
        put $ succ cases
        if cases > maxCasesInMethod
            then do
                put $ the Int 0
                goLiftToLambda False expr
            else do
                liftedAlts <- traverse liftToLambdaCon alts
                liftedDef <- traverse liftToLambdaDefault def
                pure $ NmConCase fc !(goLiftToLambda False sc) liftedAlts liftedDef
    goLiftToLambda False (NmConstCase fc sc alts def) = do
        put $ succ !get
        let var = extractedMethodArgumentName
        liftedAlts <- traverse liftToLambdaConst alts
        liftedDef <- traverse liftToLambdaDefault def
        liftedSc <- goLiftToLambda False sc
        pure $ NmApp appliedLambdaSwitchIndicator
            (NmLam fc var $ NmConstCase fc (NmLocal fc var) liftedAlts liftedDef) [liftedSc]
    goLiftToLambda True expr@(NmConstCase fc sc alts def) = do
        cases <- get
        put $ succ cases
        if cases > maxCasesInMethod
            then do
                put $ the Int 0
                goLiftToLambda False expr
            else do
                liftedSc <- goLiftToLambda False sc
                liftedAlts <- traverse liftToLambdaConst alts
                liftedDef <- traverse liftToLambdaDefault def
                pure $ NmConstCase fc liftedSc liftedAlts liftedDef
    goLiftToLambda _ (NmLam fc param sc) = pure $ NmLam fc param !(goLiftToLambda True sc)
    goLiftToLambda _ (NmApp fc f args) =
        pure $ NmApp fc !(goLiftToLambda False f) !(traverse (goLiftToLambda False) args)
    goLiftToLambda _ expr@(NmCon fc name conInfo tag args) =
        pure $ NmCon fc name conInfo tag !(traverse (goLiftToLambda False) args)
    goLiftToLambda _ (NmOp fc f args) = pure $ NmOp fc f !(traverse (goLiftToLambda False) args)
    goLiftToLambda _ (NmExtPrim fc f args) = pure $ NmExtPrim fc f !(traverse (goLiftToLambda False) args)
    goLiftToLambda _ (NmForce fc reason t) = pure $ NmForce fc reason !(goLiftToLambda False t)
    goLiftToLambda _ (NmDelay fc reason t) = pure $ NmDelay fc reason !(goLiftToLambda True t)
    goLiftToLambda _ expr = pure expr

    liftToLambdaDefault : NamedCExp -> State Int NamedCExp
    liftToLambdaDefault body = goLiftToLambda True body

    liftToLambdaCon : NamedConAlt -> State Int NamedConAlt
    liftToLambdaCon (MkNConAlt n conInfo tag args body) =
      pure $ MkNConAlt n conInfo tag args !(goLiftToLambda True body)

    liftToLambdaConst : NamedConstAlt -> State Int NamedConstAlt
    liftToLambdaConst (MkNConstAlt constant body) = pure $ MkNConstAlt constant !(goLiftToLambda True body)

liftToLambda : NamedCExp -> NamedCExp
liftToLambda expr = evalState 0 (goLiftToLambda True expr)

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

jvmTailRecName : Name
jvmTailRecName = UN $ Basic "$jvmTailRec"

mutual
    markTailRecursion : NamedCExp -> Asm NamedCExp
    markTailRecursion expr@(NmApp fc (NmRef nameFc idrisName) args) =
        let jname = jvmName idrisName
            functionName = getIdrisFunctionName !getProgramName (className jname) (methodName jname)
        in if functionName == !getRootMethodName
               then Pure (NmApp fc (NmRef nameFc jvmTailRecName) args)
               else Pure expr
    markTailRecursion expr@(NmLet fc var value body) =
        NmLet fc var value <$> markTailRecursion body
    markTailRecursion expr@(NmConCase fc sc alts def) = do
        tailRecursionMarkedAlts <- traverse markTailRecursionConAlt alts
        tailRecursionMarkedDefault <- traverse markTailRecursion def
        Pure (NmConCase fc sc tailRecursionMarkedAlts tailRecursionMarkedDefault)
    markTailRecursion (NmConstCase fc sc alts def) = do
        tailRecursionMarkedAlts <- traverse markTailRecursionConstAlt alts
        tailRecursionMarkedDefault <- traverse markTailRecursion def
        Pure (NmConstCase fc sc tailRecursionMarkedAlts tailRecursionMarkedDefault)
    markTailRecursion expr = Pure expr

    markTailRecursionConAlt : NamedConAlt -> Asm NamedConAlt
    markTailRecursionConAlt (MkNConAlt name conInfo tag args caseBody) =
        MkNConAlt name conInfo tag args <$> markTailRecursion caseBody

    markTailRecursionConstAlt : NamedConstAlt -> Asm NamedConstAlt
    markTailRecursionConstAlt (MkNConstAlt constant caseBody) = MkNConstAlt constant <$> markTailRecursion caseBody

optThunkExpr : Bool -> NamedCExp -> NamedCExp
optThunkExpr True = thunkExpr
optThunkExpr _ = id

mutual
    trampolineExpression : (isTailRec: Bool) -> NamedCExp -> NamedCExp
    -- Do not trampoline as tail recursion will be eliminated
    trampolineExpression _ (NmApp fc (NmRef nameFc (UN (Basic "$jvmTailRec"))) args) =
      NmApp fc (NmRef nameFc jvmTailRecName) (trampolineExpression False <$> args)
    trampolineExpression isTailRec (NmApp fc f args) =
      optThunkExpr isTailRec $ NmApp fc (trampolineExpression False f) (trampolineExpression False <$> args)
    trampolineExpression _ (NmLam fc param body) = NmLam fc param $ trampolineExpression False body
    trampolineExpression isTailRec (NmLet fc var value body) =
        NmLet fc var (trampolineExpression False value) $ trampolineExpression isTailRec body
    trampolineExpression _ (NmConCase fc sc alts def) =
        let trampolinedAlts = trampolineExpressionConAlt <$> alts
            trampolinedDefault = trampolineExpression True <$> def
        in NmConCase fc sc trampolinedAlts trampolinedDefault
    trampolineExpression _ (NmConstCase fc sc alts def) =
        let trampolinedAlts = trampolineExpressionConstAlt <$> alts
            trampolinedDefault = trampolineExpression True <$> def
        in NmConstCase fc sc trampolinedAlts trampolinedDefault
    trampolineExpression _ expr = expr

    trampolineExpressionConAlt : NamedConAlt -> NamedConAlt
    trampolineExpressionConAlt (MkNConAlt name conInfo tag args caseBody) =
        MkNConAlt name conInfo tag args $ trampolineExpression True caseBody

    trampolineExpressionConstAlt : NamedConstAlt -> NamedConstAlt
    trampolineExpressionConstAlt (MkNConstAlt constant caseBody) =
      MkNConstAlt constant $ trampolineExpression True caseBody

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
data LambdaType = ThunkLambda | DelayedLambda | FunctionLambda | Function2Lambda | Function3Lambda | Function4Lambda |
                    Function5Lambda

export
Eq LambdaType where
    ThunkLambda == ThunkLambda = True
    DelayedLambda == DelayedLambda = True
    FunctionLambda == FunctionLambda = True
    Function2Lambda == Function2Lambda = True
    Function3Lambda == Function3Lambda = True
    Function4Lambda == Function4Lambda = True
    Function5Lambda == Function5Lambda = True
    _ == _ = False

export
getLambdaTypeByParameter : (parameterName: Maybe Name) -> LambdaType
getLambdaTypeByParameter (Just (UN (Basic "$jvm$thunk"))) = ThunkLambda
getLambdaTypeByParameter Nothing = DelayedLambda
getLambdaTypeByParameter _ = FunctionLambda

export
getLambdaInterfaceMethodName : LambdaType -> String
getLambdaInterfaceMethodName ThunkLambda = "evaluate"
getLambdaInterfaceMethodName DelayedLambda = "evaluate"
getLambdaInterfaceMethodName _ = "apply"

export
getSamDesc : LambdaType -> String
getSamDesc ThunkLambda = "()" ++ getJvmTypeDescriptor thunkType
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
getLambdaInterfaceType : LambdaType -> InferredType -> InferredType
getLambdaInterfaceType ThunkLambda returnType = getThunkType returnType
getLambdaInterfaceType DelayedLambda returnType = delayedType
getLambdaInterfaceType _ returnType = inferredLambdaType

export
getLambdaImplementationMethodReturnType : LambdaType -> InferredType
getLambdaImplementationMethodReturnType ThunkLambda = thunkType
getLambdaImplementationMethodReturnType _ = inferredObjectType

export
getConstantType : List NamedConstAlt -> Asm InferredType
getConstantType [] = Throw emptyFC "Unknown constant switch type"
getConstantType ((MkNConstAlt constant _) :: _) = case constant of
    I _ => Pure IInt
    B8 _ => Pure IInt
    B16 _ => Pure IInt
    B32 _ => Pure IInt
    Ch _ => Pure IInt
    Str _ => Pure inferredStringType
    BI _ => Pure inferredBigIntegerType
    B64 _ => Pure inferredBigIntegerType
    unsupportedConstant => Throw emptyFC $ "Unsupported constant switch " ++ show unsupportedConstant

export
isTypeConst : TT.Constant -> Bool
isTypeConst Bits8Type   = True
isTypeConst Bits16Type  = True
isTypeConst Bits32Type  = True
isTypeConst Bits64Type  = True
isTypeConst IntType     = True
isTypeConst IntegerType = True
isTypeConst StringType  = True
isTypeConst CharType    = True
isTypeConst DoubleType  = True
isTypeConst WorldType   = True
isTypeConst _           = False

export
getIntConstantValue : FC -> TT.Constant -> Asm Int
getIntConstantValue _ (I i) = Pure i
getIntConstantValue _ (B8 i) = Pure i
getIntConstantValue _ (B16 i) = Pure i
getIntConstantValue _ (B32 i) = Pure i
getIntConstantValue _ (Ch c) = Pure $ ord c
getIntConstantValue _ WorldVal = Pure 0
getIntConstantValue fc x =
    if isTypeConst x
        then Pure 0
        else Throw fc ("Constant " ++ show x ++ " cannot be converted to integer.")

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

mutual
    inferExpr : InferredType -> NamedCExp -> Asm InferredType
    inferExpr exprTy (NmDelay _ _ expr) = inferExprLam AppliedLambdaUnknown Nothing Nothing expr
    inferExpr exprTy expr@(NmLocal _ var) = addVariableType (jvmSimpleName var) exprTy
    inferExpr exprTy (NmRef _ _) = pure exprTy
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

    inferExpr _ (NmPrimVal fc (B8 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (B16 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (B32 _)) = pure IInt
    inferExpr _ (NmPrimVal fc (B64 _)) = pure ILong
    inferExpr _ (NmPrimVal fc (I _)) = pure IInt
    inferExpr _ (NmPrimVal fc (BI _)) = pure inferredBigIntegerType
    inferExpr _ (NmPrimVal fc (Str _)) = pure inferredStringType
    inferExpr _ (NmPrimVal fc (Ch _)) = pure IChar
    inferExpr _ (NmPrimVal fc (Db _)) = pure IDouble
    inferExpr _ (NmPrimVal fc WorldVal) = pure IInt
    inferExpr exprTy (NmErased fc) = pure exprTy
    inferExpr exprTy (NmCrash fc msg) = pure exprTy
    inferExpr exprTy expr = Throw (getFC expr) ("Unsupported expr " ++ show expr)

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
    inferExtPrim fc returnType JvmInstanceMethodCall descriptors =
        inferExtPrim fc returnType JvmStaticMethodCall descriptors
    inferExtPrim _ returnType JvmStaticMethodCall [ret, NmPrimVal fc (Str fn), fargs, world]
      = do args <- getFArgs fargs
           argTypes <- traverse tySpec (map fst args)
           methodReturnType <- tySpec ret
           traverse_ inferExtPrimArg $ zip (map snd args) argTypes
           pure $ if methodReturnType == IVoid then inferredObjectType else methodReturnType
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
    inferExtPrim _ returnType MakeFuture [_, action] = do
        ignore $ inferExpr delayedType action
        pure inferredForkJoinTaskType
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
            when (lambdaType /= ThunkLambda) $
                traverse_ createAndAddVariable jvmParameterNameAndType
            maybe (Pure ()) id parameterValueExpr
            lambdaBodyReturnType <- inferExpr IUnknown expr
            currentScope <- getScope !getCurrentScopeIndex
            saveScope $ record {returnType = lambdaBodyReturnType} currentScope
            Pure lambdaBodyReturnType
        Pure $ if hasParameterValue
            then lambdaBodyReturnType
            else getLambdaInterfaceType lambdaType lambdaBodyReturnType
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
        ignore $ addVariableType varName (if isThunkType valueTy then inferredObjectType else valueTy)
        let (_, lineStart, lineEnd) = getSourceLocation expr
        withInferenceScope lineStart lineEnd $ inferExpr exprTy expr

    inferSelfTailCallParameter : Map Int InferredType -> Map Int String -> (NamedCExp, Int) -> Asm ()
    inferSelfTailCallParameter types argumentNameByIndices (arg, index) = do
        optTy <- LiftIo $ Map.get types index
        let variableType = fromMaybe IUnknown optTy
        ty <- inferExpr variableType arg
        optName <- LiftIo $ Map.get {value=String} argumentNameByIndices index
        maybe (Pure ()) (doAddVariableType ty) optName
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
    inferExprApp exprTy app@(NmApp _ (NmRef _ (UN (Basic "$jvmTailRec"))) args) =
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
    inferExprOp (Add Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Sub Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mul Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Div Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mod Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Neg Bits8Type) [x] = inferUnaryOp IInt x
    inferExprOp (ShiftL Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (ShiftR Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BAnd Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BOr Bits8Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BXOr Bits8Type) [x, y] = inferBinaryOp IInt x y

    inferExprOp (Add Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Sub Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mul Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Div Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mod Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Neg Bits16Type) [x] = inferUnaryOp IInt x
    inferExprOp (ShiftL Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (ShiftR Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BAnd Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BOr Bits16Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BXOr Bits16Type) [x, y] = inferBinaryOp IInt x y

    inferExprOp (Add Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Sub Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mul Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Div Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mod Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Neg Bits32Type) [x] = inferUnaryOp IInt x
    inferExprOp (ShiftL Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (ShiftR Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BAnd Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BOr Bits32Type) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BXOr Bits32Type) [x, y] = inferBinaryOp IInt x y

    inferExprOp (Add Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (Sub Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (Mul Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (Div Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (Mod Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (Neg Bits64Type) [x] = inferUnaryOp ILong x
    inferExprOp (ShiftL Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (ShiftR Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (BAnd Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (BOr Bits64Type) [x, y] = inferBinaryOp ILong x y
    inferExprOp (BXOr Bits64Type) [x, y] = inferBinaryOp ILong x y

    inferExprOp (Add IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Sub IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mul IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Div IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Mod IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (Neg IntType) [x] = inferUnaryOp IInt x
    inferExprOp (ShiftL IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (ShiftR IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BAnd IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BOr IntType) [x, y] = inferBinaryOp IInt x y
    inferExprOp (BXOr IntType) [x, y] = inferBinaryOp IInt x y

    inferExprOp (Add IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (Sub IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (Mul IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (Div IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (Mod IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (Neg IntegerType) [x] = inferUnaryOp inferredBigIntegerType x
    inferExprOp (ShiftL IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (ShiftR IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (BAnd IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (BOr IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y
    inferExprOp (BXOr IntegerType) [x, y] = inferBinaryOp inferredBigIntegerType x y

    inferExprOp (Add DoubleType) [x, y] = inferBinaryOp IDouble x y
    inferExprOp (Sub DoubleType) [x, y] = inferBinaryOp IDouble x y
    inferExprOp (Mul DoubleType) [x, y] = inferBinaryOp IDouble x y
    inferExprOp (Div DoubleType) [x, y] = inferBinaryOp IDouble x y
    inferExprOp (Neg DoubleType) [x] = inferUnaryOp IDouble x

    inferExprOp (LT Bits8Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (LT Bits16Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (LT Bits32Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (LT Bits64Type) [x, y] = inferBoolOp ILong x y
    inferExprOp (LT IntType) [x, y] = inferBoolOp IInt x y
    inferExprOp (LT CharType) [x, y] = inferBoolOp IChar x y
    inferExprOp (LT IntegerType) [x, y] = inferBoolOp inferredBigIntegerType x y
    inferExprOp (LT DoubleType) [x, y] = inferBoolOp IDouble x y
    inferExprOp (LT StringType) [x, y] = inferBoolOp inferredStringType x y

    inferExprOp (LTE Bits8Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (LTE Bits16Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (LTE Bits32Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (LTE Bits64Type) [x, y] = inferBoolOp ILong x y
    inferExprOp (LTE IntType) [x, y] = inferBoolOp IInt x y
    inferExprOp (LTE CharType) [x, y] = inferBoolOp IChar x y
    inferExprOp (LTE IntegerType) [x, y] = inferBoolOp inferredBigIntegerType x y
    inferExprOp (LTE DoubleType) [x, y] = inferBoolOp IDouble x y
    inferExprOp (LTE StringType) [x, y] = inferBoolOp inferredStringType x y

    inferExprOp (EQ Bits8Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (EQ Bits16Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (EQ Bits32Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (EQ Bits64Type) [x, y] = inferBoolOp ILong x y
    inferExprOp (EQ IntType) [x, y] = inferBoolOp IInt x y
    inferExprOp (EQ CharType) [x, y] = inferBoolOp IChar x y
    inferExprOp (EQ IntegerType) [x, y] = inferBoolOp inferredBigIntegerType x y
    inferExprOp (EQ DoubleType) [x, y] = inferBoolOp IDouble x y
    inferExprOp (EQ StringType) [x, y] = inferBoolOp inferredStringType x y

    inferExprOp (GT Bits8Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (GT Bits16Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (GT Bits32Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (GT Bits64Type) [x, y] = inferBoolOp ILong x y
    inferExprOp (GT IntType) [x, y] = inferBoolOp IInt x y
    inferExprOp (GT CharType) [x, y] = inferBoolOp IChar x y
    inferExprOp (GT IntegerType) [x, y] = inferBoolOp inferredBigIntegerType x y
    inferExprOp (GT DoubleType) [x, y] = inferBoolOp IDouble x y
    inferExprOp (GT StringType) [x, y] = inferBoolOp inferredStringType x y

    inferExprOp (GTE Bits8Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (GTE Bits16Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (GTE Bits32Type) [x, y] = inferBoolOp IInt x y
    inferExprOp (GTE Bits64Type) [x, y] = inferBoolOp ILong x y
    inferExprOp (GTE IntType) [x, y] = inferBoolOp IInt x y
    inferExprOp (GTE CharType) [x, y] = inferBoolOp IChar x y
    inferExprOp (GTE IntegerType) [x, y] = inferBoolOp inferredBigIntegerType x y
    inferExprOp (GTE DoubleType) [x, y] = inferBoolOp IDouble x y
    inferExprOp (GTE StringType) [x, y] = inferBoolOp inferredStringType x y

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
    inferExprOp DoubleSin [x] = inferUnaryOp IDouble x
    inferExprOp DoubleCos [x] = inferUnaryOp IDouble x
    inferExprOp DoubleTan [x] = inferUnaryOp IDouble x
    inferExprOp DoubleASin [x] = inferUnaryOp IDouble x
    inferExprOp DoubleACos [x] = inferUnaryOp IDouble x
    inferExprOp DoubleATan [x] = inferUnaryOp IDouble x
    inferExprOp DoubleSqrt [x] = inferUnaryOp IDouble x
    inferExprOp DoubleFloor [x] = inferUnaryOp IDouble x
    inferExprOp DoubleCeiling [x] = inferUnaryOp IDouble x

    inferExprOp (Cast Bits8Type Bits16Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits8Type Bits32Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits8Type IntType) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits8Type Bits64Type) [x] = inferExprCast IInt ILong x
    inferExprOp (Cast Bits8Type IntegerType) [x] = inferExprCast IInt inferredBigIntegerType x
    inferExprOp (Cast Bits8Type StringType) [x] = inferExprCast IInt inferredStringType x

    inferExprOp (Cast Bits16Type Bits8Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits16Type Bits32Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits16Type IntType) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits16Type Bits64Type) [x] = inferExprCast IInt ILong x
    inferExprOp (Cast Bits16Type IntegerType) [x] = inferExprCast IInt inferredBigIntegerType x
    inferExprOp (Cast Bits16Type StringType) [x] = inferExprCast IInt inferredStringType x

    inferExprOp (Cast Bits32Type Bits8Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits32Type Bits16Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits32Type IntType) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast Bits32Type Bits64Type) [x] = inferExprCast IInt ILong x
    inferExprOp (Cast Bits32Type IntegerType) [x] = inferExprCast IInt inferredBigIntegerType x
    inferExprOp (Cast Bits32Type StringType) [x] = inferExprCast IInt inferredStringType x

    inferExprOp (Cast Bits64Type Bits8Type) [x] = inferExprCast ILong IInt x
    inferExprOp (Cast Bits64Type Bits16Type) [x] = inferExprCast ILong IInt x
    inferExprOp (Cast Bits64Type Bits32Type) [x] = inferExprCast ILong IInt x
    inferExprOp (Cast Bits64Type IntType) [x] = inferExprCast ILong IInt x
    inferExprOp (Cast Bits64Type IntegerType) [x] = inferExprCast ILong inferredBigIntegerType x
    inferExprOp (Cast Bits64Type StringType) [x] = inferExprCast ILong inferredStringType x

    inferExprOp (Cast IntType Bits8Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast IntType Bits16Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast IntType Bits32Type) [x] = inferExprCast IInt IInt x
    inferExprOp (Cast IntType Bits64Type) [x] = inferExprCast IInt ILong x
    inferExprOp (Cast IntType IntegerType) [x] = inferExprCast IInt inferredBigIntegerType x
    inferExprOp (Cast IntType StringType) [x] = inferExprCast IInt inferredStringType x

    inferExprOp (Cast IntegerType Bits8Type)  [x] = inferExprCast inferredBigIntegerType IInt x
    inferExprOp (Cast IntegerType Bits16Type) [x] = inferExprCast inferredBigIntegerType IInt x
    inferExprOp (Cast IntegerType Bits32Type) [x] = inferExprCast inferredBigIntegerType IInt x
    inferExprOp (Cast IntegerType IntType) [x] = inferExprCast inferredBigIntegerType IInt x
    inferExprOp (Cast IntegerType Bits64Type) [x] = inferExprCast inferredBigIntegerType ILong x
    inferExprOp (Cast IntegerType StringType) [x] = inferExprCast inferredBigIntegerType inferredStringType x

    inferExprOp (Cast DoubleType StringType) [x] = inferExprCast IDouble inferredStringType x
    inferExprOp (Cast CharType StringType) [x] = inferExprCast IChar inferredStringType x
    inferExprOp (Cast DoubleType IntegerType) [x] = inferExprCast IDouble inferredBigIntegerType x
    inferExprOp (Cast CharType IntegerType) [x] = inferExprCast IChar inferredBigIntegerType x
    inferExprOp (Cast StringType IntegerType) [x] = inferExprCast inferredStringType inferredBigIntegerType x
    inferExprOp (Cast DoubleType IntType) [x] = inferExprCast IDouble IInt x
    inferExprOp (Cast StringType IntType) [x] = inferExprCast inferredStringType IInt x
    inferExprOp (Cast CharType IntType) [x] = inferExprCast IChar IInt x
    inferExprOp (Cast IntegerType DoubleType) [x] = inferExprCast inferredBigIntegerType IDouble x
    inferExprOp (Cast IntType DoubleType) [x] = inferExprCast IInt IDouble x
    inferExprOp (Cast StringType DoubleType) [x] = inferExprCast inferredStringType IDouble x
    inferExprOp (Cast IntType CharType) [x] = inferExprCast IInt IChar x

    inferExprOp BelieveMe [_, _, x] = Pure IUnknown
    inferExprOp Crash [_, msg] = Pure IUnknown
    inferExprOp op _ = Throw emptyFC ("Unsupported primitive function " ++ show op)

optimize : Jname -> TailCallCategory -> NamedCExp -> Asm NamedCExp
optimize jname tailCallCategory expr = do
  inlinedAndTailRecursionMarkedExpr <- markTailRecursion . liftToLambda $ expr
  shouldTrampoline <- LiftIo $ AsmGlobalState.shouldTrampoline !getGlobalState (show jname)
  Pure $ if shouldTrampoline && hasNonSelfTailCall tailCallCategory
    then trampolineExpression True inlinedAndTailRecursionMarkedExpr
    else inlinedAndTailRecursionMarkedExpr

export
%inline
emptyFunction : NamedCExp
emptyFunction = NmCrash emptyFC "uninitialized function"

showScopes : Int -> Asm ()
showScopes n = do
    scope <- getScope n
    debug $ show scope
    when (n > 0) $ showScopes (n - 1)

export
inferDef : String -> Name -> FC -> NamedDef -> Asm ()
inferDef programName idrisName fc (MkNmFun args body) = do
    let jname = jvmName idrisName
    let hasSelfTailCall = hasTailCall (== idrisName) body
    let hasNonSelfTailCall = hasTailCall (/= idrisName) body
    let jvmClassAndMethodName = getIdrisFunctionName programName (className jname) (methodName jname)
    let tailCallCategory = MkTailCallCategory hasSelfTailCall hasNonSelfTailCall
    let arity = length args
    let arityInt = the Int $ cast arity
    let expr = if arityInt == 0 then NmDelay fc LLazy body else body
    let argumentNames = jvmSimpleName <$> args
    argIndices <- LiftIo $ getArgumentIndices arityInt argumentNames
    let initialArgumentTypes = replicate arity inferredObjectType
    let inferredFunctionType = MkInferredFunctionType inferredObjectType initialArgumentTypes
    argumentTypesByName <- LiftIo $ Map.fromList $ zip argumentNames initialArgumentTypes
    scopes <- LiftIo $ JList.new {a=Scope}
    let function = MkFunction jname inferredFunctionType scopes 0 jvmClassAndMethodName emptyFunction
    setCurrentFunction function
    LiftIo $ AsmGlobalState.addFunction !getGlobalState jname function
    let shouldDebugExpr = shouldDebug &&
        (fromMaybe True $ ((\name => name `isInfixOf` (getSimpleName jname)) <$> debugFunction))
    when shouldDebugExpr $
      debug $ "Inferring " ++ (className jvmClassAndMethodName) ++ "." ++ (methodName jvmClassAndMethodName) ++
        ":\n" ++ showNamedCExp 0 expr
    optimizedExpr <- optimize jname tailCallCategory expr
    updateCurrentFunction $ record { optimizedBody = optimizedExpr }

    resetScope
    scopeIndex <- newScopeIndex
    let (_, lineStart, lineEnd) = getSourceLocation expr
    allVariableTypes <- LiftIo $ Map.newTreeMap {key=Int} {value=InferredType}
    allVariableIndices <- LiftIo $ Map.newTreeMap {key=String} {value=Int}
    let functionScope =
        MkScope scopeIndex Nothing argumentTypesByName allVariableTypes argIndices
            allVariableIndices IUnknown arityInt (lineStart, lineEnd) ("", "") []

    saveScope functionScope
    retTy <- inferExpr IUnknown optimizedExpr
    updateScopeVariableTypes arity
    updateCurrentFunction $ record { inferredFunctionType = inferredFunctionType }
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
                ty <- case optIndex of
                    Just index => do
                        optTy <- Map.get argumentTypesByIndex index
                        pure $ fromMaybe IUnknown optTy
                    Nothing => pure IUnknown
                go1 (ty :: acc) args

inferDef programName n fc (MkNmError expr) = inferDef programName n fc (MkNmFun [] expr)

inferDef programName idrisName fc def@(MkNmForeign foreignDescriptors argumentTypes returnType) =
    inferForeign programName idrisName fc foreignDescriptors argumentTypes returnType

inferDef _ _ _ _ = Pure ()
