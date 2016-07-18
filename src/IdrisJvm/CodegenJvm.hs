{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.CodegenJvm (codegenJvm) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.RWS
import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT

import Data.Maybe
import Data.Char
import qualified Data.DList as DL
import Data.List (find, foldl', sortBy)
import qualified Data.IntSet as IntSet
import System.Environment
import System.Exit
import System.Process (readProcessWithExitCode)
import qualified Text.PrettyPrint as PP

import IdrisJvm.Assembler hiding (assign)

codegenJvm :: CodeGenerator
codegenJvm ci = do
  let out = outputFile ci
      asm = out ++ ".asm"
      clazz = out ++ ".class"
      env = CgEnv out
      (_, _, writer) = runRWS (code ci) env initialCgState
  writeFile asm . unlines . map asmj . DL.toList $ instructions writer <> deps writer <> [ ClassCodeEnd clazz ]
  java <- fromMaybe "java" <$> lookupEnv "IDRIS_JVM"
  let errMissingLib = error $  "Idris JVM runtime java library not found.\n"
                            <> "Please set IDRIS_JVM_LIB environment variable to idrisjvm-runtime-<version>.jar path"
  lib <- fromMaybe errMissingLib <$> lookupEnv "IDRIS_JVM_LIB"
  runProcess java ["-cp", lib, asmrunner, asm] -- Interpret ASM code to create class file

runProcess :: String -> [String] -> IO ()
runProcess proc args = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode proc args ""
  case exitCode of
    ExitFailure _ -> error $ proc <> " ERROR: " <> stdout <> stderr
    _             -> return ()
    
data CgEnv = CgEnv { className :: String } deriving Show

data CgState = CgState { cgStLambdaIndex :: Int
                       , cgStSwitchIndex :: Int
                       , cgStLabelIndex :: Int
                       , cgStFunctionName :: String
                       , cgStLocalVarCount :: Int
                       , shouldDescribeFrame :: Bool
                       , cgStFunctionArgs :: [Name]
                       } deriving Show

initialCgState :: CgState
initialCgState = CgState 0 0 0 "" 0 True []

asmrunner = "mmhelloworld.idrisjvmruntime.AssemblerRunner"

rtClassSig :: String -> String
rtClassSig c = "mmhelloworld/idrisjvmruntime/" ++ c

rtFuncSig = "L" ++ rtClassSig "Function" ++ ";"
rtThunkSig = "L" ++ rtClassSig "Thunk" ++ ";"
rtRuntimeSig = "L" ++ rtClassSig "Runtime" ++ ";"
rtUtilSig = "L" ++ rtClassSig "Util" ++ ";"

freshLambdaIndex :: Cg Int
freshLambdaIndex = do
  li <- cgStLambdaIndex <$> get
  modify . updateLambdaIndex $ succ
  return li

freshSwitchIndex :: Cg Int
freshSwitchIndex = do
  si <- cgStSwitchIndex <$> get
  modify (updateSwitchIndex succ)
  return si

freshLabelIndex :: Cg Int
freshLabelIndex = do
  li <- cgStLabelIndex <$> get
  modify . updateLabelIndex $ succ
  return li

updateFunctionName :: (String -> String) -> CgState -> CgState
updateFunctionName f cgState = cgState { cgStFunctionName = f (cgStFunctionName cgState)}

updateLocalVarCount :: (Int -> Int) -> CgState -> CgState
updateLocalVarCount f cgState = cgState { cgStLocalVarCount = f (cgStLocalVarCount cgState)}

updateSwitchIndex :: (Int -> Int) -> CgState -> CgState
updateSwitchIndex f cgState = cgState { cgStSwitchIndex = f (cgStSwitchIndex cgState)}

updateLambdaIndex :: (Int -> Int) -> CgState -> CgState
updateLambdaIndex f cgState = cgState { cgStLambdaIndex = f (cgStLambdaIndex cgState)}

updateLabelIndex :: (Int -> Int) -> CgState -> CgState
updateLabelIndex f cgState = cgState { cgStLabelIndex = f (cgStLabelIndex cgState)}

updateShouldDescribeFrame :: (Bool -> Bool) -> CgState -> CgState
updateShouldDescribeFrame f cgState = cgState { shouldDescribeFrame = f (shouldDescribeFrame cgState)}

updateFunctionArgs :: ([Name] -> [Name]) -> CgState -> CgState
updateFunctionArgs f cgState = cgState { cgStFunctionArgs = f (cgStFunctionArgs cgState)}

data CgWriter = CgWriter { instructions :: DL.DList Asm, deps :: DL.DList Asm }

instance Monoid CgWriter where
  mempty = CgWriter mempty mempty
  mappend (CgWriter ins1 deps1) (CgWriter ins2 deps2) = CgWriter (mappend ins1 ins2) (mappend deps1 deps2)

writeIns :: DL.DList Asm -> Cg ()
writeIns ins = tell $ CgWriter ins []

writeDeps :: DL.DList Asm -> Cg ()
writeDeps deps = tell $ CgWriter [] deps

type Cg a = RWS CgEnv CgWriter CgState a

code :: CodegenInfo -> Cg ()
code ci = do
  cname <- className <$> ask
  writeIns [ CreateClass ComputeMaxs
           , ClassCodeStart 52 Public cname "null" "java/lang/Object" []
           ]
  defaultConstructor
  functions ci
  mainMethod 

functions :: CodegenInfo -> Cg ()
functions ci = mapM_ doCodegen (simpleDecls ci)

defaultConstructor :: Cg ()
defaultConstructor
  = writeIns [ CreateMethod [Public] "<init>" "()V" Nothing Nothing
             , MethodCodeStart
             , Aload 0
             , InvokeMethod InvokeSpecial "java/lang/Object" "<init>" "()V" False
             , Return
             , MaxStackAndLocal (-1) (-1) -- Let the asm calculate
             , MethodCodeEnd
             ]

mainMethod :: Cg ()                     
mainMethod = do
  cname <- className <$> ask
  writeIns [ CreateMethod [Public, Static] "main" "([Ljava/lang/String;)V" Nothing Nothing
           , MethodCodeStart
           , InvokeMethod InvokeStatic cname (jname $ MN 0 "runMain") "()Ljava/lang/Object;" False
           , Pop
           , Return
           , MaxStackAndLocal (-1) (-1) 
           , MethodCodeEnd
           ]

jname :: Name -> String
jname n = "idris_" ++ concatMap jchar idrisName
  where
    idrisName = showCG n
    jchar x | isAlpha x || isDigit x = [x]
            | otherwise = "_" ++ show (fromEnum x) ++ "_"

var :: Name -> String
var n = "$" ++ jname n

loc :: Int -> String
loc i = "$loc" ++ show i

doCodegen :: (Name, SDecl) -> Cg ()
doCodegen (n, SFun _ args i def) = cgFun n args def

type Locals = IntSet.IntSet

localVariables :: Locals -> SExp -> Locals
localVariables locals (SV (Glob n)) = locals
localVariables locals (SV (Loc i)) = locals
localVariables locals SApp{} = locals
localVariables locals (SLet (Loc i) v sc)
   = let newLocals = IntSet.insert i locals
         locals1 = localVariables newLocals v
         locals2 = localVariables locals sc in
       IntSet.union locals1 locals2
localVariables locals (SUpdate n e) = localVariables locals e
localVariables locals (SProj e i) = locals
localVariables locals (SCon _ t n args) = locals
localVariables locals (SCase _ e alts)
   = localVariablesSwitch locals e alts
localVariables locals (SChkCase e alts)
   = localVariablesSwitch locals e alts
localVariables locals (SConst c) = locals
localVariables locals (SOp op args) = locals
localVariables locals SNothing = locals
localVariables locals (SError x) = locals
localVariables locals _ = locals

localVariablesSwitch :: Locals -> LVar -> [SAlt] -> Locals
localVariablesSwitch locals e alts
   = let newLocals = map localVariablesAlt alts in
       IntSet.unions (locals: newLocals)
    
localVariablesAlt :: SAlt -> Locals
localVariablesAlt (SConstCase t exp) = localVariables IntSet.empty exp
localVariablesAlt (SDefaultCase exp) = localVariables IntSet.empty exp
localVariablesAlt (SConCase lv t n args exp) = IntSet.union assignmentLocals bodyLocals
   where assignmentLocals =
           if null args
             then IntSet.empty
             else IntSet.fromList [lv .. (lv + length args - 1)]
         bodyLocals = localVariables IntSet.empty exp

cgFun :: Name -> [Name] -> SExp -> Cg ()
cgFun n args def = do
    modify . updateFunctionArgs $ const args
    modify . updateFunctionName $ const functionName
    modify . updateShouldDescribeFrame $ const True
    writeIns [ CreateMethod [Private, Static] functionName (sig nArgs) Nothing Nothing
             , MethodCodeStart
             ]
    modify . updateLocalVarCount $ const nlocalVars
    modify . updateSwitchIndex $ const 0 -- reset
    writeIns . join . fmap assignNull . DL.fromList $ resultVarIndex: [nArgs .. (nlocalVars - 1)]
    methBody
    writeIns [ Aload resultVarIndex
             , Areturn
             , MaxStackAndLocal (-1) (-1)
             , MethodCodeEnd
             ]
    
  where
    nArgs = length args
    localVars = localVariables (if nArgs == 0 then IntSet.empty else IntSet.singleton $ pred nArgs) def
    nlocalVars = if IntSet.null localVars then 0 else IntSet.findMax localVars + 1
    resultVarIndex = nlocalVars
    tailRecVarIndex = succ resultVarIndex
    functionName = jname n
    shouldEliminateTco = maybe False ((==) functionName . jname) $ isTailCall def
    returnResult = returnObject resultVarIndex
    ret = if shouldEliminateTco
            then
              writeIns [ Astore resultVarIndex -- Store the result
                       , Iconst 0
                       , Istore tailRecVarIndex ] -- Base case for tailrec. Set the tailrec flag to false.
            else writeIns [ Astore resultVarIndex]
    methBody =
      if shouldEliminateTco
        then do
          writeIns [ Iconst 1
                   , Istore tailRecVarIndex
                   , CreateLabel tailRecStartLabelName
                   , LabelStart tailRecStartLabelName
                   , Frame FFull (nlocalVars + 2) (replicate nlocalVars (quoted "java/lang/Object") ++ [quoted "java/lang/Object", "Opcodes.INTEGER"]) 0 []
                   , Iload tailRecVarIndex
                   , CreateLabel tailRecEndLabelName
                   , Ifeq tailRecEndLabelName
                   ]
          modify . updateShouldDescribeFrame $ const False
          cgBody ret def
          writeIns [ Goto tailRecStartLabelName
                   , LabelStart tailRecEndLabelName
                   , Frame FSame 0 [] 0 []
                   ]
        else cgBody ret def

tailRecStartLabelName = "$tailRecStartLabel"
tailRecEndLabelName = "$tailRecEndLabel"

assignNull :: Int -> DL.DList Asm
assignNull varIndex = [Aconstnull, Astore varIndex]

returnObject :: Int -> DL.DList Asm
returnObject varIndex = [Aload varIndex, Areturn]
                   
isTailCall :: SExp -> Maybe Name
isTailCall (SApp isTailCall f _) = if isTailCall then Just f else Nothing
isTailCall (SLet _ v sc) =  isTailCall v <|> isTailCall sc
isTailCall (SUpdate n e) = isTailCall e
isTailCall (SCase _ e alts) = join . find isJust . map isTailCallSwitch $ alts
isTailCall (SChkCase e alts) = join . find isJust . map isTailCallSwitch $ alts
isTailCall _ = Nothing

isTailCallSwitch :: SAlt -> Maybe Name
isTailCallSwitch (SConstCase t exp) = isTailCall exp
isTailCallSwitch (SDefaultCase exp) = isTailCall exp
isTailCallSwitch (SConCase lv t n args exp) = isTailCall exp

assign :: Int -> Int -> DL.DList Asm
assign from to | from == to = []
assign from to              = [Aload from, Astore to]

boxInt :: Asm
boxInt = InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False

invokeError :: String -> Cg ()
invokeError x
  = writeIns [ Ldc $ show x
             , InvokeMethod InvokeStatic (rtClassSig "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False
             ]
    
sig :: Int -> String  
sig nArgs = "(" ++ concat (replicate nArgs "Ljava/lang/Object;") ++ ")Ljava/lang/Object;"

metafactoryDesc :: Descriptor
metafactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

lambdaDesc :: Descriptor
lambdaDesc = "([Ljava/lang/Object;)Ljava/lang/Object;"

invokeDynamic :: ClassName -> MethodName -> DL.DList Asm
invokeDynamic cname lambda = [ InvokeDynamic "apply" ("()" ++ rtFuncSig) metafactoryHandle metafactoryArgs] where
  metafactoryHandle = Handle HInvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory" metafactoryDesc False
  metafactoryArgs = [ asmj $ GetType lambdaDesc
                    , handlej lambdaHandle
                    , asmj $ GetType lambdaDesc
                    ]
  lambdaHandle = Handle HInvokeStatic cname lambda lambdaDesc False
  
createThunk :: MethodName -> MethodName -> [LVar] -> Cg ()
createThunk caller fname args = do
  let nArgs = length args
  cname <- className <$> ask
  lambdaIndex <- freshLambdaIndex
  let lambdaMethodName = sep "$" ["lambda", caller, show lambdaIndex]
  writeIns $ invokeDynamic cname lambdaMethodName
  writeDeps $ createLambda cname fname lambdaMethodName nArgs
  writeIns [Iconst nArgs, Anewarray "java/lang/Object"]
  let argNums = map (\(Loc i) -> i) args
      f :: (Int, Int) -> DL.DList Asm
      f (lhs, rhs) = [Dup, Iconst lhs, Aload rhs, Aastore]
  writeIns . join . fmap f . DL.fromList $ zip [0..] argNums
  writeIns [InvokeMethod InvokeStatic (rtClassSig "Runtime") "createThunk" ("(" ++ rtFuncSig ++ "[Ljava/lang/Object;)" ++ rtThunkSig) False]
  
createLambda :: ClassName -> MethodName -> MethodName -> Int -> DL.DList Asm
createLambda cname fname lambdaMethodName nArgs
  = DL.fromList [ CreateMethod [Private, Static, Synthetic] lambdaMethodName lambdaDesc Nothing Nothing
                , MethodCodeStart
                ] <>
                join (fmap (\i -> [Aload 0, Iconst i, Aaload]) [0 .. (nArgs - 1)]) <> -- Retrieve lambda args
                [ InvokeMethod InvokeStatic cname fname (sig nArgs) False -- invoke the target method
                , Areturn
                , MaxStackAndLocal (-1) (-1)
                , MethodCodeEnd
                ]

cgBody :: Cg () -> SExp -> Cg ()
cgBody ret (SV (Glob n)) = do
  cname <- className <$> ask
  writeIns [ InvokeMethod InvokeStatic cname (jname n) (sig 0) False]
  ret
                                                                                                                
cgBody ret (SV (Loc i)) = writeIns [Aload i] >> ret

cgBody ret (SApp True f args) = do
  caller <- cgStFunctionName <$> get
  if jname f == caller -- self tail call, use goto
       then do
              let g toIndex (Loc fromIndex) = assign fromIndex toIndex
              writeIns . join . DL.fromList $ zipWith g [0..] args
        else -- non-self tail call
          createThunk caller (jname f) args >> ret
    
cgBody ret (SApp False f args) = do
  cname <- className <$> ask
  writeIns $ (Aload . locIndex) <$> DL.fromList args
  writeIns [ InvokeMethod InvokeStatic cname (jname f) (sig $ length  args) False
           , InvokeMethod InvokeStatic (rtClassSig "Runtime") "unwrap" "(Ljava/lang/Object;)Ljava/lang/Object;" False
           ]
  ret

--cgBody ret (SLet (Loc i) v@(SConst (Str s)) sc) = cgBody (writeIns [Astore i]) v >> cgBody ret sc
--cgBody ret (SLet (Loc i) v@(SConst c) sc) = cgBody (writeIns [ Astore i ]) v >> cgBody ret sc
cgBody ret (SLet (Loc i) v sc) = cgBody (writeIns [Astore i]) v >> cgBody ret sc

cgBody ret (SUpdate n e) = cgBody ret e

cgBody ret (SProj (Loc v) i)
  = writeIns [ Aload v
             , Checkcast "[Ljava/lang/Object;"
             , Iconst $ succ i
             , Aaload
             ] >> ret
    

cgBody ret (SCon _ t n args) = do
    writeIns [ Iconst $ length args + 1
             , Anewarray "java/lang/Object"
             , Dup
             , Iconst 0
             , Iconst t
             , boxInt
             , Aastore
             ]
    writeIns $ join . DL.fromList $ zipWith ins [1..] args
    ret
  where
    ins :: Int -> LVar -> DL.DList Asm
    ins index (Loc varIndex)
      = [ Dup
        , Iconst index
        , Aload varIndex
        , Aastore
        ]

cgBody ret (SCase _ e alts) = cgSwitch ret e alts

cgBody ret (SChkCase e alts) = cgSwitch ret e alts

cgBody ret (SConst c) = cgConst c >> ret

cgBody ret (SOp op args) = cgOp op args >> ret

cgBody ret SNothing = writeIns [Iconst 0, boxInt] >> ret

cgBody ret (SError x) = invokeError (show x) >> ret

cgBody ret _ = invokeError "NOT IMPLEMENTED!!!!" >> ret

cgSwitch :: Cg () -> LVar -> [SAlt] -> Cg ()
cgSwitch ret e alts = do
    switchIndex <- freshSwitchIndex 
    let switchEndLbl = switchEndLabel switchIndex
        switchDefaultLbl = if hasDefault then defaultLabel switchIndex else switchEndLbl
        
    writeIns $ fmap (\(lbl, _, _) -> CreateLabel lbl) . DL.fromList $ csWithLbls switchIndex
    writeIns [ CreateLabel switchEndLbl ]
    
    if isConstructorSwitch
      then switchConstructorExpr switchVar
      else if isStrCases
             then switchStringExpr switchIndex switchVar
             else switchIntExpr switchVar
      
    writeIns [ LookupSwitch switchDefaultLbl (switchInsLabels switchIndex) (switchInsExprs switchIndex)]

    caseIns switchIndex

    writeIns [ LabelStart (switchEndLabel switchIndex)
             , Frame FSame 0 [] 0 []
             ]
  where conCase SConCase {} = True
        conCase _ = False

        defaultCase (SDefaultCase _) = True
        defaultCase _ = False

        hasDefault = any defaultCase alts
        isConstructorSwitch = any conCase alts

        switchInsLabels si = map (\(lbl, _, _) -> lbl) $ filter (\(_, _, alt) -> not (defaultCase alt)) $ csWithLbls si
        switchInsExprs si = map (\(_, e, _) -> fromMaybe 0 e) $ filter (\(_, _, alt) -> not (defaultCase alt)) $ csWithLbls si
        
        Loc switchVar = e

        cs :: [(Maybe Int, SAlt)]
        cs = zip (map caseExpr alts) alts

        isStrCases = any isStrSwitch alts

        f si (Just e, alt) i = (labelIndex si i, Just e, alt)
        f si (Nothing, alt) _ = (defaultLabel si, Nothing, alt)

        g (_, Just c1, _) (_, Just c2, _) = compare c1 c2
        g (_, Just c1, _) (_, Nothing, _) = LT
        g x@(_, Nothing, _) (_, Just c2, _) = GT
        g _ _ = EQ
        
        csWithLbls si = sortBy g $ zipWith (f si) cs [0 .. pred (length alts)]
        
        caseIns si = mapM_ (\(label, e, alt) -> cgAlt ret label si switchVar alt) (csWithLbls si)

switchEndLabel :: Int -> String
switchEndLabel switchIndex = "$switch" ++ show switchIndex ++ "$end"

labelIndex :: Int -> Int -> String
labelIndex switchIndex i = "$switch" ++ show switchIndex ++ "$label" ++ show i

defaultLabel :: Int -> String
defaultLabel switchIndex = "$switch" ++ show switchIndex ++ "$defaultLabel"

switchConstructorExpr :: Int -> Cg ()
switchConstructorExpr varIndex
 = writeIns [ Aload varIndex
            , Checkcast "[Ljava/lang/Object;"
            , Iconst 0
            , Aaload
            , Checkcast "java/lang/Integer"
            , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
            ]

switchIntExpr :: Int -> Cg ()
switchIntExpr varIndex
  = writeIns [ Aload varIndex
             , InvokeMethod InvokeStatic (rtClassSig "Util") "asInt" "(Ljava/lang/Object;)I" False
             ]

switchStringExpr :: Int -> Int -> Cg ()
switchStringExpr switchIndex varIndex
  = writeIns [ Aload varIndex
             , Checkcast "java/lang/String"
             , Astore switchIndex  -- Store the string for later to compare using "equals"
             ]

constCaseExpr :: Const -> Int
constCaseExpr (I i) = i
constCaseExpr (Ch c) = ord c
constCaseExpr (BI i) = fromIntegral i -- TODO Handle BigInteger
constCaseExpr (Str s) = hash s
constCaseExpr TheWorld = 0
constCaseExpr x | isTypeConst x = 0
constCaseExpr x = error $ "Constant " ++ show x ++ " not compilable yet"

hash :: String -> Int
hash = foldl' (\h c -> 31 * h + fromEnum c) 0

isStrSwitch :: SAlt -> Bool
isStrSwitch (SConstCase (Str s) _)  = True
isStrSwitch _ = False

caseExpr :: SAlt -> Maybe Int
caseExpr (SConstCase t _) = Just  $ constCaseExpr t
caseExpr (SConCase _ t _ _ _) = Just t
caseExpr (SDefaultCase _) = Nothing

addFrame = do
  needFrame <- shouldDescribeFrame <$> get
  nlocalVars <- cgStLocalVarCount <$> get
  if needFrame
    then do
      args <- cgStFunctionArgs <$> get
      let nArgs = length args
      writeIns [ Frame FFull (nlocalVars + 1) (replicate nlocalVars (quoted "java/lang/Object") ++ [quoted "java/lang/Object"]) 0 []]
      modify . updateShouldDescribeFrame $ const False
    else writeIns [ Frame FSame 0 [] 0 []]
    
cgAltNonConCase :: Cg () -> Label -> Int -> SExp -> Cg ()
cgAltNonConCase ret label si exp = do
  writeIns [ LabelStart label ]
  addFrame
  cgBody ret exp
  writeIns [ Goto $ switchEndLabel si ]
  
cgAlt :: Cg () -> Label -> Int -> Int -> SAlt -> Cg ()
cgAlt ret label si sv (SConstCase t exp) = cgAltNonConCase ret label si exp
       
cgAlt ret label si sv (SDefaultCase exp) = cgAltNonConCase ret label si exp
    
cgAlt ret label si sv (SConCase lv t n args exp) = do
    writeIns [ LabelStart label ]
    addFrame
    extractConParams
    cgBody ret exp
  
    writeIns [ Goto $ switchEndLabel si ]

   where
     project i v =
       writeIns [ Aload sv
                , Checkcast "[Ljava/lang/Object;"
                , Iconst i
                , Aaload
                , Astore v
                ]
         
     extractConParams =
       mapM_ (uncurry project) $ zip [1..] [lv .. (lv + length args - 1)]

cgVar :: LVar -> String
cgVar (Loc i) = loc i 
cgVar (Glob n) = var n

cgConst :: Const -> Cg ()
cgConst (I i) = writeIns [Iconst i, boxInt]
cgConst (Ch c) = writeIns [Iconst (ord c), boxInt] 
cgConst (BI i) = writeIns [Iconst $ fromIntegral i, boxInt] -- TODO: Handle BigInteger
cgConst (Str s) = writeIns [Ldc $ show s]
cgConst TheWorld = writeIns [Iconst 0, boxInt]
cgConst x | isTypeConst x = writeIns [Iconst 0, boxInt]
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

compareObj fn l r
  = writeIns [ Aload $ locIndex l
           , Aload $ locIndex r
           , InvokeMethod InvokeStatic (rtClassSig "Util") fn "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
           ]
    
locIndex (Loc i) = i
locIndex _ = error "Not a local variable"

loadLocalIntWithCast :: LVar -> DL.DList Asm
loadLocalIntWithCast i = [ Aload $ locIndex i
                         , Checkcast "java/lang/Integer"
                         , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False]
    
cgOp :: PrimFn -> [LVar] -> Cg ()
cgOp (LPlus (ATInt _)) [l, r] = do
  writeIns $ loadLocalIntWithCast l
  writeIns $ loadLocalIntWithCast r
  writeIns [ Iadd ]
  writeIns [ boxInt ]
  
cgOp (LMinus (ATInt _)) [l, r] = do
  writeIns $ loadLocalIntWithCast l
  writeIns $ loadLocalIntWithCast r
  writeIns [ Isub ]
  writeIns [ boxInt ]
  
cgOp (LTimes (ATInt _)) [l, r] = do
  writeIns $ loadLocalIntWithCast l
  writeIns $ loadLocalIntWithCast r
  writeIns [ Imul ]
  writeIns [ boxInt ]
  
cgOp (LEq (ATInt _)) [l, r] = compareObj "objectEquals" l r
                                                                                                  
cgOp (LSLt (ATInt _)) [l, r] = compareObj "intLessThan" l r
cgOp (LSLe (ATInt _)) [l, r] = compareObj "intLessThanOrEqualTo" l r
cgOp (LSGt (ATInt _)) [l, r] = compareObj "intGreaterThan" l r
cgOp (LSGe (ATInt _)) [l, r] = compareObj "intGreaterThanOrEqualTo" l r

cgOp LStrEq [l,r] = compareObj "objectEquals" l r
    
cgOp LStrRev [x]
  = writeIns [ New "java/lang/StringBuilder"
             , Dup
             , Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "(Ljava/lang/String;)V" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "reverse" "()Ljava/lang/StringBuilder;" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
             ]
    
cgOp LStrLen [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeVirtual "java/lang/String" "length" "()I" False
             , boxInt
             ]
    
cgOp LStrHead [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , Iconst 0
             , InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]
    
cgOp LStrIndex [x, y]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , Aload $ locIndex y
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , InvokeMethod InvokeVirtual "java/lang/String" "charAt" "(I)C" False
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]
    
cgOp LStrTail [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , Iconst 1
             , InvokeMethod InvokeVirtual "java/lang/String" "substring" "(I)Ljava/lang/String;" False
             ]

cgOp (LZExt ITNative ITBig) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , I2l
             , InvokeMethod InvokeStatic "java/math/BigInteger" "valueOf" "(J)Ljava/math/BigInteger;" False
             ]
    
cgOp (LIntStr _) [x]
  = writeIns [ Aload $ locIndex x
             , InvokeMethod InvokeStatic "java/util/Objects" "toString" "(Ljava/lang/Object;)Ljava/lang/String;" False
             ]

cgOp (LChInt _) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Character"
             , InvokeMethod InvokeVirtual "java/lang/Character" "charValue" "()C" False
             , InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False
             ]
    
cgOp (LIntCh _) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/Integer"
             , InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False
             , I2c
             , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False
             ]
    
cgOp (LSExt _ _) [x] = writeIns [ Aload $ locIndex x]
cgOp (LTrunc _ _) [x] = writeIns [ Aload $ locIndex x]

cgOp LWriteStr [_, str]
  = writeIns [ Field FGetStatic "java/lang/System" "out" "Ljava/io/PrintStream;"
             , Aload $ locIndex str
             , InvokeMethod InvokeVirtual "java/io/PrintStream" "print" "(Ljava/lang/Object;)V" False
             , Aconstnull
             ]
    
cgOp LReadStr [_]
  = writeIns [ InvokeMethod InvokeStatic (rtClassSig "Runtime") "readString" "()Ljava/lang/String;" False ]
  
cgOp LStrConcat [l,r]
  = writeIns [ New "java/lang/StringBuilder"
             , Dup
             , InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
             , Aload $ locIndex l
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
             , Aload $ locIndex r
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/Object;)Ljava/lang/StringBuilder;" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
             ]
    
cgOp LStrCons [l,r]
  = writeIns [ New "java/lang/StringBuilder"
             , Dup
             , InvokeMethod InvokeSpecial "java/lang/StringBuilder" "<init>" "()V" False
             , Aload $ locIndex l
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/Object;)Ljava/lang/StringBuilder;" False
             , Aload $ locIndex r
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "append" "(Ljava/lang/String;)Ljava/lang/StringBuilder;" False
             , InvokeMethod InvokeVirtual "java/lang/StringBuilder" "toString" "()Ljava/lang/String;" False
             ]

cgOp (LStrInt _) [x]
  = writeIns [ Aload $ locIndex x
             , Checkcast "java/lang/String"
             , InvokeMethod InvokeStatic "java/lang/Integer" "parseInt" "(Ljava/lang/String;)I" False
             , InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False
             ]
    
cgOp op exps = invokeError $ "OPERATOR " ++ show op ++ " NOT IMPLEMENTED!"
   -- error("Operator " ++ show op ++ " not implemented")

