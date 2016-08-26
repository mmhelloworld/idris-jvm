{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.CodegenJvm (codegenJvm) where

import           Control.Applicative ((<|>))
import           Control.Arrow       (first)
import           Control.Lens        ((^.))
import           Control.Monad.RWS
import           Idris.Core.TT
import           IRTS.CodegenCommon  hiding (Object)
import           IRTS.Lang
import           IRTS.Simplified

import           Data.Aeson
import qualified Data.Aeson          as A (decode)
import           Data.Aeson.Types    (typeMismatch)
import           Data.Char
import qualified Data.DList          as DL
import qualified Data.IntSet         as IntSet
import           Data.List           (find, foldl', sortBy)
import           Data.Maybe
import qualified Network.Wreq        as W
import           System.FilePath     (takeBaseName)

import           IdrisJvm.Assembler

codegenJvm :: CodeGenerator
codegenJvm ci = do
  let out = outputFile ci
      clazz = out ++ ".class"
      env = CgEnv $ takeBaseName out
      (_, _, writer) = runRWS (code ci) env initialCgState
      ins = DL.toList $ instructions writer <> deps writer <> [ ClassCodeEnd clazz ]
      assemblerRequest ins = object ["instructions" .= toJSON ins]
  asmResponse <- W.post "http://localhost:8080/assembler/assemble" $ assemblerRequest ins
  maybe (pure ()) putStrLn $ asmResponseMessage $ A.decode (asmResponse ^. W.responseBody)

data AsmResponse = AsmResponse { isSuccess :: Bool, message :: String }

instance FromJSON AsmResponse where
  parseJSON (Object v) = AsmResponse <$>
                         v .: "success" <*>
                         v .: "message"
  parseJSON invalid    = typeMismatch "AsmResponse" invalid

asmResponseMessage :: Maybe AsmResponse -> Maybe String
asmResponseMessage (Just (AsmResponse True _)) = Nothing
asmResponseMessage (Just (AsmResponse False msg)) = Just msg
asmResponseMessage _ = Just "Bytecode generation failed! Please check assembler server logs"

data CgEnv = CgEnv { className :: String } deriving Show

data CgState = CgState { cgStLambdaIndex     :: Int
                       , cgStSwitchIndex     :: Int
                       , cgStFunctionName    :: String
                       , cgStLocalVarCount   :: Int
                       , shouldDescribeFrame :: Bool
                       , cgStFunctionArgs    :: [Name]
                       } deriving Show

initialCgState :: CgState
initialCgState = CgState 0 0 "" 0 True []

rtClassSig :: String -> String
rtClassSig c = "mmhelloworld/idrisjvmruntime/" ++ c

rtFuncSig = "L" ++ rtClassSig "Function" ++ ";"
rtThunkSig = "L" ++ rtClassSig "Thunk" ++ ";"
rtRuntimeSig = "L" ++ rtClassSig "Runtime" ++ ";"
rtUtilSig = "L" ++ rtClassSig "Util" ++ ";"

createThunkSig = "(" ++ rtFuncSig ++ "[Ljava/lang/Object;)" ++ rtThunkSig

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

updateFunctionName :: (String -> String) -> CgState -> CgState
updateFunctionName f cgState = cgState { cgStFunctionName = f (cgStFunctionName cgState)}

updateLocalVarCount :: (Int -> Int) -> CgState -> CgState
updateLocalVarCount f cgState = cgState { cgStLocalVarCount = f (cgStLocalVarCount cgState)}

updateSwitchIndex :: (Int -> Int) -> CgState -> CgState
updateSwitchIndex f cgState = cgState { cgStSwitchIndex = f (cgStSwitchIndex cgState)}

updateLambdaIndex :: (Int -> Int) -> CgState -> CgState
updateLambdaIndex f cgState = cgState { cgStLambdaIndex = f (cgStLambdaIndex cgState)}

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
writeDeps ds = tell $ CgWriter [] ds

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
doCodegen (n, SFun _ args _ def) = cgFun n args def

type Locals = IntSet.IntSet

localVariables :: Locals -> SExp -> Locals
localVariables locals (SLet (Loc i) v sc)
   = let newLocals = IntSet.insert i locals
         locals1 = localVariables newLocals v
         locals2 = localVariables locals sc in
       IntSet.union locals1 locals2
localVariables locals (SUpdate _ e) = localVariables locals e
localVariables locals (SCase _ e alts)
   = localVariablesSwitch locals e alts
localVariables locals (SChkCase e alts)
   = localVariablesSwitch locals e alts
localVariables locals _ = locals

localVariablesSwitch :: Locals -> LVar -> [SAlt] -> Locals
localVariablesSwitch locals _ alts
   = let newLocals = map localVariablesAlt alts in
       IntSet.unions (locals: newLocals)

localVariablesAlt :: SAlt -> Locals
localVariablesAlt (SConstCase _ e) = localVariables IntSet.empty e
localVariablesAlt (SDefaultCase e) = localVariables IntSet.empty e
localVariablesAlt (SConCase lv _ _ args e) = IntSet.union assignmentLocals bodyLocals
   where assignmentLocals =
           if null args
             then IntSet.empty
             else IntSet.fromList [lv .. (lv + length args - 1)]
         bodyLocals = localVariables IntSet.empty e

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
    ret = if shouldEliminateTco
            then
              writeIns [ Astore resultVarIndex -- Store the result
                       , Iconst 0
                       , Istore tailRecVarIndex ] -- Base case for tailrec. Set the tailrec flag to false.
            else writeIns [ Astore resultVarIndex]
    tcoLocalVarCount = nlocalVars + 2 -- Two additional: tailrec loop variable, tailrec continue boolean
    tcoLocalVarTypes = replicate nlocalVars "java/lang/Object"
                     ++ [ "java/lang/Object", "Opcodes.INTEGER" ]
    tcoFrame = Frame FFull tcoLocalVarCount tcoLocalVarTypes 0 []
    methBody =
      if shouldEliminateTco
        then do
          writeIns [ Iconst 1
                   , Istore tailRecVarIndex
                   , CreateLabel tailRecStartLabelName
                   , LabelStart tailRecStartLabelName
                   , tcoFrame
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

tailRecStartLabelName :: String
tailRecStartLabelName = "$tailRecStartLabel"

tailRecEndLabelName :: String
tailRecEndLabelName = "$tailRecEndLabel"

assignNull :: Int -> DL.DList Asm
assignNull varIndex = [Aconstnull, Astore varIndex]

isTailCall :: SExp -> Maybe Name
isTailCall (SApp tailCall f _) = if tailCall then Just f else Nothing
isTailCall (SLet _ v sc) =  isTailCall v <|> isTailCall sc
isTailCall (SUpdate _ e) = isTailCall e
isTailCall (SCase _ _ alts) = join . find isJust . map isTailCallSwitch $ alts
isTailCall (SChkCase _ alts) = join . find isJust . map isTailCallSwitch $ alts
isTailCall _ = Nothing

isTailCallSwitch :: SAlt -> Maybe Name
isTailCallSwitch (SConstCase _ e) = isTailCall e
isTailCallSwitch (SDefaultCase e) = isTailCall e
isTailCallSwitch (SConCase _ _ _ _ e) = isTailCall e

assign :: Int -> Int -> DL.DList Asm
assign from to | from == to = []
assign from to              = [Aload from, Astore to]

boxInt :: Asm
boxInt = InvokeMethod InvokeStatic "java/lang/Integer" "valueOf" "(I)Ljava/lang/Integer;" False

unboxInt :: Asm
unboxInt = InvokeMethod InvokeVirtual "java/lang/Integer" "intValue" "()I" False

invokeError :: String -> Cg ()
invokeError x
  = writeIns [ Ldc $ StringConst x
             , InvokeMethod InvokeStatic (rtClassSig "Runtime") "error" "(Ljava/lang/Object;)Ljava/lang/Object;" False
             ]

sig :: Int -> String
sig nArgs = "(" ++ concat (replicate nArgs "Ljava/lang/Object;") ++ ")Ljava/lang/Object;"

metafactoryDesc :: Descriptor
metafactoryDesc =
  join [ "("
         , "Ljava/lang/invoke/MethodHandles$Lookup;"
         , "Ljava/lang/String;Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodType;"
         , "Ljava/lang/invoke/MethodHandle;"
         , "Ljava/lang/invoke/MethodType;"
         , ")"
         , "Ljava/lang/invoke/CallSite;"
         ]

lambdaDesc :: Descriptor
lambdaDesc = "([Ljava/lang/Object;)Ljava/lang/Object;"

invokeDynamic :: ClassName -> MethodName -> DL.DList Asm
invokeDynamic cname lambda = [ InvokeDynamic "apply" ("()" ++ rtFuncSig) metafactoryHandle metafactoryArgs] where
  metafactoryHandle = Handle HInvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory" metafactoryDesc False
  metafactoryArgs = [ BsmArgGetType lambdaDesc
                    , BsmArgHandle lambdaHandle
                    , BsmArgGetType lambdaDesc
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
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Runtime") "createThunk" createThunkSig False ]

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
                  g _ _ = error "Unexpected global variable"
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

cgBody ret (SLet (Loc i) v sc) = cgBody (writeIns [Astore i]) v >> cgBody ret sc

cgBody ret (SUpdate _ e) = cgBody ret e

cgBody ret (SProj (Loc v) i)
  = writeIns [ Aload v
             , Checkcast "[Ljava/lang/Object;"
             , Iconst $ succ i
             , Aaload
             ] >> ret


cgBody ret (SCon _ t _ args) = do
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
    ins _ _ = error "Unexpected global variable"

cgBody ret (SCase _ e alts) = cgSwitch ret e alts

cgBody ret (SChkCase e alts) = cgSwitch ret e alts

cgBody ret (SConst c) = cgConst c >> ret

cgBody ret (SOp op args) = cgOp op args >> ret

cgBody ret SNothing = writeIns [Iconst 0, boxInt] >> ret

cgBody ret (SError x) = invokeError (show x) >> ret

cgBody ret (SForeign returns fdesc args) = cgForeign (parseDescriptor returns fdesc args) where
  descriptor returnDesc = asm $ MethodDescriptor (fdescFieldDescriptor . fst <$> args) returnDesc
  argsWithTypes = first fdescFieldDescriptor <$> args

  cgForeign (JStatic clazz fn) = do
    let returnDesc = fdescTypeDescriptor returns
    loadAndCast argsWithTypes
    writeIns [ InvokeMethod InvokeStatic clazz fn (descriptor returnDesc) False ]
    boxIfNeeded returnDesc
    ret
  cgForeign (JVirtual clazz fn) = do
    let returnDesc = fdescTypeDescriptor returns
        descriptor = asm $ MethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
    loadAndCast argsWithTypes -- drop first arg type as it is an implicit 'this'
    writeIns [ InvokeMethod InvokeVirtual clazz fn descriptor False ]
    boxIfNeeded returnDesc
    ret
  cgForeign (JInterface clazz fn) = do
    let returnDesc = fdescTypeDescriptor returns
        descriptor = asm $ MethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
    loadAndCast argsWithTypes
    writeIns [ InvokeMethod InvokeInterface clazz fn descriptor True ]
    boxIfNeeded returnDesc
    ret
  cgForeign (JConstructor clazz) = do
    let returnDesc = VoidDescriptor -- Constructors always return void.
    writeIns [ New clazz, Dup ]
    loadAndCast argsWithTypes
    writeIns [ InvokeMethod InvokeSpecial clazz "<init>" (descriptor returnDesc) False ]
    ret

cgBody _ _ = error "NOT IMPLEMENTED!!!!"

data JForeign = JStatic String String
              | JVirtual String String
              | JInterface String String
              | JConstructor String

parseDescriptor :: FDesc -> FDesc -> [(FDesc, LVar)] -> JForeign
parseDescriptor _ (FApp ffi [FApp nativeTy [FStr cname], FStr fn]) _
  | ffi == sUN "Static" && nativeTy == sUN "Class" = JStatic cname fn

parseDescriptor _ (FApp ffi [FStr _]) []
  | ffi == sUN "Instance" = error "Instance methods should have atleast one argument"

parseDescriptor _ (FApp ffi [FStr fn]) ((declClass, _):_)
  | ffi == sUN "Instance" = case fdescRefDescriptor declClass of
    ClassDesc cname -> JVirtual cname fn
    InterfaceDesc cname -> JInterface cname fn

parseDescriptor returns (FCon ffi) _
 | ffi == sUN "Constructor" = case fdescRefDescriptor returns of
   ClassDesc cname -> JConstructor cname
   InterfaceDesc _ -> error $ "Invalid FFI descriptor for constructor. " ++
                              "A constructor can't return an interface type. " ++
                              show returns

parseDescriptor _ fdesc _ = error $ "Unsupported descriptor: " ++ show fdesc

loadAndCast :: [(FieldTypeDescriptor, LVar)] -> Cg ()
loadAndCast = mapM_ f where
  f (ty, v) = do
    writeIns [ Aload $ locIndex v ]
    case ty of
      FieldTyDescInt -> writeIns [ Checkcast "java/lang/Integer", unboxInt ]
      FieldTyDescReference refTy -> writeIns [ Checkcast $ refTyClassName refTy]
      _ -> error $ "unknown type: " ++ show ty

boxIfNeeded :: TypeDescriptor -> Cg ()
boxIfNeeded (FieldDescriptor FieldTyDescInt) = writeIns [ boxInt ]
boxIfNeeded _ = pure () -- TODO: implement for other types

fdescTypeDescriptor :: FDesc -> TypeDescriptor
fdescTypeDescriptor (FCon (UN "JVM_Unit")) = VoidDescriptor
fdescTypeDescriptor (FIO t) = fdescTypeDescriptor t
fdescTypeDescriptor fdesc = FieldDescriptor $ fdescFieldDescriptor fdesc

fdescFieldDescriptor :: FDesc -> FieldTypeDescriptor
fdescFieldDescriptor (FApp intTy [_, FCon (UN "JVM_IntNative")]) | intTy == sUN "JVM_IntT" = FieldTyDescInt
fdescFieldDescriptor (FCon (UN "JVM_Str")) = FieldTyDescReference $ ClassDesc "java/lang/String"
fdescFieldDescriptor (FCon (UN "JVM_Float")) = FieldTyDescFloat
fdescFieldDescriptor fdesc = FieldTyDescReference $ fdescRefDescriptor fdesc

fdescRefDescriptor :: FDesc -> ReferenceTypeDescriptor
fdescRefDescriptor (FApp jvmTy [FApp nativeTy [FStr typeName]])
  | jvmTy == sUN "JVM_NativeT" && nativeTy == sUN "Class"
    = ClassDesc typeName
  | jvmTy == sUN "JVM_NativeT" && nativeTy == sUN "Interface"
    = InterfaceDesc typeName
  | otherwise = error "Invalid reference type descriptor. Expected a class or interface descriptor."
fdescRefDescriptor _ = error "Invalid reference type descriptor. Expected a class or interface descriptor."

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
isStrSwitch (SConstCase (Str _) _)  = True
isStrSwitch _ = False

caseExpr :: SAlt -> Maybe Int
caseExpr (SConstCase t _) = Just  $ constCaseExpr t
caseExpr (SConCase _ t _ _ _) = Just t
caseExpr (SDefaultCase _) = Nothing

addFrame :: Cg ()
addFrame = do
  needFrame <- shouldDescribeFrame <$> get
  nlocalVars <- cgStLocalVarCount <$> get
  if needFrame
    then do
      args <- cgStFunctionArgs <$> get
      writeIns [ Frame FFull (nlocalVars + 1) (replicate nlocalVars "java/lang/Object" ++ ["java/lang/Object"]) 0 []]
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

cgConst :: Const -> Cg ()
cgConst (I i) = writeIns [Iconst i, boxInt]
cgConst (Ch c) = writeIns [ Iconst (ord c)
                          , InvokeMethod InvokeStatic "java/lang/Character" "valueOf" "(C)Ljava/lang/Character;" False]
cgConst (BI i) = writeIns [Iconst $ fromIntegral i, boxInt] -- TODO: Handle BigInteger
cgConst (Str s) = writeIns [Ldc $ StringConst s]
cgConst TheWorld = writeIns [Iconst 0, boxInt]
cgConst x | isTypeConst x = writeIns [Iconst 0, boxInt]
cgConst x = error $ "Constant " ++ show x ++ " not compilable yet"

compareObj :: MethodName -> LVar -> LVar -> Cg ()
compareObj fn l r
  = writeIns [ Aload $ locIndex l
             , Aload $ locIndex r
             , InvokeMethod InvokeStatic (rtClassSig "Util") fn "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" False
             ]

locIndex :: LVar -> Int
locIndex (Loc i) = i
locIndex _ = error "Unexpected global variable"

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

cgOp op _ = invokeError $ "OPERATOR " ++ show op ++ " NOT IMPLEMENTED!"
   -- error("Operator " ++ show op ++ " not implemented")
