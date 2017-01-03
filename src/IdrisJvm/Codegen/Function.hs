{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Function where

import           Control.Applicative          ((<|>))
import           Control.Arrow                (first)
import           Control.Monad.RWS
import qualified Data.DList                   as DL
import           Data.List                    (find)
import           Data.Maybe
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Common
import           IdrisJvm.Codegen.Constant
import           IdrisJvm.Codegen.ControlFlow
import           IdrisJvm.Codegen.Foreign
import           IdrisJvm.Codegen.Operator
import           IdrisJvm.Codegen.Types
import           IRTS.Lang
import           IRTS.Simplified

cgBody :: Cg () -> SExp -> Cg ()
cgBody ret (SV (Glob n)) = do
  let JMethodName cname mname = jname n
  writeIns [ InvokeMethod InvokeStatic cname mname (sig 0) False]
  ret

cgBody ret (SV (Loc i)) = writeIns [Aload i] >> ret

cgBody ret (SApp True f args) = do
  caller <- cgStFunctionName <$> get
  if jname f == caller -- self tail call, use goto
       then do
              let g toIndex (Loc fromIndex)
                    = assign fromIndex toIndex
                  g _ _ = error "Unexpected global variable"
              writeIns . join . DL.fromList $ zipWith g [0..] args
        else -- non-self tail call
          createThunk caller (jname f) args >> ret

cgBody ret (SApp False f args) = do
  writeIns $ (Aload . locIndex) <$> DL.fromList args
  let JMethodName cname mname = jname f
  writeIns [ InvokeMethod InvokeStatic cname mname (sig $ length  args) False
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

cgBody ret (SCase _ e alts) = cgSwitch ret cgBody e alts

cgBody ret (SChkCase e alts) = cgSwitch ret cgBody e alts

cgBody ret (SConst c) = cgConst c >> ret

cgBody ret (SOp op args) = cgOp op args >> ret

cgBody ret SNothing = writeIns [Iconst 0, boxInt] >> ret

cgBody ret (SError x) = invokeError (show x) >> ret

cgBody ret (SForeign returns fdesc args) = cgForeign (parseDescriptor returns fdesc args) where
  argsWithTypes = first fdescFieldDescriptor <$> args

  cgForeign (JStatic clazz fn) = do
    let returnDesc = fdescTypeDescriptor returns
        descriptor r = asm $ MethodDescriptor (fdescFieldDescriptor . fst <$> args) r
    idrisToJava argsWithTypes
    writeIns [ InvokeMethod InvokeStatic clazz fn (descriptor returnDesc) False ]
    javaToIdris returnDesc
    ret

  cgForeign (JGetStaticField clazz fieldName) = do
    let returnDesc = fdescTypeDescriptor returns
    writeIns [ Field FGetStatic clazz fieldName (asm returnDesc) ]
    javaToIdris returnDesc
    ret

  cgForeign (JSetStaticField clazz fieldName)
    | [ty] <- args = do
      idrisToJava argsWithTypes
      let desc = asm $ fdescFieldDescriptor . fst $ ty
      writeIns [ Field FPutStatic clazz fieldName desc ]
      javaToIdris (fdescTypeDescriptor returns)
      ret
    | otherwise = error $ "There can be only one argument for setting a static field: " <> clazz <> "." <> fieldName

  cgForeign (JVirtual clazz fn) = do
    let returnDesc = fdescTypeDescriptor returns
        -- drop first arg type as it is an implicit 'this'
        descriptor = asm $ MethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
    idrisToJava argsWithTypes
    writeIns [ InvokeMethod InvokeVirtual clazz fn descriptor False ]
    javaToIdris returnDesc
    ret

  cgForeign (JGetInstanceField clazz fieldName) = do
    let returnDesc = fdescTypeDescriptor returns
    idrisToJava argsWithTypes
    writeIns [ Field FGetField clazz fieldName (asm returnDesc) ]
    javaToIdris returnDesc
    ret

  cgForeign (JSetInstanceField clazz fieldName)
    | [_, arg] <- args = do
      idrisToJava argsWithTypes
      let desc = asm . fdescTypeDescriptor . fst $ arg
      writeIns [ Field FPutField clazz fieldName desc ]
      javaToIdris (fdescTypeDescriptor returns)
      ret
    | otherwise = error "Setting an instance field must take 2 arguments: the instance and the field value"

  cgForeign (JInterface clazz fn) = do
    let returnDesc = fdescTypeDescriptor returns
        descriptor = asm $ MethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
    idrisToJava argsWithTypes
    writeIns [ InvokeMethod InvokeInterface clazz fn descriptor True ]
    javaToIdris returnDesc
    ret

  cgForeign (JNew clazz) = do
    let returnDesc = VoidDescriptor -- Constructors always return void.
        descriptor r = asm $ MethodDescriptor (fdescFieldDescriptor . fst <$> args) r
    writeIns [ New clazz, Dup ]
    idrisToJava argsWithTypes
    writeIns [ InvokeMethod InvokeSpecial clazz "<init>" (descriptor returnDesc) False ]
    ret

cgBody _ _ = error "NOT IMPLEMENTED!!!!"

data ExportCall = ExportCallConstructor
                | ExportCallInstance
                | ExportCallStatic
                | ExportCallSuper
                deriving (Eq, Show)

exportCode :: ExportIFace -> Cg ()
exportCode (Export (NS (UN "FFI_JVM") _) exportedClassName exportItems) = do
    let (cls, optParent, intf) = parseExportedClassName exportedClassName
        parent = fromMaybe "java/lang/Object" optParent
    writeIns [ CreateClass ComputeMaxs
             , ClassCodeStart 52 Public cls Nothing parent intf ]
    unless (hasConstructorExport exportItems) $ defaultConstructor cls parent
    mapM_ (cgExport cls parent) exportItems
exportCode e = error $ "Unsupported Export: " <> show e

cgExport :: ClassName -> ClassName -> Export -> Cg ()

cgExport _ _ (ExportData (FStr exportedType)) = createClassForIdrisType exportedType

cgExport cname _ (ExportFun _ (FApp ffi [FStr fieldName]) typeDesc _)
  | ffi == sUN "ExportInstanceField" = createField [Public] cname fieldName typeDesc

cgExport cname _ (ExportFun _ (FApp ffi [FStr fieldName]) typeDesc _)
  | ffi == sUN "ExportStaticField" = createField [Public, Static] cname fieldName typeDesc

cgExport cname parent (ExportFun n (FApp ffi [FStr superMethodName]) returns args)
  | ffi == sUN "Super"
    = exportFun cname mname parent superMethodName ExportCallSuper parent returns args where
      JMethodName _ mname = jname n

cgExport cname parent (ExportFun n (FApp ffi [FStr mname]) returns args)
  | ffi == sUN "ExportStatic"
    = exportFun cname mname sourceCname sourceMname ExportCallStatic parent returns args where
      JMethodName sourceCname sourceMname = jname n

cgExport cname parent (ExportFun n (FCon (UN "ExportDefault")) returns args)
  = exportFun cname sourceMname sourceCname sourceMname ExportCallInstance parent returns args where
    JMethodName sourceCname sourceMname = jname n

cgExport cname parent (ExportFun n (FCon (UN "Constructor")) returns args)
  = exportFun cname "<init>" sourceCname sourceMname ExportCallConstructor parent returns args where
    JMethodName sourceCname sourceMname = jname n

cgExport cname parent (ExportFun n (FApp ffi [FStr mname]) returns args)
  | ffi == sUN "ExportInstance"
    = exportFun cname mname sourceCname sourceMname ExportCallInstance parent returns args where
      JMethodName sourceCname sourceMname = jname n

cgExport _ _ exportDef = error $ "Unsupported export definition: " <> show exportDef

exportFun :: ClassName
             -> MethodName
             -> ClassName
             -> MethodName
             -> ExportCall
             -> ClassName
             -> FDesc
             -> [FDesc]
             -> Cg ()
exportFun targetCname targetMethName sourceCname sourceMname exportCall parent returns args = do
  let argDescs = fdescFieldDescriptor <$> (if isStatic then args else drop 1 args) -- for instance method, drop `this`
      returnDesc = if isConstructor then VoidDescriptor else fdescTypeDescriptor returns
      isStatic = exportCall == ExportCallStatic
      isSuperExport = exportCall == ExportCallSuper
      isConstructor = exportCall == ExportCallConstructor
      argStartIndex = if isStatic then 0 else 1 -- drop `this` for instance methods
      loadArgs = zipWithM_ f argDescs [argStartIndex..]
      invType = if isSuperExport then InvokeSpecial else InvokeStatic
      sourceMdesc | isSuperExport = asm $ MethodDescriptor argDescs returnDesc
                  | otherwise = sig $ length args
      f desc index = do
        loadJavaVar index desc
        javaToIdris $ FieldDescriptor desc
      mdesc = asm $ MethodDescriptor argDescs returnDesc
      accessMods = if isStatic then [Public,Static] else [Public]

  writeIns [ CreateMethod accessMods targetCname targetMethName mdesc Nothing Nothing
           , MethodCodeStart
           ]
  when (not isSuperExport && isExportIO returns) $
    writeIns [ Aconstnull -- Setup the 2 null args for "call__IO"
             , Dup
             ]
  when isConstructor $ do -- Call matching super Constructor
    writeIns [ Aload 0 ]
    loadArgs
    writeIns [ InvokeMethod InvokeSpecial parent "<init>" mdesc False ]
  unless isStatic $ writeIns [ Aload 0 ] -- load `this`
  loadArgs
  writeIns [ InvokeMethod invType sourceCname sourceMname sourceMdesc False ]
  unless isSuperExport $
    writeIns [ InvokeMethod InvokeStatic (rtClassSig "Runtime") "unwrap" (sig 1) False ]
  when (not isSuperExport && isExportIO returns) unwrapExportedIO
  returnExport exportCall returnDesc

unwrapExportedIO :: Cg ()
unwrapExportedIO
  = writeIns [ InvokeMethod InvokeStatic "main/Main" "call__IO" (sig 3) False
             , InvokeMethod InvokeStatic (rtClassSig "Runtime") "unwrap" (sig 1) False ]


hasConstructorExport :: [Export] -> Bool
hasConstructorExport = any isConstructorExport where
  isConstructorExport (ExportFun _ (FCon (UN "Constructor")) _ _) = True
  isConstructorExport _                                           = False

cgFun :: [Access] -> ClassName -> MethodName -> [Name] -> SExp -> Cg ()
cgFun access clsName fname args def = do
    modify . updateFunctionArgs $ const args
    modify . updateFunctionName $ const $ JMethodName clsName fname
    modify . updateShouldDescribeFrame $ const True
    writeIns [ CreateMethod access clsName fname signature Nothing Nothing
             , MethodCodeStart
             ]
    modify . updateLocalVarCount $ const nLocalVars
    modify . updateSwitchIndex $ const 0 -- reset
    modify . updateIfIndex $ const 0 -- reset
    writeIns . join . fmap assignNull . DL.fromList $ resultVarIndex: [nArgs .. (nLocalVars - 1)]
    methBody
    writeIns [ Aload resultVarIndex
             , Areturn
             , MaxStackAndLocal (-1) (-1)
             , MethodCodeEnd
             ]

  where
    nArgs = length args
    signature = if static then sig nArgs else sig (pred nArgs)
    static = Static `elem` access
    nLocalVars = localVariables nArgs def
    resultVarIndex = nLocalVars
    tailRecVarIndex = succ resultVarIndex
    totalVars = nLocalVars + 2
    eqThisMethod that = thatClassName == clsName && thatFname == fname where
      JMethodName thatClassName thatFname = jname that
    shouldEliminateTco = maybe False eqThisMethod $ isTailCall def
    ret = if shouldEliminateTco
            then
              writeIns [ Astore resultVarIndex -- Store the result
                       , Iconst 0
                       , Istore tailRecVarIndex ] -- Base case for tailrec. Set the tailrec flag to false.
            else writeIns [ Astore resultVarIndex]
    tcoLocalVarTypes = replicate (nLocalVars + 1) "java/lang/Object"
                     ++ [ "Opcodes.INTEGER" ]
    tcoFrame = Frame FFull totalVars tcoLocalVarTypes 0 []
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

returnExport :: ExportCall -> TypeDescriptor -> Cg ()
returnExport exportCall returnDesc = do
  unless (exportCall == ExportCallSuper) $ idrisDescToJava returnDesc
  javaReturn returnDesc
  writeIns [ MaxStackAndLocal (-1) (-1)
           , MethodCodeEnd
           ]

createClassForIdrisType :: String -> Cg ()
createClassForIdrisType idrisType = do
    writeIns [ CreateClass ComputeMaxs
           , ClassCodeStart 52 Public idrisType Nothing "java/lang/Object" []
           , CreateField [Private, Final] idrisType "value" "Ljava/lang/Object;" Nothing Nothing
           , FieldEnd ]
    constructor
    getter
    createDef
  where
    constructor
      = writeIns [ CreateMethod [Public] idrisType "<init>" "(Ljava/lang/Object;)V" Nothing Nothing
                 , MethodCodeStart
                 , Aload 0
                 , InvokeMethod InvokeSpecial "java/lang/Object" "<init>" "()V" False
                 , Aload 0
                 , Aload 1
                 , Field FPutField idrisType "value" "Ljava/lang/Object;"
                 , Return
                 , MaxStackAndLocal (-1) (-1)
                 , MethodCodeEnd ]
    getter
      = writeIns [ CreateMethod [Public] idrisType "getValue" "()Ljava/lang/Object;" Nothing Nothing
                 , MethodCodeStart
                 , Aload 0
                 , Field FGetField idrisType "value" "Ljava/lang/Object;"
                 , Areturn
                 , MaxStackAndLocal (-1) (-1)
                 , MethodCodeEnd ]
    createDesc = "(Ljava/lang/Object;)L" ++ idrisType ++ ";"
    createDef
      = writeIns [ CreateMethod [Public, Static] idrisType "create" createDesc Nothing Nothing
                 , New idrisType
                 , Dup
                 , Aload 0
                 , InvokeMethod InvokeSpecial idrisType "<init>"  "(Ljava/lang/Object;)V" False
                 , Areturn
                 , MaxStackAndLocal (-1) (-1)
                 , MethodCodeEnd ]

createField :: [Access] -> ClassName -> FieldName -> FDesc -> Cg ()
createField access cname fieldName typeDesc
  = writeIns [ CreateField access cname fieldName desc Nothing Nothing
             , FieldEnd ] where
               desc = asm $ fdescTypeDescriptor typeDesc

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
isTailCallSwitch (SConstCase _ e)     = isTailCall e
isTailCallSwitch (SDefaultCase e)     = isTailCall e
isTailCallSwitch (SConCase _ _ _ _ e) = isTailCall e

localVariables :: Int -> SExp -> Int
localVariables n (SLet (Loc i) v sc)
   = let nAssignmentExp = localVariables 0 v
         nLetBodyExp = localVariables 0 sc in
       maximum ([n, i, nAssignmentExp, nLetBodyExp] :: [Int])
localVariables n (SUpdate _ e) = localVariables n e
localVariables n (SCase _ e alts) = localVariablesSwitch n e alts
localVariables n (SChkCase e alts) = localVariablesSwitch n e alts
localVariables locals _ = locals

localVariablesSwitch :: Int -> LVar -> [SAlt] -> Int
localVariablesSwitch locals _ alts
   = let newLocals = localVariablesAlt <$> alts
         nonDefaultCases = filter (not . defaultCase) alts
     in if all isIntCase alts
          then maximum (locals: newLocals)
          else maximum (locals: newLocals) + length nonDefaultCases

localVariablesAlt :: SAlt -> Int
localVariablesAlt (SConstCase _ e) = localVariables 0 e
localVariablesAlt (SDefaultCase e) = localVariables 0 e
localVariablesAlt (SConCase lv _ _ args e) = max assignmentLocals bodyLocals
   where assignmentLocals = if null args then 0 else lv + length args - 1
         bodyLocals = localVariables 0 e
