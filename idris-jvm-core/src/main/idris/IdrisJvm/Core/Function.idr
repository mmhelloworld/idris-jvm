module IdrisJvm.Core.Function

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.Core.Constant
import IdrisJvm.Core.ControlFlow
import IdrisJvm.Core.Foreign
import IdrisJvm.Core.Operator
import IdrisJvm.Core.Inference
import IdrisJvm.IR.Types
import Data.SortedMap

%access public export

data ExportCall = ExportCallConstructor
                | ExportCallInstance
                | ExportCallStatic
                | ExportCallSuper

Show ExportCall where
    show ExportCallConstructor = "ExportCallConstructor"
    show ExportCallInstance = "ExportCallInstance"
    show ExportCallStatic = "ExportCallStatic"
    show ExportCallSuper = "ExportCallSuper"

Eq ExportCall where
    ExportCallConstructor == ExportCallConstructor = True
    ExportCallInstance == ExportCallInstance = True
    ExportCallStatic == ExportCallStatic = True
    ExportCallSuper == ExportCallSuper = True
    _ == _ = False

createIdrisObject : Int -> List LVar -> Asm ()
createIdrisObject constructorId [] =
  if constructorId < cast unrolledConstructorPropsCount then
    let fieldName = "NO_ARG_CONSTRUCTOR_" ++ cast constructorId
    in Field FGetStatic idrisObjectType fieldName (classSig idrisObjectType)
  else do
    New idrisObjectType
    Dup
    Iconst constructorId
    InvokeMethod InvokeSpecial idrisObjectType "<init>" "(I)V" False

createIdrisObject constructorId args = do
  locTypes <- GetFunctionLocTypes
  New idrisObjectType
  Dup
  Iconst constructorId
  let unrolledArgs = take unrolledConstructorPropsCount args
  let unrolledArgTys = getLocTy locTypes <$> unrolledArgs
  loadUnrolledArgs $ List.zip unrolledArgs unrolledArgTys
  createArrayProperties
  InvokeMethod InvokeSpecial idrisObjectType "<init>" constructorSig False
where
  argsLength : Int
  argsLength = cast $ length args

  arrayProps : List LVar
  arrayProps = drop unrolledConstructorPropsCount args

  loadUnrolledArgs : List (LVar, InferredType) -> Asm ()
  loadUnrolledArgs [] = pure ()
  loadUnrolledArgs ((var, varTy) :: vars) = do
    locTys <- GetFunctionLocTypes
    loadVar locTys varTy inferredObjectType var
    loadUnrolledArgs vars

  arrayPropsLength : Int
  arrayPropsLength = cast $ length arrayProps

  constructorSig : String
  constructorSig =
    if arrayPropsLength == 0 then
      "(I" ++ repeatObjectDesc (cast argsLength) ++ ")V"
    else
      "(I" ++ repeatObjectDesc unrolledConstructorPropsCount ++ "[Ljava/lang/Object;)V"

  storeArrayProp : InferredTypeStore -> Int -> LVar -> Asm ()
  storeArrayProp types idx var = do
    Dup
    Iconst idx
    let varTy = getLocTy types var
    loadVar types varTy inferredObjectType var
    Aastore

  storeArrayProps : Asm ()
  storeArrayProps =
    when (arrayPropsLength > 0) $ do
        locTypes <- GetFunctionLocTypes
        sequence_ $ List.zipWith (storeArrayProp locTypes) [0 .. (arrayPropsLength - 1)] arrayProps

  createArrayProperties : Asm ()
  createArrayProperties =
    if arrayPropsLength == 0 then
      Pure ()
    else do
      Iconst arrayPropsLength
      Anewarray "java/lang/Object"
      storeArrayProps

hasConstructorExport : List Export -> Bool
hasConstructorExport = any isConstructorExport where
    isConstructorExport (ExportFun _ (FCon "Constructor") _ _) = True
    isConstructorExport _                                      = False

tailRecStartLabelName : String
tailRecStartLabelName = "$tailRecStartLabel"

tailRecEndLabelName : String
tailRecEndLabelName = "$tailRecEndLabel"

assignNull : InferredType -> LVar -> Asm ()
assignNull ty var = do Aconstnull; storeVar ty ty var

initializeInferredVar : InferredType -> Int -> Asm ()
initializeInferredVar ty varIndex =
    let var = Loc varIndex
    in case ty of
        IBool => do Iconst 0; storeVar ty ty var
        IByte => do Iconst 0; storeVar ty ty var
        IChar => do Iconst 0; storeVar ty ty var
        IShort => do Iconst 0; storeVar ty ty var
        IInt => do Iconst 0; storeVar ty ty var
        ILong => do Lconst 0; storeVar ty ty var
        IFloat => do Fconst 0; storeVar ty ty var
        IDouble => do Dconst 0; storeVar ty ty var
        Ref refTy => assignNull ty var
        IArray elemTy => assignNull ty var
        IUnknown => assignNull ty var

initializeLocalVars : List (Int, InferredType) -> Asm ()
initializeLocalVars [] = pure ()
initializeLocalVars ((var, ty) :: vars) = do
    initializeInferredVar ty var
    initializeLocalVars vars

mutual

  cgBody : (InferredType -> Asm ()) -> SExp -> Asm ()
  cgBody ret (SV (Glob n)) = do
    let fname = jname n
    let MkJMethodName cname mname = fname
    (retTy, _) <- getFunctionType fname
    let desc = getInferredFunDesc [] retTy
    InvokeMethod InvokeStatic cname mname desc False
    ret inferredObjectType

  cgBody ret (SV var) = do
    locTypes <- GetFunctionLocTypes
    let varTy = getLocTy locTypes var
    loadVar locTypes varTy varTy var
    ret varTy

  cgBody ret (SApp True f args) = do
    caller <- GetFunctionName
    let targetMethodName = jname f
    if targetMethodName == caller then
       -- self tail call, assign parameters then loop
       let argsLength = the Int $ cast $ length args
       in when (argsLength > 0) $ assign (Loc <$> [0 .. (pred argsLength)]) args
    -- non-self tail call
    else do
      createThunk caller targetMethodName args
      ret (Ref thunkClass)

  cgBody ret (SApp False f args) = cgSApp ret False f args

  cgBody ret (SLet i v sc) = do
      locTys <- GetFunctionLocTypes
      let iTy = getLocTy locTys i
      assign iTy
      cgBody ret sc
    where
      assign : InferredType -> Asm ()
      assign iTy = case v of
                     SNothing => do Aconstnull; storeVar inferredObjectType iTy i
                     _ => cgBody (\vTy => storeVar vTy iTy i) v

  cgBody ret (SUpdate _ e) = cgBody ret e

  cgBody ret (SProj (Loc v) i) = do
    idrisObjectProperty v i
    ret inferredObjectType

  cgBody ret (SCon _ 0 "Prelude.Bool.False" []) = do
    Iconst 0
    ret IBool
  cgBody ret (SCon _ 1 "Prelude.Bool.True" []) = do
    Iconst 1
    ret IBool

  cgBody ret (SCon _ 0 "Prelude.Maybe.Nothing" []) = do Aconstnull; ret inferredObjectType
  cgBody ret (SCon _ 1 "Prelude.Maybe.Just" [var]) = do
    locTypes <- GetFunctionLocTypes
    let varTy = getLocTy locTypes var
    loadVar locTypes varTy inferredObjectType var
    ret inferredObjectType

  cgBody ret (SCon _ t _ args) = do createIdrisObject t args; ret inferredIdrisObjectType

  cgBody ret (SCase _ e ((SConCase _ 0 "Prelude.Bool.False" [] falseAlt) ::
                         (SConCase _ 1 "Prelude.Bool.True" [] trueAlt) ::
                         _))
    = cgIfTrueElse ret cgBody e trueAlt falseAlt

  cgBody ret (SCase _ e ((SConCase _ 1 "Prelude.Bool.True" [] trueAlt) ::
                        (SDefaultCase falseAlt) ::
                        _))
    = cgIfTrueElse ret cgBody e trueAlt falseAlt

  cgBody ret (SCase _ e ((SConCase _ 0 "Prelude.Bool.False" [] falseAlt) ::
                        (SDefaultCase trueAlt) ::
                        _))
    = cgIfTrueElse ret cgBody e trueAlt falseAlt

  cgBody ret (SCase _ e ((SConCase justValueStore 1 "Prelude.Maybe.Just" [_] justExpr) ::
                         (SConCase _ 0 "Prelude.Maybe.Nothing" [] nothingExpr) ::
                         _))
    = cgIfNonNull ret cgBody e justValueStore justExpr nothingExpr

  cgBody ret (SCase _ e ((SConCase justValueStore 1 "Prelude.Maybe.Just" [_] justExpr) ::
                         (SDefaultCase defaultExpr) ::
                         _))
    = cgIfNonNull ret cgBody e justValueStore justExpr defaultExpr

  cgBody ret (SCase _ e ((SConCase _ 0 "Prelude.Maybe.Nothing" [] nothingExpr) ::
                         (SDefaultCase defaultExpr) ::
                         _))
    = cgIfNull ret cgBody e nothingExpr defaultExpr

  cgBody ret (SCase _ e [SConCase lv _ _ args expr]) = cgConCase ret cgBody (locIndex e) lv args expr

  cgBody ret (SCase _ e [SDefaultCase defaultCaseExpr]) = cgBody ret defaultCaseExpr

  cgBody ret (SCase _ e alts) =
    if any defaultCase alts
        then cgSwitch ret cgBody e alts
        else cgSwitch ret cgBody e (alts ++ [SDefaultCase SNothing])

  cgBody ret (SChkCase e [SDefaultCase defaultCaseExpr]) = cgBody ret defaultCaseExpr

  cgBody ret (SChkCase e alts) = cgSwitch ret cgBody e alts

  cgBody ret (SConst c) = cgConst ret c

  cgBody ret (SOp op args) = cgOp ret op args

  cgBody ret SNothing = do
    invokeError "Unexpected case encountered. This could be due to non-exhaustive patterns."
    ret inferredObjectType

  cgBody ret (SError x) = do invokeError (show x); ret IUnknown

  cgBody ret (SForeign returns fdesc args) = cgForeign (parseDescriptor returns fdesc args) where

    argsLength : Nat
    argsLength = length args

    argVars : List LVar
    argVars = snd <$> args

    targetArgIndices : List Int
    targetArgIndices = if argsLength > 0 then [0 .. (pred $ the Int $ cast argsLength)] else []

    lambdaArgVars : List LVar
    lambdaArgVars = Loc <$> targetArgIndices

    inferredRetTy : TypeDescriptor -> InferredType
    inferredRetTy returnDesc = typeDescriptorToInferredType returnDesc

    retVoidOrBoxed : TypeDescriptor -> Asm ()
    retVoidOrBoxed VoidDescriptor = Aconstnull
    retVoidOrBoxed returnDesc = box (inferredRetTy returnDesc)

    synthenticMethodArgTys : InferredTypeStore
    synthenticMethodArgTys = SortedMap.fromList $ List.zip targetArgIndices $ replicate argsLength inferredObjectType

    targetLocTys : InferredTypeStore
    targetLocTys =
      let targetArgTys = List.zip targetArgIndices $ fst <$> args
      in SortedMap.fromList $
        (\(var, fdesc) => (var, fieldTypeDescriptorToInferredType $ fdescFieldDescriptor fdesc)) <$> targetArgTys

    cgForeign (JStatic clazz fn) = case fdescTypeDescriptor returns of
      ThrowableDescriptor returnDesc => do
        locTys <- GetFunctionLocTypes
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) returnDesc
        let arity = length args
        let lambdaBody = do
          loadVars synthenticMethodArgTys targetLocTys lambdaArgVars
          InvokeMethod InvokeStatic clazz fn descriptor False
          retVoidOrBoxed returnDesc
        caller <- GetFunctionName
        let targetMethodName = MkJMethodName clazz fn
        createExceptionHandlerThunk caller targetMethodName argVars lambdaBody
        InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
        ret inferredObjectType
      returnDesc => do
        locTys <- GetFunctionLocTypes
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) returnDesc
        loadVars locTys targetLocTys argVars
        InvokeMethod InvokeStatic clazz fn descriptor False
        when (returnDesc == VoidDescriptor) Aconstnull
        ret (inferredRetTy returnDesc)

    cgForeign (JGetStaticField clazz fieldName) = do
      let returnDesc = fdescTypeDescriptor returns
      Field FGetStatic clazz fieldName (asmTypeDesc returnDesc)
      ret (typeDescriptorToInferredType returnDesc)

    cgForeign (JSetStaticField clazz fieldName) = case args of
      [ty] => do
        locTys <- GetFunctionLocTypes
        loadVars locTys targetLocTys argVars
        let desc = asmFieldTypeDesc $ fdescFieldDescriptor . fst $ ty
        Field FPutStatic clazz fieldName desc
        let returnDesc = fdescTypeDescriptor returns
        Aconstnull
        ret (typeDescriptorToInferredType returnDesc)
      otherwise => jerror $ "There can be only one argument for setting a static field: " ++ clazz ++ "." ++ fieldName

    cgForeign (JVirtual clazz fn) = case fdescTypeDescriptor returns of
      ThrowableDescriptor returnDesc => do
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
        locTys <- GetFunctionLocTypes
        let lambdaBody = do
          loadVars synthenticMethodArgTys targetLocTys lambdaArgVars
          InvokeMethod InvokeVirtual clazz fn descriptor False
          retVoidOrBoxed returnDesc
        caller <- GetFunctionName
        let targetMethodName = MkJMethodName clazz fn
        createExceptionHandlerThunk caller targetMethodName argVars lambdaBody
        InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
        ret inferredObjectType
      returnDesc => do
          locTys <- GetFunctionLocTypes
          -- drop first arg type as it is an implicit 'this'
          let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
          loadVars locTys targetLocTys argVars
          InvokeMethod InvokeVirtual clazz fn descriptor False
          when (returnDesc == VoidDescriptor) Aconstnull
          ret (inferredRetTy returnDesc)

    cgForeign (JGetInstanceField clazz fieldName) = do
      locTys <- GetFunctionLocTypes
      let returnDesc = fdescTypeDescriptor returns
      loadVars locTys targetLocTys argVars
      Field FGetField clazz fieldName (asmTypeDesc returnDesc)
      ret (inferredRetTy returnDesc)

    cgForeign (JSetInstanceField clazz fieldName) = case args of
      [_, arg] => do
        locTys <- GetFunctionLocTypes
        loadVars locTys targetLocTys argVars
        let desc = asmTypeDesc . fdescTypeDescriptor . fst $ arg
        Field FPutField clazz fieldName desc
        let returnDesc = fdescTypeDescriptor returns
        Aconstnull
        ret (typeDescriptorToInferredType returnDesc)
      otherwise => jerror "Setting an instance field must take 2 arguments: the instance and the field value"

    cgForeign (JInterface clazz fn) = case fdescTypeDescriptor returns of
        ThrowableDescriptor returnDesc => do
          locTys <- GetFunctionLocTypes
          let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
          let lambdaBody = do
            loadVars synthenticMethodArgTys targetLocTys lambdaArgVars
            InvokeMethod InvokeInterface clazz fn descriptor True
            retVoidOrBoxed returnDesc
          caller <- GetFunctionName
          let targetMethodName = MkJMethodName clazz fn
          createExceptionHandlerThunk caller targetMethodName argVars lambdaBody
          InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
          ret inferredObjectType
        returnDesc => do
          locTys <- GetFunctionLocTypes
          let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
          loadVars locTys targetLocTys argVars
          InvokeMethod InvokeInterface clazz fn descriptor True
          when (returnDesc == VoidDescriptor) Aconstnull
          ret (inferredRetTy returnDesc)

    cgForeign (JNew clazz) = case fdescTypeDescriptor returns of
      ThrowableDescriptor returnDesc => do
        locTys <- GetFunctionLocTypes
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) VoidDescriptor
        let lambdaBody = do
          New clazz
          Dup
          loadVars synthenticMethodArgTys targetLocTys lambdaArgVars
          InvokeMethod InvokeSpecial clazz "<init>" descriptor False
        caller <- GetFunctionName
        let targetMethodName = MkJMethodName clazz "<init>"
        createExceptionHandlerThunk caller targetMethodName argVars lambdaBody
        InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
        ret inferredObjectType
      returnDesc => do
        locTys <- GetFunctionLocTypes
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) VoidDescriptor
        New clazz
        Dup
        loadVars locTys targetLocTys argVars
        InvokeMethod InvokeSpecial clazz "<init>" descriptor False
        ret (inferredRetTy returnDesc)

    cgForeign JNewArray = case args of
      [ty] => do
        locTys <- GetFunctionLocTypes
        loadVars locTys targetLocTys argVars
        let arrDesc = fdescFieldDescriptor returns
        let elemDesc = case arrDesc of
          FieldTyDescReference (ArrayDesc elem) => elem
          otherwise => jerror $ "Invalid return type while creating an array" ++ show returns
        anewarray elemDesc
        ret $ typeDescriptorToInferredType (FieldDescriptor arrDesc)
      otherwise => jerror $ "There can be only one argument (length) to create an array" ++ show args

    cgForeign JMultiNewArray = do
        locTys <- GetFunctionLocTypes
        loadVars locTys targetLocTys argVars
        let returnDesc = fdescRefDescriptor returns
        let arrayType = refTyClassName returnDesc
        Multianewarray arrayType (List.length args)
        ret $ fieldTypeDescriptorToInferredType (FieldTyDescReference returnDesc)

    cgForeign JSetArray = case args of
      ((arrFDesc, arr) :: rest@(arg1 :: arg2 :: args)) => do
        let value = last rest
        let indices = init rest
        let valueDesc = fdescFieldDescriptor (fst value)
        aload $ locIndex arr
        Checkcast $ refTyClassName (fdescRefDescriptor arrFDesc)
        idrisToJavaLoadArray $ (\(fdesc, lvar) => (fdescFieldDescriptor fdesc, lvar)) <$> indices
        idrisToJava [(valueDesc, snd value)]
        arrayStore valueDesc
        Aconstnull
        ret IUnknown
      otherwise => jerror $ "Invalid arguments while trying to set an element inside array: " ++ show args

    cgForeign JGetArray = case args of
      ((arrFDesc, arr) :: indices@(index :: restIndices)) => do
        aload $ locIndex arr
        Checkcast $ refTyClassName (fdescRefDescriptor arrFDesc)
        idrisToJavaLoadArray $ (\(fdesc, lvar) => (fdescFieldDescriptor fdesc, lvar)) <$> indices
        let returnDesc = fdescTypeDescriptor returns
        arrayLoad $ typeDescToarrayElemDesc returnDesc
        ret $ typeDescriptorToInferredType returnDesc
      otherwise => jerror $ "Invalid arguments while trying to get an element from array: " ++ show args

    cgForeign JArrayLength = case args of
      [(arrFDesc, arr)] => do
        aload $ locIndex arr
        Checkcast $ refTyClassName (fdescRefDescriptor arrFDesc)
        Arraylength
        ret IInt
      otherwise => jerror $ "Invalid arguments while trying to get length of an array: " ++ show args

    cgForeign (JInstanceOf className) = case args of
      [ty] => do
          locTys <- GetFunctionLocTypes
          loadVars locTys targetLocTys argVars
          InstanceOf className
          let returnDesc = fdescTypeDescriptor returns
          ret $ typeDescriptorToInferredType returnDesc

    cgForeign (JClassLiteral "int") = do getPrimitiveClass "java/lang/Integer"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral "byte") = do getPrimitiveClass "java/lang/Byte"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral "char") = do getPrimitiveClass "java/lang/Character"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral "short") = do getPrimitiveClass "java/lang/Short"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral "boolean") = do getPrimitiveClass "java/lang/Boolean"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral "long") = do getPrimitiveClass "java/lang/Long"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral "float") = do getPrimitiveClass "java/lang/Float"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral "double") = do getPrimitiveClass "java/lang/Double"; ret $ Ref "java/lang/Class"
    cgForeign (JClassLiteral clazz) = do
      Ldc $ TypeConst $ classSig clazz
      ret $ Ref "java/lang/Class"

    cgForeign (JLambda clazz interfaceFnName) = case args of
      [(interfaceFnTy, _), (lambdaTy, lambdaVar)] => do
        caller <- GetFunctionName
        createJvmFunctionalObject lambdaVar caller (MkJMethodName clazz interfaceFnName) interfaceFnTy lambdaTy
        ret $ Ref clazz
      otherwise => jerror $ "There can be only two arguments while passing a java lambda for " ++ clazz ++
        "." ++ interfaceFnName ++ ": the interface method signature and the lambda signature"

  cgBody _ _ = jerror "NOT IMPLEMENTED!!!!"

  cgSApp : (InferredType -> Asm ()) -> Bool -> String -> List LVar -> Asm ()
  cgSApp ret isTailCall f args = do
    callerLocTys <- GetFunctionLocTypes
    let fname = jname f
    let MkJMethodName cname mname = fname
    let argsLength = length args
    (targetRetTy, targetLocTys) <- getFunctionType fname
    loadVars callerLocTys targetLocTys args
    let targetArgTys = if argsLength > 0
        then getLocTy targetLocTys . Loc . cast <$> [0 .. (Nat.pred argsLength)]
        else []
    let targetMethodDesc = getInferredFunDesc targetArgTys targetRetTy
    InvokeMethod InvokeStatic cname mname targetMethodDesc False
    when (not isTailCall && (isThunk targetRetTy || isObject targetRetTy)) $
        InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" "(Ljava/lang/Object;)Ljava/lang/Object;" False
    ret targetRetTy

  exportCode : SortedMap JMethodName InferredFunctionType -> ExportIFace -> Asm ()
  exportCode types (MkExportIFace "IdrisJvm.FFI.FFI_JVM" exportedClassName exportItems) = do
      let (cls, parent, intf) = parseExportedClassName exportedClassName
      let (classAnns, otherExports) = parseClassAnnotations exportItems
      CreateClass ComputeMaxs
      ClassCodeStart 52 Public cls Nothing parent intf classAnns
      SourceInfo "IdrisGenerated.idr"
      when (not $ hasConstructorExport exportItems) $ defaultConstructor cls parent
      sequence_ $ (cgExport types cls parent) <$> otherExports
  exportCode _ e = jerror $ "Unsupported Export: " ++ show e

  cgExport : SortedMap JMethodName InferredFunctionType -> ClassName -> ClassName -> Export -> Asm ()
  cgExport _ _ _ (ExportData (FStr exportedType)) = createClassForIdrisType exportedType

  cgExport _ cname _ (ExportFun _ (FApp "ExportInstanceField" [FStr fieldName]) typeDesc _)
    = createField [Public] cname fieldName typeDesc

  cgExport _ cname _ (ExportFun _ (FApp "ExportStaticField" [FStr fieldName]) typeDesc _)
    = createField [Public, Static] cname fieldName typeDesc

  cgExport types cname parent (ExportFun n (FApp "Super" [FStr superMethodName]) returns args)
    = exportFun types cname (jmethName (jname n)) parent superMethodName ExportCallSuper parent returns args [] []

  cgExport types cname parent (ExportFun n (FApp "ExportStatic" [FStr mname]) returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun types cname mname sourceCname sourceMname ExportCallStatic parent returns args [] []

  cgExport types cname parent (ExportFun n (FCon "ExportDefault") returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun types cname sourceMname sourceCname sourceMname ExportCallInstance parent returns args [] []

  cgExport types cname parent (ExportFun n (FCon "Constructor") returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun types cname "<init>" sourceCname sourceMname ExportCallConstructor parent returns args [] []

  cgExport types cname parent (ExportFun n (FApp "ExportInstance" [FStr mname]) returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun types cname mname sourceCname sourceMname ExportCallInstance parent returns args [] []

  cgExport types cname parent
      (ExportFun n (FApp "ExportInstanceWithAnn" [FStr mname, FApp "::" annDescs, paramAnnDescs]) returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
          anns = join $ parseAnnotations <$> annDescs
          paramAnns = parseParamAnnotations paramAnnDescs
      in exportFun types cname mname sourceCname sourceMname ExportCallInstance parent returns args anns paramAnns

  cgExport _ _ _ exportDef = jerror $ "Unsupported export definition: " ++ show exportDef

  parseClassAnnotations : List Export -> (List Annotation, List Export)
  parseClassAnnotations exports@(ExportFun n (FApp "Anns" desc) _ _:: rest) =
    if jmethName (jname n) == "classWith" then (join $ parseAnnotations <$> desc, rest)
    else ([], exports)
  parseClassAnnotations nonClassAnnotations = ([], nonClassAnnotations)

  loadExportFunArgs : List (Int, FieldTypeDescriptor) -> InferredTypeStore -> InferredType -> Int -> Asm ()
  loadExportFunArgs exportedArgs argTys argTy argIndex = do
    let loc = Loc argIndex
    case find (\(index, argDesc) => index == argIndex) exportedArgs of
        Just (index, (FieldTyDescReference refTy)) => do
            let cname = refTyClassName refTy
            loadVar argTys (getLocTy argTys loc) (Ref cname) loc
            InvokeMethod InvokeVirtual cname "getValue" "()Ljava/lang/Object;" False
            cgCast inferredObjectType argTy
        Nothing => loadVar argTys (getLocTy argTys loc) argTy loc

  exportFun : SortedMap JMethodName InferredFunctionType
           -> ClassName
           -> MethodName
           -> ClassName
           -> MethodName
           -> ExportCall
           -> ClassName
           -> FDesc
           -> List FDesc
           -> List Annotation
           -> List (List Annotation)
           -> Asm ()
  exportFun types targetCname targetMethName sourceCname sourceMname exportCall parent returns args anns paramAnns = do
      let isStatic = exportCall == ExportCallStatic
      let sourceMethodName = MkJMethodName sourceCname sourceMname
      let argsLength = length args
      let sourceMethodArity = maybe argsLength arity (lookup sourceMethodName types)
      let accessMods = if isStatic then [Public, Static] else [Public]
      let argStartIndex = the Int $ if isStatic then 0 else 1 -- drop `this` for instance methods
      let isSuperExport = exportCall == ExportCallSuper
      let isConstructor = exportCall == ExportCallConstructor
      let isIo = isExportIO returns
      let argDescs = fdescFieldDescriptor <$> (if isStatic then args else drop 1 args) -- for instance method, drop `this`
      let returnDesc = if isConstructor then VoidDescriptor else fdescTypeDescriptor returns
      let mdesc = asmMethodDesc $ MkMethodDescriptor argDescs returnDesc
      let argIndices = intRange argStartIndex (cast argsLength)
      let exportedArgs = filter (\(index, arg) => isExportedDesc arg) $ List.zip argIndices argDescs
      let targetArgTys = SortedMap.fromList $ List.zip argIndices $ fieldTypeDescriptorToInferredType <$> argDescs
      let f = \toTy, index => loadExportFunArgs exportedArgs targetArgTys toTy index
      (sourceRetTy, sourceLocTyStore) <- getFunctionType (MkJMethodName sourceCname sourceMname)
      let sourceLocTys = values sourceLocTyStore
      let sourceArgTys = List.take (length argDescs) $ (if isStatic then sourceLocTys else drop 1 sourceLocTys)
      let loadArgs = sequence_ $ List.zipWith f sourceArgTys argIndices
      let invType = if isSuperExport then InvokeSpecial else InvokeStatic
      let shouldApplyCallIo = argsLength == sourceMethodArity
      let sourceMdesc = if isSuperExport
                            then asmMethodDesc $ MkMethodDescriptor argDescs returnDesc
                            else getInferredFunDesc (take sourceMethodArity $ values sourceLocTyStore) sourceRetTy
      let nullAdditionalArgs = the (Asm ()) $ case isLTE argsLength sourceMethodArity of
          Yes smaller =>
              let diff = sourceMethodArity - argsLength
              in case diff of
                   Z => pure ()
                   n => sequence_ $ replicate n Aconstnull
          No _ => pure ()
      CreateMethod accessMods targetCname targetMethName mdesc Nothing Nothing anns paramAnns
      MethodCodeStart
      when isConstructor $ do -- Call matching super Constructor
        Aload 0 -- load "this"
        loadArgs
        InvokeMethod InvokeSpecial parent "<init>" mdesc False
      when (not isSuperExport && isIo && shouldApplyCallIo) $ do
        Aconstnull -- Setup the 2 null args for "call__IO"
        Dup
      when (not isStatic) $ Aload 0 -- load "this"
      loadArgs
      nullAdditionalArgs
      InvokeMethod invType sourceCname sourceMname sourceMdesc False
      let isPossibleThunk = isThunk sourceRetTy || isObject sourceRetTy
      when (not isSuperExport && isPossibleThunk) $
          InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" (sig 1) False
      when (not isSuperExport && isIo) $ unwrapExportedIO sourceRetTy shouldApplyCallIo
      case returnDesc of
          (FieldDescriptor (FieldTyDescReference (IdrisExportDesc cname))) =>
              let desc = "(Ljava/lang/Object;)L" ++ cname ++ ";"
              in InvokeMethod InvokeStatic cname "create" desc False
          _ => when (not isSuperExport && (isPossibleThunk || isIo)) $
                  cgCast inferredObjectType (typeDescriptorToInferredType returnDesc)
      javaReturn returnDesc
      MaxStackAndLocal (-1) (-1)
      MethodCodeEnd
    where
      unwrapExportedIO : InferredType -> (shouldApplyCallIo: Bool) -> Asm ()
      unwrapExportedIO sourceRetTy shouldApplyCallIo =
        if shouldApplyCallIo then do
          let fname = MkJMethodName "main/Main" "call__IO"
          (retTy, locTys) <- getFunctionType fname
          let argTys = getLocTy locTys <$> the (List LVar) [Loc 0, Loc 1, Loc 2]
          let (_ :: _ :: lastArgTy :: _) = argTys
          let desc = getInferredFunDesc argTys retTy
          cgCast sourceRetTy lastArgTy
          InvokeMethod InvokeStatic "main/Main" "call__IO" desc False
          InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" (sig 1) False
        else do
          cgCast sourceRetTy inferredObjectType
          InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" (sig 1) False

  cgFun : List Access -> String -> ClassName -> MethodName -> List String -> Int -> SExp -> Asm ()
  cgFun access idrisName clsName fname args locs def = do
      let jMethodName = MkJMethodName clsName fname
      UpdateFunctionName jMethodName
      functionTypes <- GetFunctionTypes
      let (retTy, types, nLocVars) = calculateFunctionType jMethodName functionTypes
      UpdateFunctionTypes $ SortedMap.insert jMethodName
        (MkInferredFunctionType jMethodName retTy types arity) functionTypes
      UpdateFunctionLocTypes types
      UpdateFunctionRetType retTy
      UpdateLocalVarCount $ cast (if shouldEliminateTco then nLocVars + 2 else nLocVars + 1)
      locTys <- GetFunctionLocTypes
      Debug ("### " ++ clsName ++ "." ++ fname ++ ": " ++ show (toList locTys) ++ " -> " ++ show retTy)
      UpdateShouldDescribeFrame True
      let argTys = if nArgs > 0 then getLocTy types <$> (Loc <$> [0 .. pred nArgs]) else []
      CreateMethod access clsName fname (getInferredFunDesc argTys retTy) Nothing Nothing [] []
      MethodCodeStart
      UpdateSwitchIndex 0 -- reset
      UpdateIfIndex 0 -- reset
      initializeLocalVars $ drop (cast nArgs) $ toList types
      methBody nLocVars

      let resultVarIndex = nLocVars
      loadVar locTys retTy retTy (Loc resultVarIndex)
      javaReturn (FieldDescriptor $ inferredTypeToFieldTypeDesc retTy)

      MaxStackAndLocal (-1) (-1)
      MethodCodeEnd
    where
      arity : Nat
      arity = length args

      nArgs : Int
      nArgs = cast arity

      getLocalVarCount : InferredTypeStore -> Int
      getLocalVarCount locTys = case keys locTys of
        [] => nArgs + locs
        locVars@(_ :: _) => max (last locVars + 1) (nArgs + locs)

      shouldEliminateTco : Bool
      shouldEliminateTco = any (== idrisName) (findTailCallApps [] def)

      calculateFunctionType : JMethodName
                           -> SortedMap JMethodName InferredFunctionType
                           -> (InferredType, InferredTypeStore, Int)
      calculateFunctionType fname types =
        let MkInferredFunctionType _ retTy localTys _ =
                fromMaybe (MkInferredFunctionType fname IUnknown SortedMap.empty arity)
                  $ lookup fname types
            nLocVars = getLocalVarCount localTys
            resultVarIndex = nLocVars
            tailRecVarIndex = nLocVars + 1
            withTcoType = if shouldEliminateTco then SortedMap.insert tailRecVarIndex IInt localTys else localTys
            withResultVarType = SortedMap.insert resultVarIndex retTy withTcoType
            totalVars = if shouldEliminateTco then nLocVars + 2 else nLocVars + 1
            allVarIndices = if totalVars > 0 then [0 .. pred totalVars] else []
            missingVars = (\var => (var, IUnknown)) <$> (allVarIndices \\ keys withResultVarType)
            allVars = insertFrom missingVars withResultVarType
        in (retTy, allVars, nLocVars)

      methBody : Int -> Asm ()
      methBody nLocVars =
            if shouldEliminateTco
            then do
              types <- GetFunctionLocTypes
              Iconst 1
              let tailRecVar = Loc tailRecVarIndex
              opWithWordSize types Istore tailRecVar
              CreateLabel tailRecStartLabelName
              LabelStart tailRecStartLabelName
              UpdateShouldDescribeFrame True
              addFrame
              opWithWordSize types Iload tailRecVar
              CreateLabel tailRecEndLabelName
              Ifeq tailRecEndLabelName
              cgBody ret def
              Goto tailRecStartLabelName
              LabelStart tailRecEndLabelName
              Frame FSame 0 [] 0 []
            else cgBody ret def
        where
            tailRecVarIndex : Int
            tailRecVarIndex = nLocVars + 1

            ret : InferredType -> Asm ()
            ret ty = do
              retTy <- GetFunctionRetType
              let resultVarIndex = nLocVars
              storeVar ty retTy (Loc resultVarIndex)
              when shouldEliminateTco $
                 do Iconst 0
                    Istore tailRecVarIndex -- Base case for tailrec. Set the tailrec flag to false.

  createClassForIdrisType : String -> Asm ()
  createClassForIdrisType idrisType = do
      CreateClass ComputeMaxs
      ClassCodeStart 52 Public idrisType Nothing "java/lang/Object" [] []
      SourceInfo "IdrisGenerated.idr"
      CreateField [Private, Final] idrisType "value" "Ljava/lang/Object;" Nothing Nothing
      FieldEnd
      createConstructor
      getter
      createDef
    where
      createConstructor : Asm ()
      createConstructor = do
        CreateMethod [Public] idrisType "<init>" "(Ljava/lang/Object;)V" Nothing Nothing [] []
        MethodCodeStart
        Aload 0
        InvokeMethod InvokeSpecial "java/lang/Object" "<init>" "()V" False
        Aload 0
        Aload 1
        Field FPutField idrisType "value" "Ljava/lang/Object;"
        Return
        MaxStackAndLocal (-1) (-1)
        MethodCodeEnd

      getter : Asm ()
      getter = do
        CreateMethod [Public] idrisType "getValue" "()Ljava/lang/Object;" Nothing Nothing [] []
        MethodCodeStart
        Aload 0
        Field FGetField idrisType "value" "Ljava/lang/Object;"
        Areturn
        MaxStackAndLocal (-1) (-1)
        MethodCodeEnd

      createDesc : String
      createDesc = "(Ljava/lang/Object;)L" ++ idrisType ++ ";"

      createDef : Asm ()
      createDef = do
        CreateMethod [Public, Static] idrisType "create" createDesc Nothing Nothing [] []
        New idrisType
        Dup
        Aload 0
        InvokeMethod InvokeSpecial idrisType "<init>"  "(Ljava/lang/Object;)V" False
        Areturn
        MaxStackAndLocal (-1) (-1)
        MethodCodeEnd

  createField : List Access -> ClassName -> FieldName -> FDesc -> Asm ()
  createField access cname fieldName typeDesc = do
      CreateField access cname fieldName desc Nothing Nothing
      FieldEnd
    where
      desc = asmTypeDesc $ fdescTypeDescriptor typeDesc

  parseAnnotations : FDesc -> List Annotation
  parseAnnotations (FApp "::" [FCon "Annotation", FApp "Ann" [FStr annTypeName, attr], rest])
    = let typeDesc = "L" ++ annTypeName ++ ";"
      in (MkAnnotation typeDesc (parseAnnotationAttr attr)) :: parseAnnotations rest

  parseAnnotations (FApp "Ann" [FStr annTypeName, attr])
    = let typeDesc = "L" ++ annTypeName ++ ";"
      in MkAnnotation typeDesc (parseAnnotationAttr attr) :: []

  parseAnnotations (FApp "Nil" [FCon "Annotation"]) = []
  parseAnnotations (FCon "Annotation") = []
  parseAnnotations desc = jerror $ "parseAnnotation: not implemented: " ++ show desc

  parseParamAnnotations : FDesc -> List (List Annotation)
  parseParamAnnotations (FApp "::" [FApp "List" [FCon "Annotation"], ann, rest])
    = parseAnnotations ann :: parseParamAnnotations rest
  parseParamAnnotations (FApp "Nil" [FApp "List" [FCon "Annotation"]]) = []

  parseAnnotationAttr : FDesc -> List AnnotationProperty
  parseAnnotationAttr (FApp "::" [ FApp "Pair" [ FUnknown, FCon "AnnotationValue" ]
                                 , FApp "MkPair" [ FUnknown
                                               , FCon "AnnotationValue"
                                               , FStr attrName
                                               , annValueDesc
                                               ]
                                  , rest
                                  ])
    = (attrName, parseAnnotationValue annValueDesc) :: parseAnnotationAttr rest
  parseAnnotationAttr (FApp "Nil" [ FApp "Pair" [ FUnknown , FCon "AnnotationValue"]]) = []
  parseAnnotationAttr (FApp "Nil" [ FCon "AnnotationNameValuePair" ]) = []
  parseAnnotationAttr desc = jerror $ "Annotation attribute value not yet supported: " ++ show desc

  parseAnnArrayElements : List FDesc -> List AnnotationValue
  parseAnnArrayElements [] = []
  parseAnnArrayElements [FApp "::" (FCon "AnnotationValue" :: value :: values)] =
    parseAnnotationValue value :: parseAnnArrayElements values
  parseAnnArrayElements [FApp "Nil" [FCon "AnnotationValue"]] = []

  parseAnnotationValue : FDesc -> AnnotationValue
  parseAnnotationValue (FApp "AnnString" [FStr value]) = AnnString value
  parseAnnotationValue (FApp "AnnEnum" [FStr enum, FStr value]) = AnnEnum (asmRefTyDesc (ClassDesc enum)) value
  parseAnnotationValue (FApp "AnnArray" values) = AnnArray (parseAnnArrayElements values)
  parseAnnotationValue desc = jerror $ "Invalid or unsupported annotation value: " ++ show desc

  findTailCallApps : List String -> SExp -> List String
  findTailCallApps acc (SApp tailCall f _) = if tailCall then f :: acc else acc
  findTailCallApps acc (SLet _ v sc) =  acc ++ findTailCallApps [] v ++ findTailCallApps [] sc
  findTailCallApps acc (SUpdate _ e) = findTailCallApps acc e
  findTailCallApps acc (SCase _ _ alts) = acc ++ concat (findTailCallAppsCase <$> alts)
  findTailCallApps acc (SChkCase _ alts) = acc ++ concat (findTailCallAppsCase <$> alts)
  findTailCallApps acc _ = acc

  findTailCallAppsCase : SAlt -> List String
  findTailCallAppsCase (SConstCase _ e)     = findTailCallApps [] e
  findTailCallAppsCase (SDefaultCase e)     = findTailCallApps [] e
  findTailCallAppsCase (SConCase _ _ _ _ e) = findTailCallApps [] e
