module IdrisJvm.Core.Function

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.Core.Constant
import IdrisJvm.Core.ControlFlow
import IdrisJvm.Core.Foreign
import IdrisJvm.Core.Operator
import IdrisJvm.IR.Types

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

assignLocal : Nat -> LVar -> Asm ()
assignLocal toIndex (Loc fromIndex) = assign fromIndex (cast toIndex)
assignLocal _ _ = jerror "Unexpected global variable"

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
  New idrisObjectType
  Dup
  Iconst constructorId
  sequence_ $ Aload . locIndex <$> take unrolledConstructorPropsCount args
  createUnrolledProperties
  InvokeMethod InvokeSpecial idrisObjectType "<init>" constructorSig False
where
  argsLength : Int
  argsLength = cast $ length args

  unrolledProps : List LVar
  unrolledProps = drop unrolledConstructorPropsCount args

  unrolledPropsLength : Int
  unrolledPropsLength = cast $ length unrolledProps

  constructorSig : String
  constructorSig =
    if unrolledPropsLength == 0 then
      "(I" ++ repeatObjectDesc (cast argsLength) ++ ")V"
    else
      "(I" ++ repeatObjectDesc unrolledConstructorPropsCount ++ "[Ljava/lang/Object;)V"

  ins : Int -> LVar -> Asm ()
  ins idx (Loc varIndex) = do
    Dup
    Iconst idx
    Aload varIndex
    Aastore
  ins _ _ = jerror "Unexpected global variable"

  storeUnrolledProperties : Asm ()
  storeUnrolledProperties =
    sequence_ $ List.zipWith ins [0 .. (unrolledPropsLength - 1)] unrolledProps

  createUnrolledProperties : Asm ()
  createUnrolledProperties =
    if unrolledPropsLength == 0 then
      Pure ()
    else do
      Iconst unrolledPropsLength
      Anewarray "java/lang/Object"
      storeUnrolledProperties

hasConstructorExport : List Export -> Bool
hasConstructorExport = any isConstructorExport where
    isConstructorExport (ExportFun _ (FCon "Constructor") _ _) = True
    isConstructorExport _                                      = False

tailRecStartLabelName : String
tailRecStartLabelName = "$tailRecStartLabel"

tailRecEndLabelName : String
tailRecEndLabelName = "$tailRecEndLabel"

assignNull : Int -> Asm ()
assignNull varIndex = do Aconstnull; Astore varIndex

mutual

  cgBody : Lazy (Asm ()) -> SExp -> Asm ()
  cgBody ret (SV (Glob n)) = do
    let MkJMethodName cname mname = jname n
    InvokeMethod InvokeStatic cname mname (sig 0) False
    ret
  cgBody ret (SV (Loc i)) = do Aload i; ret

  cgBody ret (SApp True f args) = do
    caller <- GetFunctionName
    if jname f == caller -- self tail call, use goto
       then sequence_ $ zipWith assignLocal [0 .. (length args)] args
       else do createThunk caller (jname f) args; ret -- non-self tail call

  cgBody ret (SApp False f args) = do
    sequence_ $ (Aload . locIndex) <$> args
    let MkJMethodName cname mname = jname f
    InvokeMethod InvokeStatic cname mname (sig $ length  args) False
    InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" "(Ljava/lang/Object;)Ljava/lang/Object;" False
    ret

  cgBody ret (SLet (Loc i) v sc) = do cgBody (Astore i) v; cgBody ret sc

  cgBody ret (SUpdate _ e) = cgBody ret e

  cgBody ret (SProj (Loc v) i) = do
    idrisObjectProperty v i
    ret

  cgBody ret (SCon _ 1 "Prelude.Maybe.Just" [(Loc v)]) = do Aload v; ret
  cgBody ret (SCon _ 0 "Prelude.Maybe.Nothing" []) = do Aconstnull; ret

  cgBody ret (SCon _ t _ args) = do createIdrisObject t args; ret

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

  cgBody ret (SCase _ e alts) = cgSwitch ret cgBody e alts

  cgBody ret (SChkCase e alts) = cgSwitch ret cgBody e alts

  cgBody ret (SConst c) = do cgConst c; ret

  cgBody ret (SOp op args) = do cgOp op args; ret

  cgBody ret SNothing = do Iconst 0; boxInt; ret

  cgBody ret (SError x) = do invokeError (show x); ret

  cgBody ret (SForeign returns fdesc args) = cgForeign (parseDescriptor returns fdesc args) where

    argsWithTypes : List (FieldTypeDescriptor, LVar)
    argsWithTypes = (\(fdesc, lvar) => (fdescFieldDescriptor fdesc, lvar)) <$> args

    loadLambdaArgs : Nat -> List FieldTypeDescriptor -> Asm ()
    loadLambdaArgs _ [] = pure ()
    loadLambdaArgs index (ty :: types) = do
        Aload $ cast index
        idrisDescToJava (FieldDescriptor ty)
        loadLambdaArgs (S index) types

    cgForeign (JStatic clazz fn) = case fdescTypeDescriptor returns of
      ThrowableDescriptor returnDesc => do
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) returnDesc
        let lambdaBody = do
          loadLambdaArgs 0 (fst <$> argsWithTypes)
          InvokeMethod InvokeStatic clazz fn descriptor False
          javaToIdris returnDesc
        caller <- GetFunctionName
        createExceptionHandlerThunk caller argsWithTypes lambdaBody
        InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
        ret
      returnDesc => do
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) returnDesc
        idrisToJava argsWithTypes
        InvokeMethod InvokeStatic clazz fn descriptor False
        javaToIdris returnDesc
        ret

    cgForeign (JGetStaticField clazz fieldName) = do
      let returnDesc = fdescTypeDescriptor returns
      Field FGetStatic clazz fieldName (asmTypeDesc returnDesc)
      javaToIdris returnDesc
      ret

    cgForeign (JSetStaticField clazz fieldName) = case args of
      [ty] => do
        idrisToJava argsWithTypes
        let desc = asmFieldTypeDesc $ fdescFieldDescriptor . fst $ ty
        Field FPutStatic clazz fieldName desc
        javaToIdris (fdescTypeDescriptor returns)
        ret
      otherwise => jerror $ "There can be only one argument for setting a static field: " ++ clazz ++ "." ++ fieldName

    cgForeign (JVirtual clazz fn) = case fdescTypeDescriptor returns of
      ThrowableDescriptor returnDesc => do
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
        let lambdaBody = do
          loadLambdaArgs 0 (fst <$> argsWithTypes)
          InvokeMethod InvokeVirtual clazz fn descriptor False
          javaToIdris returnDesc
        caller <- GetFunctionName
        createExceptionHandlerThunk caller argsWithTypes lambdaBody
        InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
        ret
      returnDesc => do
          -- drop first arg type as it is an implicit 'this'
          let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
          idrisToJava argsWithTypes
          InvokeMethod InvokeVirtual clazz fn descriptor False
          javaToIdris returnDesc
          ret

    cgForeign (JGetInstanceField clazz fieldName) = do
      let returnDesc = fdescTypeDescriptor returns
      idrisToJava argsWithTypes
      Field FGetField clazz fieldName (asmTypeDesc returnDesc)
      javaToIdris returnDesc
      ret

    cgForeign (JSetInstanceField clazz fieldName) = case args of
      [_, arg] => do
        idrisToJava argsWithTypes
        let desc = asmTypeDesc . fdescTypeDescriptor . fst $ arg
        Field FPutField clazz fieldName desc
        javaToIdris (fdescTypeDescriptor returns)
        ret
      otherwise => jerror "Setting an instance field must take 2 arguments: the instance and the field value"

    cgForeign (JInterface clazz fn) = case fdescTypeDescriptor returns of
        ThrowableDescriptor returnDesc => do
          let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
          let lambdaBody = do
            loadLambdaArgs 0 (fst <$> argsWithTypes)
            InvokeMethod InvokeInterface clazz fn descriptor True
            javaToIdris returnDesc
          caller <- GetFunctionName
          createExceptionHandlerThunk caller argsWithTypes lambdaBody
          InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
          ret
        returnDesc => do
          let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> drop 1 args) returnDesc
          idrisToJava argsWithTypes
          InvokeMethod InvokeInterface clazz fn descriptor True
          javaToIdris returnDesc
          ret

    cgForeign (JNew clazz) = case fdescTypeDescriptor returns of
      ThrowableDescriptor returnDesc => do
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) VoidDescriptor
        let lambdaBody = do
          New clazz
          Dup
          loadLambdaArgs 0 (fst <$> argsWithTypes)
          InvokeMethod InvokeSpecial clazz "<init>" descriptor False
        caller <- GetFunctionName
        createExceptionHandlerThunk caller argsWithTypes lambdaBody
        InvokeMethod InvokeStatic utilClass "throwable" ("(" ++ rtThunkSig ++ ")Ljava/lang/Object;") False
        ret
      returnDesc => do
        let descriptor = asmMethodDesc $ MkMethodDescriptor (fdescFieldDescriptor . fst <$> args) VoidDescriptor
        New clazz
        Dup
        idrisToJava argsWithTypes
        InvokeMethod InvokeSpecial clazz "<init>" descriptor False
        ret

    cgForeign JNewArray = case args of
      [ty] => do
        idrisToJava argsWithTypes
        let arrDesc = fdescFieldDescriptor returns
        let elemDesc = case arrDesc of
          FieldTyDescReference (ArrayDesc elem) => elem
          otherwise => jerror $ "Invalid return type while creating an array" ++ show returns
        anewarray elemDesc
        ret
      otherwise => jerror $ "There can be only one argument (length) to create an array" ++ show args

    cgForeign JMultiNewArray = do
        idrisToJava argsWithTypes
        let arrayType = refTyClassName $ fdescRefDescriptor returns
        Multianewarray arrayType (List.length args)
        ret

    cgForeign JSetArray = case args of
      ((arrFDesc, arr) :: rest@(arg1 :: arg2 :: args)) => do
        let value = last rest
        let indices = init rest
        let valueDesc = fdescFieldDescriptor (fst value)
        Aload $ locIndex arr
        Checkcast $ refTyClassName (fdescRefDescriptor arrFDesc)
        idrisToJavaLoadArray $ (\(fdesc, lvar) => (fdescFieldDescriptor fdesc, lvar)) <$> indices
        idrisToJava [(valueDesc, snd value)]
        arrayStore valueDesc
        javaToIdris (fdescTypeDescriptor returns)
        ret
      otherwise => jerror $ "Invalid arguments while trying to set an element inside array: " ++ show args

    cgForeign JGetArray = case args of
      ((arrFDesc, arr) :: indices@(index :: restIndices)) => do
        Aload $ locIndex arr
        Checkcast $ refTyClassName (fdescRefDescriptor arrFDesc)
        idrisToJavaLoadArray $ (\(fdesc, lvar) => (fdescFieldDescriptor fdesc, lvar)) <$> indices
        let returnDesc = fdescTypeDescriptor returns
        arrayLoad $ typeDescToarrayElemDesc returnDesc
        javaToIdris returnDesc
        ret
      otherwise => jerror $ "Invalid arguments while trying to get an element from array: " ++ show args

    cgForeign JArrayLength = case args of
      [(arrFDesc, arr)] => do
        Aload $ locIndex arr
        Checkcast $ refTyClassName (fdescRefDescriptor arrFDesc)
        Arraylength
        javaToIdris (fdescTypeDescriptor returns)
        ret
      otherwise => jerror $ "Invalid arguments while trying to get length of an array: " ++ show args

    cgForeign (JInstanceOf className) = case args of
      [ty] => do
          idrisToJava argsWithTypes
          InstanceOf className
          javaToIdris (fdescTypeDescriptor returns)
          ret

    cgForeign (JClassLiteral "int") = do getPrimitiveClass "java/lang/Integer"; ret
    cgForeign (JClassLiteral "byte") = do getPrimitiveClass "java/lang/Byte"; ret
    cgForeign (JClassLiteral "char") = do getPrimitiveClass "java/lang/Character"; ret
    cgForeign (JClassLiteral "short") = do getPrimitiveClass "java/lang/Short"; ret
    cgForeign (JClassLiteral "boolean") = do getPrimitiveClass "java/lang/Boolean"; ret
    cgForeign (JClassLiteral "long") = do getPrimitiveClass "java/lang/Long"; ret
    cgForeign (JClassLiteral "float") = do getPrimitiveClass "java/lang/Float"; ret
    cgForeign (JClassLiteral "double") = do getPrimitiveClass "java/lang/Double"; ret
    cgForeign (JClassLiteral clazz) = do
      Ldc $ TypeConst $ classSig clazz
      ret

    cgForeign (JLambda clazz interfaceFnName) = case args of
      [(interfaceFnTy, _), (lambdaTy, lambdaVar)] => do
        caller <- GetFunctionName
        createJvmFunctionalObject lambdaVar caller (MkJMethodName clazz interfaceFnName) interfaceFnTy lambdaTy
        ret
      otherwise => jerror $ "There can be only two arguments while passing a java lambda for " ++ clazz ++
        "." ++ interfaceFnName ++ ": the interface method signature and the lambda signature"

  cgBody _ _ = jerror "NOT IMPLEMENTED!!!!"

  exportCode : ExportIFace -> Asm ()
  exportCode (MkExportIFace "IdrisJvm.FFI.FFI_JVM" exportedClassName exportItems) = do
      let (cls, parent, intf) = parseExportedClassName exportedClassName
      let (classAnns, otherExports) = parseClassAnnotations exportItems
      CreateClass ComputeMaxs
      ClassCodeStart 52 Public cls Nothing parent intf classAnns
      SourceInfo "IdrisGenerated.idr"
      when (not $ hasConstructorExport exportItems) $ defaultConstructor cls parent
      sequence_ $ (cgExport cls parent) <$> otherExports
  exportCode e = jerror $ "Unsupported Export: " ++ show e

  cgExport : ClassName -> ClassName -> Export -> Asm ()
  cgExport _ _ (ExportData (FStr exportedType)) = createClassForIdrisType exportedType

  cgExport cname _ (ExportFun _ (FApp "ExportInstanceField" [FStr fieldName]) typeDesc _)
    = createField [Public] cname fieldName typeDesc

  cgExport cname _ (ExportFun _ (FApp "ExportStaticField" [FStr fieldName]) typeDesc _)
    = createField [Public, Static] cname fieldName typeDesc

  cgExport cname parent (ExportFun n (FApp "Super" [FStr superMethodName]) returns args)
    = exportFun cname (jmethName (jname n)) parent superMethodName ExportCallSuper parent returns args [] []

  cgExport cname parent (ExportFun n (FApp "ExportStatic" [FStr mname]) returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun cname mname sourceCname sourceMname ExportCallStatic parent returns args [] []

  cgExport cname parent (ExportFun n (FCon "ExportDefault") returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun cname sourceMname sourceCname sourceMname ExportCallInstance parent returns args [] []

  cgExport cname parent (ExportFun n (FCon "Constructor") returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun cname "<init>" sourceCname sourceMname ExportCallConstructor parent returns args [] []

  cgExport cname parent (ExportFun n (FApp "ExportInstance" [FStr mname]) returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
      in exportFun cname mname sourceCname sourceMname ExportCallInstance parent returns args [] []

  cgExport cname parent (ExportFun n (FApp "ExportInstanceWithAnn" [FStr mname, FApp ":" annDescs]) returns args)
    = let MkJMethodName sourceCname sourceMname = jname n
          anns = join $ parseAnnotations <$> annDescs
      in exportFun cname mname sourceCname sourceMname ExportCallInstance parent returns args anns []

  cgExport _ _ exportDef = jerror $ "Unsupported export definition: " ++ show exportDef

  parseClassAnnotations : List Export -> (List Annotation, List Export)
  parseClassAnnotations exports@(ExportFun n (FApp "Anns" desc) _ _:: rest) =
    if jmethName (jname n) == "classWith" then (join $ parseAnnotations <$> desc, rest)
    else ([], exports)
  parseClassAnnotations nonClassAnnotations = ([], nonClassAnnotations)

  exportFun : ClassName
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
  exportFun targetCname targetMethName sourceCname sourceMname exportCall parent returns args anns paramAnns = do
    let isStatic = exportCall == ExportCallStatic
    let accessMods = if isStatic then [Public,Static] else [Public]
    let argStartIndex = if isStatic then 0 else 1 -- drop `this` for instance methods
    let isSuperExport = exportCall == ExportCallSuper
    let isConstructor = exportCall == ExportCallConstructor
    let argDescs = fdescFieldDescriptor <$> (if isStatic then args else drop 1 args) -- for instance method, drop `this`
    let returnDesc = if isConstructor then VoidDescriptor else fdescTypeDescriptor returns
    let mdesc = asmMethodDesc $ MkMethodDescriptor argDescs returnDesc
    let f = \desc, index => do loadJavaVar index desc; javaToIdris $ FieldDescriptor desc
    let loadArgs = sequence_ $ List.zipWith f argDescs (listRange argStartIndex (cast $ List.length argDescs))
    let invType = if isSuperExport then InvokeSpecial else InvokeStatic
    let sourceMdesc = if isSuperExport
                          then asmMethodDesc $ MkMethodDescriptor argDescs returnDesc
                          else sig $ length args

    CreateMethod accessMods targetCname targetMethName mdesc Nothing Nothing anns paramAnns
    MethodCodeStart
    when (not isSuperExport && isExportIO returns) $ do
      Aconstnull -- Setup the 2 null args for "call__IO"
      Dup
    when isConstructor $ do -- Call matching super Constructor
      Aload 0
      loadArgs
      InvokeMethod InvokeSpecial parent "<init>" mdesc False
    when (not isStatic) $ Aload 0 -- load `this`
    loadArgs
    InvokeMethod invType sourceCname sourceMname sourceMdesc False
    when (not isSuperExport) $ InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" (sig 1) False
    when (not isSuperExport && isExportIO returns) unwrapExportedIO
    returnExport exportCall returnDesc

  cgFun : List Access -> String -> ClassName -> MethodName -> List String -> Int -> SExp -> Asm ()
  cgFun access idrisName clsName fname args locs def = do
      UpdateFunctionName $ MkJMethodName clsName fname
      UpdateShouldDescribeFrame True
      CreateMethod access clsName fname signature Nothing Nothing [] []
      MethodCodeStart
      UpdateLocalVarCount $ cast nLocalVars
      UpdateSwitchIndex 0 -- reset
      UpdateIfIndex 0 -- reset
      sequence_ $ map assignNull (the (List Int) $ resultVarIndex :: (listRange (cast nArgs) (nLocalVars - 1)))
      methBody
      Aload resultVarIndex
      Areturn
      MaxStackAndLocal (-1) (-1)
      MethodCodeEnd
    where
      nArgs : Nat
      nArgs = length args

      static : Bool
      static = elem Static access

      signature : String
      signature = if static then sig nArgs else sig (pred nArgs)

      nLocalVars : Int
      nLocalVars = cast nArgs + locs

      resultVarIndex : Int
      resultVarIndex = nLocalVars

      tailRecVarIndex : Int
      tailRecVarIndex = succ resultVarIndex

      totalVars : Int
      totalVars = nLocalVars + 2

      shouldEliminateTco : Bool
      shouldEliminateTco = any (== idrisName) (findTailCallApps [] def)

      ret : Lazy (Asm ())
      ret = if shouldEliminateTco
              then do Astore resultVarIndex
                      Iconst 0
                      Istore tailRecVarIndex -- Base case for tailrec. Set the tailrec flag to false.
              else Astore resultVarIndex

      tcoLocalVarTypes : List Signature
      tcoLocalVarTypes = List.(++) (replicate (cast nLocalVars + 1) "java/lang/Object") ["Opcodes.INTEGER"]

      tcoFrame : Asm ()
      tcoFrame = Frame FFull (cast totalVars) tcoLocalVarTypes 0 []

      methBody : Lazy (Asm ())
      methBody = if shouldEliminateTco
                    then do
                      Iconst 1
                      Istore tailRecVarIndex
                      CreateLabel tailRecStartLabelName
                      LabelStart tailRecStartLabelName
                      tcoFrame
                      Iload tailRecVarIndex
                      CreateLabel tailRecEndLabelName
                      Ifeq tailRecEndLabelName
                      UpdateShouldDescribeFrame False
                      cgBody ret def
                      Goto tailRecStartLabelName
                      LabelStart tailRecEndLabelName
                      Frame FSame 0 [] 0 []
                    else cgBody ret def

  returnExport : ExportCall -> TypeDescriptor -> Asm ()
  returnExport exportCall returnDesc = do
    when (exportCall /= ExportCallSuper) $ idrisDescToJava returnDesc
    javaReturn returnDesc
    MaxStackAndLocal (-1) (-1)
    MethodCodeEnd

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
  parseAnnotations (FApp ":" [FCon "Annotation", FApp "Ann" [FStr annTypeName, attr], rest])
    = let typeDesc = "L" ++ annTypeName ++ ";"
      in (MkAnnotation typeDesc (parseAnnotationAttr attr)) :: parseAnnotations rest

  parseAnnotations (FApp "Ann" [FStr annTypeName, attr])
    = let typeDesc = "L" ++ annTypeName ++ ";"
      in MkAnnotation typeDesc (parseAnnotationAttr attr) :: []

  parseAnnotations (FApp "Nil" [FCon "Annotation"]) = []
  parseAnnotations (FCon "Annotation") = []
  parseAnnotations desc = jerror $ "parseAnnotation: not implemented: " ++ show desc

  parseAnnotationAttr : FDesc -> List AnnotationProperty
  parseAnnotationAttr (FApp ":" [ FApp "Pair" [ FUnknown, FCon "AnnotationValue" ]
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

  parseAnnotationValue : FDesc -> AnnotationValue
  parseAnnotationValue (FApp "AnnString" [ FStr value]) = AnnString value
  parseAnnotationValue desc = jerror $ "Annotation value not yet supported: " ++ show desc

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

