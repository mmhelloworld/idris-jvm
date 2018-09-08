module IdrisJvm.Core.Foreign

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.IR.Types
import Data.SortedMap

%access public export

data JForeign = JStatic String String
              | JGetStaticField String String
              | JSetStaticField String String
              | JVirtual String String
              | JGetInstanceField String String
              | JSetInstanceField String String
              | JInterface String String
              | JNew String
              | JNewArray
              | JMultiNewArray
              | JSetArray
              | JGetArray
              | JArrayLength
              | JClassLiteral String
              | JLambda String String
              | JInstanceOf String

javaToIdris : TypeDescriptor -> Asm ()
javaToIdris (FieldDescriptor FieldTyDescBoolean) = pure ()
javaToIdris (FieldDescriptor FieldTyDescByte) = pure ()
javaToIdris (FieldDescriptor FieldTyDescShort) = pure ()
javaToIdris (FieldDescriptor FieldTyDescInt)     = pure ()
javaToIdris (FieldDescriptor FieldTyDescChar)    = pure ()
javaToIdris (FieldDescriptor FieldTyDescLong)    = pure ()
javaToIdris (FieldDescriptor FieldTyDescFloat)   = pure ()
javaToIdris (FieldDescriptor FieldTyDescDouble)  = pure ()
javaToIdris VoidDescriptor                       = Aconstnull
javaToIdris _                                    = pure ()

javaReturn : TypeDescriptor -> Asm ()
javaReturn VoidDescriptor                       = Return
javaReturn (FieldDescriptor FieldTyDescBoolean) = Ireturn
javaReturn (FieldDescriptor FieldTyDescByte)    = Ireturn
javaReturn (FieldDescriptor FieldTyDescShort)   = Ireturn
javaReturn (FieldDescriptor FieldTyDescInt)     = Ireturn
javaReturn (FieldDescriptor FieldTyDescChar)    = Ireturn
javaReturn (FieldDescriptor FieldTyDescLong)    = Lreturn
javaReturn (FieldDescriptor FieldTyDescFloat)   = Freturn
javaReturn (FieldDescriptor FieldTyDescDouble)  = Dreturn
javaReturn _                                    = Areturn

mutual
  fdescFieldDescriptor : FDesc -> FieldTypeDescriptor
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntNative"]) = FieldTyDescInt
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntChar"]) = FieldTyDescChar
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits8"]) = FieldTyDescByte
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits16"]) = FieldTyDescShort
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits32"]) = FieldTyDescInt
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits64"]) = FieldTyDescLong
  fdescFieldDescriptor (FCon "JVM_Float") = FieldTyDescFloat
  fdescFieldDescriptor (FCon "JVM_Double") = FieldTyDescDouble
  fdescFieldDescriptor (FCon "JVM_Bool") = FieldTyDescBoolean

  fdescFieldDescriptor (FCon "JVM_Str") = FieldTyDescReference $ ClassDesc "java/lang/String"
  fdescFieldDescriptor fdesc = FieldTyDescReference $ fdescRefDescriptor fdesc

  fdescRefDescriptor : FDesc -> ReferenceTypeDescriptor
  fdescRefDescriptor desc@(FApp "JVM_NativeT" [FApp "Class" [FStr typeName]]) = ClassDesc typeName
  fdescRefDescriptor desc@(FApp "JVM_NativeT" [FApp "Interface" [FStr typeName]]) = InterfaceDesc typeName

  fdescRefDescriptor desc@(FApp "JVM_ArrayT" [_, elemDesc]) = ArrayDesc $ fdescFieldDescriptor elemDesc

  fdescRefDescriptor desc@(FApp "JVM_Nullable" [FApp "Class" [FStr typeName]]) = NullableRefDesc typeName
  fdescRefDescriptor desc@(FApp "JVM_Nullable" [FApp "Interface" [FStr typeName]]) = NullableRefDesc typeName

  fdescRefDescriptor (FCon "JVM_Str") = ClassDesc "java/lang/String"
  fdescRefDescriptor (FStr exportedType) = IdrisExportDesc exportedType
  fdescRefDescriptor (FCon "JVM_NullableStr") = NullableStrDesc
  fdescRefDescriptor desc = jerror $ "Invalid reference type descriptor: " ++ show desc


fdescTypeDescriptor : FDesc -> TypeDescriptor
fdescTypeDescriptor (FCon "JVM_Unit") = VoidDescriptor
fdescTypeDescriptor (FIO t) = fdescTypeDescriptor t
fdescTypeDescriptor (FApp "JVM_ThrowableT" [_, desc]) = ThrowableDescriptor $ fdescTypeDescriptor desc
fdescTypeDescriptor fdesc = FieldDescriptor $ fdescFieldDescriptor fdesc

isExportIO : FDesc -> Bool
isExportIO (FIO _) = True
isExportIO _       = False

parseExportedClassName : String -> (ClassName, ClassName, List ClassName)
parseExportedClassName desc
= let parts = filter ((/=) "") . map trim . Strings.split (\c => c == ' ' || c == ',') $ desc
  in case parts of
       [c] => (c, "java/lang/Object", [])
       [c, "extends", p] => (c, p, [])
       (c :: "implements" :: intfs) => (c, "java/lang/Object", intfs)
       (c :: "extends" :: p :: "implements" :: intfs) => (c, p, intfs)
       _ => jerror $ "invalid class descriptor in export: " ++ desc

fieldTypeDescriptorToInferredType : FieldTypeDescriptor -> InferredType
fieldTypeDescriptorToInferredType FieldTyDescByte             = IByte
fieldTypeDescriptorToInferredType FieldTyDescChar             = IChar
fieldTypeDescriptorToInferredType FieldTyDescShort            = IShort
fieldTypeDescriptorToInferredType FieldTyDescBoolean          = IBool
fieldTypeDescriptorToInferredType FieldTyDescDouble           = IDouble
fieldTypeDescriptorToInferredType FieldTyDescFloat            = IFloat
fieldTypeDescriptorToInferredType FieldTyDescInt              = IInt
fieldTypeDescriptorToInferredType FieldTyDescLong             = ILong
fieldTypeDescriptorToInferredType (FieldTyDescReference (ArrayDesc elemTy))
    = IArray $ fieldTypeDescriptorToInferredType elemTy
fieldTypeDescriptorToInferredType (FieldTyDescReference refTy) = Ref (refTyClassName refTy)

typeDescriptorToInferredType : TypeDescriptor -> InferredType
typeDescriptorToInferredType (ThrowableDescriptor _) = inferredObjectType
typeDescriptorToInferredType VoidDescriptor          = IUnknown
typeDescriptorToInferredType (FieldDescriptor fieldTyDesc) = fieldTypeDescriptorToInferredType fieldTyDesc

loadJavaVar : Int -> FieldTypeDescriptor -> Asm ()
loadJavaVar index FieldTyDescBoolean = Iload index
loadJavaVar index FieldTyDescByte    = Iload index
loadJavaVar index FieldTyDescShort   = Iload index
loadJavaVar index FieldTyDescInt     = Iload index
loadJavaVar index FieldTyDescChar    = Iload index
loadJavaVar index FieldTyDescLong    = Lload index
loadJavaVar index FieldTyDescFloat   = Fload index
loadJavaVar index FieldTyDescDouble  = Dload index
loadJavaVar index (FieldTyDescReference (IdrisExportDesc cname)) = do
  Aload index
  InvokeMethod InvokeVirtual cname "getValue" "()Ljava/lang/Object;" False
  checkcast idrisObjectType
loadJavaVar index _                  = Aload index

storeJavaVar : Int -> FieldTypeDescriptor -> Asm ()
storeJavaVar index FieldTyDescBoolean = Istore index
storeJavaVar index FieldTyDescByte    = Istore index
storeJavaVar index FieldTyDescShort   = Istore index
storeJavaVar index FieldTyDescInt     = Istore index
storeJavaVar index FieldTyDescChar    = Istore index
storeJavaVar index FieldTyDescLong    = Lstore index
storeJavaVar index FieldTyDescFloat   = Fstore index
storeJavaVar index FieldTyDescDouble  = Dstore index
storeJavaVar index _                  = Astore index

idrisToJava : List (FieldTypeDescriptor, LVar) -> Asm ()
idrisToJava vars = do
    locTypes <- GetFunctionLocTypes
    sequence_ $ map (f locTypes) vars
  where
    f :  InferredTypeStore -> (FieldTypeDescriptor, LVar) -> Asm ()
    f locTypes (ty, v) = do
      let vTy = getLocTy locTypes v
      loadVar locTypes vTy (fieldTypeDescriptorToInferredType ty) v

idrisToJavaLoadArray : List (FieldTypeDescriptor, LVar) -> Asm ()
idrisToJavaLoadArray = sequence_ . intersperse Aaload . map f where
  f : (FieldTypeDescriptor, LVar) -> Asm ()
  f (ty, v) = do
    locTypes <- GetFunctionLocTypes
    let vTy = getLocTy locTypes v
    loadVar locTypes vTy (fieldTypeDescriptorToInferredType ty) v

parseDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
parseDescriptor returns ffi argsDesc = tryParseStaticMethodDescriptor returns ffi argsDesc where

  unsupportedDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  unsupportedDescriptor returns ffiDesc args =
      jerror $ "Unsupported descriptor: " ++ show ffiDesc ++ ", returns: " ++ show returns ++ ", args: " ++ show args

  tryParseInstanceOfDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseInstanceOfDescriptor returns (FApp "InstanceOf" [FStr cname]) argsDesc = JInstanceOf cname
  tryParseInstanceOfDescriptor returns ffi argsDesc = unsupportedDescriptor returns ffi argsDesc

  tryParseLambdaDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseLambdaDescriptor returns ffiDesc@(FApp ffi [FApp nativeTy [FStr cname], FStr interfaceMethodName]) argsDesc
    = if ffi == "Lambda" && nativeTy == "Interface"
        then JLambda cname interfaceMethodName
        else tryParseInstanceOfDescriptor returns ffiDesc argsDesc
  tryParseLambdaDescriptor returns ffi argsDesc = tryParseInstanceOfDescriptor returns ffi argsDesc

  tryParseClassLiteralDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseClassLiteralDescriptor returns ffiDesc@(FApp ffi [FStr cname]) argsDesc
    = if ffi == "ClassLiteral"
        then JClassLiteral cname
        else tryParseLambdaDescriptor returns ffiDesc argsDesc
  tryParseClassLiteralDescriptor returns ffi argsDesc = tryParseLambdaDescriptor returns ffi argsDesc

  tryParseArrayLengthDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseArrayLengthDescriptor returns (FCon "ArrayLength") argsDesc = JArrayLength
  tryParseArrayLengthDescriptor returns ffi argsDesc = tryParseClassLiteralDescriptor returns ffi argsDesc

  tryParseGetArrayDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseGetArrayDescriptor returns (FCon "GetArray") argsDesc = JGetArray
  tryParseGetArrayDescriptor returns ffi argsDesc = tryParseArrayLengthDescriptor returns ffi argsDesc

  tryParseSetArrayDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseSetArrayDescriptor returns (FCon "SetArray") argsDesc = JSetArray
  tryParseSetArrayDescriptor returns ffi argsDesc = tryParseGetArrayDescriptor returns ffi argsDesc

  tryParseMultiNewArrayDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseMultiNewArrayDescriptor returns (FCon "MultiNewArray") argsDesc = JMultiNewArray
  tryParseMultiNewArrayDescriptor returns ffi argsDesc = tryParseSetArrayDescriptor returns ffi argsDesc

  tryParseNewArrayDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseNewArrayDescriptor returns (FCon "NewArray") argsDesc = JNewArray
  tryParseNewArrayDescriptor returns ffi argsDesc = tryParseMultiNewArrayDescriptor returns ffi argsDesc

  tryParseConstructorDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseConstructorDescriptor returns ffiDesc@(FCon ffi) argsDesc
    = if ffi == "New" then
        case fdescTypeDescriptor returns of
            (FieldDescriptor (FieldTyDescReference (ClassDesc cname)))    => JNew cname
            (ThrowableDescriptor (FieldDescriptor (FieldTyDescReference (ClassDesc cname)))) => JNew cname
            (FieldDescriptor (FieldTyDescReference (InterfaceDesc _)))    =>
                jerror $ "Invalid FFI descriptor for constructor. A constructor can't return an interface type. "
            (FieldDescriptor (FieldTyDescReference (ArrayDesc _)))        => jerror "No constructors for array types"
            (FieldDescriptor (FieldTyDescReference (IdrisExportDesc _)))  =>
                jerror "Cannot invoke constructor of Idris exported type"
            (FieldDescriptor (FieldTyDescReference (NullableStrDesc)))    =>
                jerror "A constructor cannot return a nullable string"
            (FieldDescriptor (FieldTyDescReference (NullableRefDesc _)))  =>
                jerror "A constructor cannot return a nullable reference"
        else
            tryParseNewArrayDescriptor returns ffiDesc argsDesc
  tryParseConstructorDescriptor returns ffi argsDesc = tryParseNewArrayDescriptor returns ffi argsDesc

  tryParseSetInstanceFieldDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseSetInstanceFieldDescriptor returns ffiDesc@(FApp ffi [FStr fieldName]) argsDesc@((declClass, _)::_)
    = if ffi == "SetInstanceField"
        then case fdescRefDescriptor declClass of
                ClassDesc cname => JSetInstanceField cname fieldName
                _               => jerror "Fields can be set only from classes"
      else tryParseConstructorDescriptor returns ffiDesc argsDesc
  tryParseSetInstanceFieldDescriptor returns ffi argsDesc = tryParseConstructorDescriptor returns ffi argsDesc

  tryParseGetInstanceFieldDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseGetInstanceFieldDescriptor returns ffiDesc@(FApp ffi [FStr fieldName]) argsDesc@((declClass, _)::_)
    = if ffi == "GetInstanceField"
        then case fdescRefDescriptor declClass of
                ClassDesc cname => JGetInstanceField cname fieldName
                _               => jerror "Fields can be accessed only from classes"
        else tryParseSetInstanceFieldDescriptor returns ffiDesc argsDesc
  tryParseGetInstanceFieldDescriptor returns ffi argsDesc = tryParseSetInstanceFieldDescriptor returns ffi argsDesc

  tryParseInstanceMethodDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseInstanceMethodDescriptor returns ffiDesc@(FApp ffi [FStr fn]) argsDesc@((declClass, _)::_)
    = if ffi == "Instance"
        then case fdescRefDescriptor declClass of
                ClassDesc cname     => JVirtual cname fn
                InterfaceDesc cname => JInterface cname fn
                NullableStrDesc     => jerror "Instance method cannot have the first argument as nullable string"
                NullableRefDesc _   => jerror "Instance method cannot have the first argument as nullable reference"
                ArrayDesc _         => jerror "No instance methods on an array"
                IdrisExportDesc _   => jerror "No instance methods on an Idris exported type"
        else tryParseGetInstanceFieldDescriptor returns ffiDesc argsDesc
  tryParseInstanceMethodDescriptor returns ffi argsDesc = tryParseGetInstanceFieldDescriptor returns ffi argsDesc

  tryParseInstanceMethodNoArgDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseInstanceMethodNoArgDescriptor returns ffiDesc@(FApp ffi [FStr _]) []
    = if ffi == "Instance"
        then jerror "Instance methods should have atleast one argument"
        else tryParseInstanceMethodDescriptor returns ffiDesc []
  tryParseInstanceMethodNoArgDescriptor returns ffi argsDesc = tryParseInstanceMethodDescriptor returns ffi argsDesc

  tryParseSetStaticFieldDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseSetStaticFieldDescriptor returns ffiDesc@(FApp ffi [FApp nativeTy [FStr cname], FStr fieldName]) argsDesc
    = if ffi == "SetStaticField" && nativeTy == "Class"
        then JSetStaticField cname fieldName
        else tryParseInstanceMethodNoArgDescriptor returns ffiDesc argsDesc
  tryParseSetStaticFieldDescriptor returns ffi argsDesc = tryParseInstanceMethodNoArgDescriptor returns ffi argsDesc

  tryParseStaticFieldDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseStaticFieldDescriptor returns ffiDesc@(FApp ffi [FApp nativeTy [FStr cname], FStr fieldName]) argsDesc
    = if ffi == "GetStaticField" && nativeTy == "Class"
        then JGetStaticField cname fieldName
        else tryParseSetStaticFieldDescriptor returns ffiDesc argsDesc
  tryParseStaticFieldDescriptor returns ffi argsDesc = tryParseSetStaticFieldDescriptor returns ffi argsDesc

  tryParseStaticMethodDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  tryParseStaticMethodDescriptor returns ffiDesc@(FApp ffi [FApp nativeTy [FStr cname], FStr fn]) argsDesc
    = if ffi == "Static"
        then JStatic cname fn
        else tryParseStaticFieldDescriptor returns ffiDesc argsDesc
  tryParseStaticMethodDescriptor returns ffi argsDesc = tryParseStaticFieldDescriptor returns ffi argsDesc

fdescFunctionArgDescriptor : FDesc -> List FieldTypeDescriptor
fdescFunctionArgDescriptor (FApp "JVM_FnBase" [_,ty]) = []
fdescFunctionArgDescriptor (FApp "JVM_FnIO" [_,ty]) = []
fdescFunctionArgDescriptor (FApp "JVM_FnT" [_,ty]) = fdescFunctionArgDescriptor ty
fdescFunctionArgDescriptor (FApp c [_,_, FCon "JVM_Unit",fn]) = []
fdescFunctionArgDescriptor (FApp "JVM_Fn" [_,_, ty,fn]) = fdescFieldDescriptor ty :: fdescFunctionArgDescriptor fn
fdescFunctionArgDescriptor _ = []

fdescFunctionRetDescriptor : FDesc -> TypeDescriptor
fdescFunctionRetDescriptor (FApp "JVM_FnBase" [_,ty]) = fdescTypeDescriptor ty
fdescFunctionRetDescriptor (FApp "JVM_FnIO" [_,ty]) = fdescTypeDescriptor ty
fdescFunctionRetDescriptor (FApp "JVM_FnT" [_,ty]) = fdescFunctionRetDescriptor ty
fdescFunctionRetDescriptor (FApp "JVM_Fn" [_,_,ty,fn]) = fdescFunctionRetDescriptor fn
fdescFunctionRetDescriptor desc = fdescTypeDescriptor desc

anewarray : FieldTypeDescriptor -> Asm ()
anewarray FieldTyDescByte          = Anewbytearray
anewarray FieldTyDescChar          = Anewchararray
anewarray FieldTyDescShort         = Anewshortarray
anewarray FieldTyDescBoolean       = Anewbooleanarray
anewarray FieldTyDescDouble        = Anewdoublearray
anewarray FieldTyDescFloat         = Anewfloatarray
anewarray FieldTyDescInt           = Anewintarray
anewarray FieldTyDescLong          = Anewlongarray
anewarray (FieldTyDescReference f) = Anewarray $ asmRefTyDesc f

arrayStore : FieldTypeDescriptor -> Asm ()
arrayStore FieldTyDescByte = Bastore
arrayStore FieldTyDescChar = Castore
arrayStore FieldTyDescShort = Sastore
arrayStore FieldTyDescBoolean = Bastore
arrayStore FieldTyDescDouble = Dastore
arrayStore FieldTyDescFloat = Fastore
arrayStore FieldTyDescInt = Iastore
arrayStore FieldTyDescLong = Lastore
arrayStore (FieldTyDescReference ReferenceTypeDescriptor) = Aastore

arrayLoad : FieldTypeDescriptor -> Asm ()
arrayLoad FieldTyDescByte                                = Baload
arrayLoad FieldTyDescChar                                = Caload
arrayLoad FieldTyDescShort                               = Saload
arrayLoad FieldTyDescBoolean                             = Baload
arrayLoad FieldTyDescDouble                              = Daload
arrayLoad FieldTyDescFloat                               = Faload
arrayLoad FieldTyDescInt                                 = Iaload
arrayLoad FieldTyDescLong                                = Laload
arrayLoad (FieldTyDescReference ReferenceTypeDescriptor) = Aaload

typeDescToarrayElemDesc : TypeDescriptor -> FieldTypeDescriptor
typeDescToarrayElemDesc VoidDescriptor = jerror $ "An array cannot have 'void' elements"
typeDescToarrayElemDesc (FieldDescriptor desc) = desc

arrayDesc : String -> Nat -> String
arrayDesc cname dimensions =
  let arrayPrefix = cast $ replicate dimensions '['
  in arrayPrefix ++ cname

invokeDynamic : ClassName -> MethodName -> MethodName -> String -> String -> String -> String -> Asm ()
invokeDynamic implClassName implMethodName interfaceMethodName invokeDynamicDesc samDesc implMethodDesc instantiatedMethodDesc =
  let metafactoryHandle = MkHandle HInvokeStatic "java/lang/invoke/LambdaMetafactory" "metafactory" metafactoryDesc False
      implMethodHandle = MkHandle HInvokeStatic implClassName implMethodName implMethodDesc False
      metafactoryArgs = [ BsmArgGetType samDesc
                        , BsmArgHandle implMethodHandle
                        , BsmArgGetType instantiatedMethodDesc
                        ]
  in InvokeDynamic interfaceMethodName invokeDynamicDesc metafactoryHandle metafactoryArgs

createLambdaForThunk : JMethodName -> JMethodName -> List LVar -> (MethodName -> Asm ()) -> Asm ()
createLambdaForThunk caller targetFname args lambdaCode = do
  let nArgs = length args
  locTypes <- GetFunctionLocTypes
  (_, targetLocTys) <- getFunctionType targetFname
  let cname = jmethClsName caller
  lambdaIndex <- FreshLambdaIndex cname
  let implMethodName = sep "$" ["lambda", jmethName caller, show lambdaIndex]
  loadVars locTypes targetLocTys args
  let targetArgTys = if nArgs > 0 then getLocTy targetLocTys <$> (Loc . cast <$> [ 0 .. Nat.pred nArgs]) else []
  let invokeDynamicDesc = getInferredFunDesc targetArgTys (Ref thunkClass)
  let implMethodDesc = getInferredFunDesc targetArgTys (Ref "java/lang/Object")
  let samDesc = "()" ++ classSig "java/lang/Object"
  let instantiatedMethodDesc = samDesc
  invokeDynamic cname implMethodName "call" invokeDynamicDesc samDesc implMethodDesc instantiatedMethodDesc
  lambdaCode implMethodName

createLambdaForJavaFunction : JMethodName
                           -> JMethodName
                           -> List FieldTypeDescriptor
                           -> TypeDescriptor
                           -> List FieldTypeDescriptor
                           -> TypeDescriptor
                           -> (MethodName -> Asm ())
                           -> Asm ()
createLambdaForJavaFunction caller (MkJMethodName interfaceName interfaceMethodName) interfaceMethodArgs interfaceMethodReturns implMethodArgs implMethodReturns lambdaCode = do
  let cname = jmethClsName caller
  lambdaIndex <- FreshLambdaIndex cname
  let implMethodName = sep "$" ["lambda", jmethName caller, show lambdaIndex]
  -- takes idris function as input, returns an instance of Java interface
  let invokeDynamicDesc = "(Ljava/lang/Object;)" ++ classSig interfaceName
  let samDesc = asmMethodDesc $ MkMethodDescriptor interfaceMethodArgs interfaceMethodReturns
  let objectDesc = FieldTyDescReference $ ClassDesc "java/lang/Object"
  let implMethodDesc = asmMethodDesc $
    -- first objectDesc is for idris function capture
    MkMethodDescriptor (objectDesc :: implMethodArgs) implMethodReturns
  let instantiatedMethodDesc = asmMethodDesc $ MkMethodDescriptor implMethodArgs implMethodReturns
  invokeDynamic cname implMethodName interfaceMethodName invokeDynamicDesc samDesc implMethodDesc instantiatedMethodDesc
  lambdaCode implMethodName

createWrapperForThunkLambda : JMethodName -> ClassName -> MethodName -> List LVar -> Asm ()
createWrapperForThunkLambda fname@(MkJMethodName cname mname) callerCname lambdaMethodName args = do
  (targetRetTy, targetLocTys) <- getFunctionType fname
  let nArgs = the Int $ cast $ length args
  let argNums = if nArgs > 0 then [ 0 .. pred nArgs] else []
  let syntheticMethodArgs = Loc <$> argNums
  let targetArgTys = getLocTy targetLocTys <$> syntheticMethodArgs
  let syntheticMethodArgTys = SortedMap.fromList $ List.zip argNums targetArgTys
  let implMethodDesc = getInferredFunDesc targetArgTys (Ref "java/lang/Object")
  let targetMethodDesc = getInferredFunDesc targetArgTys targetRetTy
  CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName implMethodDesc Nothing Nothing [] []
  MethodCodeStart
  loadVars syntheticMethodArgTys targetLocTys syntheticMethodArgs
  InvokeMethod InvokeStatic cname mname targetMethodDesc False
  box targetRetTy
  Areturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

createExceptionLambdaWrapper : ClassName -> MethodName -> Nat -> Asm () -> Asm ()
createExceptionLambdaWrapper callerCname lambdaMethodName nArgs body = do
  let desc = sig nArgs
  CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName desc Nothing Nothing [] []
  MethodCodeStart
  body
  Areturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

createWrapperForJavaLambda : ClassName -> MethodName -> List FieldTypeDescriptor -> TypeDescriptor -> Asm ()
createWrapperForJavaLambda callerCname lambdaMethodName argDescs returnDesc = do
    let descriptor = asmMethodDesc $ MkMethodDescriptor locDescs returnDesc
    CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName descriptor Nothing Nothing [] []
    MethodCodeStart
    when (isNil argDescs) $ do
      Aconstnull
      Dup
    (applyRetTy, applyLocTys) <- applyFnType
    let applyArgTys = take 2 (values applyLocTys)
    let (applyArg0 :: _ :: _) = applyArgTys
    loadVar locTys inferredObjectType applyArg0 (Loc 0)
    applyArgs 1 argDescs
    if (isThunk applyRetTy || isObject applyRetTy) then do
        InvokeMethod InvokeStatic (rtClass "Runtime") "unwrap" (sig 1) False
        cgCast inferredObjectType (typeDescriptorToInferredType returnDesc)
    else
        cgCast applyRetTy (typeDescriptorToInferredType returnDesc)
    javaReturn returnDesc
    MaxStackAndLocal (-1) (-1)
    MethodCodeEnd
  where
    idrisApplyMethod : JMethodName
    idrisApplyMethod = jname "{APPLY_0}"

    locDescs : List FieldTypeDescriptor
    locDescs = (FieldTyDescReference $ ClassDesc "java/lang/Object") :: argDescs

    locTys : InferredTypeStore
    locTys = SortedMap.fromList $ List.zip argVars $ fieldTypeDescriptorToInferredType <$> locDescs where
        argVars : List Int
        argVars = [0 .. (pred . cast $ length locDescs)]

    applyFnClassName : String
    applyFnClassName = jmethClsName idrisApplyMethod

    applyFnMethodName : String
    applyFnMethodName = jmethName idrisApplyMethod

    applyFnType : Asm (InferredType, InferredTypeStore)
    applyFnType = getFunctionType idrisApplyMethod

    callIoType : Asm (InferredType, InferredTypeStore)
    callIoType = getFunctionType (jname "call__IO")

    applyArgs : Nat -> List FieldTypeDescriptor -> Asm ()
    applyArgs (S Z) [] = do
        (callIoRetTy, callIoLocTys) <- callIoType
        let callIoArgTys = take 3 $ (values callIoLocTys)
        let callIoDesc = getInferredFunDesc callIoArgTys callIoRetTy
        InvokeMethod InvokeStatic "main/Main" "call__IO" callIoDesc False
    applyArgs index (argDesc :: argDescs) = do
      (applyRetTy, applyArgVarAndTys) <- applyFnType
      let applyLocTys = SortedMap.values applyArgVarAndTys
      let applyArgTys = take 2 applyLocTys
      let (_ :: applyArg1 :: _) = applyArgTys
      loadVar locTys (fieldTypeDescriptorToInferredType argDesc) applyArg1 (Loc $ cast index)
      let applyFnDesc = getInferredFunDesc applyArgTys applyRetTy
      InvokeMethod InvokeStatic applyFnClassName applyFnMethodName applyFnDesc False
      applyArgs (index + 1) argDescs
    applyArgs _ _ = pure ()

createThunk : JMethodName -> JMethodName -> List LVar -> Asm ()
createThunk caller@(MkJMethodName callerCname _) fname args = do
  let lambdaCode =
    \lambdaMethodName =>
      Subroutine $ createWrapperForThunkLambda fname callerCname lambdaMethodName args
  createLambdaForThunk caller fname args lambdaCode

createExceptionHandlerThunk : JMethodName -> JMethodName -> List LVar -> Asm () -> Asm ()
createExceptionHandlerThunk caller@(MkJMethodName callerCname _) targetFname args body = do
  let nArgs = List.length args
  let lambdaCode = \lambdaMethodName => Subroutine $ createExceptionLambdaWrapper callerCname lambdaMethodName nArgs body
  createLambdaForThunk caller targetFname args lambdaCode

createJvmFunctionalObject : LVar -> JMethodName -> JMethodName -> FDesc -> FDesc -> Asm ()
createJvmFunctionalObject idrisFunction caller@(MkJMethodName callerCname _) interfaceMethod interfaceFnTy lambdaTy  = do
  let implMethodArgDescs = fdescFunctionArgDescriptor lambdaTy
  let interfaceMethodArgDescs = fdescFunctionArgDescriptor interfaceFnTy
  let implMethodReturnDesc = fdescFunctionRetDescriptor lambdaTy
  let interfaceMethodReturnDesc = fdescFunctionRetDescriptor interfaceFnTy
  let lambdaCode = \implMethodName => Subroutine $ createWrapperForJavaLambda callerCname implMethodName implMethodArgDescs implMethodReturnDesc
  Aload $ locIndex idrisFunction
  createLambdaForJavaFunction caller interfaceMethod interfaceMethodArgDescs interfaceMethodReturnDesc implMethodArgDescs implMethodReturnDesc lambdaCode

createWrapperForThunkParLambda : JMethodName -> ClassName -> MethodName -> Nat -> Asm ()
createWrapperForThunkParLambda fname@(MkJMethodName cname mname) callerCname lambdaMethodName nArgs = do
  (targetRetTy, targetLocTys) <- getFunctionType fname

  let argNums = the (List Int) $ if nArgs > 0 then [0 .. Prelude.pred $ cast nArgs] else []
  let syntheticMethodArgs = Loc <$> argNums
  let targetArgTys = getLocTy targetLocTys <$> syntheticMethodArgs
  let syntheticMethodArgTys = SortedMap.fromList $ List.zip argNums targetArgTys
  let implMethodDesc = getInferredFunDesc targetArgTys (Ref "java/lang/Object")
  let targetMethodDesc = getInferredFunDesc targetArgTys targetRetTy
  CreateMethod [Private, Static, Synthetic] callerCname lambdaMethodName implMethodDesc Nothing Nothing [] []
  MethodCodeStart
  loadVars syntheticMethodArgTys targetLocTys syntheticMethodArgs
  InvokeMethod InvokeStatic cname mname targetMethodDesc False -- invoke the target method
  box targetRetTy
  Astore 1
  Aload 1
  InstanceOf idrisObjectType
  CreateLabel "elseLabel"
  Ifeq "elseLabel"
  Aload 1
  cgCast targetRetTy inferredIdrisObjectType
  InvokeMethod InvokeStatic cname mname targetMethodDesc False
  box targetRetTy
  Areturn
  LabelStart "elseLabel"
  Frame FAppend 1 ["java/lang/Object"] 0 []
  Aload 1
  Areturn
  MaxStackAndLocal (-1) (-1)
  MethodCodeEnd

createParThunk : JMethodName -> JMethodName -> List LVar -> Asm ()
createParThunk caller@(MkJMethodName callerCname _) fname args = do
  let nArgs = List.length args
  let lambdaCode = \lambdaMethodName => Subroutine $ createWrapperForThunkParLambda fname callerCname lambdaMethodName nArgs
  createLambdaForThunk caller fname args lambdaCode
