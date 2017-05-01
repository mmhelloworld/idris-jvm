module IdrisJvm.Core.Foreign

import IdrisJvm.Core.Asm
import IdrisJvm.Core.Common
import IdrisJvm.IR.Types

%access public export

data JForeign = JStatic String String
              | JGetStaticField String String
              | JSetStaticField String String
              | JVirtual String String
              | JGetInstanceField String String
              | JSetInstanceField String String
              | JInterface String String
              | JNew String
              | JClassLiteral String

mutual
  parseDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
  parseDescriptor returns ffi argsDesc = tryParseStaticMethodDescriptor returns ffi argsDesc where

    unsupportedDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
    unsupportedDescriptor _ fdesc _ = jerror $ "Unsupported descriptor"

    tryParseClassLiteralDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
    tryParseClassLiteralDescriptor returns ffiDesc@(FApp ffi [FStr cname]) argsDesc
      = if ffi == "ClassLiteral"
          then JClassLiteral cname
          else unsupportedDescriptor returns ffiDesc argsDesc
    tryParseClassLiteralDescriptor returns ffi argsDesc = unsupportedDescriptor returns ffi argsDesc

    tryParseConstructorDescriptor : FDesc -> FDesc -> List (FDesc, LVar) -> JForeign
    tryParseConstructorDescriptor returns ffiDesc@(FCon ffi) argsDesc
      = if ffi == "New"
          then case fdescRefDescriptor returns of
                  ClassDesc cname    => JNew cname
                  InterfaceDesc _    => jerror $ "Invalid FFI descriptor for constructor. " ++
                                            "A constructor can't return an interface type. "
                  ArrayDesc _        => jerror "Array construction is not yet supported"
                  IdrisExportDesc _  => jerror "Cannot invoke constructor of Idris exported type"
                  NullableStrDesc    => jerror "A constructor cannot return a nullable string"
                  NullableRefDesc _  => jerror "A constructor cannot return a nullable reference"
          else tryParseClassLiteralDescriptor returns ffiDesc argsDesc
    tryParseConstructorDescriptor returns ffi argsDesc = tryParseClassLiteralDescriptor returns ffi argsDesc

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
      = if ffi == "Static" && nativeTy == "Class"
          then JStatic cname fn
          else tryParseStaticFieldDescriptor returns ffiDesc argsDesc
    tryParseStaticMethodDescriptor returns ffi argsDesc = tryParseStaticFieldDescriptor returns ffi argsDesc

  idrisToJava : List (FieldTypeDescriptor, LVar) -> Asm ()
  idrisToJava = sequence_ . map f where
    f : (FieldTypeDescriptor, LVar) -> Asm ()
    f (ty, v) = do
      Aload $ locIndex v
      idrisDescToJava (FieldDescriptor ty)

  javaToIdris : TypeDescriptor -> Asm ()
  javaToIdris (FieldDescriptor FieldTyDescBoolean) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "boolToIdrisBool" "(Z)Ljava/lang/Object;" False
  javaToIdris (FieldDescriptor FieldTyDescByte) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "byteToIdrisBits8" "(B)Ljava/lang/Object;" False
  javaToIdris (FieldDescriptor FieldTyDescShort) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "shortToIdrisBits16" "(S)Ljava/lang/Object;" False
  javaToIdris (FieldDescriptor FieldTyDescInt)     = boxInt
  javaToIdris (FieldDescriptor FieldTyDescChar)    = boxChar
  javaToIdris (FieldDescriptor FieldTyDescLong)    = boxLong
  javaToIdris (FieldDescriptor FieldTyDescFloat)   = do F2d; boxDouble
  javaToIdris (FieldDescriptor FieldTyDescDouble)  = boxDouble
  javaToIdris VoidDescriptor                       = Aconstnull
  javaToIdris (FieldDescriptor (FieldTyDescReference NullableStrDesc)) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "nullableRefToMaybe" "(Ljava/lang/Object;)Ljava/lang/Object;" False
  javaToIdris (FieldDescriptor (FieldTyDescReference (NullableRefDesc _))) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "nullableRefToMaybe" "(Ljava/lang/Object;)Ljava/lang/Object;" False
  javaToIdris _                                    = pure ()

  loadJavaVar : Int -> FieldTypeDescriptor -> Asm ()
  loadJavaVar index FieldTyDescBoolean = Iload index
  loadJavaVar index FieldTyDescByte    = Iload index
  loadJavaVar index FieldTyDescShort   = Iload index
  loadJavaVar index FieldTyDescInt     = Iload index
  loadJavaVar index FieldTyDescChar    = Iload index
  loadJavaVar index FieldTyDescLong    = Lload index
  loadJavaVar index FieldTyDescFloat   = Fload index
  loadJavaVar index FieldTyDescDouble  = Dload index
  loadJavaVar index (FieldTyDescReference (IdrisExportDesc cname))
    = do Aload index
         InvokeMethod InvokeVirtual cname "getValue" "()Ljava/lang/Object;" False
  loadJavaVar index _                  = Aload index

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
  javaReturn (FieldDescriptor (FieldTyDescReference (IdrisExportDesc cname)))
    = let desc = "(Ljava/lang/Object;)L" ++ cname ++ ";"
      in do InvokeMethod InvokeStatic cname "create" desc False
            Areturn
  javaReturn _                                    = Areturn

  idrisDescToJava : TypeDescriptor -> Asm ()
  idrisDescToJava (FieldDescriptor FieldTyDescBoolean) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "idrisBoolToBool" "(Ljava/lang/Object;)Z" False
  idrisDescToJava (FieldDescriptor FieldTyDescByte) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "idrisBits8ToByte" "(Ljava/lang/Object;)B" False
  idrisDescToJava (FieldDescriptor FieldTyDescShort) =
    InvokeMethod InvokeStatic (rtClassSig "Util") "idrisBits16ToShort" "(Ljava/lang/Object;)S" False
  idrisDescToJava (FieldDescriptor FieldTyDescInt) = do Checkcast "java/lang/Integer"; unboxInt
  idrisDescToJava (FieldDescriptor FieldTyDescChar) = do Checkcast "java/lang/Character"; unboxChar
  idrisDescToJava (FieldDescriptor FieldTyDescLong) = do Checkcast "java/lang/Long"; unboxLong
  idrisDescToJava (FieldDescriptor FieldTyDescFloat)
    = do Checkcast "java/lang/Double"
         InvokeMethod InvokeVirtual "java/lang/Double" "floatValue" "()F" False
  idrisDescToJava (FieldDescriptor FieldTyDescDouble) = do Checkcast "java/lang/Double"; unboxDouble
  idrisDescToJava (FieldDescriptor (FieldTyDescReference (IdrisExportDesc _))) = pure () -- converted using factory method on the exported type
  idrisDescToJava (FieldDescriptor (FieldTyDescReference NullableStrDesc)) =
    do InvokeMethod InvokeStatic (rtClassSig "Util") "maybeToNullableRef" "(Ljava/lang/Object;)Ljava/lang/Object;" False
       Checkcast "java/lang/String"
  idrisDescToJava (FieldDescriptor (FieldTyDescReference (NullableRefDesc cname))) = do
    InvokeMethod InvokeStatic (rtClassSig "Util") "maybeToNullableRef" "(Ljava/lang/Object;)Ljava/lang/Object;" False
    cast cname
  idrisDescToJava (FieldDescriptor (FieldTyDescReference refTy)) = cast $ refTyClassName refTy
  idrisDescToJava VoidDescriptor = pure ()
  idrisDescToJava ty = jerror $ "unknown type: " ++ show ty

  cast : String -> Asm ()
  cast "java/lang/Object" = pure ()
  cast cname              = Checkcast cname

  fdescTypeDescriptor : FDesc -> TypeDescriptor
  fdescTypeDescriptor (FCon "JVM_Unit") = VoidDescriptor
  fdescTypeDescriptor (FIO t) = fdescTypeDescriptor t
  fdescTypeDescriptor fdesc = FieldDescriptor $ fdescFieldDescriptor fdesc

  fdescFieldDescriptor : FDesc -> FieldTypeDescriptor
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntNative"]) = FieldTyDescInt
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntChar"]) = FieldTyDescChar
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits8"]) = FieldTyDescByte
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits16"]) = FieldTyDescShort
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits32"]) = FieldTyDescInt
  fdescFieldDescriptor (FApp "JVM_IntT" [_, FCon "JVM_IntBits64"]) = FieldTyDescLong
  fdescFieldDescriptor (FCon "JVM_Str") = FieldTyDescReference $ ClassDesc "java/lang/String"
  fdescFieldDescriptor (FCon "JVM_Float") = FieldTyDescFloat
  fdescFieldDescriptor (FCon "JVM_Double") = FieldTyDescDouble
  fdescFieldDescriptor (FCon "JVM_Bool") = FieldTyDescBoolean
  fdescFieldDescriptor fdesc = FieldTyDescReference $ fdescRefDescriptor fdesc

  fdescRefDescriptor : FDesc -> ReferenceTypeDescriptor
  fdescRefDescriptor desc@(FApp "JVM_NativeT" [FApp "Class" [FStr typeName]]) = ClassDesc typeName
  fdescRefDescriptor desc@(FApp "JVM_NativeT" [FApp "Interface" [FStr typeName]]) = InterfaceDesc typeName
  fdescRefDescriptor desc@(FApp "JVM_Nullable" [FApp "Class" [FStr typeName]]) = NullableRefDesc typeName
  fdescRefDescriptor desc@(FApp "JVM_Nullable" [FApp "Interface" [FStr typeName]]) = NullableRefDesc typeName
  fdescRefDescriptor (FApp "JVM_NativeT" [FApp "Array" [FApp "Class" [FStr typeName]]]) = ArrayDesc (ClassDesc typeName)
  fdescRefDescriptor (FCon "JVM_Str") = ClassDesc "java/lang/String"
  fdescRefDescriptor (FStr exportedType) = IdrisExportDesc exportedType
  fdescRefDescriptor (FCon "JVM_NullableStr") = NullableStrDesc
  fdescRefDescriptor desc = jerror $ "Expected a class or interface descriptor. " ++
    "Invalid reference type descriptor: " ++ show desc

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