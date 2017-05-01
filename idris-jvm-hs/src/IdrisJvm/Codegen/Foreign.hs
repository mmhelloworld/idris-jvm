{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Foreign where

import           Data.List.Split
import           Data.Monoid
import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Common
import           IdrisJvm.Codegen.Types
import           IRTS.Lang

data JForeign = JStatic String String
              | JGetStaticField String String
              | JSetStaticField String String
              | JVirtual String String
              | JGetInstanceField String String
              | JSetInstanceField String String
              | JInterface String String
              | JNew String
              | JClassLiteral String

parseDescriptor :: FDesc -> FDesc -> [(FDesc, LVar)] -> JForeign
parseDescriptor _ (FApp ffi [FApp nativeTy [FStr cname], FStr fn]) _
  | ffi == sUN "Static" && nativeTy == sUN "Class" = JStatic cname fn

parseDescriptor _ (FApp ffi [FApp nativeTy [FStr cname], FStr fieldName]) _
  | ffi == sUN "GetStaticField" && nativeTy == sUN "Class" = JGetStaticField cname fieldName

parseDescriptor _ (FApp ffi [FApp nativeTy [FStr cname], FStr fieldName]) _
  | ffi == sUN "SetStaticField" && nativeTy == sUN "Class" = JSetStaticField cname fieldName

parseDescriptor _ (FApp ffi [FStr _]) []
  | ffi == sUN "Instance"
    = error "Instance methods should have atleast one argument"

parseDescriptor _ (FApp ffi [FStr fn]) ((declClass, _):_)
  | ffi == sUN "Instance" = case fdescRefDescriptor declClass of
    ClassDesc cname     -> JVirtual cname fn
    InterfaceDesc cname -> JInterface cname fn
    NullableStrDesc     -> error "Instance method cannot have the first argument as nullable string"
    NullableRefDesc _     -> error "Instance method cannot have the first argument as nullable reference"
    ArrayDesc _         -> error "No instance methods on an array"
    IdrisExportDesc _ -> error "No instance methods on an Idris exported type"

parseDescriptor _ (FApp ffi [FStr fieldName]) ((declClass, _):_)
  | ffi == sUN "GetInstanceField" = case fdescRefDescriptor declClass of
    ClassDesc cname -> JGetInstanceField cname fieldName
    _               -> error "Fields can be accessed only from classes"

parseDescriptor _ (FApp ffi [FStr fieldName]) ((declClass, _):_)
  | ffi == sUN "SetInstanceField" = case fdescRefDescriptor declClass of
    ClassDesc cname -> JSetInstanceField cname fieldName
    _               -> error "Fields can be set only from classes"

parseDescriptor returns (FCon ffi) _
 | ffi == sUN "New" = case fdescRefDescriptor returns of
    ClassDesc cname -> JNew cname
    InterfaceDesc _ -> error $ "Invalid FFI descriptor for constructor. " <>
                              "A constructor can't return an interface type. " <>
                              show returns
    ArrayDesc _ -> error "Array construction is not yet supported"
    IdrisExportDesc _ -> error "Cannot invoke constructor of Idris exported type"
    NullableStrDesc     -> error "A constructor cannot return a nullable string"
    NullableRefDesc _     -> error "A constructor cannot return a nullable reference"

parseDescriptor _ (FApp ffi [FStr cname]) _
 | ffi == sUN "ClassLiteral" = JClassLiteral cname

parseDescriptor _ fdesc _ = error $ "Unsupported descriptor: " ++ show fdesc

idrisToJava :: [(FieldTypeDescriptor, LVar)] -> Cg ()
idrisToJava = mapM_ f where
  f (ty, v) = do
    writeIns [ Aload $ locIndex v ]
    idrisDescToJava (FieldDescriptor ty)

javaToIdris :: TypeDescriptor -> Cg ()
javaToIdris (FieldDescriptor FieldTyDescBoolean) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
               "boolToIdrisBool"
               "(Z)Ljava/lang/Object;" False ]
javaToIdris (FieldDescriptor FieldTyDescByte) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
               "byteToIdrisBits8"
               "(B)Ljava/lang/Object;" False ]
javaToIdris (FieldDescriptor FieldTyDescShort) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
               "shortToIdrisBits16"
               "(S)Ljava/lang/Object;" False ]
javaToIdris (FieldDescriptor FieldTyDescInt)     = writeIns [ boxInt ]
javaToIdris (FieldDescriptor FieldTyDescChar)    = writeIns [ boxChar ]
javaToIdris (FieldDescriptor FieldTyDescLong)    = writeIns [ boxLong ]
javaToIdris (FieldDescriptor FieldTyDescFloat)   = writeIns [ F2d, boxDouble ]
javaToIdris (FieldDescriptor FieldTyDescDouble)  = writeIns [ boxDouble ]
javaToIdris VoidDescriptor                       = writeIns [ Aconstnull ]
javaToIdris (FieldDescriptor (FieldTyDescReference NullableStrDesc)) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util") "nullableRefToMaybe" "(Ljava/lang/Object;)Ljava/lang/Object;" False ]
javaToIdris (FieldDescriptor (FieldTyDescReference (NullableRefDesc _))) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util") "nullableRefToMaybe" "(Ljava/lang/Object;)Ljava/lang/Object;" False ]
javaToIdris _                                    = pure ()

loadJavaVar :: Int -> FieldTypeDescriptor -> Cg ()
loadJavaVar index FieldTyDescBoolean = writeIns [ Iload index ]
loadJavaVar index FieldTyDescByte    = writeIns [ Iload index ]
loadJavaVar index FieldTyDescShort   = writeIns [ Iload index ]
loadJavaVar index FieldTyDescInt     = writeIns [ Iload index ]
loadJavaVar index FieldTyDescChar    = writeIns [ Iload index ]
loadJavaVar index FieldTyDescLong    = writeIns [ Lload index ]
loadJavaVar index FieldTyDescFloat   = writeIns [ Fload index ]
loadJavaVar index FieldTyDescDouble  = writeIns [ Dload index ]
loadJavaVar index (FieldTyDescReference (IdrisExportDesc cname))
  = writeIns [ Aload index
             , InvokeMethod InvokeVirtual cname "getValue" "()Ljava/lang/Object;" False ]
loadJavaVar index _                  = writeIns [ Aload index ]

javaReturn :: TypeDescriptor -> Cg ()
javaReturn VoidDescriptor                       = writeIns [ Return ]
javaReturn (FieldDescriptor FieldTyDescBoolean) = writeIns [ Ireturn ]
javaReturn (FieldDescriptor FieldTyDescByte)    = writeIns [ Ireturn ]
javaReturn (FieldDescriptor FieldTyDescShort)   = writeIns [ Ireturn ]
javaReturn (FieldDescriptor FieldTyDescInt)     = writeIns [ Ireturn ]
javaReturn (FieldDescriptor FieldTyDescChar)    = writeIns [ Ireturn ]
javaReturn (FieldDescriptor FieldTyDescLong)    = writeIns [ Lreturn ]
javaReturn (FieldDescriptor FieldTyDescFloat)   = writeIns [ Freturn ]
javaReturn (FieldDescriptor FieldTyDescDouble)  = writeIns [ Dreturn ]
javaReturn (FieldDescriptor (FieldTyDescReference (IdrisExportDesc cname)))
 = writeIns [ InvokeMethod InvokeStatic cname "create" desc False
            , Areturn ]
   where desc = "(Ljava/lang/Object;)L" ++ cname ++ ";"
javaReturn _                                    = writeIns [ Areturn ]

idrisDescToJava :: TypeDescriptor -> Cg ()
idrisDescToJava (FieldDescriptor FieldTyDescBoolean) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
               "idrisBoolToBool"
               "(Ljava/lang/Object;)Z" False ]
idrisDescToJava (FieldDescriptor FieldTyDescByte) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
               "idrisBits8ToByte"
               "(Ljava/lang/Object;)B" False ]
idrisDescToJava (FieldDescriptor FieldTyDescShort) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
               "idrisBits16ToShort"
               "(Ljava/lang/Object;)S" False ]
idrisDescToJava (FieldDescriptor FieldTyDescInt) = writeIns [ Checkcast "java/lang/Integer", unboxInt ]
idrisDescToJava (FieldDescriptor FieldTyDescChar) = writeIns [ Checkcast "java/lang/Character", unboxChar ]
idrisDescToJava (FieldDescriptor FieldTyDescLong) = writeIns [ Checkcast "java/lang/Long", unboxLong ]
idrisDescToJava (FieldDescriptor FieldTyDescFloat)
  = writeIns [ Checkcast "java/lang/Double"
             , InvokeMethod InvokeVirtual "java/lang/Double" "floatValue" "()F" False ]
idrisDescToJava (FieldDescriptor FieldTyDescDouble) = writeIns [ Checkcast "java/lang/Double", unboxDouble ]
idrisDescToJava (FieldDescriptor (FieldTyDescReference (IdrisExportDesc _))) = pure () -- converted using factory method on the exported type
idrisDescToJava (FieldDescriptor (FieldTyDescReference NullableStrDesc)) =
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util") "maybeToNullableRef" "(Ljava/lang/Object;)Ljava/lang/Object;" False
           , Checkcast "java/lang/String"]
idrisDescToJava (FieldDescriptor (FieldTyDescReference (NullableRefDesc cname))) = do
  writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util") "maybeToNullableRef" "(Ljava/lang/Object;)Ljava/lang/Object;" False ]
  cast cname
idrisDescToJava (FieldDescriptor (FieldTyDescReference refTy)) = cast $ refTyClassName refTy
idrisDescToJava VoidDescriptor = pure ()
idrisDescToJava ty = error $ "unknown type: " ++ show ty

cast :: String -> Cg ()
cast "java/lang/Object" = pure ()
cast cname              = writeIns [ Checkcast cname ]

fdescTypeDescriptor :: FDesc -> TypeDescriptor
fdescTypeDescriptor (FCon (UN "JVM_Unit")) = VoidDescriptor
fdescTypeDescriptor (FIO t) = fdescTypeDescriptor t
fdescTypeDescriptor fdesc = FieldDescriptor $ fdescFieldDescriptor fdesc

fdescFieldDescriptor :: FDesc -> FieldTypeDescriptor
fdescFieldDescriptor (FApp intTy [_, FCon (UN "JVM_IntNative")]) | intTy == sUN "JVM_IntT" = FieldTyDescInt
fdescFieldDescriptor (FApp intTy [_, FCon (UN "JVM_IntChar")]) | intTy == sUN "JVM_IntT" = FieldTyDescChar
fdescFieldDescriptor (FApp intTy [_, FCon (UN "JVM_IntBits8")]) | intTy == sUN "JVM_IntT" = FieldTyDescByte
fdescFieldDescriptor (FApp intTy [_, FCon (UN "JVM_IntBits16")]) | intTy == sUN "JVM_IntT" = FieldTyDescShort
fdescFieldDescriptor (FApp intTy [_, FCon (UN "JVM_IntBits32")]) | intTy == sUN "JVM_IntT" = FieldTyDescInt
fdescFieldDescriptor (FApp intTy [_, FCon (UN "JVM_IntBits64")]) | intTy == sUN "JVM_IntT" = FieldTyDescLong
fdescFieldDescriptor (FCon (UN "JVM_Str")) = FieldTyDescReference $ ClassDesc "java/lang/String"
fdescFieldDescriptor (FCon (UN "JVM_Float")) = FieldTyDescFloat
fdescFieldDescriptor (FCon (UN "JVM_Double")) = FieldTyDescDouble
fdescFieldDescriptor (FCon (UN "JVM_Bool")) = FieldTyDescBoolean
fdescFieldDescriptor fdesc = FieldTyDescReference $ fdescRefDescriptor fdesc

fdescRefDescriptor :: FDesc -> ReferenceTypeDescriptor
fdescRefDescriptor desc@(FApp jvmTy [FApp nativeTy [FStr typeName]])
  | jvmTy == sUN "JVM_NativeT" && nativeTy == sUN "Class"
    = ClassDesc typeName
  | jvmTy == sUN "JVM_NativeT" && nativeTy == sUN "Interface"
    = InterfaceDesc typeName
  | jvmTy == sUN "JVM_Nullable" && nativeTy == sUN "Class"
    = NullableRefDesc typeName
  | jvmTy == sUN "JVM_Nullable" && nativeTy == sUN "Interface"
    = NullableRefDesc typeName
  | otherwise = error $ "Expected a class or interface descriptor. " ++
                  "Invalid reference type descriptor: " ++ show desc
fdescRefDescriptor (FApp jvmTy [FApp nativeTy [FApp descTy [FStr typeName]]])
  | jvmTy == sUN "JVM_NativeT" && nativeTy == sUN "Array" && descTy == sUN "Class"
    = ArrayDesc (ClassDesc typeName)
fdescRefDescriptor (FCon (UN "JVM_Str")) = ClassDesc "java/lang/String"
fdescRefDescriptor (FStr exportedType) = IdrisExportDesc exportedType
fdescRefDescriptor (FCon (UN "JVM_NullableStr")) = NullableStrDesc
fdescRefDescriptor desc = error $ "Expected a class or interface descriptor. " ++
  "Invalid reference type descriptor: " ++ show desc

isExportIO :: FDesc -> Bool
isExportIO (FIO _) = True
isExportIO _       = False

parseExportedClassName :: ClassName -> (ClassName, Maybe ClassName, [ClassName])
parseExportedClassName = build . parseParts where
  parseParts = split (keepDelimsL $ oneOf ["extends", "implements"]) .
              split (condense . dropDelims $ oneOf " ,")

  build :: [[ClassName]] -> (ClassName, Maybe ClassName, [ClassName])
  build [] = error "Class name cannot be empty"
  build [[cls@(_:_)]] = (cls, Nothing, [])
  build [cls, ["extends", parent@(_:_)]] = (parseClassPart cls, Just parent, [])
  build [cls, "implements": intf] = (parseClassPart cls, Nothing, intf)
  build [cls, extends, "implements":intf]
   = (parseClassPart cls, Just $ parseExtendsPart extends, intf)
  build _ = error "Invalid class name descriptor in export definition"

  parseClassPart :: [ClassName] -> ClassName
  parseClassPart [cls@(_:_)] = cls
  parseClassPart _ = error "Missing class name in class name descriptor"

  parseExtendsPart ("extends": parent@(_:_): _) = parent
  parseExtendsPart _ = error "Invalid 'extends' part in class name descriptor"
