{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.Codegen.Foreign where

import           Idris.Core.TT
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Common
import           IdrisJvm.Codegen.Types
import           IRTS.Lang

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
    ClassDesc cname     -> JVirtual cname fn
    InterfaceDesc cname -> JInterface cname fn

parseDescriptor returns (FCon ffi) _
 | ffi == sUN "Constructor" = case fdescRefDescriptor returns of
   ClassDesc cname -> JConstructor cname
   InterfaceDesc _ -> error $ "Invalid FFI descriptor for constructor. " ++
                              "A constructor can't return an interface type. " ++
                              show returns

parseDescriptor _ fdesc _ = error $ "Unsupported descriptor: " ++ show fdesc

idrisToJava :: [(FieldTypeDescriptor, LVar)] -> Cg ()
idrisToJava = mapM_ f where
  f (ty, v) = do
    writeIns [ Aload $ locIndex v ]
    case ty of
      FieldTyDescBoolean ->
        writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
                     "idrisBoolToBool"
                     "(Ljava/lang/Object;)Z" False ]
      FieldTyDescByte ->
        writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
                     "idrisBits8ToByte"
                     "(Ljava/lang/Object;)B" False ]
      FieldTyDescShort ->
        writeIns [ InvokeMethod InvokeStatic (rtClassSig "Util")
                     "idrisBits16ToShort"
                     "(Ljava/lang/Object;)S" False ]
      FieldTyDescInt -> writeIns [ Checkcast "java/lang/Integer", unboxInt ]
      FieldTyDescChar -> writeIns [ Checkcast "java/lang/Character", unboxChar ]
      FieldTyDescLong -> writeIns [ Checkcast "java/lang/Long", unboxLong ]
      FieldTyDescReference refTy -> writeIns [ Checkcast $ refTyClassName refTy]
      _ -> error $ "unknown type: " ++ show ty

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
javaToIdris VoidDescriptor                       = writeIns [Aconstnull]
javaToIdris _                                    = pure () -- TODO: implement for other types

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
fdescFieldDescriptor (FCon (UN "JVM_Bool")) = FieldTyDescBoolean
fdescFieldDescriptor fdesc = FieldTyDescReference $ fdescRefDescriptor fdesc

fdescRefDescriptor :: FDesc -> ReferenceTypeDescriptor
fdescRefDescriptor desc@(FApp jvmTy [FApp nativeTy [FStr typeName]])
  | jvmTy == sUN "JVM_NativeT" && nativeTy == sUN "Class"
    = ClassDesc typeName
  | jvmTy == sUN "JVM_NativeT" && nativeTy == sUN "Interface"
    = InterfaceDesc typeName
  | otherwise = error $ "Expected a class or interface descriptor. " ++
                  "Invalid reference type descriptor: " ++ show desc
fdescRefDescriptor (FCon (UN "JVM_Str")) = ClassDesc "java/lang/String"
fdescRefDescriptor desc = error $ "Expected a class or interface descriptor. " ++
  "Invalid reference type descriptor: " ++ show desc
