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
boxIfNeeded VoidDescriptor                   = writeIns [Aconstnull]
boxIfNeeded _                                = pure () -- TODO: implement for other types

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
