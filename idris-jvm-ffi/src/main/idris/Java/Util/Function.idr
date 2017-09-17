module Function

import IdrisJvm.IO
import Java.Lang

%access public export

namespace Function
  FunctionInterface : JVM_NativeTy
  FunctionInterface = Interface "java/util/function/Function"

  Function : Type
  Function = JVM_Native FunctionInterface

  %inline
  jlambda : fTy -> {auto jvmElemTy: JVM_FnTypes fTy} -> Function
  jlambda f = javalambda "apply" (Object -> Object) f

  apply : (Inherits Function function, Inherits Object arg) => function -> arg -> JVM_IO Object
  apply function arg = invokeInstance "apply" (Function -> Object -> JVM_IO Object) (believe_me function) (believe_me arg)

namespace BiFunction
  BiFunctionInterface : JVM_NativeTy
  BiFunctionInterface = Interface "java/util/function/BiFunction"

  BiFunction : Type
  BiFunction = JVM_Native BiFunctionInterface

  %inline
  jlambda : fTy -> {auto jvmElemTy: JVM_FnTypes fTy} -> BiFunction
  jlambda f = javalambda "apply" (Object -> Object -> Object) f

  apply : (Inherits BiFunction function, Inherits Object arg1, Inherits Object arg2) => function -> arg1 -> arg2 -> JVM_IO Object
  apply function arg1 arg2 = invokeInstance "apply" (BiFunction -> Object -> Object -> JVM_IO Object) (believe_me function) (believe_me arg1) (believe_me arg2)

namespace Predicate
  Predicate : Type
  Predicate = javaInterface "java/util/function/Predicate"

  %inline
  jlambda : fTy -> {auto jvmElemTy: JVM_FnTypes fTy} -> Predicate
  jlambda f = javalambda "test" (Object -> Bool) f

namespace Supplier
  Supplier : Type
  Supplier = javaInterface "java/util/function/Supplier"

  %inline
  jlambda : JVM_IO ty -> {auto jvmTy: JVM_Types ty} -> Supplier
  jlambda {ty} f = javalambda "get" (JVM_IO Object) g
    where
      g : () -> ty
      g x = believe_me $ unsafePerformIO f

namespace IntUnaryOperator
  IntUnaryOperatorInterface : JVM_NativeTy
  IntUnaryOperatorInterface = Interface "java/util/function/IntUnaryOperator"

  IntUnaryOperator : Type
  IntUnaryOperator = JVM_Native IntUnaryOperatorInterface

  %inline
  jlambda : (Int -> Int) -> IntUnaryOperator
  jlambda f = javalambda "applyAsInt" (Int -> Int) f