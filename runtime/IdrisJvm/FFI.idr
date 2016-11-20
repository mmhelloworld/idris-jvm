module IdrisJvm.FFI

%access public export

data JVM_NativeTy = Primitive String
                  | Class String
                  | Interface String
                  | Array JVM_NativeTy

data JVM_Native  : JVM_NativeTy -> Type where
  MkJVMNative : (ty : JVM_NativeTy) -> JVM_Native ty

data JVM_FfiFn = Static JVM_NativeTy  String
               | Constructor
               | Instance String

mutual
  data JVM_IntTypes : Type -> Type where
      JVM_IntChar   : JVM_IntTypes Char
      JVM_IntNative : JVM_IntTypes Int
      JVM_IntBits8  : JVM_IntTypes Bits8
      JVM_IntBits16 : JVM_IntTypes Bits16
      JVM_IntBits32 : JVM_IntTypes Bits32
      JVM_IntBits64 : JVM_IntTypes Bits64

  ||| Supported JVM function types
  data JVM_FnTypes : Type -> Type where
      JVM_Fn : JVM_Types s -> JVM_FnTypes t -> JVM_FnTypes (s -> t)
      JVM_FnIO : JVM_Types t -> JVM_FnTypes (IO' FFI_JVM t)
      JVM_FnBase : JVM_Types t -> JVM_FnTypes t

  ||| Supported JVM foreign types
  data JVM_Types : Type -> Type where
      JVM_Bool    : JVM_Types Bool
      JVM_Str     : JVM_Types String
      JVM_Float   : JVM_Types Double
      JVM_Unit    : JVM_Types ()
      JVM_NativeT : JVM_Types (JVM_Native a)
      JVM_IntT    : JVM_IntTypes i -> JVM_Types i

  ||| A descriptor for the JVM FFI. See the constructors of `JVM_Types`
  ||| and `JVM_IntTypes` for the concrete types that are available.
  FFI_JVM : FFI
  FFI_JVM = MkFFI JVM_Types JVM_FfiFn String

JVM_IO : Type -> Type
JVM_IO = IO' FFI_JVM

namespace Java.Lang.Object
  ObjectClass : JVM_NativeTy
  ObjectClass = Class "java/lang/Object"

  Object : Type
  Object = JVM_Native ObjectClass


interface Inherits a b where {}

Inherits Object Object where {}

Inherits Object String where {}

%inline
javacall : (fname : JVM_FfiFn) -> (ty : Type) ->
           {auto fty : FTy FFI_JVM [] ty} -> ty
javacall fname ty = foreign FFI_JVM fname ty

%inline
new : (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
new ty = javacall Constructor ty

printLn : Show a => a -> JVM_IO ()
printLn = printLn'

putStrLn : String -> JVM_IO ()
putStrLn = putStrLn'
