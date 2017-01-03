module IdrisJvm.FFI

%access public export

data JVM_NativeTy = Class String
                  | Interface String
                  | Primitive String
                  | Array JVM_NativeTy

data JVM_Native  : JVM_NativeTy -> Type where
  MkJVMNative : (ty : JVM_NativeTy) -> JVM_Native ty

data JVM_FfiFn = Static JVM_NativeTy  String
               | GetStaticField JVM_NativeTy String
               | SetStaticField JVM_NativeTy String
               | Constructor
               | New
               | Instance String
               | GetInstanceField String
               | SetInstanceField String
               | Super String
               | ExportStaticField String
               | ExportInstanceField String
               | ExportStatic String
               | ExportInstance String
               | ExportDefault -- Export an instance method with idris function name

mutual
  data JVM_IntTypes : Type -> Type where
      JVM_IntChar   : JVM_IntTypes Char
      JVM_IntNative : JVM_IntTypes Int
      JVM_IntBits8  : JVM_IntTypes Bits8
      JVM_IntBits16 : JVM_IntTypes Bits16
      JVM_IntBits32 : JVM_IntTypes Bits32
      JVM_IntBits64 : JVM_IntTypes Bits64

  data JFloat = Float Double

  ||| Supported JVM function types
  data JVM_FnTypes : Type -> Type where
      JVM_Fn : JVM_Types s -> JVM_FnTypes t -> JVM_FnTypes (s -> t)
      JVM_FnIO : JVM_Types t -> JVM_FnTypes (IO' FFI_JVM t)
      JVM_FnBase : JVM_Types t -> JVM_FnTypes t

  ||| Supported JVM foreign types
  data JVM_Types : Type -> Type where
      JVM_Bool    : JVM_Types Bool
      JVM_Str     : JVM_Types String
      JVM_Double  : JVM_Types Double
      JVM_Float   : JVM_Types JFloat
      JVM_Unit    : JVM_Types ()
      JVM_NativeT : JVM_Types (JVM_Native a)
      JVM_IntT    : JVM_IntTypes i -> JVM_Types i
      JVM_ArrayT  : JVM_Types (JVM_Array t)

  data JVM_Array : JVM_NativeTy -> Type where
      MkArray : (ty: JVM_NativeTy) -> JVM_Array ty

  ||| A descriptor for the JVM FFI. See the constructors of `JVM_Types`
  ||| and `JVM_IntTypes` for the concrete types that are available.
  FFI_JVM : FFI
  FFI_JVM = MkFFI JVM_Types JVM_FfiFn String

Show JFloat where
  show (Float d) = show d

JVM_IO : Type -> Type
JVM_IO = IO' FFI_JVM

%inline
javacall : (fname : JVM_FfiFn) -> (ty : Type) ->
           {auto fty : FTy FFI_JVM [] ty} -> ty
javacall fname ty = foreign FFI_JVM fname ty

javaClass : String -> Type
javaClass = JVM_Native . Class

javaInterface : String -> Type
javaInterface = JVM_Native . Interface

%inline
new : (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
new ty = javacall New ty

%inline
invokeInstance : String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
invokeInstance method = javacall (Instance method)

%inline
getInstanceField : String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
getInstanceField fieldName = javacall (GetInstanceField fieldName)

%inline
setInstanceField : String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
setInstanceField fieldName = javacall (SetInstanceField fieldName)

%inline
invokeStatic : JVM_NativeTy -> String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
invokeStatic klass method = javacall (Static klass method)

%inline
getStaticField : JVM_NativeTy -> String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
getStaticField klass fieldName = javacall (GetStaticField klass fieldName)

%inline
setStaticField : JVM_NativeTy -> String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
setStaticField klass fieldName = javacall (SetStaticField klass fieldName)

interface Inherits a b where {}
