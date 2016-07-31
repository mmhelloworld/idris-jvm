module IdrisJvm.FFI

%access public export

data JVMAnyTy = Primitive String
              | Class String
              | Interface String
              | Array JVMAnyTy

data JVMAny   : JVMAnyTy -> Type where
  MkJVMAny : (ty : JVMAnyTy) -> JVMAny ty

data JVMFfiFn = Static String String

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
      JVM_Str   : JVM_Types String
      JVM_Float : JVM_Types Double
      JVM_Unit  : JVM_Types ()
      JVM_Any   : JVM_Types (JVMAny a)
      JVM_IntT  : JVM_IntTypes i -> JVM_Types i

  ||| A descriptor for the JVM FFI. See the constructors of `JVM_Types`
  ||| and `JVM_IntTypes` for the concrete types that are available.
  FFI_JVM : FFI
  FFI_JVM = MkFFI JVM_Types JVMFfiFn String

JVM_IO : Type -> Type
JVM_IO = IO' FFI_JVM

%inline
javacall : (fname : JVMFfiFn) -> (ty : Type) ->
           {auto fty : FTy FFI_JVM [] ty} -> ty
javacall fname ty = foreign FFI_JVM fname ty

printLn : Show a => a -> JVM_IO ()
printLn = printLn'

putStrLn : String -> JVM_IO ()
putStrLn = putStrLn'
