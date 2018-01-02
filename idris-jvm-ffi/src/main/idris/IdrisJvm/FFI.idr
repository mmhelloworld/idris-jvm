module IdrisJvm.FFI

%access public export

AnnotationTypeName : Type
AnnotationTypeName = String

data AnnotationValue = AnnInt Int
                     | AnnString String
                     | AnnArray (List AnnotationValue)

AnnotationNameValuePair : Type
AnnotationNameValuePair = (String, AnnotationValue)

data Annotation = Ann AnnotationTypeName (List AnnotationNameValuePair)

data JVM_NativeTy = Class String
                  | Interface String

data JVM_Native  : JVM_NativeTy -> Type where
  MkJVMNative : (ty : JVM_NativeTy) -> JVM_Native ty

mutual
  data JVM_FfiFn = Static JVM_NativeTy String
                 | GetStaticField JVM_NativeTy String
                 | SetStaticField JVM_NativeTy String
                 | Lambda JVM_NativeTy String
                 | Constructor
                 | New
                 | NewArray
                 | GetArray
                 | SetArray
                 | MultiNewArray
                 | ArrayLength
                 | ClassLiteral String
                 | Instance String
                 | GetInstanceField String
                 | SetInstanceField String
                 | Super String
                 | ExportStaticField String
                 | ExportInstanceField String
                 | ExportStatic String
                 | ExportInstance String
                 | ExportDefault -- Export an instance method with idris function name
                 | Anns (List Annotation)
                 | ExportInstanceWithAnn String (List Annotation)

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
      JVM_FnIO : JVM_Types t -> JVM_FnTypes (JVM_IO t)
      JVM_FnBase : JVM_Types t -> JVM_FnTypes t

  data JVM_FunctionalObject : Type -> Type where
    MkJVMFunctionalObject : (lambdaFunctionTy : t) -> JVM_FunctionalObject t

  ||| Supported JVM foreign types
  data JVM_Types : Type -> Type where
      JVM_Bool        : JVM_Types Bool
      JVM_Str         : JVM_Types String
      JVM_Double      : JVM_Types Double
      JVM_Float       : JVM_Types JFloat
      JVM_Unit        : JVM_Types ()
      JVM_NullableStr : JVM_Types (Maybe String)
      JVM_Nullable    : JVM_Types (Maybe (JVM_Native t))
      JVM_NativeT     : JVM_Types (JVM_Native a)
      JVM_IntT        : JVM_IntTypes i -> JVM_Types i
      JVM_ArrayT      : JVM_Types t -> JVM_Types (JVM_Array t)
      JVM_FnT         : JVM_FnTypes t -> JVM_Types (JVM_FunctionalObject t)

  data JVM_Array : Type -> Type where
    MkJvmTypesArray : JVM_Types t -> JVM_Array t

  ||| A descriptor for the JVM FFI. See the constructors of `JVM_Types`
  ||| and `JVM_IntTypes` for the concrete types that are available.
  FFI_JVM : FFI
  FFI_JVM = MkFFI JVM_Types JVM_FfiFn String

  JVM_IO : Type -> Type
  JVM_IO = IO' FFI_JVM

Show JFloat where
  show (Float d) = show d

%used MkJVMFunctionalObject lambdaFunctionTy

%inline
javacall : (fname : JVM_FfiFn) -> (ty : Type) ->
           {auto fty : FTy FFI_JVM [] ty} -> ty
javacall fname ty = foreign FFI_JVM fname ty

%inline
jcall : (fname : JVM_FfiFn) -> (ty : Type) ->
           {fty : FTy FFI_JVM [] ty} -> ty
jcall fname ty = foreign FFI_JVM fname ty

javaClass : String -> Type
javaClass = JVM_Native . Class

javaInterface : String -> Type
javaInterface = JVM_Native . Interface

%inline
javalambda : String
        -> (interfaceFnTy: Type)
        -> {auto interfaceFnJvmTy: JVM_FnTypes interfaceFnTy}
        -> lambdaTy
        -> {auto lambdaJvmTy: JVM_FnTypes lambdaTy}
        -> JVM_Native nativeTy
javalambda {nativeTy} {lambdaTy} fname interfaceFnTy lambda = unsafePerformIO $
  javacall
    (Lambda nativeTy fname)
    (JVM_FunctionalObject interfaceFnTy -> JVM_FunctionalObject lambdaTy -> JVM_IO (JVM_Native nativeTy))

    {- This value is not used. This is here so that the corresponding type can provide the interface method signature
     - which can be different from the actual lambda signature
     -}
    (believe_me "")

    (MkJVMFunctionalObject lambda)

%inline
new : (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
new ty = javacall New ty

%inline
newArray : (elemTy: Type) -> {auto jvmElemTy: JVM_Types elemTy} -> Nat -> JVM_IO (JVM_Array elemTy)
newArray elemTy size = javacall NewArray (Int -> JVM_IO $ JVM_Array elemTy) (cast size)

%inline
setArray : (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
setArray ty = javacall SetArray ty

%inline
arrayLength : {auto ft: JVM_Types elemTy} -> JVM_Array elemTy -> JVM_IO Nat
arrayLength {elemTy} arr = cast <$> javacall ArrayLength (JVM_Array elemTy -> JVM_IO Int) arr

%inline
getArray : (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
getArray ty = javacall GetArray ty

%inline
newMultiArray : (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
newMultiArray = javacall MultiNewArray

%inline
invokeInstance : String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
invokeInstance method = javacall (Instance method)

%inline
jcallInstance : String -> (ty : Type) -> FTy FFI_JVM [] ty -> ty
jcallInstance method ty fty = jcall (Instance method) ty {fty=fty}

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
jcallStatic : JVM_NativeTy -> String -> (ty : Type) -> FTy FFI_JVM [] ty -> ty
jcallStatic klass method ty fty = jcall (Static klass method) ty {fty=fty}

%inline
getStaticField : JVM_NativeTy -> String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
getStaticField klass fieldName = javacall (GetStaticField klass fieldName)

%inline
setStaticField : JVM_NativeTy -> String -> (ty : Type) -> {auto fty : FTy FFI_JVM [] ty} -> ty
setStaticField klass fieldName = javacall (SetStaticField klass fieldName)

interface Inherits a b where {}

Inherits a a where { }

namespace Class

  ClassClass : JVM_NativeTy
  ClassClass = Class "java/lang/Class"

  Class : Type
  Class = JVM_Native ClassClass

  forName : String -> Class
  forName className = unsafePerformIO $ invokeStatic ClassClass "forName" (String -> JVM_IO Class) className

%inline
classLit : String -> Class.Class
classLit s = unsafePerformIO $ javacall (ClassLiteral s) (JVM_IO Class.Class)

RuntimeClass : JVM_NativeTy
RuntimeClass = Class "io/github/mmhelloworld/idrisjvm/runtime/Runtime"
