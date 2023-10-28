module Compiler.Jvm.InferredType

import Compiler.Jvm.Jname
import Core.CompileExpr
import Core.Core
import Core.Name
import Data.List1
import Data.String
import System.FFI

mutual
  public export
  data JavaReferenceType = Class | Interface

  public export
  data InferredType = IBool | IByte | IChar | IShort | IInt | ILong | IFloat | IDouble | IRef String JavaReferenceType
                    | IArray InferredType | IVoid
                    | IFunction JavaLambdaType
                    | IUnknown

  public export
  record InferredFunctionType where
      constructor MkInferredFunctionType
      returnType : InferredType
      parameterTypes : List InferredType

  public export
  record JavaLambdaType where
    constructor MkJavaLambdaType
    javaInterface: InferredType
    methodName: String
    methodType: InferredFunctionType
    implementationType: InferredFunctionType

mutual
  export
  Eq InferredFunctionType where
    (MkInferredFunctionType returnType1 argumentTypes1) ==
      (MkInferredFunctionType returnType2 argumentTypes2) =
        assert_total $ returnType1 == returnType2 && argumentTypes1 == argumentTypes2

  export
  Eq JavaLambdaType where
    (MkJavaLambdaType intf1 method1 methodType1 implementationType1) ==
      (MkJavaLambdaType intf2 method2 methodType2 implementationType2) = intf1 == intf2 && method1 == method2 &&
        methodType1 == methodType2 && implementationType1 == implementationType2


  export
  Eq InferredType where
    IBool == IBool = True
    IByte == IByte = True
    IChar == IChar = True
    IShort == IShort = True
    IInt == IInt = True
    ILong == ILong = True
    IFloat == IFloat = True
    IDouble == IDouble = True
    (IRef ty1 _) == (IRef ty2 _) = ty1 == ty2
    (IArray elemTy1) == (IArray elemTy2) = elemTy1 == elemTy2
    (IFunction javaLambdaType1) == (IFunction javaLambdaType2) = assert_total $ javaLambdaType1 == javaLambdaType2
    IUnknown == IUnknown = True
    IVoid == IVoid = True
    _ == _ = False

mutual
  export
  Show InferredFunctionType where
    show (MkInferredFunctionType returnType argumentTypes) =
      assert_total $ showSep "⟶" (show <$> (argumentTypes ++ [returnType]))

  export
  Show JavaLambdaType where
    show (MkJavaLambdaType intf method methodType implementationType) =
      showSep " " ["Lambda", show intf, method, show methodType, show implementationType]

  export
  Show InferredType where
      show IBool = "boolean"
      show IByte = "byte"
      show IChar = "char"
      show IShort = "short"
      show IInt = "int"
      show ILong = "long"
      show IFloat = "float"
      show IDouble = "double"
      show (IRef clsName _) = clsName
      show (IArray elemTy) = "Array " ++ show elemTy
      show (IFunction lambdaTy) = assert_total $ "Function " ++ show lambdaTy
      show IUnknown = "unknown"
      show IVoid = "void"

export
inferredObjectType : InferredType
inferredObjectType = IRef "java/lang/Object" Class

%inline
public export
bigIntegerClass : String
bigIntegerClass = "java/math/BigInteger"

export
inferredBigIntegerType : InferredType
inferredBigIntegerType = IRef bigIntegerClass Class

%inline
public export
stringClass : String
stringClass = "java/lang/String"

export
inferredStringType : InferredType
inferredStringType = IRef stringClass Class

export
inferredLambdaType : InferredType
inferredLambdaType = IRef "java/util/function/Function" Interface

export
function2Type : InferredType
function2Type = IRef "java/util/function/BiFunction" Interface

export
getFunctionInterface : (arity: Nat) -> InferredType
getFunctionInterface 1 = inferredLambdaType
getFunctionInterface 2 = function2Type
getFunctionInterface arity = IRef ("io/github/mmhelloworld/idrisjvm/runtime/Function" ++ show arity) Interface

%inline
public export
inferredForkJoinTaskType : InferredType
inferredForkJoinTaskType = IRef "java/util/concurrent/ForkJoinTask" Class

%inline
public export
arrayListClass : String
arrayListClass = "java/util/ArrayList"

%inline
public export
arrayListType : InferredType
arrayListType = IRef arrayListClass Class

%inline
public export
idrisSystemClass : String
idrisSystemClass = "io/github/mmhelloworld/idrisjvm/runtime/IdrisSystem"

%inline
public export
idrisListClass : String
idrisListClass = "io/github/mmhelloworld/idrisjvm/runtime/IdrisList"

export
idrisListType : InferredType
idrisListType = IRef idrisListClass Class

%inline
public export
idrisNilClass : String
idrisNilClass = "io/github/mmhelloworld/idrisjvm/runtime/IdrisList$Nil"

export
idrisNilType : InferredType
idrisNilType = IRef idrisNilClass Class

%inline
public export
idrisConsClass : String
idrisConsClass = "io/github/mmhelloworld/idrisjvm/runtime/IdrisList$Cons"

export
idrisConsType : InferredType
idrisConsType = IRef idrisConsClass Class

%inline
public export
idrisNothingClass : String
idrisNothingClass = "io/github/mmhelloworld/idrisjvm/runtime/Maybe$Nothing"

export
idrisNothingType : InferredType
idrisNothingType = IRef idrisNothingClass Class

%inline
public export
idrisJustClass : String
idrisJustClass = "io/github/mmhelloworld/idrisjvm/runtime/Maybe$Just"

export
idrisJustType : InferredType
idrisJustType = IRef idrisJustClass Class

export
idrisMaybeType : InferredType
idrisMaybeType = IRef "io/github/mmhelloworld/idrisjvm/runtime/Maybe" Class

%inline
public export
functionsClass : String
functionsClass = "io/github/mmhelloworld/idrisjvm/runtime/Functions"

export
isPrimitive : InferredType -> Bool
isPrimitive IBool = True
isPrimitive IByte = True
isPrimitive IChar = True
isPrimitive IShort = True
isPrimitive IInt = True
isPrimitive ILong = True
isPrimitive IFloat = True
isPrimitive IDouble = True
isPrimitive _ = False

%inline
public export
getRuntimeClass : String -> String
getRuntimeClass name = "io/github/mmhelloworld/idrisjvm/runtime/" ++ name

%inline
public export
delayedClass : String
delayedClass = getRuntimeClass "Delayed"

export
delayedType : InferredType
delayedType = IRef delayedClass Interface

%inline
public export
idrisObjectClass : String
idrisObjectClass = getRuntimeClass "IdrisObject"

%inline
public export
arraysClass : String
arraysClass = getRuntimeClass "Arrays"

%inline
public export
refClass : String
refClass = getRuntimeClass "Ref"

export
refType : InferredType
refType = IRef refClass Class

export
idrisObjectType : InferredType
idrisObjectType = IRef idrisObjectClass Interface

public export
isRefType : InferredType -> Bool
isRefType (IRef _ _) = True
isRefType _ = False

public export
Semigroup InferredType where
  IUnknown <+> ty2 = ty2
  ty1 <+> IUnknown = ty1
  ty1 <+> ty2 = if ty1 == ty2 then ty1 else inferredObjectType

%inline
public export
Monoid InferredType where
  neutral = IUnknown

export
iref : String -> InferredType
iref className =
  let isInterface = "i:" `isPrefixOf` className
      referenceType = if isInterface then Interface else Class
  in IRef (if isInterface then substr 2 (length className) className else className) referenceType

export
stripInterfacePrefix : InferredType -> InferredType
stripInterfacePrefix (IRef className _) = iref className
stripInterfacePrefix ty = ty

public export
%foreign "jvm:.startsWith(java/lang/String java/lang/String boolean),java/lang/String"
startsWith : String -> String -> Bool

export
%foreign
    jvm' "io/github/mmhelloworld/idrisjvm/assembler/IdrisName" "getIdrisConstructorClassName"
        "String" "String"
getIdrisConstructorClassName : String -> String

export
parse : String -> InferredType
parse "boolean" = IBool
parse "byte" = IByte
parse "char" = IChar
parse "short" = IShort
parse "int" = IInt
parse "long" = ILong
parse "float" = IFloat
parse "double" = IDouble
parse "String" = inferredStringType
parse "BigInteger" = inferredBigIntegerType
parse "void" = IVoid
parse "[" = assert_total $ idris_crash "Invalid type descriptor: ["
parse desc =
  if startsWith desc "["
    then IArray (parse (assert_total (strTail desc)))
    else iref desc

mutual
  createExtPrimTypeSpecFn : InferredFunctionType -> String
  createExtPrimTypeSpecFn (MkInferredFunctionType returnType parameterTypes) =
    showSep "⟶" (createExtPrimTypeSpec <$> parameterTypes) ++ "⟶" ++ createExtPrimTypeSpec returnType

  export
  createExtPrimTypeSpec : InferredType -> String
  createExtPrimTypeSpec IBool = "Bool"
  createExtPrimTypeSpec IByte = "Byte"
  createExtPrimTypeSpec IShort = "Short"
  createExtPrimTypeSpec IInt = "Int"
  createExtPrimTypeSpec IChar = "Char"
  createExtPrimTypeSpec ILong = "long"
  createExtPrimTypeSpec IFloat = "float"
  createExtPrimTypeSpec IDouble = "Double"
  createExtPrimTypeSpec IVoid = "void"
  createExtPrimTypeSpec (IRef ty Interface) = "i:" ++ ty
  createExtPrimTypeSpec (IRef ty _) = ty
  createExtPrimTypeSpec (IFunction (MkJavaLambdaType intf method methodType implementationType)) =
    "λ" ++ showSep "," [createExtPrimTypeSpec intf, method, createExtPrimTypeSpecFn methodType,
      createExtPrimTypeSpecFn implementationType]
  createExtPrimTypeSpec (IArray ty) = "[" ++ createExtPrimTypeSpec ty
  createExtPrimTypeSpec IUnknown = createExtPrimTypeSpec inferredObjectType

export
isObjectType : InferredType -> Bool
isObjectType IUnknown = True
isObjectType (IRef "java/lang/Object" _) = True
isObjectType _ = False
