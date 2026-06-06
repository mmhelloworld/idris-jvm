module Compiler.Jvm.Jname

import Core.Name
import Data.List

public export
data Jname = Jqualified String String
           | Jsimple String

%inline
export
assemblerClass : String -> String
assemblerClass name = "io/github/mmhelloworld/idrisjvm/assembler/" ++ name

%foreign "jvm:transformCharacters,io/github/mmhelloworld/idrisjvm/assembler/IdrisName"
export
cleanupIdentifier : String -> String

export
%foreign "jvm:.replace,java/lang/String"
replace : String -> Char -> Char -> String

public export
%foreign "jvm:getStackTraceString(String),io/github/mmhelloworld/idrisjvm/runtime/Runtime"
getStackTraceString : PrimIO String

getSimpleNameWithSep : String -> Jname -> String
getSimpleNameWithSep _ (Jsimple n) = n
getSimpleNameWithSep sep (Jqualified q n) = q ++ sep ++ n

export
getSimpleName : Jname -> String
getSimpleName = getSimpleNameWithSep "/"

export
Eq Jname where
    name1 == name2 = getSimpleName name1 == getSimpleName name2

export
Ord Jname where
  compare name1 name2 = compare (getSimpleName name1) (getSimpleName name2)

export
Show Jname where
    show = getSimpleName

export
className : Jname -> String
className (Jqualified cname _) = cname
className name = "Main" -- For Idris functions like `csegen`

export
methodName : Jname -> String
methodName (Jqualified _ mname) = mname
methodName (Jsimple mname) = mname

%foreign
    "jvm:getIdrisFunctionName(String String String io/github/mmhelloworld/idrisjvm/runtime/IdrisList),io/github/mmhelloworld/idrisjvm/assembler/IdrisName"
jgetIdrisFunctionName : String -> String -> String -> List String

export
getIdrisFunctionName : String -> String -> String -> Jname
getIdrisFunctionName programName moduleName idrisFunctionName =
    case jgetIdrisFunctionName programName moduleName idrisFunctionName of
        (className :: functionName :: _) => Jqualified className functionName
        _ => Jqualified moduleName idrisFunctionName

jvmName' : Name -> Jname
jvmName' (NS ns n) =
  Jqualified (replace (cleanupIdentifier $ showNSWithSep "$" ns) '$' '/') $ getSimpleNameWithSep "$" (jvmName' n)
jvmName' (UN n) = Jsimple $ cleanupIdentifier (displayUserName n)
jvmName' (MN n i) = Jsimple $ cleanupIdentifier n ++ "$" ++ show i
jvmName' (PV n d) = Jsimple $ "$p" ++ getSimpleNameWithSep "$" (jvmName' n)
jvmName' (DN str n) = Jsimple $ cleanupIdentifier str ++ "$" ++ getSimpleNameWithSep "$" (jvmName' n)
jvmName' (Nested (i, x) n) = Jsimple $ "$n" ++ show i ++ "$" ++ show x ++ "$" ++ getSimpleNameWithSep "$" (jvmName' n)
jvmName' (CaseBlock x y) = Jsimple $ "$c" ++ cleanupIdentifier (show x) ++ "$" ++ show y
jvmName' (WithBlock x y) = Jsimple $ "$w" ++ cleanupIdentifier (show x) ++ "$" ++ show y
jvmName' (Resolved i) = Jsimple $ "$r" ++ show i

%foreign "jvm:.contains(java/lang/String java/lang/CharSequence boolean),java/lang/String"
contains: String -> String -> Bool

export
jvmName : String -> Name -> Jname
jvmName programName idrisName =
  let jname = jvmName' idrisName
      fnName = methodName jname
      isExtracted = contains fnName "$idrisjvm$extr" || contains fnName "$sp"
  in if isExtracted then jname else getIdrisFunctionName programName (className jname) fnName

export
jvmSimpleName : Name -> String
jvmSimpleName = getSimpleName . jvmName'

%inline
jvmIdrisMainMethodName : String
jvmIdrisMainMethodName = "jvm$idrisMain"

export
idrisMainFunctionName : String -> Name
idrisMainFunctionName rootPackage = NS (mkNamespace $ rootPackage ++ ".Main") (UN $ Basic jvmIdrisMainMethodName)

export
getIdrisName : Jname -> Name
getIdrisName jname =
  let className = replace (className jname) '/' '.'
  in NS (mkNamespace className) (UN $ Basic (methodName jname))
