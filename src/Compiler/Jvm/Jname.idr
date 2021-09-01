module Compiler.Jvm.Jname

import Core.Name
import Data.List

public export
data Jname = Jqualified String String
           | Jsimple String

export
className : Jname -> String
className (Jqualified cname _) = cname
className (Jsimple _) = "main/Main"

export
methodName : Jname -> String
methodName (Jqualified _ mname) = mname
methodName (Jsimple mname) = mname

export
assemblerClass : String -> String
assemblerClass name = "io/github/mmhelloworld/idrisjvm/assembler/" ++ name

export
cleanupIdentifier : String -> String
cleanupIdentifier value = unsafePerformIO $
    jvmStatic String "io/github/mmhelloworld/idrisjvm/assembler/IdrisName.transformCharacters" [value]

replace : Char -> Char -> String -> String
replace sourceChar replacementChar string = unsafePerformIO $
    jvmInstance String "java/lang/String.replace" [string, sourceChar, replacementChar]

export
getSimpleName : Jname -> String
getSimpleName (Jsimple n) = n
getSimpleName (Jqualified q n) = q ++ "/" ++ n

export
implementation Eq Jname where
    name1 == name2 = getSimpleName name1 == getSimpleName name2

export
implementation Ord Jname where
  compare name1 name2 = compare (getSimpleName name1) (getSimpleName name2)

export
implementation Show Jname where
    show = getSimpleName

export
jvmName : Name -> Jname
jvmName (NS ns n) = Jqualified (replace '$' '/' (cleanupIdentifier $ showNSWithSep "$" ns)) $ getSimpleName (jvmName n)
jvmName (UN n) = Jsimple $ cleanupIdentifier n
jvmName (MN n i) = Jsimple $ cleanupIdentifier n ++ "$" ++ show i
jvmName (PV n d) = Jsimple $ "$patvar" ++ getSimpleName (jvmName n)
jvmName (DN str n) = Jsimple $ cleanupIdentifier str ++ getSimpleName (jvmName n)
jvmName (RF str) = Jsimple $ cleanupIdentifier str
jvmName (Nested (i, x) n) = Jsimple $ "$nested" ++ show i ++ "$" ++ show x ++ "$" ++ getSimpleName (jvmName n)
jvmName (CaseBlock x y) = Jsimple $ "$case" ++ cleanupIdentifier (show x) ++ "$" ++ show y
jvmName (WithBlock x y) = Jsimple $ "$with" ++ cleanupIdentifier (show x) ++ "$" ++ show y
jvmName (Resolved i) = Jsimple $ "$resolved" ++ show i

export
jvmSimpleName : Name -> String
jvmSimpleName = getSimpleName . jvmName

jvmIdrisMainMethodName : String
jvmIdrisMainMethodName = "jvm$idrisMain"

jvmIdrisMainClass : String -> String
jvmIdrisMainClass rootPackage = rootPackage ++ "/Main"

export
idrisMainFunctionName : String -> Name
idrisMainFunctionName rootPackage = NS (mkNamespace $ rootPackage ++ ".Main") (UN jvmIdrisMainMethodName)