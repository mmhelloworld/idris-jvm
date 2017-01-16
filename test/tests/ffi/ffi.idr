module Main

import IdrisJvm.FFI
import IdrisJvm.IO
import Java.Lang
import Java.Util
import Java.Math

IdrisThreadClass : JVM_NativeTy
IdrisThreadClass = Class "hello/IdrisThread"

IdrisThread : Type
IdrisThread = JVM_Native IdrisThreadClass

add : IdrisThread -> Int -> Int -> Int
add _ x y = x + y

exportedBoolToString : Bool -> String
exportedBoolToString = show

helloFromIdris : IdrisThread -> String -> JVM_IO String
helloFromIdris this name = do
  printLn "Hi "
  pure $ "Hello " ++ name

Inherits Thread IdrisThread where {}

setName : IdrisThread -> String -> JVM_IO ()
setName this name = invokeInstance "setName" (IdrisThread -> String -> JVM_IO ()) this name

getThreadName : IdrisThread -> JVM_IO String
getThreadName = invokeInstance "getThreadName" (IdrisThread -> JVM_IO String)

run : IdrisThread -> JVM_IO ()
run this = do
  threadName <- getThreadName this
  printLn !(helloFromIdris this threadName)

jmain : StringArray -> JVM_IO ()
jmain args = do
  printLn $ exportedBoolToString True
  Arrays.toString args >>= printLn

idrisThreadConstructor : IdrisThread -> JVM_IO ()
idrisThreadConstructor thread = printLn "hello from constructor"

idrisThreadWithNameConstructor : IdrisThread -> String -> JVM_IO ()
idrisThreadWithNameConstructor thread name = printLn $ "hello from constructor with name: " ++ name

newIdrisThread : JVM_IO IdrisThread
newIdrisThread = FFI.new (JVM_IO IdrisThread)

newIdrisThreadWithName : String -> JVM_IO IdrisThread
newIdrisThreadWithName = FFI.new (String -> JVM_IO IdrisThread)

startIdrisThread : IdrisThread -> JVM_IO ()
startIdrisThread thread = invokeInstance "start" (IdrisThread -> JVM_IO ()) thread

jadd : IdrisThread -> Int -> Int -> Int
jadd thread x y = unsafePerformIO $ invokeInstance "jadd" (IdrisThread -> Int -> Int -> JVM_IO Int) thread x y

jboolToString : Bool -> String
jboolToString b = unsafePerformIO $ invokeStatic IdrisThreadClass "boolToString" (Bool -> JVM_IO String) b

jhelloFromIdris : IdrisThread -> String -> JVM_IO String
jhelloFromIdris thread str = invokeInstance "helloFromIdris" (IdrisThread -> String -> JVM_IO String) thread str

emptyList : List Int
emptyList = []

cons : Int -> List Int -> List Int
cons x xs = x :: xs

append : List Int -> List Int -> List Int
append xs ys = xs ++ ys

showList : List Int -> String
showList = show

ListIntClass : JVM_NativeTy
ListIntClass = Class "hello/ListInt"

ListInt : Type
ListInt = JVM_Native ListIntClass

JExportTestClass : JVM_NativeTy
JExportTestClass = Class "hello/JExportTest"

JExportTest : Type
JExportTest = JVM_Native JExportTestClass

jappend : ListInt -> ListInt -> ListInt
jappend xs ys = unsafePerformIO $ invokeStatic JExportTestClass "append" (ListInt -> ListInt -> JVM_IO ListInt) xs ys

jcons : Int -> ListInt -> ListInt
jcons x xs = unsafePerformIO $ invokeStatic JExportTestClass "cons" (Int -> ListInt -> JVM_IO ListInt) x xs

jemptyList : ListInt
jemptyList = unsafePerformIO $ invokeStatic JExportTestClass "emptyList" (JVM_IO ListInt)

jshowList : ListInt -> String
jshowList xs = unsafePerformIO $ invokeStatic JExportTestClass "showList" (ListInt -> JVM_IO String) xs

joneToTen : ListInt
joneToTen = unsafePerformIO $ invokeStatic JExportTestClass "oneToTen" (JVM_IO ListInt)

Show ListInt where
  show = jshowList

setNumbers : IdrisThread -> ListInt -> JVM_IO ()
setNumbers thread nums = setInstanceField "numbers" (IdrisThread -> ListInt ->  JVM_IO ()) thread nums

getNumbers : IdrisThread -> JVM_IO (ListInt)
getNumbers = getInstanceField "numbers" (IdrisThread -> JVM_IO (ListInt))

setStaticNumbers : ListInt -> JVM_IO ()
setStaticNumbers nums = setStaticField IdrisThreadClass "numbersStatic" (ListInt ->  JVM_IO ()) nums

getStaticNumbers : JVM_IO (ListInt)
getStaticNumbers = getStaticField IdrisThreadClass "numbersStatic" (JVM_IO (ListInt))

oneToTen : List Int
oneToTen = [1..10]

-- currently only null is allowed for field initialization but the value can be set later using `setInstanceField`
-- and `setStaticField` functions. See above for the usage of those functions.
numbersField : List Int
numbersField = believe_me prim__null

-- To test passing nulls to Java using Maybe
nullableToString : Inherits Object that => Maybe that -> String
nullableToString obj = unsafePerformIO $ invokeStatic ObjectsClass "toString" (Maybe Object -> JVM_IO String) (believe_me obj)

-- To test passing null to Java for String using Maybe
equalsIgnoreCase : String -> Maybe String -> Bool
equalsIgnoreCase str1 str2 = unsafePerformIO $ invokeInstance "equalsIgnoreCase" (String -> Maybe String -> JVM_IO Bool) str1 str2

getName : Class -> String
getName clazz = unsafePerformIO $ invokeInstance "getName" (Class -> JVM_IO String) clazz

stringClassLit : Class
stringClassLit = classLit "java/lang/String"

intClassLit : Class
intClassLit = classLit "int"

booleanClassLit : Class
booleanClassLit = classLit "boolean"

byteClassLit : Class
byteClassLit = classLit "byte"

charClassLit : Class
charClassLit = classLit "char"

shortClassLit : Class
shortClassLit = classLit "short"

longClassLit : Class
longClassLit = classLit "long"

floatClassLit : Class
floatClassLit = classLit "float"

doubleClassLit : Class
doubleClassLit = classLit "double"

main : JVM_IO ()
main = do
  -- Test ffi calls
  printLn $ maxInt 3 5
  printLn $ maxFloat (Float 3.2) (Float 6.4)
  printLn $ maxDouble 7.5 6.8
  printLn $ boolToString $ parseBool "true"
  printLn $ charToString $ charAt "foobarbaz" 3
  printLn $ byteToString $ parseByte "127"
  printLn $ shortToString $ parseShort "32767"
  printLn $ longToString $ parseLong "9223372036854775807"

  printLn !(getProperty "foo")  -- Test returning null string from FFI call
  System.setProperty "foo" "fooval"
  printLn !(getProperty "foo")  -- Test returning a non-null string from FFI call
  hashMap <- HashMap.new
  printLn $ Objects.toString <$> !(HashMap.get hashMap "bar") -- Test returning null object as Nothing
  HashMap.put hashMap (Just "foo") (Just "fooval")
  printLn $ Objects.toString <$> !(HashMap.get hashMap "foo") -- Test returning a non-null object as Just

  printLn $ nullableToString (the (Maybe BigInteger) Nothing) -- Test passing Nothing as a null to FFI call
  bigOne <- BigInteger.new "1"
  printLn $ nullableToString (Just bigOne) -- Test passing a `Just` as a non null value to FFI call
  printLn $ equalsIgnoreCase "foo" (Just "foo") -- Test passing a `Just` as a non null String to FFI call
  printLn $ equalsIgnoreCase "foo" (the (Maybe String) Nothing) -- Test passing nothing as a null String to FFI call
  bigInt1 <- BigInteger.new "11111111111111111111"
  bigInt2 <- BigInteger.new "11111111111111111111"
  printLn $ BigInteger.toString $ BigInteger.add bigInt1 bigInt2
  jlist <- ArrayList.new
  printLn !(size jlist)
  -- Test exports
  thread1 <- newIdrisThread
  setName thread1 "idris-thread"
  startIdrisThread thread1
  printLn $ Main.add thread1 2 3
  printLn $ jadd thread1 3 5
  printLn $ jboolToString True
  printLn $ !(jhelloFromIdris thread1 "from idris")

  thread2 <- newIdrisThreadWithName "idris-thread-2"
  startIdrisThread thread2

  printLn $ jshowList $ jappend (jcons 3 (jcons 4 jemptyList)) (jcons 5 (jcons 6 jemptyList))

  setNumbers thread1 joneToTen  -- Test setting an instance field
  printLn !(getNumbers thread1) -- Test getting an instance field

  setStaticNumbers joneToTen -- Test setting a static field
  printLn !getStaticNumbers -- Test getting a static field

  printLn $ getName <$> [ stringClassLit, intClassLit, byteClassLit, charClassLit, shortClassLit, booleanClassLit
                        , longClassLit, floatClassLit, doubleClassLit ]

exports1 : FFI_Export FFI_JVM "hello/IdrisThread extends java/lang/Thread implements java/lang/Runnable" []
exports1 =
  Data (List Int) "hello/ListInt" $
  Fun numbersField (ExportInstanceField "numbers") $
  Fun numbersField (ExportStaticField "numbersStatic") $
  Fun exportedBoolToString (ExportStatic "boolToString") $
  Fun Main.add (ExportInstance "jadd") $
  Fun helloFromIdris ExportDefault $
  Fun idrisThreadConstructor Constructor $
  Fun idrisThreadWithNameConstructor Constructor $
  Fun run ExportDefault $
  Fun getThreadName (Super "getName") $
  Fun Main.jmain (ExportStatic "main") $
  End

exports2 : FFI_Export FFI_JVM "hello/JExportTest" []
exports2 =
  Data (List Int) "hello/ListInt" $
  Fun cons (ExportStatic "cons") $
  Fun emptyList (ExportStatic "emptyList") $
  Fun showList (ExportStatic "showList") $
  Fun append (ExportStatic "append") $
  Fun oneToTen (ExportStatic "oneToTen") $
  End
