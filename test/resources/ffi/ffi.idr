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
  printLn "Hello from Idris"
  printLn !(helloFromIdris this "inside thread")
  setName this "idris-thread"
  printLn !(getThreadName this)

jmain : StringArray -> JVM_IO ()
jmain args = do
  printLn $ exportedBoolToString True
  Arrays.toString args >>= printLn

newIdrisThread : JVM_IO IdrisThread
newIdrisThread = FFI.new (JVM_IO IdrisThread)

startIdrisThread : IdrisThread -> JVM_IO ()
startIdrisThread thread = invokeInstance "start" (IdrisThread -> JVM_IO ()) thread

jadd : IdrisThread -> Int -> Int -> Int
jadd thread x y = unsafePerformIO $ invokeInstance "jadd" (IdrisThread -> Int -> Int -> JVM_IO Int) thread x y

jboolToString : Bool -> String
jboolToString b = unsafePerformIO $ invokeStatic IdrisThreadClass "boolToString" (Bool -> JVM_IO String) b

jhelloFromIdris : IdrisThread -> String -> JVM_IO String
jhelloFromIdris thread str = invokeInstance "helloFromIdris" (IdrisThread -> String -> JVM_IO String) thread str

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
  printLn !(getProperty "idris_jvm_ffi_invalid_prop" "foo")
  bigInt1 <- BigInteger.new "11111111111111111111"
  bigInt2 <- BigInteger.new "11111111111111111111"
  printLn $ BigInteger.toString $ BigInteger.add bigInt1 bigInt2
  jlist <- ArrayList.new
  printLn !(size jlist)

  -- Test exports
  thread <- newIdrisThread
  startIdrisThread thread
  printLn $ Main.add thread 2 3
  printLn $ jadd thread 3 5
  printLn $ jboolToString True
  printLn $ !(jhelloFromIdris thread "from idris")

exports : FFI_Export FFI_JVM "hello/IdrisThread extends java/lang/Thread implements java/lang/Runnable" []
exports =
  Fun exportedBoolToString (ExportStatic "boolToString") $
  Fun Main.add (ExportInstance "jadd") $
  Fun helloFromIdris ExportDefault $
  Fun run ExportDefault $
  Fun getThreadName (Super "getName") $
  Fun Main.jmain (ExportStatic "main") $
  End
