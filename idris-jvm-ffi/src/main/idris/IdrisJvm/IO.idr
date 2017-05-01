module IdrisJvm.IO

import public IdrisJvm.FFI
import Java.Lang

%access public export

printLn : Show a => a -> JVM_IO ()
printLn = printLn'

putStrLn : String -> JVM_IO ()
putStrLn = putStrLn'

getLine : JVM_IO String
getLine = getLine'

putCh : Char -> JVM_IO ()
putCh ch = PrintStream.printCh !System.out ch

getCh : JVM_IO Char
getCh = invokeStatic RuntimeClass "readChar" (JVM_IO Char)
