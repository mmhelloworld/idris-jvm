module IdrisJvm.IO

import public IdrisJvm.FFI
import Java.Lang

%access public export

printLn : Show a => a -> JVM_IO ()
printLn = printLn'

putStrLn : String -> JVM_IO ()
putStrLn = putStrLn'

print : Show a => a -> JVM_IO ()
print = print'

putStr : String -> JVM_IO ()
putStr = putStr'

getLine : JVM_IO String
getLine = getLine'

putCh : Char -> JVM_IO ()
putCh ch = PrintStream.printCh System.stdout ch

getCh : JVM_IO Char
getCh = invokeStatic RuntimeClass "readChar" (JVM_IO Char)

putChar : Char -> JVM_IO ()
putChar ch = PrintStream.printCh System.stdout ch

getChar : JVM_IO Char
getChar = invokeStatic RuntimeClass "readChar" (JVM_IO Char)

jputChar : Char -> JVM_IO ()
jputChar ch = PrintStream.printCh System.stdout ch

jgetChar : JVM_IO Char
jgetChar = invokeStatic RuntimeClass "readChar" (JVM_IO Char)
