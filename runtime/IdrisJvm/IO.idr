module IdrisJvm.IO

import public IdrisJvm.FFI

%access public export

printLn : Show a => a -> JVM_IO ()
printLn = printLn'

putStrLn : String -> JVM_IO ()
putStrLn = putStrLn'

getLine : JVM_IO String
getLine = getLine'
