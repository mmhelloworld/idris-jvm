module Main

import IdrisJvm.IO
import IdrisJvm.File

%hide Prelude.File.File
%hide Prelude.File.FileError
%hide Prelude.File.openFile

mwhile : (test : JVM_IO Bool) -> (body : JVM_IO ()) -> JVM_IO ()
mwhile t b = do v <- t
                case v of
                     True => do b
                                mwhile t b
                     False => pure ()

dumpFile : String -> JVM_IO ()
dumpFile fn = do { Right h <- openFile fn Read
                   mwhile (do { -- putStrLn "TEST"
                                x <- fEOF h
                                pure (not x) })
                          (do { Right l <- fGetLine h
                                putStrLn l })
                   closeFile h }

main : JVM_IO ()
main = do { Right h <- openFile "testfile" WriteTruncate
            fPutStr h "Hello!\nWorld!\n...\n3\n4\nLast line\n"
            closeFile h
            putStrLn "Reading testfile"
            Right f <- readFile "testfile"
            putStrLn f
            putStrLn "---"
            dumpFile "testfile"
            putStrLn "---"
          }

