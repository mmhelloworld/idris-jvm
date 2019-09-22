module IdrisJvm.File

import Java.Util.Stream
import Java.Lang
import Java.Math
import IdrisJvm.JvmImport
import Data.Vect
import Java.Nio
import Java.IO
import Java.Util
import Java.Util.Concurrent
import IdrisJvm.ClientServerSocket
import IdrisJvm.FileChannelIo
import IdrisJvm.Files
import IdrisJvm.System

%hide Prelude.File.File
%hide Prelude.File.FileError
%hide Java.IO.File.File

public export
data File = MkFileStdin
          | MkFileStdout
          | MkFileStderr
          | MkFile FileChannelIo
          | MkFileClientServerSocket ClientServerSocket

public export
FileError : Type
FileError = Throwable

export
FileReadError : FileError
FileReadError = believe_me $ unsafePerformIO $ IOException.new "File Read Error"

export
GenericFileError : Int -> FileError
GenericFileError errNo = believe_me $ unsafePerformIO $ IOException.new $ "Generic File Error " ++ show errNo

export
unableToOpenFile : String -> JVM_IO IOException
unableToOpenFile fileName = IOException.new ("Unable to open file: " ++ fileName)

export
unableToReadFile : String -> JVM_IO IOException
unableToReadFile fileName = IOException.new ("Unable to read file: " ++ fileName)

export
unableToWriteFile : String -> JVM_IO IOException
unableToWriteFile fileName = IOException.new ("Unable to write file: " ++ fileName)

modeToOpenOption : Mode -> List OpenOption
modeToOpenOption Read              = [ StandardOpenOption.read ]
modeToOpenOption WriteTruncate     = believe_me <$> [ StandardOpenOption.create, StandardOpenOption.write,
                                        StandardOpenOption.truncateExisting ]
modeToOpenOption Append            = believe_me <$> [ StandardOpenOption.append ]
modeToOpenOption ReadWrite         = believe_me <$> [ StandardOpenOption.create, StandardOpenOption.read,
                                       StandardOpenOption.write ]
modeToOpenOption ReadWriteTruncate = believe_me <$> [ StandardOpenOption.create, StandardOpenOption.read,
                                       StandardOpenOption.write, StandardOpenOption.truncateExisting ]
modeToOpenOption ReadAppend        = believe_me <$> [ StandardOpenOption.read, StandardOpenOption.append ]

export
total
openFile : (f : String) -> (m : Mode) -> JVM_IO (Either FileError File)
openFile f m = assert_total $ do
      path <- Files.createPath f
      ensureParentDir path
      openOptions <- listToArray (modeToOpenOption m)
      exceptionOrFile <- FileChannelIo.open path openOptions
      case exceptionOrFile of
        Left exception => pure $ Left $ believe_me exception
        Right file => pure $ Right (MkFile file)
   where
     hasWriteMode : Bool
     hasWriteMode = case m of
        WriteTruncate => True
        ReadWrite => True
        ReadWriteTruncate => True
        _ => False

     ensureParentDir : Path -> JVM_IO ()
     ensureParentDir path =
       if hasWriteMode then do
         optParent <- nullableToMaybe <$> invokeInstance "getParent" (Path -> JVM_IO Path) path
         case optParent of
            Nothing => pure ()
            Just pathDir => do
              Files.createDirectories pathDir
              pure ()
       else pure ()

export
getChar : File -> JVM_IO Char
getChar (MkFile fileChannelIo) = getChar fileChannelIo
getChar (MkFileClientServerSocket clientServerSocket) = getChar clientServerSocket
getChar MkFileStdin = IO.getChar
getChar _ = do
    putStrLn "Unable to read a character"
    exit 1

export
getLine : File -> JVM_IO String
getLine (MkFile fileChannelIo) = getLine fileChannelIo
getLine (MkFileClientServerSocket clientServerSocket) = ClientServerSocket.getLine clientServerSocket
getLine MkFileStdin = IO.getLine
getLine _ = do
    putStrLn "Unable to read a line"
    exit 1

export
writeString : File -> String -> JVM_IO ()
writeString (MkFile fileChannelIo) str = writeString fileChannelIo str
writeString (MkFileClientServerSocket socket) str = writeString socket str
writeString MkFileStdout str = print str
writeString _ _ = pure ()

export
closeFile : File -> JVM_IO ()
closeFile (MkFile file) = FileChannelIo.close file
closeFile (MkFileClientServerSocket clientServerSocket) = ClientServerSocket.close clientServerSocket
closeFile _ = pure ()

export
changeDir : String -> JVM_IO Bool
changeDir = Files.changeDir

export
getTemporaryFileName : JVM_IO String
getTemporaryFileName = Files.getTemporaryFileName

export
chmod : String -> Int -> JVM_IO ()
chmod = Files.chmod

export
total
createDir : String -> JVM_IO (Either FileError ())
createDir d = assert_total $ Files.createDirectory d

export
currentDir : JVM_IO String
currentDir = Files.getWorkingDir

-- This is returning Int to conform to Idris fileSize function type
-- even though Java's FileChannel returns long
export
fileSize : File -> JVM_IO (Either FileError Int)
fileSize (MkFile f) = do
  exceptionOrSize <- FileChannelIo.size f
  case exceptionOrSize of
    Left exception => pure $ Left $ believe_me exception
    Right size => pure $ Right $ Long.intValue size
fileSize _ = pure . Left . believe_me $ !(IOException.new "Cannot determine size")

export
fflush : File -> JVM_IO ()
fflush (MkFile file) = FileChannelIo.flush file
fflush MkFileStdout = invokeStatic RuntimeClass "flushStdout" (JVM_IO ())
fflush MkFileStderr = invokeStatic RuntimeClass "flushStderr" (JVM_IO ())
fflush _ = pure ()

||| Standard input
export
stdin : File
stdin = MkFileStdin

||| Standard output
export
stdout : File
stdout = MkFileStdout

||| Standard error
export
stderr : File
stderr = MkFileStderr

export
total
readFile : String -> JVM_IO (Either FileError String)
readFile pathStr = assert_total $ Files.readFile pathStr

export
writeFile : String -> String -> JVM_IO (Either FileError ())
writeFile file content = Files.writeFile file content

export
fileModifiedTime : File -> JVM_IO (Either FileError Integer)
fileModifiedTime (MkFile file) = FileChannelIo.getFileModifiedTime file
fileModifiedTime _ = pure . Left $ believe_me !(IOException.new "Cannot get file modified time")

export
fileAccessTime : File -> JVM_IO (Either FileError Integer)
fileAccessTime (MkFile file) = FileChannelIo.getFileAccessTime file
fileAccessTime _ = pure . Left $ believe_me !(IOException.new "Cannot get file access time")

export
fileStatusTime : File -> JVM_IO (Either FileError Integer)
fileStatusTime (MkFile file) = FileChannelIo.getFileStatusTime file
fileStatusTime _ = pure . Left $ believe_me !(IOException.new "Cannot get file status time")

||| Check if a file handle has reached the end
export
fEOF : File -> JVM_IO Bool
fEOF (MkFile file) = FileChannelIo.isEof file
fEOF MkFileStdin = invokeStatic RuntimeClass "isStdinEof" (JVM_IO Bool)
fEOF _ = pure False

export
socketListenAndAccept : Int -> JVM_IO (Either String File)
socketListenAndAccept port = do
    byteBufferIoOrError <- ClientServerSocket.listenAndAccept port
    case byteBufferIoOrError of
        Left throwable => Left <$> Objects.toString throwable
        Right byteBufferIo => pure . Right $ MkFileClientServerSocket byteBufferIo

export
fRemove : (s : String) -> JVM_IO Bool
fRemove f = do
    path <- Files.createPath f
    deleteIfExists path

||| Read a line from a file
||| @h a file handle which must be open for reading
export
fGetLine : (h : File) -> JVM_IO (Either FileError String)
fGetLine file = do
  str <- getLine file
  pure $ Right str

||| Read up to a number of characters from a file
||| @h a file handle which must be open for reading
export
fGetChars : (h : File) -> (len : Int) -> JVM_IO (Either FileError String)
fGetChars file len = (Right . pack . reverse) <$> go [] len where
    go : List Char -> Int -> JVM_IO (List Char)
    go acc 0 = pure acc
    go acc n = do
        c <- getChar file
        go (c :: acc) (n - 1)

||| Write a line to a file
||| @h a file handle which must be open for writing
||| @str the line to write to the file
export
fPutStr : (h : File) -> (str : String) -> JVM_IO (Either FileError ())
fPutStr file s = Right <$> writeString file s
