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

%language TypeProviders
%language ElabReflection

%hide Prelude.File.File
%hide Prelude.File.FileError
%hide Java.IO.File.File

jdkimport [
    (stringClass, ["getBytes"]),
    (pathsClass, ["get"]),
    (fileChannelClass, ["open"]),
    (filesClass, ["lines", "write", "readAttributes", "createDirectory"]),
    (fileTimeClass, ["to"]),
    (streamClass, ["collect"]),
    (collectorsClass, ["joining"])
  ]

public export
data File = MkFileStdin
          | MkFileStdout
          | MkFileStderr
          | MkFile FileChannel Path

public export
FileError : Type
FileError = Throwable

export
FileReadError : FileError
FileReadError = believe_me $ unsafePerformIO $ IOException.new "File Read Error"

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
      path <- invokeStatic RuntimeClass "createPath" (String -> JVM_IO Path) f
      ensureParentDir path
      openOptions <- listToArray (modeToOpenOption m)
      exceptionOrFile <- (fileChannelClass <.> "open(java/nio/file/Path,Array 1 i java/nio/file/OpenOption)") path openOptions
      case exceptionOrFile of
        Left exception => pure $ Left $ believe_me exception
        Right Nothing => pure $ Left . believe_me $ !(unableToOpenFile f)
        Right (Just file) => pure $ Right (MkFile file path)
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
         optParent <- invokeInstance "getParent" (Path -> JVM_IO (Maybe Path)) path
         case optParent of
            Nothing => pure ()
            Just pathDir => do
              _ <- invokeStatic FilesClass "createDirectories" (Path -> JVM_Array FileAttribute -> JVM_IO Path) pathDir
                     !(newArray FileAttribute 0)
              pure ()
       else pure ()

export
closeFile : File -> JVM_IO ()
closeFile (MkFile file _) = Channel.close file
closeFile _ = pure ()

export
changeDir : String -> JVM_IO Bool
changeDir = invokeStatic RuntimeClass "changeDir" (String -> JVM_IO Bool)

export
getTemporaryFileName : JVM_IO String
getTemporaryFileName = invokeStatic RuntimeClass "getTemporaryFileName" (JVM_IO String)

export
chmod : String -> Int -> JVM_IO ()
chmod f m = invokeStatic RuntimeClass "chmod" (String -> Int -> JVM_IO ()) f m

export
total
createDir : String -> JVM_IO (Either FileError ())
createDir d = assert_total $ do
  path <- invokeStatic RuntimeClass "createPath" (String -> JVM_IO Path) d
  exceptionOrPath <- (filesClass <.> "createDirectory") path !(newArray FileAttribute 0)
  pure $ const () <$> exceptionOrPath

export
currentDir : JVM_IO String
currentDir = invokeStatic RuntimeClass "getWorkingDir" (JVM_IO String)

-- This is returning Int to conform to Idris fileSize function type
-- even though Java's FileChannel returns long
export
fileSize : File -> JVM_IO (Either FileError Int)
fileSize (MkFile f _) = do
  exceptionOrSize <- invokeInstance "size" (FileChannel -> JVM_IO (JVM_Throwable Bits64)) f
  case exceptionOrSize of
    Left exception => pure $ Left $ believe_me exception
    Right size => pure $ Right $ Long.intValue size
fileSize _ = pure . Left . believe_me $ !(IOException.new "Cannot determine size for stdin/stdout/stderr")

export
fflush : File -> JVM_IO ()
fflush (MkFile file _) = invokeInstance "force" (FileChannel -> Bool -> JVM_IO ()) file True
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
readFile pathStr = assert_total $ do
    path <- invokeStatic RuntimeClass "createPath" (String -> JVM_IO Path) pathStr
    exceptionOrlines <- (filesClass <.> "lines(java/nio/file/Path)") path
    case exceptionOrlines of
      Left exception => pure $ Left exception
      Right Nothing => pure $ Left . believe_me $ !(unableToReadFile pathStr)
      Right (Just lines) => do
        joiningCollector <- (collectorsClass <.!> "joining(java/lang/CharSequence)") $ believe_me !(lineSeparator)
        pure . Right $ believe_me !((streamClass <.!> "collect") lines joiningCollector)

export
writeFile : String -> String -> JVM_IO (Either FileError ())
writeFile file content = do
  path <- invokeStatic RuntimeClass "createPath" (String -> JVM_IO Path) file
  bytes <- (stringClass <.!> "getBytes(java/lang/String)") content "UTF-8"
  exceptionOrPath <- (filesClass <.> "write(java/nio/file/Path,Array 1 byte,Array 1 i java/nio/file/OpenOption)") path
            bytes !(newArray OpenOption 0)
  pure $ either (const . Left . believe_me $ !(unableToWriteFile file)) (const $ Right ()) exceptionOrPath

export
fileModifiedTime : File -> JVM_IO (Either FileError Integer)
fileModifiedTime (MkFile _ path) = do
    attrs <- (filesClass <.!> "readAttributes(java/nio/file/Path,java/lang/Class,Array 1 java/nio/file/LinkOption)")
               path (classLit basicFileAttributesClass) !(newArray LinkOption 0)
    fileTime <- invokeInstance "lastModifiedTime" (BasicFileAttributes -> JVM_IO FileTime) attrs
    seconds <- (fileTimeClass <.!> "to") fileTime TimeUnit.seconds
    pure . Right $ believe_me $ BigInteger.valueOf seconds
fileModifiedTime _ = pure . Left $ believe_me !(IOException.new "Cannot get file modified time for stdin, stdout or stderr")

export
fileAccessTime : File -> JVM_IO (Either FileError Integer)
fileAccessTime (MkFile _ path) = do
    attrs <- (filesClass <.!> "readAttributes(java/nio/file/Path,java/lang/Class,Array 1 java/nio/file/LinkOption)")
                path (classLit basicFileAttributesClass) !(newArray LinkOption 0)
    fileTime <- invokeInstance "lastAccessTime" (BasicFileAttributes -> JVM_IO FileTime) attrs
    seconds <- (fileTimeClass <.!> "to") fileTime TimeUnit.seconds
    pure . Right $ believe_me $ BigInteger.valueOf seconds
fileAccessTime _ = pure . Left $ believe_me !(IOException.new "Cannot get file access time for stdin, stdout or stderr")

export
fileStatusTime : File -> JVM_IO (Either FileError Integer)
fileStatusTime (MkFile _ path) = do
    attrs <- (filesClass <.!> "readAttributes(java/nio/file/Path,java/lang/Class,Array 1 java/nio/file/LinkOption)")
               path (classLit basicFileAttributesClass) !(newArray LinkOption 0)
    fileTime <- invokeInstance "creationTime" (BasicFileAttributes -> JVM_IO FileTime) attrs
    seconds <- (fileTimeClass <.!> "to") fileTime TimeUnit.seconds
    pure . Right $ believe_me $ BigInteger.valueOf seconds
fileStatusTime _ = pure . Left $ believe_me !(IOException.new "Cannot get file status time for stdin, stdout or stderr")

||| Check if a file handle has reached the end
export
fEOF : File -> JVM_IO Bool
fEOF (MkFile file _) = do
  position <- invokeInstance "position" (FileChannel -> JVM_IO Bits64) file
  size <- invokeInstance "size" (FileChannel -> JVM_IO Bits64) file
  pure (position == size)
fEOF MkFileStdin = invokeStatic RuntimeClass "isStdinEof" (JVM_IO Bool)
fEOF _ = pure False
