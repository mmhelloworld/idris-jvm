module IdrisJvm.Files

import Java.Nio
import IdrisJvm.IO

%access public export

namespace Files
  FilesClass : JVM_NativeTy
  FilesClass = Class "io/github/mmhelloworld/idrisjvm/io/Files"

  Files : Type
  Files = JVM_Native FilesClass

  readFile : String -> JVM_IO (Either Throwable String)
  readFile = invokeStatic FilesClass "readFile" (String -> JVM_IO (Either Throwable String))

  writeFile : String -> String -> JVM_IO (Either Throwable ())
  writeFile = invokeStatic FilesClass "writeFile" (String -> String -> JVM_IO (Either Throwable ()))

  createDirectory : String -> JVM_IO (Either Throwable ())
  createDirectory = invokeStatic FilesClass "createDirectory" (String -> JVM_IO (Either Throwable ()))

  createDirectories : Path -> JVM_IO (Either Throwable ())
  createDirectories = invokeStatic FilesClass "createDirectories" (Path -> JVM_IO (Either Throwable ()))

  changeDir : String -> JVM_IO Bool
  changeDir = invokeStatic FilesClass "changeDir" (String -> JVM_IO Bool)

  getTemporaryFileName : JVM_IO String
  getTemporaryFileName = invokeStatic FilesClass "getTemporaryFileName" (JVM_IO String)

  chmod : String -> Int -> JVM_IO ()
  chmod = invokeStatic FilesClass "chmod" (String -> Int -> JVM_IO ())

  getWorkingDir : JVM_IO String
  getWorkingDir = invokeStatic FilesClass "getWorkingDir" (JVM_IO String)

  createPath : String -> JVM_IO Path
  createPath = invokeStatic FilesClass "createPath" (String -> JVM_IO Path)

