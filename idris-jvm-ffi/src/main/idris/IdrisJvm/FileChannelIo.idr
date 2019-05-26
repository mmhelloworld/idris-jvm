module IdrisJvm.FileChannelIo

import IdrisJvm.IO
import Java.Nio

%access public export

namespace FileChannelIo
  FileChannelIoClass : JVM_NativeTy
  FileChannelIoClass = Class "io/github/mmhelloworld/idrisjvm/io/FileChannelIo"

  FileChannelIo : Type
  FileChannelIo = JVM_Native FileChannelIoClass

  open : Path -> JVM_Array OpenOption -> JVM_IO (Either Throwable FileChannelIo)
  open = invokeStatic FileChannelIoClass "open" (Path -> JVM_Array OpenOption -> JVM_IO (Either Throwable FileChannelIo))
  
  getChar : FileChannelIo -> JVM_IO Char
  getChar = invokeInstance "getChar" (FileChannelIo -> JVM_IO Char)
    
  getLine : FileChannelIo -> JVM_IO String
  getLine = invokeInstance "getLine" (FileChannelIo -> JVM_IO String)

  writeString : FileChannelIo -> String -> JVM_IO ()
  writeString = invokeInstance "writeString" (FileChannelIo -> String -> JVM_IO ())

  getPath : FileChannelIo -> JVM_IO Path
  getPath = invokeInstance "getPath" (FileChannelIo -> JVM_IO Path)

  isEof : FileChannelIo -> JVM_IO Bool
  isEof = invokeInstance "isEof" (FileChannelIo -> JVM_IO Bool)

  size : FileChannelIo -> JVM_IO (Either Throwable Bits64)
  size = invokeInstance "size" (FileChannelIo -> JVM_IO (Either Throwable Bits64))

  getFileModifiedTime : FileChannelIo -> JVM_IO (Either Throwable Integer)
  getFileModifiedTime = invokeInstance "getFileModifiedTime" (FileChannelIo -> JVM_IO (Either Throwable Integer))

  getFileAccessTime : FileChannelIo -> JVM_IO (Either Throwable Integer)
  getFileAccessTime = invokeInstance "getFileAccessTime" (FileChannelIo -> JVM_IO (Either Throwable Integer))

  getFileStatusTime : FileChannelIo -> JVM_IO (Either Throwable Integer)
  getFileStatusTime = invokeInstance "getFileStatusTime" (FileChannelIo -> JVM_IO (Either Throwable Integer))

  flush : FileChannelIo -> JVM_IO ()
  flush = invokeInstance "flush" (FileChannelIo -> JVM_IO ())

  close : FileChannelIo -> JVM_IO ()
  close = invokeInstance "close" (FileChannelIo -> JVM_IO ())

  Inherits Channel FileChannelIo where {}
  Inherits ReadableByteChannel FileChannelIo where {}
  Inherits WritableByteChannel FileChannelIo where {}
