module Java.IO

import IdrisJvm.IO
import Java.Lang

%access public export

namespace File
    fileClass : String
    fileClass = "java/io/File"

    FileClass : JVM_NativeTy
    FileClass = Class fileClass

    File : Type
    File = JVM_Native FileClass

namespace IOException

  IOException : Type
  IOException = JVM_Native (Class "java/io/IOException")

  new : String -> JVM_IO IOException
  new = FFI.new (String -> JVM_IO IOException)

namespace OutputStream

  OutputStream : Type
  OutputStream = JVM_Native (Class "java/io/OutputStream")

  Inherits Closeable OutputStream where {}

  Inherits Flushable OutputStream where {}

namespace BufferedReader
    BufferedReader : Type
    BufferedReader = JVM_Native (Class "java/io/BufferedReader")
