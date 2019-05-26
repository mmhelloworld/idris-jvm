module IdrisJvm.ClientServerSocket

import IdrisJvm.IO
import Java.Nio

%access public export

namespace ClientServerSocket
  ClientServerSocketClass : JVM_NativeTy
  ClientServerSocketClass = Class "io/github/mmhelloworld/idrisjvm/io/ClientServerSocket"

  ClientServerSocket : Type
  ClientServerSocket = JVM_Native ClientServerSocketClass

  listenAndAccept : Int -> JVM_IO (Either Throwable ClientServerSocket)
  listenAndAccept =
    invokeStatic ClientServerSocketClass "listenAndAccept" (Int -> JVM_IO (Either Throwable ClientServerSocket))

  getChar : ClientServerSocket -> JVM_IO Char
  getChar = invokeInstance "getChar" (ClientServerSocket -> JVM_IO Char)

  getLine : ClientServerSocket -> JVM_IO String
  getLine = invokeInstance "getLine" (ClientServerSocket -> JVM_IO String)

  writeString : ClientServerSocket -> String -> JVM_IO ()
  writeString = invokeInstance "writeString" (ClientServerSocket -> String -> JVM_IO ())

  close : ClientServerSocket -> JVM_IO ()
  close = invokeInstance "close" (ClientServerSocket -> JVM_IO ())

  Inherits Channel ClientServerSocket where {}
  Inherits ReadableByteChannel ClientServerSocket where {}
  Inherits WritableByteChannel ClientServerSocket where {}

