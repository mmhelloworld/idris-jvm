module IdrisJvm.Data.IORef

import IdrisJvm.IO

export
data IORef a = MkIORef a

public export
interface HasReference (ffi : FFI) where
  newIORef' : a -> IO' ffi (IORef a)
  readIORef' : IORef a -> IO' ffi a
  writeIORef' : IORef a -> a -> IO' ffi ()

export
modifyIORef': HasReference ffi => IORef a -> (a -> a) -> IO' ffi ()
modifyIORef' ref fn =
  do
    val <- readIORef' ref
    writeIORef' ref (fn val)

export
Ref : Type
Ref = JVM_Native (Class ("io/github/mmhelloworld/idrisjvm/runtime/Ref"))

export
implementation HasReference FFI_JVM where
  newIORef' val = (MkIORef . believe_me) <$> FFI.new (Object -> JVM_IO Ref) (believe_me val)

  readIORef' (MkIORef ref) = believe_me <$> invokeInstance "getValue" (Ref -> JVM_IO Object) (believe_me ref)

  writeIORef' (MkIORef ref) val = invokeInstance "setValue" (Ref -> Object -> JVM_IO ()) (believe_me ref) (believe_me val)

||| Build a new IORef
export
newIORef : a -> JVM_IO (IORef a)
newIORef = newIORef'

||| read the value of an IORef
export
readIORef : IORef a -> JVM_IO a
readIORef = readIORef'

||| write the value of an IORef
export
writeIORef : IORef a -> a -> JVM_IO ()
writeIORef = writeIORef'

||| mutate the contents of an IORef
export
modifyIORef : IORef a -> (a -> a) -> JVM_IO ()
modifyIORef = modifyIORef'
