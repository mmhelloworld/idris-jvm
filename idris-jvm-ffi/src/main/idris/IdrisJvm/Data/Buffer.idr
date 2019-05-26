module IdrisJvm.Data.Buffer

import IdrisJvm.IO
import IdrisJvm.File
import Java.Nio
import IdrisJvm.FileChannelIo
import IdrisJvm.ClientServerSocket

%hide Prelude.File.File

-- JVM version of Idris Data.Buffer

namespace IdrisBuffer
  idrisBufferClass : String
  idrisBufferClass = "io/github/mmhelloworld/idrisjvm/runtime/IdrisBuffer"

  IdrisBufferClass : JVM_NativeTy
  IdrisBufferClass = Class idrisBufferClass

  IdrisBuffer : Type
  IdrisBuffer = JVM_Native IdrisBufferClass

||| A buffer is a pointer to a sized, unstructured, mutable chunk of memory.
||| There are primitive operations for getting and setting bytes, ints (32 bit)
||| and strings at a location in the buffer. These operations silently fail
||| if the location is out of bounds, so bounds checking should be done in
||| advance.
export
record Buffer where
  constructor MkBuffer
  ||| Raw bytes, as a pointer to a block of memory
  rawdata : IdrisBuffer
  ||| Cached size of block
  buf_size : Int
  ||| Next location to read/write (e.g. when reading from file)
  location : Int

||| Create a new buffer 'size' bytes long. Returns 'Nothing' if allocation
||| fails
export
newBuffer : (size : Int) -> JVM_IO (Maybe Buffer)
newBuffer size = do bptr <- FFI.new (Int -> JVM_IO IdrisBuffer) size
                    pure (Just (MkBuffer bptr size 0))

||| Reset the 'next location' pointer of the buffer to 0.
||| The 'next location' pointer gives the location for the next file read/write
||| so resetting this means you can write it again
export
resetBuffer : Buffer -> Buffer
resetBuffer buf = record { location = 0 } buf

||| Return the space available in the buffer
export
rawSize : Buffer -> JVM_IO Int
rawSize b = invokeInstance "size" (IdrisBuffer -> JVM_IO Int) (rawdata b)

export
size : Buffer -> Int
size b = buf_size b

||| Set the byte at position 'loc' to 'val'.
||| Does nothing if the location is outside the bounds of the buffer
export
setByte : Buffer -> (loc : Int) -> (val : Bits8) -> JVM_IO ()
setByte b loc val = invokeInstance "setByte" (IdrisBuffer -> Int -> Bits8 -> JVM_IO ()) (rawdata b) loc val

||| Set the int at position 'loc' to 'val'.
||| Uses 4 bytes (assumes up to 32 bit Int).
||| Does nothing if the location is outside the bounds of the buffer
export
setInt : Buffer -> (loc : Int) -> (val : Int) -> JVM_IO ()
setInt b loc val = invokeInstance "setInt" (IdrisBuffer -> Int -> Int -> JVM_IO ()) (rawdata b) loc val

||| Set the double at position 'loc' to 'val'.
||| Uses 8 bytes (assumes 64 bit double).
||| Does nothing if the location is outside the bounds of the buffer
export
setDouble : Buffer -> (loc : Int) -> (val : Double) -> JVM_IO ()
setDouble b loc val = invokeInstance "setDouble" (IdrisBuffer -> Int -> Double -> JVM_IO ()) (rawdata b) loc val

||| Set the byte at position 'loc' to 'val'.
||| Does nothing if the location is out of bounds of the buffer, or the string
||| is too long for the location
export
setString : Buffer -> Int -> String -> JVM_IO ()
setString b loc val
    = do invokeInstance "setString" (IdrisBuffer -> Int -> String -> JVM_IO ()) (rawdata b) loc val
         pure ()

||| Copy data from 'src' to 'dest'. Reads 'len' bytes starting at position
||| 'start' in 'src', and writes them starting at position 'loc' in 'dest'.
||| Does nothing if a location is out of bounds, or there is not enough room
export
copyData : (src : Buffer) -> (start, len : Int) ->
           (dest : Buffer) -> (loc : Int) -> JVM_IO ()
copyData src start len dest loc
    = invokeInstance "copy" (IdrisBuffer -> Int -> Int -> IdrisBuffer -> Int -> JVM_IO ())
              (rawdata src) start len (rawdata dest) loc

||| Return the value at the given location in the buffer.
||| Returns 0 if out of bounds.
export
getByte : Buffer -> (loc : Int) -> JVM_IO Bits8
getByte b loc = invokeInstance "getByte" (IdrisBuffer -> Int -> JVM_IO Bits8) (rawdata b) loc

||| Return the value at the given location in the buffer, assuming 4
||| bytes to store the Int.
||| Returns 0 if out of bounds.
export
getInt : Buffer -> (loc : Int) -> JVM_IO Int
getInt b loc = invokeInstance "getInt" (IdrisBuffer -> Int -> JVM_IO Int) (rawdata b) loc

||| Return the value at the given location in the buffer, assuming 8
||| bytes to store the Double.
||| Returns 0 if out of bounds.
export
getDouble : Buffer -> (loc : Int) -> JVM_IO Double
getDouble b loc = invokeInstance "getDouble" (IdrisBuffer -> Int -> JVM_IO Double) (rawdata b) loc

||| Return the string at the given location in the buffer, with the given
||| length. Returns "" if out of bounds.
export
getString : Buffer -> (loc : Int) -> (len : Int) -> JVM_IO String
getString b loc len = invokeInstance "getString" (IdrisBuffer -> Int -> Int -> JVM_IO String) (rawdata b) loc len

export
readBufferFromReadableByteChannel :
    Inherits ReadableByteChannel channel => channel -> Buffer -> (maxbytes : Int) -> JVM_IO Buffer
readBufferFromReadableByteChannel channel buf max = do
    numread <- invokeInstance "readFromFile" (IdrisBuffer -> ReadableByteChannel -> Int -> Int -> JVM_IO Int)
                       (rawdata buf) (believe_me channel) (location buf) max
    pure (record { location $= (+numread) } buf)

export
writeBufferToWritableByteChannel :
    Inherits WritableByteChannel channel => channel -> Buffer -> (maxbytes : Int) -> JVM_IO Buffer
writeBufferToWritableByteChannel channel buf max = do
    let maxwrite = size buf - location buf
    let max' = if maxwrite < max then maxwrite else max
    invokeInstance "writeToFile" (IdrisBuffer -> WritableByteChannel -> Int -> Int -> JVM_IO ())
            (rawdata buf) (believe_me channel) (location buf) max'
    pure (record { location $= (+max') } buf)

||| Read 'maxbytes' into the buffer from a file, returning a new
||| buffer with the 'locaton' pointer moved along
export
total
readBufferFromFile : File -> Buffer -> (maxbytes : Int) -> JVM_IO Buffer
readBufferFromFile (MkFile fileChannelIo) buf max =
    readBufferFromReadableByteChannel fileChannelIo buf max
readBufferFromFile (MkFileClientServerSocket clientServerSocket) buf max =
    readBufferFromReadableByteChannel clientServerSocket buf max
readBufferFromFile _ buf max = pure buf

||| Write 'maxbytes' from the buffer from a file, returning a new
||| buffer with the 'location' pointer moved along
export
total
writeBufferToFile : File -> Buffer -> (maxbytes : Int) -> JVM_IO Buffer
writeBufferToFile (MkFile channel) buf max = writeBufferToWritableByteChannel channel buf max
writeBufferToFile (MkFileClientServerSocket channel) buf max = writeBufferToWritableByteChannel channel buf max
writeBufferToFile _ buf max = pure buf

||| Return the contents of the buffer as a list
export
bufferData : Buffer -> JVM_IO (List Bits8)
bufferData b = do let len = size b
                  unpackTo [] len
  where unpackTo : List Bits8 -> Int -> JVM_IO (List Bits8)
        unpackTo acc 0 = pure acc
        unpackTo acc loc = do val <- getByte b (loc - 1)
                              unpackTo (val :: acc)
                                       (assert_smaller loc (loc - 1))

||| Create a new buffer, copying the contents of the old buffer to the new.
||| Returns 'Nothing' if resizing fails
export
resizeBuffer : Buffer -> Int -> JVM_IO (Maybe Buffer)
resizeBuffer old newsize
    = do Just buf <- newBuffer newsize
              | Nothing => pure Nothing
         -- If the new buffer is smaller than the old one, just copy what
         -- fits
         let oldsize = size old
         let len = if newsize < oldsize then newsize else oldsize
         copyData old 0 len buf 0
         pure (Just buf)
