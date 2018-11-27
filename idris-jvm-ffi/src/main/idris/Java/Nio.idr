module Java.Nio

import IdrisJvm.IO

%access public export

namespace OpenOption
    openOptionClass : String
    openOptionClass = "java/nio/file/OpenOption"

    OpenOption : Type
    OpenOption = JVM_Native (Interface openOptionClass)

namespace StandardOpenOption
    standardOpenOptionClass : String
    standardOpenOptionClass = "java/nio/file/StandardOpenOption"

    StandardOpenOptionClass : JVM_NativeTy
    StandardOpenOptionClass = Class standardOpenOptionClass

    StandardOpenOption : Type
    StandardOpenOption = JVM_Native StandardOpenOptionClass

    Inherits OpenOption StandardOpenOption where {}

    append : StandardOpenOption
    append = unsafePerformIO $ getStaticField StandardOpenOptionClass "APPEND" (JVM_IO StandardOpenOption)

    create : StandardOpenOption
    create = unsafePerformIO $ getStaticField StandardOpenOptionClass "CREATE" (JVM_IO StandardOpenOption)

    createNew : StandardOpenOption
    createNew = unsafePerformIO $ getStaticField StandardOpenOptionClass "CREATE_NEW" (JVM_IO StandardOpenOption)

    deleteOnClose: StandardOpenOption
    deleteOnClose = unsafePerformIO $ getStaticField StandardOpenOptionClass "DELETE_ON_CLOSE" (JVM_IO StandardOpenOption)

    dsync : StandardOpenOption
    dsync = unsafePerformIO $ getStaticField StandardOpenOptionClass "DSYNC" (JVM_IO StandardOpenOption)

    read : StandardOpenOption
    read = unsafePerformIO $ getStaticField StandardOpenOptionClass "READ" (JVM_IO StandardOpenOption)

    sparse : StandardOpenOption
    sparse = unsafePerformIO $ getStaticField StandardOpenOptionClass "SPARSE" (JVM_IO StandardOpenOption)

    sync : StandardOpenOption
    sync = unsafePerformIO $ getStaticField StandardOpenOptionClass "SYNC" (JVM_IO StandardOpenOption)

    truncateExisting : StandardOpenOption
    truncateExisting = unsafePerformIO $ getStaticField StandardOpenOptionClass "TRUNCATE_EXISTING" (JVM_IO StandardOpenOption)

    write : StandardOpenOption
    write = unsafePerformIO $ getStaticField StandardOpenOptionClass "WRITE" (JVM_IO StandardOpenOption)

namespace Path
    pathClass : String
    pathClass = "java/nio/file/Path"

    Path : Type
    Path = JVM_Native (Interface pathClass)

Show Path where
    show path = unsafePerformIO $ invokeInstance "toString" (Path -> JVM_IO String) path

namespace Files
    filesClass : String
    filesClass = "java/nio/file/Files"

    FilesClass : JVM_NativeTy
    FilesClass = Class filesClass

    Files : Type
    Files = JVM_Native FilesClass

namespace Files
    pathsClass : String
    pathsClass = "java/nio/file/Paths"

    PathsClass : JVM_NativeTy
    PathsClass = Class pathsClass

    Paths : Type
    Paths = JVM_Native PathsClass

namespace Channel
    channelClass : String
    channelClass = "java/nio/channels/Channel"

    ChannelClass : JVM_NativeTy
    ChannelClass = Interface channelClass

    Channel : Type
    Channel = JVM_Native ChannelClass

    close : Inherits Channel channel => channel -> JVM_IO ()
    close channel = invokeInstance "close" (Channel -> JVM_IO ()) (believe_me channel)

namespace SeekableByteChannel
    seekableByteChannelClass : String
    seekableByteChannelClass = "java/nio/channels/SeekableByteChannel"

    SeekableByteChannel : Type
    SeekableByteChannel = JVM_Native (Interface seekableByteChannelClass)

    Inherits Channel SeekableByteChannel where {}

 namespace FileChannel
     fileChannelClass : String
     fileChannelClass = "java/nio/channels/FileChannel"

     FileChannel : Type
     FileChannel = JVM_Native (Class fileChannelClass)

     Inherits Channel FileChannel where {}
     Inherits SeekableByteChannel FileChannel where {}

namespace FileAttribute
    fileAttributeClass : String
    fileAttributeClass = "java/nio/file/attribute/FileAttribute"

    FileAttribute : Type
    FileAttribute = JVM_Native (Interface fileAttributeClass)

namespace BasicFileAttributes
    basicFileAttributesClass : String
    basicFileAttributesClass = "java/nio/file/attribute/BasicFileAttributes"

    BasicFileAttributes : Type
    BasicFileAttributes = JVM_Native (Interface basicFileAttributesClass)

namespace LinkOption
    linkOptionClass : String
    linkOptionClass = "java/nio/file/LinkOption"

    LinkOption : Type
    LinkOption = JVM_Native (Class "java/nio/file/LinkOption")

namespace FileTime
    fileTimeClass : String
    fileTimeClass = "java/nio/file/attribute/FileTime"

    FileTime : Type
    FileTime = JVM_Native (Class fileTimeClass)

