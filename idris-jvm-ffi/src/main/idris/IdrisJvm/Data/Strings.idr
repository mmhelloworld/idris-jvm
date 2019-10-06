module IdrisJvm.Data.Strings

import IdrisJvm.IO

public export
StringsClass : JVM_NativeTy
StringsClass = Class "io/github/mmhelloworld/idrisjvm/runtime/Strings"

export
bytesLengthUtf8 : String -> JVM_IO Int
bytesLengthUtf8 = invokeStatic StringsClass "bytesLengthUtf8" (String -> JVM_IO Int)
