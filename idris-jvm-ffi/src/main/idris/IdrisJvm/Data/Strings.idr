module IdrisJvm.Data.Strings

import IdrisJvm.IO
import Java.Lang

%hide Prelude.Strings.StringBuffer

public export
StringsClass : JVM_NativeTy
StringsClass = Class "io/github/mmhelloworld/idrisjvm/runtime/Strings"

export
bytesLengthUtf8 : String -> JVM_IO Int
bytesLengthUtf8 = invokeStatic StringsClass "bytesLengthUtf8" (String -> JVM_IO Int)

export
StringBuffer : Type
StringBuffer = StringBuilder

export
newStringBuffer : (len : Int) -> JVM_IO StringBuffer
newStringBuffer len = StringBuilder.new len

export
addToStringBuffer : StringBuffer -> String -> JVM_IO ()
addToStringBuffer buffer str = StringBuilder.appendString buffer str >>= (\buf => pure ())

export
getStringFromBuffer : StringBuffer -> JVM_IO String
getStringFromBuffer buffer = StringBuilder.toString buffer
