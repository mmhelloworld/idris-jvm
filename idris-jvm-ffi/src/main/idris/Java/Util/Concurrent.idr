module Java.Util.Concurrent

import IdrisJvm.FFI

%access public export

namespace TimeUnit
  TimeUnitClass : JVM_NativeTy
  TimeUnitClass = Class "java/util/concurrent/TimeUnit"

  TimeUnit : Type
  TimeUnit = JVM_Native TimeUnitClass

  microseconds : TimeUnit
  microseconds = unsafePerformIO $ getStaticField TimeUnitClass "MICROSECONDS" (JVM_IO TimeUnit)

  sleep : TimeUnit -> Bits64 -> JVM_IO ()
  sleep timeunit timeout = invokeInstance "sleep" (TimeUnit -> Bits64 -> JVM_IO ()) timeunit timeout