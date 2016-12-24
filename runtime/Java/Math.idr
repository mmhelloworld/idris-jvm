module Java.Math

import IdrisJvm.FFI
import Java.Lang

%access public export

namespace BigInteger
  BigIntegerClass : JVM_NativeTy
  BigIntegerClass = Class "java/math/BigInteger"

  BigInteger : Type
  BigInteger = JVM_Native BigIntegerClass

  -- Constructor example
  new : String -> JVM_IO BigInteger
  new s = FFI.new (String -> JVM_IO BigInteger) s

  toString : BigInteger -> String
  toString bigInt = unsafePerformIO $ invokeInstance "toString" (BigInteger -> JVM_IO String) bigInt

  add : BigInteger -> BigInteger -> BigInteger
  add b1 b2 = unsafePerformIO $ invokeInstance "add" (BigInteger -> BigInteger -> JVM_IO BigInteger) b1 b2

Inherits Object BigInteger where {}
