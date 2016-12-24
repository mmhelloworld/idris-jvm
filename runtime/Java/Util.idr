module Java.Util

import IdrisJvm.FFI
import Java.Lang

%access public export

namespace Arrays

  ArraysClass : JVM_NativeTy
  ArraysClass = Class "java/util/Arrays"

  Arrays : Type
  Arrays = JVM_Native ArraysClass

  toString : Inherits ObjectArray that => that -> JVM_IO String
  toString arr = invokeStatic ArraysClass "toString" (ObjectArray -> JVM_IO String) (believe_me arr)

namespace List
  JList : Type
  JList = javaInterface "java/util/List"

  size : Inherits JList list => list -> JVM_IO Nat
  size list = cast <$> invokeInstance "size" (JList -> JVM_IO Int) (believe_me list)

Inherits Object JList where {}

namespace ArrayList

  ArrayList : Type
  ArrayList = javaClass "java/util/ArrayList"

  new : JVM_IO ArrayList
  new = FFI.new (JVM_IO ArrayList)

Inherits JList ArrayList where {}
Inherits Object ArrayList where {}

namespace Objects

  ObjectsClass : JVM_NativeTy
  ObjectsClass = Class "java/util/Objects"

  -- Respects inheritance so that this function can be called on all types
  -- that extend java.lang.Object, which is everything.
  toString : Inherits Object that => that -> String
  toString obj = unsafePerformIO $ invokeStatic ObjectsClass "toString" (Object -> JVM_IO String) (believe_me obj)
