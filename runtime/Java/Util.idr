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

namespace ArrayList

  ArrayList : Type
  ArrayList = javaClass "java/util/ArrayList"

  new : JVM_IO ArrayList
  new = FFI.new (JVM_IO ArrayList)

Inherits JList ArrayList where {}

namespace HashMap
  HashMap : Type
  HashMap = JVM_Native $ Class "java/util/HashMap"

  new : JVM_IO HashMap
  new = FFI.new (JVM_IO HashMap)

  get : Inherits Object key => HashMap -> key -> JVM_IO (Maybe Object)
  get this key = invokeInstance "get" (HashMap -> Object -> JVM_IO (Maybe Object)) this (believe_me key)

  put : (Inherits Object key, Inherits Object value) => HashMap -> Maybe key -> Maybe value -> JVM_IO (Maybe Object)
  put this key value = invokeInstance "put" (HashMap -> Maybe Object -> Maybe Object -> JVM_IO (Maybe Object)) this (believe_me key) (believe_me value)


namespace Objects

  ObjectsClass : JVM_NativeTy
  ObjectsClass = Class "java/util/Objects"

  -- Respects inheritance so that this function can be called on all types
  -- that extend java.lang.Object, which is everything.
  toString : Inherits Object that => that -> String
  toString obj = unsafePerformIO $ invokeStatic ObjectsClass "toString" (Object -> JVM_IO String) (believe_me obj)

  isNull : Inherits Object that => that -> Bool
  isNull obj = unsafePerformIO $ invokeStatic ObjectsClass "isNull" (Object -> JVM_IO Bool) (believe_me obj)
