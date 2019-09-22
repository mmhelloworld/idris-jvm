module Java.Util

import IdrisJvm.IO
import Java.Lang
import Java.Util.Stream

%access public export

namespace Collection
  collectionClass : String
  collectionClass = "java/util/Collection"

  CollectionInterface : JVM_NativeTy
  CollectionInterface = Interface collectionClass

  Collection : Type
  Collection = JVM_Native CollectionInterface

  stream : Inherits Collection collection => collection -> JVM_IO JStream
  stream collection = invokeInstance "stream" (Collection -> JVM_IO JStream) (believe_me collection)

namespace Arrays

  ArraysClass : JVM_NativeTy
  ArraysClass = Class "java/util/Arrays"

  Arrays : Type
  Arrays = JVM_Native ArraysClass

  toString : Inherits ObjectArray that => that -> JVM_IO String
  toString arr = invokeStatic ArraysClass "toString" (ObjectArray -> JVM_IO String) (believe_me arr)

  toStringBooleanArray : JVM_Array Bool -> JVM_IO String
  toStringBooleanArray arr = invokeStatic ArraysClass "toString" (JVM_Array Bool -> JVM_IO String) arr

  toStringByteArray : JVM_Array Bits8 -> JVM_IO String
  toStringByteArray arr = invokeStatic ArraysClass "toString" (JVM_Array Bits8 -> JVM_IO String) arr

  toStringCharArray : JVM_Array Char -> JVM_IO String
  toStringCharArray arr = invokeStatic ArraysClass "toString" (JVM_Array Char -> JVM_IO String) arr

  toStringShortArray : JVM_Array Bits16 -> JVM_IO String
  toStringShortArray arr = invokeStatic ArraysClass "toString" (JVM_Array Bits16 -> JVM_IO String) arr

  toStringIntArray : JVM_Array Int -> JVM_IO String
  toStringIntArray arr = invokeStatic ArraysClass "toString" (JVM_Array Int -> JVM_IO String) arr

  toStringLongArray : JVM_Array Bits64 -> JVM_IO String
  toStringLongArray arr = invokeStatic ArraysClass "toString" (JVM_Array Bits64 -> JVM_IO String) arr

  toStringFloatArray : JVM_Array JFloat -> JVM_IO String
  toStringFloatArray arr = invokeStatic ArraysClass "toString" (JVM_Array JFloat -> JVM_IO String) arr

  toStringDoubleArray : JVM_Array Double -> JVM_IO String
  toStringDoubleArray arr = invokeStatic ArraysClass "toString" (JVM_Array Double -> JVM_IO String) arr

  deepToString : Inherits ObjectArray that => that -> JVM_IO String
  deepToString arr = invokeStatic ArraysClass "deepToString" (ObjectArray -> JVM_IO String) (believe_me arr)

namespace Iterator

  Iterator : Type
  Iterator = javaInterface "java/util/Iterator"

  hasNext : Inherits Iterator that => that -> JVM_IO Bool
  hasNext itr = invokeInstance "hasNext" (Iterator -> JVM_IO Bool) (believe_me itr)

  next : Inherits Iterator that => that -> JVM_IO Object
  next itr = invokeInstance "next" (Iterator -> JVM_IO Object) (believe_me itr)

  toList : Inherits Iterator itr => itr -> JVM_IO (List a)
  toList itr = do
    hasNext' <- hasNext itr
    if hasNext'
      then do
        item <- next itr
        rest <- toList itr
        pure (believe_me item :: rest)
      else pure []

namespace JList
  listClass: String
  listClass = "java/util/List"

  ListInterface : JVM_NativeTy
  ListInterface = Interface listClass

  JList : Type
  JList = JVM_Native ListInterface

  Inherits Collection JList where {}

  add : Inherits JList list => list -> a -> JVM_IO Bool
  add list item = invokeInstance "add" (JList -> Object -> JVM_IO Bool) (believe_me list) (believe_me item)

  size : Inherits JList list => list -> JVM_IO Nat
  size list = cast <$> invokeInstance "size" (JList -> JVM_IO Int) (believe_me list)

  iterator : Inherits JList list => list -> JVM_IO Iterator
  iterator list = invokeInstance "iterator" (JList -> JVM_IO Iterator) (believe_me list)

namespace ArrayList
  arrayListClass: String
  arrayListClass = "java/util/ArrayList"

  ArrayListClass : JVM_NativeTy
  ArrayListClass = Class arrayListClass

  ArrayList : Type
  ArrayList = JVM_Native ArrayListClass

  Inherits Collection ArrayList where {}
  Inherits JList ArrayList where {}

  new : JVM_IO ArrayList
  new = FFI.new (JVM_IO ArrayList)

  fromList : List a -> JVM_IO ArrayList
  fromList xs = do
      arrayList <- ArrayList.new
      fromList' arrayList xs
    where
      fromList' : ArrayList -> List a -> JVM_IO ArrayList
      fromList' arrayList [] = pure arrayList
      fromList' arrayList (x :: xs) = do
        JList.add arrayList x
        fromList' arrayList xs

namespace HashMap
  HashMap : Type
  HashMap = JVM_Native $ Class "java/util/HashMap"

  new : JVM_IO HashMap
  new = FFI.new (JVM_IO HashMap)

  get : Inherits Object key => HashMap -> key -> JVM_IO (Maybe Object)
  get this key = nullableToMaybe <$> invokeInstance "get" (HashMap -> Object -> JVM_IO Object) this (believe_me key)

  put : (Inherits Object key, Inherits Object value) => HashMap -> Maybe key -> Maybe value -> JVM_IO (Maybe Object)
  put this key value = nullableToMaybe <$> invokeInstance "put" (HashMap -> Object -> Object -> JVM_IO Object) this (maybeToNullable $ believe_me key) (maybeToNullable $ believe_me value)

namespace Objects

  ObjectsClass : JVM_NativeTy
  ObjectsClass = Class "java/util/Objects"

  toString : Inherits Object that => that -> JVM_IO String
  toString obj = invokeStatic ObjectsClass "toString" (Object -> JVM_IO String) (believe_me obj)

  isNull : Inherits Object that => that -> JVM_IO Bool
  isNull obj = invokeStatic ObjectsClass "isNull" (Object -> JVM_IO Bool) (believe_me obj)

namespace Scanner
    scannerClass : JVM_NativeTy
    scannerClass = Class "java/util/Scanner"

    Scanner : Type
    Scanner = JVM_Native scannerClass

    hasNext : Scanner -> JVM_IO Bool
    hasNext = invokeInstance "hasNext" (Scanner -> JVM_IO Bool)

    Inherits Closeable Scanner where {}
