module Java.Util.Stream

import IdrisJvm.IO
import Java.Lang
import Java.Util.Function

%access public export

namespace Collector
  Collector : Type
  Collector = javaInterface "java/util/stream/Collector"

namespace Collectors
  CollectorsClass : JVM_NativeTy
  CollectorsClass = Class "java/util/stream/Collectors"

  Collectors : Type
  Collectors = JVM_Native CollectorsClass

  toList : JVM_IO Collector
  toList = invokeStatic CollectorsClass "toList" (JVM_IO Collector)

namespace JStream
  JStreamInterface : JVM_NativeTy
  JStreamInterface = Interface "java/util/stream/Stream"

  JStream : Type
  JStream = JVM_Native JStreamInterface

  map : Inherits JStream stream => Function -> stream -> JVM_IO JStream
  map f stream = invokeInstance "map" (JStream -> Function -> JVM_IO JStream) (believe_me stream) f

  filter : Inherits JStream stream => Predicate -> stream -> JVM_IO JStream
  filter predicate stream = invokeInstance "filter" (JStream -> Predicate -> JVM_IO JStream) (believe_me stream) predicate

  collect : (Inherits JStream stream, Inherits Collector collector) => collector -> stream -> JVM_IO Object
  collect collector stream = invokeInstance "collect" (JStream -> Collector -> JVM_IO Object) (believe_me stream) (believe_me collector)

  generate : Inherits Supplier supplier => supplier -> JVM_IO JStream
  generate supplier = invokeStatic JStreamInterface "generate" (Supplier -> JVM_IO JStream) (believe_me supplier)

  limit : Inherits JStream stream => Bits64 -> stream -> JVM_IO JStream
  limit maxSize stream = invokeInstance "limit" (JStream -> Bits64 -> JVM_IO JStream) (believe_me stream) maxSize

namespace IntStream
  IntStreamInterface : JVM_NativeTy
  IntStreamInterface = Interface "java/util/stream/IntStream"

  IntStream : Type
  IntStream = JVM_Native IntStreamInterface

  map : (Inherits IntStream stream, Inherits IntUnaryOperator operator) => operator -> stream -> JVM_IO IntStream
  map operator stream = invokeInstance "map" (IntStream -> IntUnaryOperator -> JVM_IO IntStream) (believe_me stream) (believe_me operator)

  sum : Inherits IntStream stream => stream -> JVM_IO Int
  sum stream = invokeInstance "sum" (IntStream -> JVM_IO Int) (believe_me stream)

  from : JVM_Array Int -> JVM_IO IntStream
  from values = invokeStatic IntStreamInterface "of" (JVM_Array Int -> JVM_IO IntStream) values
