module Main

import IdrisJvm.IO
import IdrisJvm.JvmImport
import Java.Lang

%access public export

%language TypeProviders
%language ElabReflection

stringClass: String
stringClass = "java/lang/String"

listInterface: String
listInterface = "java/util/List"

arrayListClass: String
arrayListClass = "java/util/ArrayList"

collectionInterface : String
collectionInterface = "java/util/Collection"

systemClass: String
systemClass = "java/lang/System"

comparatorClass : String
comparatorClass = "java/util/Comparator"

pointClass : String
pointClass = "java/awt/Point"

collectionsClass : String
collectionsClass = "java/util/Collections"

stringBuilderClass : String
stringBuilderClass = "java/lang/StringBuilder"

objectsClass : String
objectsClass = "java/util/Objects"

integerClass : String
integerClass = "java/lang/Integer"

jdkimport [
    (systemClass, ["getProperty", "setProperty"]),
    (stringClass, ["substring", "CASE_INSENSITIVE_ORDER", "valueOf"]),
    (integerClass, ["parseInt"]),
    (comparatorClass, ["compare"]),
    (arrayListClass, ["<init>", "add"]),
    (listInterface, ["get"]),
    (collectionsClass, ["max"]),
    (stringBuilderClass, ["<init>", "toString"]),
    (objectsClass, ["toString"]),
    (pointClass, ["<init>", "x"])
  ]

NumberFormatExceptionClass : JVM_NativeTy
NumberFormatExceptionClass = Class "java/lang/NumberFormatException"

NullPointerExceptionClass : JVM_NativeTy
NullPointerExceptionClass = Class "java/lang/NullPointerException"

IllegalArgumentExceptionClass : JVM_NativeTy
IllegalArgumentExceptionClass = Class "java/lang/IllegalArgumentException"

SecurityExceptionClass : JVM_NativeTy
SecurityExceptionClass = Class "java/lang/SecurityException"

ArrayListClass : JVM_NativeTy
ArrayListClass = Class arrayListClass

ListInterface : JVM_NativeTy
ListInterface = Interface listInterface

CollectionInterface : JVM_NativeTy
CollectionInterface = Interface collectionInterface

ArrayListClass inherits CollectionInterface
ArrayListClass inherits ListInterface

objectToString : Inherits Object obj => obj -> JVM_IO String
objectToString obj = (objectsClass <.!> "toString") $ believe_me obj

main : JVM_IO ()
main = do

  -- Safe static methods. Since the return type is primitive and it cannot be null, the return type needs to be only
  -- exception safe so the type is `JVM_IO (Either Throwable Int)`.
  exceptionOrInt <- (integerClass <.> "parseInt") "not a number"
  printLn $ either (const 0) id exceptionOrInt

  -- Unsafe Static method. Type: JVM_IO Int
  printLn !((integerClass <.!> "parseInt") "23")

  -- overload resolution
  printLn !((stringClass <.!> "valueOf(double)") 2.5)
  printLn !((stringClass <.!> "valueOf(char)") 'B')

  -- exception handling
  propValue <- try ((systemClass <.> "getProperty(?java/lang/String)") Nothing) [
    ([catch IllegalArgumentExceptionClass, catch NullPointerExceptionClass], \t =>
        do
          printLn "property name is null or empty"
          pure Nothing
    ),
    ([catchNonFatal], \t =>
      do
        printLn "unable to get property value"
        pure Nothing
    )
  ]
  printLn propValue

  -- Safe instance methods. Here as the return type `String` is not primitive, it can be null so the return type has to
  -- be both exception safe and null safe so the type is `JVM_IO (Either Throwable (Maybe String))`
  noSecondCharException <- (stringClass <.> "substring(int)") "" 1
  putStrLn !(either objectToString (pure . show) noSecondCharException)

  s <- (stringClass <.> "substring(int)") "Foobar" 1
  putStrLn !(either throw (pure . show) s) -- rethrowing

  -- Unsafe instance method
  printLn $ !((stringClass <.!> "substring(int)") "Foobar" 1)

  -- Safe constructor. As constructors cannot return null, it needs to be only exception safe so the return type is
  -- `JVM_IO (Either Throwable ArrayList)`
  -- It also demonstrates constructor overloading resolution
  invalidList <- (arrayListClass <.> "<init>(int)") (-1)
  putStrLn !(either objectToString objectToString invalidList)

  -- Unsafe constructor
  list <- arrayListClass <.!> "<init>()"
  exceptionOrMaybeFirstItem <- (listInterface <.> "get") list 0
  putStrLn !(either objectToString (maybe (pure "") objectToString) exceptionOrMaybeFirstItem)

  arrayList <- arrayListClass <.!> "<init>()"
  (arrayListClass <.!> "add(java/lang/Object)") arrayList "hello"
  (arrayListClass <.!> "add(java/lang/Object)") arrayList "world"
  -- Passing ArrayList for a Collection. It works because of the "inherits" declaration above.
  max <- (collectionsClass <.!> "max(java/util/Collection)") arrayList
  printLn !(objectToString max)

  -- static field getter
  caseInsensitiveComparator <- stringClass <.#!> "CASE_INSENSITIVE_ORDER"
  printLn !((comparatorClass <.!> "compare") caseInsensitiveComparator "Bar" "august")

  point <- (pointClass <.!> "<init>(int,int)") 2 3

  -- instance field getter
  printLn !((pointClass <.#> "x") point)

  -- instance field setter
  (pointClass <.=> "x") point 34
  printLn !((pointClass <.#> "x") point)