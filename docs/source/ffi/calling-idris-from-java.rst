.. _ffi-calling-idris-from-java:

#########################
Calling Idris from Java
#########################

Exporting Idris Functions
=========================

.. code-block:: idris

    %export """
        jvm:public static replicate
        {
            "enclosingType": "idris/String",
            "arguments": [{"type": "BigInteger"}, {"type": "char"}],
            "returnType": "String"
        }
        """
    exportStringReplicate : Nat -> Char -> String
    exportStringReplicate = String.replicate

This code exports ``replicate`` function from ``Data.String`` Idris module. The export block starting with ``%export``
needs to specify which Java class the function should belong to, argument types and return type. As Idris ``Nat`` maps
to Java's `BigInteger <https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html>`_ in the JVM bytecote, first
argument type is ``BigInteger``.

This is how it looks in the Java code:

.. code-block:: java

  import static java.math.BigInteger.TEN;

  public class Main {

      public static void main(String[] args) {
          System.out.println(idris.String.replicate(TEN, 'A'));
      }

  }

The code prints ``AAAAAAAAAA`` as expected.

JVM Import Aliases
==================

In the example above, we need to specify types in the export block in a few places like ``enclosingType``, ``arguments``
and ``returnType``. Annotations would be another place which we will cover later. If we are exporting multiple Idris
functions, we don't want to repeat the types everywhere. We can use import aliases to avoid repeating the fully
qualified names of types.

.. code-block:: idris

    %export
        """
        jvm:import
        idris/data/List IdrisList
        idris/data/Maybe IdrisMaybe
        idris/prelude/Show
        """
    jvmImports : List String
    jvmImports = []


The export block defines three imports and aliases. The Idris function that defines the export block name must be named
``jvmImports``. With this, the export functions can simply use ``IdrisList``, ``IdrisMaybe`` and ``Show``
instead of fully qualified exported Java names. Note that the last import ``idris/prelude/Show`` doesn't provide
an explicit alias but the last part of qualified name would implicitly serve as alias.

These imports can be used with any export functions defined in the current namespace and any descendant namespaces.
For example, if ``helloworld.foo`` defines this export block, all the functions in ``helloworld.foo`` and
``helloworld.foo.bar`` namespaces can use the aliases.

Export Idris Types
==================

To be able to export functions with Idris types, Idris types need to be exported as well. This is necessary so that we
don't depend on compiler generated interal types in Java. Another reason is that Idris compiler optimizes certain types.
For example, ``data Color = Red | Green`` doesn't have a ``Color`` type at runtime. Instead ``Red`` and ``Green`` are just
integers which Java code shouldn't rely on.

.. code-block:: idris

  %export
      """
      jvm:export
      idris/data/List
      helloworld/Color
      """
  typeExports : List String
  typeExports = []

Similar to ``jvmExports`` block explained above, ``typeExports`` is a special export function that exports Idris types
to Java. The function must be named ``typeExports``. These exported types can then be used in other exported functions
in current namespace as well as any descendant namespaces similar to ``jvmImports``.

Exporting Idris functions with Generic Types
============================================

.. code-block:: idris

    %export
        """
        jvm:import
        idris/data/List IdrisList
        """
    jvmImports : List String
    jvmImports = []

    %export
        """
        jvm:export
        IdrisList
        """
    typeExports : List String
    typeExports = []

    %export """
        jvm:public static nil
        {
            "enclosingType": "IdrisList",
            "returnType": "IdrisList"
        }
        """
    idrisNil : List a
    idrisNil = []

    %export """
        jvm:public static cons
        {
            "enclosingType": "IdrisList",
            "arguments": [{"type": "Object"}, {"type": "IdrisList"}],
            "returnType": "IdrisList"
        }
        """
    idrisCons : a -> List a -> List a
    idrisCons = (::)

Here we are exporting Idris's ``[]`` and ``::`` functions from ``List`` module. The code also exports ``List`` type
itself under class ``idris/data/List`` for Java using ``typeExports`` function. Also note that the exported functions
and exported types make use of import alias ``IdrisList`` through ``jvmImports``.

This is how we use the functions in Java:

.. code-block:: java

    import idris.data.List;

    import static idris.data.List.cons;
    import static idris.data.List.nil;

    public class Main {

        public static void main(String[] args) {
            List idrisIntList = cons(23, cons(45, nil()));
            System.out.println(idrisIntList);
        }

    }

The code prints something like ``idris.data.List@506e1b77`` as we don't have ``toString`` method defined. We will see
next how we can use Idris ``Show`` interface to print Idris ``List`` for different element types.

Exporting Idris functions with type class instances
===================================================

Idris type class instances can be exported to Java. This will be useful to call Idris functions that require
implicit values of type class instances. The type class parameters will be passed explicitly from Java.

.. code-block:: idris

  %export """
      jvm:public static show
      {
          "enclosingType": "idris/Int",
          "returnType": "Show"
      }
      """
  showInt : Show Int
  showInt = %search

Here we are returning the ``Show`` instance for `Int` type. Similarly we can return type class instances that require
other type class instances as well. For example, ``Show`` instance for ``List a`` needs ``Show`` instance for ``a``.

.. code-block:: idris

  %export """
      jvm:public static show
      {
          "enclosingType": "IdrisList",
          "arguments": [{"type": "Show"}],
          "returnType": "Show"
      }
      """
  exportShowList : Show a => Show (List a)
  exportShowList = %search

Now we can export the ``show`` function to Java and pass the type class instances to print Idris ``List Int``.

.. code-block:: idris

  %export """
      jvm:public static show
      {
          "enclosingType": "Show",
          "arguments": [{"type": "Show"}, {"type": "Object"}],
          "returnType": "String"
      }
      """
  exportShow : Show a => a -> String
  exportShow = show

Here is a complete example in Idris and Java:

.. code-block:: idris

  module Main

  import Data.String
  import Data.List

  %export
      """
      jvm:import
      idris/String IdrisString
      idris/data/List IdrisList
      idris/data/Maybe IdrisMaybe
      idris/prelude/Show
      helloworld/Color
      """
  jvmImports : List String
  jvmImports = []

  %export
      """
      jvm:export
      IdrisList
      IdrisMaybe
      Show
      Color
      """
  typeExports : List String
  typeExports = []

  %export """
      jvm:public static nil
      {
          "enclosingType": "IdrisList",
          "returnType": "IdrisList"
      }
      """
  idrisNil : List a
  idrisNil = []

  %export """
      jvm:public static cons
      {
          "enclosingType": "IdrisList",
          "arguments": [{"type": "Object"}, {"type": "IdrisList"}],
          "returnType": "IdrisList"
      }
      """
  idrisCons : a -> List a -> List a
  idrisCons = (::)

  %export """
      jvm:public static show
      {
          "enclosingType": "idris/Int",
          "returnType": "Show"
      }
      """
  showInt : Show Int
  showInt = %search

  %export """
      jvm:public static show
      {
          "enclosingType": "IdrisString",
          "returnType": "Show"
      }
      """
  showString : Show String
  showString = %search

  data Color = Red | Green | Blue

  Show Color where
      show Red = "Red"
      show Green = "Green"
      show Blue = "Blue"

  %export """
      jvm:public static red
      {
          "enclosingType": "Color",
          "returnType": "Color"
      }
      """
  red : Color
  red = Red

  %export """
      jvm:public static green
      {
          "enclosingType": "Color",
          "returnType": "Color"
      }
      """
  green : Color
  green = Green

  %export """
      jvm:public static blue
      {
          "enclosingType": "Color",
          "returnType": "Color"
      }
      """
  blue : Color
  blue = Blue

  %export """
      jvm:public static show
      {
          "enclosingType": "Color",
          "returnType": "Show"
      }
      """
  exportShowColor : Show Color
  exportShowColor = %search

  %export """
      jvm:public static show
      {
          "enclosingType": "Color",
          "arguments": [{"type": "Color"}],
          "returnType": "String"
      }
      """
  showColor : Color -> String
  showColor = show

  %export """
      jvm:public static show
      {
          "enclosingType": "IdrisList",
          "arguments": [{"type": "Show"}],
          "returnType": "Show"
      }
      """
  exportShowList : Show a => Show (List a)
  exportShowList = %search

  %export """
      jvm:public static just
      {
          "enclosingType": "IdrisMaybe",
          "arguments": [{"type": "Object"}],
          "returnType": "IdrisMaybe"
      }
      """
  exportJust : a -> Maybe a
  exportJust = Just

  %export """
      jvm:public static nothing
      {
          "enclosingType": "IdrisMaybe",
          "returnType": "IdrisMaybe"
      }
      """
  exportNothing : Maybe a
  exportNothing = Nothing

  %export """
      jvm:public static show
      {
          "enclosingType": "IdrisMaybe",
          "arguments": [{"type": "Show"}],
          "returnType": "Show"
      }
      """
  exportShowMaybe : Show a => Show (Maybe a)
  exportShowMaybe = %search

  %export """
      jvm:public static show
      {
          "enclosingType": "Show",
          "arguments": [{"type": "Show"}, {"type": "Object"}],
          "returnType": "String"
      }
      """
  exportShow : Show a => a -> String
  exportShow = show

  %export """
      jvm:public static replicate
      {
          "enclosingType": "IdrisString",
          "arguments": [{"type": "BigInteger"}, {"type": "char"}],
          "returnType": "String"
      }
      """
  exportStringReplicate : Nat -> Char -> String
  exportStringReplicate = String.replicate

  main : IO ()
  main = pure ()

Java calling the Idris functions:

.. code-block:: java
   :linenos:

    package hello;

    import helloworld.Color;
    import idris.Int;
    import idris.data.List;
    import idris.data.Maybe;
    import idris.prelude.Show;

    import static helloworld.Color.blue;
    import static helloworld.Color.green;
    import static helloworld.Color.red;
    import static idris.data.List.cons;
    import static idris.data.List.nil;
    import static idris.data.Maybe.just;
    import static idris.data.Maybe.nothing;
    import static idris.prelude.Show.show;
    import static java.math.BigInteger.TEN;

    public class Main {

        public static void main(String[] args) {
            List idrisIntList = cons(23, cons(45, nil())); // Create an Idris list of integers
            List idrisStringList = cons("foo", cons("bar", nil()));

            // Create an Idris list of Colors defined as data Color = Red | Green | Blue
            List idrisColorList = cons(red().toIdris(), cons(blue().toIdris(), nil()));

            // Get Show instance for Idris List given a show Instance of Int
            Show intListShow = List.show(Int.show());
            Show stringListShow = List.show(idris.String.show());
            Show colorShow = Color.show();
            Show colorListShow = List.show(colorShow);
            Show colorMaybeShow = Maybe.show(colorShow);

            System.out.println(show(intListShow, idrisIntList.toIdris()));
            System.out.println(show(stringListShow, idrisStringList.toIdris()));
            System.out.println(show(colorListShow, idrisColorList.toIdris()));

            System.out.println(show(colorShow, green().toIdris()));
            System.out.println(Color.show(blue()));

            System.out.println(show(colorMaybeShow, just(green().toIdris()).toIdris()));
            System.out.println(show(colorMaybeShow, nothing().toIdris()));
            System.out.println(idris.String.replicate(TEN, 'A'));
        }

    }

This is the output:

.. code-block:: text

    [23, 45]
    ["foo", "bar"]
    [Red, Blue]
    Green
    Blue
    Just Green
    Nothing
    AAAAAAAAAA


Here line numbers 29-33 retrieve type class instances for different types and the instances are passed in the lines
below when ``show`` function is called. Another thing to note here is that when we call generic Idris functions, Idris
types that are wrapped in Java types needs to be converted before passing to Idris functions. For example, line 23
passes ``String`` values directly but line 26 passes ``Color`` values after calling ``toIdris`` to generic ``cons``
function. Same logic applies at line 35 while passing ``idris.data.List`` to ``show``. If the function type is
monomorphic, we don't need to explicitly convert. For example, line 40 passes ``blue()`` directly to monomorphic
``Color.show``.

Exporting Idris Types and Functions with Java Annotations
=========================================================

Idris types and functions can be exported to Java with annotations which will help integrate with some Java frameworks
such as Spring Boot.

.. code-block:: idris
   :linenos:

   %export
       """
       jvm:import
       org/springframework/web/bind/annotation/GetMapping Get
       org/springframework/web/bind/annotation/PostMapping Post
       io/github/mmhelloworld/helloworld/Employee
       io/github/mmhelloworld/helloworld/EmployeeController
       io/github/mmhelloworld/helloworld/EmployeeRepository
       io/github/mmhelloworld/helloworld/PayrollConfiguration
       io/github/mmhelloworld/helloworld/PayrollApplication
       org/springframework/context/annotation/Configuration
       org/springframework/web/bind/annotation/RestController
       org/springframework/web/bind/annotation/RequestBody
       org/springframework/boot/CommandLineRunner
       org/springframework/boot/SpringApplication
       org/springframework/boot/autoconfigure/SpringBootApplication
       org/springframework/context/annotation/Bean
       org/springframework/stereotype/Component
       java/util/List
       """
   jvmImports : List String
   jvmImports = []

   -- Spring boot controller that defines REST endpoints
   namespace EmployeeController

       %export """
               jvm:public EmployeeController
               {
                   "annotations": [
                       {"NoArgsConstructor": {}},
                       {"RestController": {}}
                   ]
               }
               """
       public export
       EmployeeController : Type
       EmployeeController = Struct "io/github/mmhelloworld/helloworld/EmployeeController" []

       -- GET endpoint to retrieve all the employees
       %export """
            jvm:public getEmployees
            {
                "annotations": [
                    {"Get": ["/employees"]}
                ],
                "enclosingType": "EmployeeController",
                 "arguments": [
                     {
                         "type": "EmployeeController"
                     }
                 ],
                 "returnType": "List<Employee>"
            }
        """
       employees : EmployeeController -> PrimIO (JList Employee)
       employees _ = toPrim $ run getEmployees

       -- POST endpoint to save an employee given a JSON payload
       %export """
            jvm:public saveEmployee
            {
                "annotations": [
                    {"Post": ["/employee"]}
                ],
                "enclosingType": "EmployeeController",
                "arguments": [
                    {
                        "type": "EmployeeController"
                    },
                    {
                        "type": "Employee",
                        "annotations": [
                           {"RequestBody": {}}
                        ]
                    }
                ],
                "returnType": "Employee"
            }
        """
       saveEmployee : EmployeeController -> Employee -> PrimIO Employee
       saveEmployee _ employee = toPrim $ run (PayrollApp.saveEmployee employee)


The above code exports a Spring Boot controller with ``@RestController`` annotation and two REST endpoint functions
annotated with ``@GetMapping`` and ``@PostMapping`` allowing to retrieve all employees and save an employee
respectively. Also note that line 73 provides an annotation to the second function parameter allowing the Idris
function to receive a POST body mapping to ``Employee`` JSON. See
`here <https://github.com/mmhelloworld/idris-spring-boot-example>`_ for a complete Spring Boot example written
completely in Idris.
