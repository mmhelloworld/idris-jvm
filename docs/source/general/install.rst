.. _general-install:

Install
=======

  * Download the latest Idris 2 JVM release from `releases page <https://github.com/mmhelloworld/idris-jvm/releases/latest>`_. If you prefer Maven, Idris compiler is available in
    Maven central from `here <https://central.sonatype.com/artifact/io.github.mmhelloworld/idris-jvm-compiler>`_.
  * Extract the archive and add ``idris2`` launcher script directory ``<EXTRACTED_DIRECTORY_ROOT>/exec`` to ``PATH``.
  * Create an environment variable ``IDRIS2_PREFIX`` pointing to ``<EXTRACTED_DIRECTORY_ROOT>/env``

IDE Support
===========

The `Idris 2 (JVM) plugin for IntelliJ IDEA <https://plugins.jetbrains.com/plugin/32261-idris-2-jvm>`_
provides editor support backed by this compiler, including syntax highlighting, code completion,
interactive editing (case split, add clause, proof search, generate definition, holes),
go-to-definition, and documentation lookup. It also surfaces the JVM FFI: completing on a Java
class offers its full set of callable members and generates the binding on demand.

Install it from the link above, or from within the IDE via *Settings → Plugins → Marketplace* and
searching for "Idris 2 JVM". The plugin drives the ``idris2`` executable from this repository, so
make sure it is installed as described above.
