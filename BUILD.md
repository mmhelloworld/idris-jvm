Build with Maven
==============

### Prerequisites:
* Download previous version (0.2.2) of compiler from here https://github.com/mmhelloworld/Idris2/releases/tag/v0.2.2-SNAPSHOT-20210821.
* Extract the archive and make sure that `idris2` launcher script is on PATH.

### Build options:
+ To install with tests: `mvn install`
+ To install without tests `mvn install -DskipTests`
+ To recompile libraries without building compiler `mvn install -DskipIdrisCompile`
+ To recompile without building libraries `mvn install -DskipIdrisInstallLibrary`
+ To run only tests `mvn -f tests/pom.xml integration-test`
+ To run a single test `mvn -f tests/pom.xml integration-test -Didris.tests="only=idris2/basic001"`