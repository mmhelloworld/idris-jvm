# What does `setup` script do?

1. Download [JVM Assembler](https://github.com/mmhelloworld/jvm-assembler/releases)
1. Download [Idris JVM Runtime jar](https://github.com/mmhelloworld/idris-jvm-runtime/releases)
1. Install Idris JVM code generator (this repository)
1. install JVM runtime Idris package (`runtime` directory in this repository)
1. Start JVM assemler server

The files will be downloaded into `$USER_HOME/.idrisjvm` by default but it can be overridden by specifying a directory for the `setup` script like `bin/setup --work-dir path/to/directory`.
