Idris 2 for JVM
============
[![Build Status](https://github.com/mmhelloworld/idris-jvm/actions/workflows/install.yml/badge.svg)](https://github.com/mmhelloworld/idris-jvm/actions/workflows/install.yml)

[Idris 2](https://idris-lang.org/) is a purely functional programming language with first class types. This repository provides Idris 2 compiler targeting JVM bytecode so that Idris can run on the JVM.

## Install

* Download the latest Idris 2 JVM release from [Github releases](https://github.com/mmhelloworld/idris-jvm/releases/latest). If you prefer Maven, Idris compiler is available in
  Maven central from [here](https://central.sonatype.com/artifact/io.github.mmhelloworld/idris-jvm-compiler).
* Extract the archive and add `idris2` launcher script directory `<EXTRACTED_DIRECTORY_ROOT>/exec` to PATH.
* Create an environment variable `IDRIS2_PREFIX` pointing to `<EXTRACTED_DIRECTORY_ROOT>/env`

## IDE Support

The [**Idris 2 (JVM)** plugin for IntelliJ IDEA](https://plugins.jetbrains.com/plugin/32261-idris-2-jvm)
provides editor support backed by this compiler, including syntax highlighting, code completion,
interactive editing (case split, add clause, proof search, generate definition, holes), go-to-definition,
and documentation lookup. It also surfaces the JVM FFI: completing on a Java class offers its full set of
callable members and generates the binding on demand.

Install it from the link above, or from within the IDE via
*Settings → Plugins → Marketplace* and searching for "Idris 2 JVM". The plugin drives the `idris2`
executable from this repository, so make sure it is installed (see [Install](#install) above).

## Example

#### helloworld.idr

```idris
    module Main

    data Tree a = Leaf
                | Node (Tree a) a (Tree a)

    inorder : Tree a -> List a
    inorder Leaf = []
    inorder (Node left a right) = inorder left ++ [a] ++ inorder right

    tree : Tree String
    tree = Node
            (Node
              (Node Leaf "3" Leaf)
              "+"
              (Node Leaf "7" Leaf))
            "/"
            (Node Leaf "2" Leaf)

    main : IO ()
    main = printLn $ inorder tree
```

#### Compile

`idris2 helloworld.idr -o main`

#### Run

```shell
% build/exec/main
["3", "+", "7", "/", "2"]
```

## Documentation
* [Idris JVM documentation](https://idris-jvm.readthedocs.io/en/latest/)
* [Idris 2 documentation](https://idris2.readthedocs.io/en/latest/index.html)
* [Idris 2 (JVM) IntelliJ plugin](https://plugins.jetbrains.com/plugin/32261-idris-2-jvm)

## License
This repository extends [idris-lang/Idris2](https://github.com/idris-lang/Idris2) repository with JVM backend. Files from [idris-lang/Idris2](https://github.com/idris-lang/Idris2) are covered by that repository's [license](https://github.com/idris-lang/Idris2/blob/main/LICENSE).
All other files from this repository are covered by BSD-3-Clause License. See [LICENSE](LICENSE).
