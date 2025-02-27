Idris 2 for JVM
============
[![Build Status](https://github.com/mmhelloworld/idris-jvm/actions/workflows/install.yml/badge.svg)](https://github.com/mmhelloworld/idris-jvm/actions/workflows/install.yml)

[Idris 2](https://idris-lang.org/) is a purely functional programming language
with first class types. This repository provides Idris 2 compiler targeting JVM bytecode so that Idris 2 compiler and Idris 2 programs can run on the JVM.

![Alt](https://repobeats.axiom.co/api/embed/ac64c0b18c12514f8bdd3d0b6768437187ea4d88.svg "Repobeats analytics image")

## Install

* Download the latest Idris 2 JVM release from [releases page](https://github.com/mmhelloworld/idris-jvm/releases/latest).
* Extract the archive and add `idris2` launcher script directory `<EXTRACTED_DIRECTORY_ROOT>/exec` to PATH.
* Create an environment variable `IDRIS2_PREFIX` pointing to `<EXTRACTED_DIRECTORY_ROOT>/env`

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

## License
This repository extends [idris-lang/Idris2](https://github.com/idris-lang/Idris2) repository with JVM backend. Files from [idris-lang/Idris2](https://github.com/idris-lang/Idris2) are covered by that repository's [license](https://github.com/idris-lang/Idris2/blob/main/LICENSE).
All other files from this repository are covered by BSD-3-Clause License. See [LICENSE](LICENSE).
