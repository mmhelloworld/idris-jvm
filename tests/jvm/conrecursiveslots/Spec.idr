module Main

-- Ordinary DATACONs (escapes the listy/maybe/enum shapes): Node's Tree
-- slots are recursive and the only data-carrying constructor is Node
-- itself, so the spec class Node$I$L$L gets its subtree slots typed as
-- the family interface Tree$I$L$L.
data Tree = Leaf | Node Int Tree Tree

-- Tail-recursive over the left spine: the $idrisTailRec reassignment of
-- the Tree-typed parameter from the typed accessor needs no checkcast.
sumLeft : Int -> Tree -> Int
sumLeft acc Leaf = acc
sumLeft acc (Node n l _) = sumLeft (acc + n) l

build : Int -> Tree
build 0 = Leaf
build n = Node n (build (n - 1)) Leaf

main : IO ()
main = do
  printLn (sumLeft 0 (Node 5 (Node 6 Leaf Leaf) Leaf))
  printLn (sumLeft 0 (build 100))
