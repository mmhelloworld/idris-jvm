module Compiler.Jvm.Tree

import Data.List
import Data.String

public export
data Tree a = Node a (List (Tree a))

export
displayTree : (a -> String) -> Tree a -> String
displayTree show tree = unlines . reverse $ go [] [] 0 tree where
    indent : Nat -> String
    indent level = concat $ replicate level "  "

    showNodeData : Nat -> a -> String
    showNodeData level d = indent level ++ show d

    go : List String -> List (Nat, Tree a) -> Nat -> Tree a -> List String
    go acc [] level (Node d []) = showNodeData level d :: acc
    go acc ((treeLevel, tree) :: nextTree) nodeLevel (Node d []) =
      go (showNodeData nodeLevel d :: acc) nextTree treeLevel tree
    go acc stack level (Node d (child :: children)) =
      let childrenWithLevel = ((\c => (level + 1, c)) <$> children)
      in assert_total $ go (showNodeData level d :: acc) (childrenWithLevel ++ stack) (level + 1) child

implementation Show a => Show (Tree a) where
  show tree = displayTree show tree

export
traverseDepthFirst : Tree a -> List a
traverseDepthFirst tree = go [] [] tree where
  go : (acc: List a) -> (stack: List (Tree a)) -> Tree a -> List a
  go acc [] (Node d []) = d :: acc
  go acc (next :: rest) (Node d []) = go (d :: acc) rest next
  go acc stack (Node d (child :: children)) = go acc (child :: stack) (Node d children)
