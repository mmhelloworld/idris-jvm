module Compiler.Jvm.Tree

import Data.List
import Data.Strings

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
      in go (showNodeData level d :: acc) (childrenWithLevel ++ stack) (level + 1) child

implementation Show a => Show (Tree a) where
  show tree = displayTree show tree

export
levelOrder : Tree a -> List (List a)
levelOrder tree = go [] [tree]
  where
    element : Tree b -> b
    element (Node value _) = value

    subtrees : Tree b -> List (Tree b)
    subtrees (Node _ children) = children

    go : List (List b) -> List (Tree b) -> List (List b)
    go acc [] = acc
    go acc trees = go (map element trees :: acc) (concatMap subtrees trees)