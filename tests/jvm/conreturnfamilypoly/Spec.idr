module Main

-- Parametric, family-eligible sum type.
data Tree a = Leaf | Node a (Tree a) (Tree a)

-- POLYMORPHIC producer; the `Int` call site forces mkTree$sp0(int), which
-- (with spec-return propagation) returns the concrete family interface
-- Tree$F$I instead of Object.
mkTree : a -> Tree a
mkTree x = Node x Leaf Leaf

-- Consumer fed purely by the producer's result (not a direct construction).
sumTree : Tree Int -> Int
sumTree Leaf = 0
sumTree (Node x l r) = x + sumTree l + sumTree r

main : IO ()
main = printLn (sumTree (mkTree 7))
