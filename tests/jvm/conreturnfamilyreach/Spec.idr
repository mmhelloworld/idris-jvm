module Main

data Tree a = Leaf | Node a (Tree a) (Tree a)

leafOf : a -> Tree a
leafOf x = Node x Leaf Leaf

-- Polymorphic; main calls viaOuter$sp0, so NATURAL viaOuter is dead. Its
-- natural body references natural leafOf (x:Object matches no spec), so
-- natural leafOf is referenced ONLY from dead code.
viaOuter : a -> Tree a
viaOuter x = leafOf x

sumTree : Tree Int -> Int
sumTree Leaf = 0
sumTree (Node x l r) = x + sumTree l + sumTree r

main : IO ()
main = do
  printLn (sumTree (Node 5 Leaf Leaf))   -- direct: earns sumTree$sp0(Tree$F$I)
  printLn (sumTree (viaOuter 7))         -- forces viaOuter$sp0 -> natural viaOuter -> natural leafOf
