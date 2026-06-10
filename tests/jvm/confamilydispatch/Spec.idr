module Main

-- Ordinary DATACONs (not Maybe/List/Nat-shaped): the family interface is
-- discovered via findTConForDCon.
data Shape = Circle Int | Rect Int Int

area : Shape -> Int
area (Circle r) = r * r * 3
area (Rect w h) = w * h

-- Synthetic JUST/NOTHING shape: the family is the builtin Maybe.
sumO : Maybe Int -> Int
sumO Nothing = 0
sumO (Just x) = x + 1

main : IO ()
main = do
  printLn (area (Circle 5))
  printLn (area (Rect 3 4))
  printLn (sumO (Just 41))
  printLn (sumO Nothing)
