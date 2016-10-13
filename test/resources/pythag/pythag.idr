module Main

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y * y == z * z]

main : IO ()
main = printLn (pythag 50)
