import System.Random

main : IO ()
main = do
  value <- randomRIO (the Double 10, the Double 30.5)
  printLn $ value > 10