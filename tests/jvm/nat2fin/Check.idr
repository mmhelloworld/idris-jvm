import Data.List
import Data.String
import System.File

path : String
path = "build/exec/test_app/test-decompiled.txt"

mainLine : String -> Bool
mainLine str =
  ("(define Main-main(" `isPrefixOf` str) && (" 375))))" `isInfixOf` str)

isOptimized : List String -> Bool
isOptimized [] = False
isOptimized (_ :: _ :: []) = False
isOptimized (_ :: []) = False
isOptimized (line1 :: line2 :: line3 :: rest) = (("String 375" `isSuffixOf` line1) &&
  ("Method java/math/BigInteger.\"<init>\":(Ljava/lang/String;)V" `isSuffixOf` line2) &&
  ("Method M_Prelude/IO.printLn:(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;" `isSuffixOf` line3)) ||
  isOptimized (line2 :: line3 :: rest)

main : IO ()
main = do
  Right str <- readFile path
    | Left err => putStrLn "Error when reading \{path}"
  case isOptimized (lines str) of
    True  => putStrLn "natToFinLt optimized away"
    False => putStrLn "failed to optimize away natToFinLt"
