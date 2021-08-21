import System
import Data.Maybe

f : String -> IO String
f str = do
  putStrLn str
  pure $ str ++ "!!"

main : IO ()
main = do
    prop <- getEnv "foo"
    when (not $ isJust prop) $
      let info = map (\s => show $ length s) !(traverse f ["x", "y", "z"]) in
          putStrLn $ "result " ++ show info
