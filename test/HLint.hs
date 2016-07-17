module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

sourceFolders :: [String]
sourceFolders =
    [ "src"
    , "app"
    , "test"
    ]

main :: IO ()
main = do
    hints <- hlint sourceFolders
    if null hints then exitSuccess else exitFailure
