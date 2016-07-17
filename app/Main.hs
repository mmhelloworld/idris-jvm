module Main where

import Idris.Core.TT
import Idris.AbsSyntax
import Idris.ElabDecls
import Idris.REPL

import IRTS.Compiler
import IdrisJvm.CodegenJvm

import System.Environment
import System.Exit

data Opts = Opts { inputs :: [FilePath]
                 , output :: FilePath
                 }

showUsage = do putStrLn "JVM bytecode generator which is intended to be called by the Idris compiler, not by an user."
               putStrLn "Usage: idris-codegen-jvm <ibc-files> [-o <output-file>]"
               exitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.java") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("--yes-really":xs) = process opts xs -- GRRR
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

cgMain :: Opts -> Idris ()
cgMain opts = do elabPrims
                 loadInputs (inputs opts) Nothing
                 mainProg <- elabMain
                 ir <- compile (Via IBCFormat "jvm") (output opts) (Just mainProg)
                 runIO $ codegenJvm ir

main :: IO ()
main = do opts <- getOpts
          if null (inputs opts) 
             then showUsage
             else runMain (cgMain opts)


