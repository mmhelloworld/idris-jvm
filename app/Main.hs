module Main where

import           Idris.AbsSyntax
import           Idris.ElabDecls
import           Idris.Main          (runMain)
import           Idris.ModeCommon    (loadInputs)

import           IdrisJvm.CodegenJvm
import           IRTS.Compiler

import           System.Environment
import           System.Exit

data Opts = Opts { inputs :: [FilePath]
                 , interface :: Bool
                 , output :: FilePath
                 }

showUsage :: IO ()
showUsage = do putStrLn "JVM bytecode generator is intended to be called by the Idris compiler, not by an user."
               putStrLn "Usage: idris-codegen-jvm <ibc-files> [-o <output-file>]"
               exitSuccess

getOpts :: IO Opts
getOpts = process (Opts [] False "target") <$> getArgs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts ("--interface":xs) = process (opts { interface = True }) xs
    process opts (x:xs)      = process (opts { inputs = x:inputs opts }) xs
    process opts []          = opts

cgMain :: Opts -> Idris ()
cgMain opts = do elabPrims
                 _ <- loadInputs (inputs opts) Nothing
                 mainProg <- if interface opts
                                then pure Nothing -- Generate from export list
                                else Just <$> elabMain -- Generate from main
                 ir <- compile (Via IBCFormat "jvm") (output opts) mainProg
                 runIO $ codegenJvm ir

main :: IO ()
main = do opts <- getOpts
          if null (inputs opts)
             then showUsage
             else runMain (cgMain opts)
