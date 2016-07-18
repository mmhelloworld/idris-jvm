module IdrisJvm.CodegenJvmSpec where

import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.List
import Data.Monoid((<>))
import System.Directory
import System.Exit
import System.Environment
import System.FilePath
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, parallel, describe, it)
import qualified Test.Hspec as H
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "idris-jvm" $ do
  testcases <- H.runIO tests
  parallel $ forM_ testcases runTest

tests = do
  cwd <- getCurrentDirectory
  let testRoot = cwd </> "test" </> "resources"
  dirs <- map (testRoot </>) <$> getDirectoryContents testRoot
  let isSpecial d =  null baseName || baseName == "." where baseName = takeBaseName d
  return $ sort . filter (not . isSpecial) $ dirs

runTest dir = it ("can compile `" <> dir <> "`") $ do
  expected <- readFile $ dir </> "expected"
  actual <- compileAndRun (dir </> (takeBaseName dir ++ ".idr"))
  actual `shouldBe` expected
  
compileAndRun pgm = do
  let className = capitalize $ takeBaseName pgm
  runProcess "idris" [pgm, "--codegen", "jvm", "-o", className]
  lib <- getEnv "IDRIS_JVM_LIB"
  (_, stdout, _) <- runProcess "java" ["-cp", lib ++ ":.", className]
  return stdout
  
lsWithExt dir ext = do
  fs <- getDirectoryContents dir
  return $ map (dir </>) $ filter ((==) ext . takeExtension) fs

runProcess :: String -> [String] -> IO (ExitCode, String, String)
runProcess proc args = do
  res@(exitCode, stdout, stderr) <- readProcessWithExitCode proc args ""
  case exitCode of
    ExitFailure _ -> error $ proc <> " ERROR: " <> stdout <> stderr
    _             -> return res

capitalize [] = []
capitalize (x:xs) = toUpper x: xs

