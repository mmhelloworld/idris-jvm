module IdrisJvm.CodegenJvmSpec where

import           Control.Monad                  (forM_)
import           Data.Char                      (isSpace, toUpper)
import           Data.List
import           Data.Monoid                    ((<>))
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process                 (readProcessWithExitCode)
import           Test.Hspec                     (Spec, describe, it, parallel)
import qualified Test.Hspec                     as H
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = describe "idris-jvm" $ do
  testcases <- H.runIO tests
  parallel $ forM_ testcases runTest

tests :: IO [FilePath]
tests = do
  testRoot <- getTestRoot
  dirs <- map (testRoot </>) <$> getDirectoryContents testRoot
  let isSpecial d = null baseName || baseName == "."
                  where baseName = takeBaseName d
  return $ sort . filter (not . isSpecial) $ dirs

runTest :: String -> H.SpecWith ()
runTest dir = it ("can compile `" <> dir <> "`") $ do
  expected <- readFile $ dir </> "expected"
  actual <- compileAndRun dir (dir </> (takeBaseName dir ++ ".idr"))
  actual `shouldBe` expected

compileAndRun :: FilePath -> String -> IO String
compileAndRun dir pgm = do
  let className = capitalize $ takeBaseName pgm
      classFile = dir </> className
  (_, compilerOut, compilerErr) <- runProcess "idris" [ "--codegen", "jvm", "-p", "idrisjvmruntime", pgm, "-o", classFile]
  putStrLnNonEmpty compilerOut
  putStrLnNonEmpty compilerErr
  (_, stdout, _) <- runProcess "java" ["-cp", "idris-jvm-runtime.jar:" ++ dir, className]
  return stdout

getTestRoot :: IO FilePath
getTestRoot = do
  cwd <- getCurrentDirectory
  return $ cwd </> "test" </> "resources"

runProcess :: String -> [String] -> IO (ExitCode, String, String)
runProcess proc args = do
  res@(exitCode, stdout, stderr) <- readProcessWithExitCode proc args ""
  case exitCode of
    ExitFailure _ -> error $ proc <> " ERROR: " <> stdout <> stderr
    _             -> return res

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x: xs

putStrLnNonEmpty :: String -> IO ()
putStrLnNonEmpty [] = pure ()
putStrLnNonEmpty (x: xs) | isSpace x = putStrLnNonEmpty xs
putStrLnNonEmpty xs = putStrLn xs
