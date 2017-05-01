module IdrisJvm.CodegenJvmSpec where

import           Control.Monad                  (forM_)
import           Data.Char                      (isSpace, toUpper)
import           Data.List
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import           System.Directory
import           System.Environment             (lookupEnv)
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
  testToRun <- lookupEnv "IDRIS_JVM_TEST"
  let allTests = map (testRoot </>) <$> getDirectoryContents testRoot
  dirs <- maybe allTests (pure . (:[]) . (testRoot </>)) testToRun
  let isSpecial d = null baseName || baseName == "."
                  where baseName = takeBaseName d
  return $ sort . filter (not . isSpecial) $ dirs

runTest :: String -> H.SpecWith ()
runTest dir = it ("can compile `" <> dir <> "`") $ do
  expected <- readFile $ dir </> "expected"
  actual <- compileAndRun dir (dir </> (takeBaseName dir ++ ".idr"))
  let stripRes = stripPrefix "--any order\n" expected
      stripCompare stripped = (stripped \\ actual) `shouldBe` []
  maybe (actual `shouldBe` expected) stripCompare stripRes

compileAndRun :: FilePath -> String -> IO String
compileAndRun dir pgm = do
  let out = dir </> "target"
  (_, compilerOut, compilerErr) <-
    runProcess "idris" [ "--codegen", "jvm",
      "-p", "idrisjvmruntime", "-p", "effects", pgm, "-o", out]
  putStrLnNonEmpty compilerOut
  putStrLnNonEmpty compilerErr
  workingDir <- getWorkingDir
  let runtimeJar = workingDir </> "idris-jvm-runtime-1.0-SNAPSHOT.jar"
      args = ["-cp", runtimeJar ++ (classpathSep: out), "main.Main"]
  (_, stdout, _) <- runProcess "java" args
  return stdout

getTestRoot :: IO FilePath
getTestRoot = do
  cwd <- getCurrentDirectory
  return $ cwd </> "test" </> "tests"

getWorkingDir :: IO FilePath
getWorkingDir = do
  defaultWorkingDir <- (</> ".idrisjvm") <$> getHomeDirectory
  fromMaybe defaultWorkingDir <$> lookupEnv "IDRIS_JVM_WORK_DIR"

runProcess :: String -> [String] -> IO (ExitCode, String, String)
runProcess proc args = do
  res@(exitCode, stdout, stderr) <- readProcessWithExitCode proc args ""
  case exitCode of
    ExitFailure _ -> error $ proc <> " ERROR: " <> stdout <> stderr
    _             -> return res

classpathSep :: Char
classpathSep | pathSeparator == '/' = ':'
             | otherwise = ';'

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x: xs

putStrLnNonEmpty :: String -> IO ()
putStrLnNonEmpty []      = pure ()
putStrLnNonEmpty (x: xs) | isSpace x = putStrLnNonEmpty xs
putStrLnNonEmpty xs      = putStrLn xs
