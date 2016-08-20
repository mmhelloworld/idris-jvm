{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad      (forM)
import           Data.List          (find, foldl')
import           Data.Maybe         (fromMaybe)
import           System.Directory   (doesDirectoryExist, getCurrentDirectory,
                                     getDirectoryContents)
import           System.Environment (lookupEnv)
import           System.FilePath    (takeDirectory, (</>))

data JvmPaths = JvmPaths { jniHPath   :: FilePath
                         , jniMdHPath :: FilePath
                         , jvmLibPath :: FilePath
                         } deriving (Eq, Show)

-- Taken from RWH
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

missingJavaHome = error "Please set environment variable IDRIS_JAVA_HOME"
jniMissing = error "jni.h is missing under JDK. Please provide valid IDRIS_JAVA_HOME."
jniMdMissing = error "jni_md.h is missing under JDK. Please provide valid IDRIS_JAVA_HOME."
jvmLibMissing
  = error . concat $ ["jvm library is missing under JDK. ",
                      "Depending on your OS, One of ", show jvmLibs,
                      " is expected to be available under JDK. ",
                      "Please provide valid IDRIS_JAVA_HOME."]

jvmLibs = ["libjvm.so", "libjvm.dylib", "jvm.dll"]

findJvmPaths :: IO JvmPaths
findJvmPaths = do
  javaHome <- fromMaybe missingJavaHome <$> lookupEnv "IDRIS_JAVA_HOME"
  fs <- getRecursiveContents javaHome
  let jni = takeDirectory $ fromMaybe jniMissing $ find (endsWith "jni.h") fs
      jniMd = takeDirectory $ fromMaybe jniMdMissing $ find (endsWith "jni_md.h") fs
      jvmLib = takeDirectory $ fromMaybe jvmLibMissing $ find (\f -> any (`endsWith` f) jvmLibs) fs
  return $ JvmPaths jni jniMd jvmLib

substEnv :: IO ()
substEnv = do
  cwd <- getCurrentDirectory
  let configIn = cwd </> "idris-jvm.cabal.template"
      configOut = cwd </> "idris-jvm.cabal"
  !stackConfig <- readFile configIn
  jvmPaths <- findJvmPaths
  let f config = foldl' (\acc f -> f acc) config [ replace "${jni_h}" (jniHPath jvmPaths)
                 , replace "${jni_md_h}" (jniMdHPath jvmPaths)
                 , replace "${jvm_lib}" (jvmLibPath jvmPaths)
                 ]
  length stackConfig `seq` writeFile configOut $ f stackConfig

endsWith :: String -> String -> Bool
endsWith s1 s2 = end == s1 where
  end = drop (length s2 - length s1) s2

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace find repl s@(x:xs) =
    if take (length find) s == find
        then repl ++ replace find repl (drop (length find) s)
        else x : replace find repl xs


main :: IO ()
main = substEnv
