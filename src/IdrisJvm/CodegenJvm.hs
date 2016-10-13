{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.CodegenJvm (codegenJvm) where

import           Control.Monad.RWS
import           Idris.Core.TT
import           IRTS.CodegenCommon         hiding (Object)
import           IRTS.Simplified


import qualified Data.DList                 as DL
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Function

import           IdrisJvm.AssemblerService
import           IdrisJvm.Codegen.Types
import           System.Directory           (getCurrentDirectory)
import           System.FilePath            (isRelative, takeBaseName, (</>))


codegenJvm :: CodeGenerator
codegenJvm ci = do
  cwd <- getCurrentDirectory
  asmResponse <- runAssembler (assemble cwd ci)
  maybe (pure ()) putStrLn asmResponse

assemble :: FilePath -> CodegenInfo -> [Asm]
assemble cwd ci = DL.toList $ instructions cgWriter <> deps cgWriter <> [ ClassCodeEnd classFilePath ] where
  out = outputFile ci
  cname = out ++ ".class"
  classFilePath = if isRelative cname then cwd </> cname else cname

  env = CgEnv $ takeBaseName (outputFile ci)
  (_, _, cgWriter) = runRWS (code ci) env initialCgState

code :: CodegenInfo -> Cg ()
code ci = do
  cname <- className <$> ask
  writeIns [ CreateClass ComputeMaxs
           , ClassCodeStart 52 Public cname "null" "java/lang/Object" []
           ]
  defaultConstructor
  functions ci
  mainMethod

functions :: CodegenInfo -> Cg ()
functions ci = mapM_ doCodegen (simpleDecls ci)


doCodegen :: (Name, SDecl) -> Cg ()
doCodegen (n, SFun _ args _ def) = cgFun n args def
