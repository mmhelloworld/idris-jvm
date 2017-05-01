{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module IdrisJvm.CodegenJvm (codegenJvm) where

import           Control.Monad.RWS
import           Idris.Core.TT
import           IRTS.CodegenCommon         hiding (Object)
import           IRTS.Simplified


import qualified Data.DList                 as DL
import           IdrisJvm.Codegen.Assembler
import           IdrisJvm.Codegen.Common
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
  classFilePath = if isRelative out then cwd </> out else out

  env = CgEnv $ takeBaseName out
  (_, _, cgWriter) = runRWS (code ci) env initialCgState

code :: CodegenInfo -> Cg ()
code ci = do
  functions ci
  mainMethod
  mapM_ exportCode (exportDecls ci)

functions :: CodegenInfo -> Cg ()
functions ci = mapM_ doCodegen (simpleDecls ci)

doCodegen :: (Name, SDecl) -> Cg ()
doCodegen (n, SFun _ args _ def) = cgFun [Public, Static] clsName fname args def where
  JMethodName clsName fname = jname n
