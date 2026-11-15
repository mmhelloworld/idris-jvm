||| Build-driven JVM FFI binding generation.
|||
||| The classes a project imports from the JVM (`jvmimports` in its package file, or
||| `--jvm-ffi-import` on the command line) are reflected off the project classpath, rendered
||| as Idris binding modules (`Compiler.Jvm.FfiGen`), compiled to TTCs under
||| `<build dir>/jvm-ffi`, and that directory is put on the import search path. User code then
||| imports them like any installed package module (`import Java.Util`), and the source tree is
||| left untouched. This lives outside `Idris.SetOptions` because compiling the generated
||| modules needs `Idris.ModTree`.
module Idris.JvmFfi

import Compiler.Jvm.Reflection

import Core.Context
import Core.Core
import Core.Directory
import Core.Options

import Idris.CommandLine
import Idris.ModTree
import Idris.REPL.Opts
import Idris.Syntax

import Data.List
import Data.Maybe
import Data.String

import Libraries.Utils.Path

import System.Directory
import System.File

%default covering

-- Directory portion of a file path (everything up to the last '/'), or "." if none.
dirOf : String -> String
dirOf p = case break (== '/') (reverse (unpack p)) of
            (_, '/' :: dir) => pack (reverse dir)
            _               => "."

listDirOrEmpty : String -> IO (List String)
listDirOrEmpty dir = either (const []) id <$> listDir dir

-- Recursively collect every `.idr` source path under `dir`. A leaf (file) lists as empty via
-- `listDirOrEmpty`, so we recurse into anything that is not itself an `.idr` file.
gatherIdrSources : String -> IO (List String)
gatherIdrSources dir
    = do entries <- listDirOrEmpty dir
         concat <$> traverse visit entries
  where
    visit : String -> IO (List String)
    visit entry = let path = dir </> entry in
                  if extension entry == Just "idr"
                     then pure [path]
                     else gatherIdrSources path

||| The classes named by every `--jvm-ffi-import` option.
export
jvmFfiImportsOf : List CLOpt -> List String
jvmFfiImportsOf = concatMap classes
  where
    classes : CLOpt -> List String
    classes (JvmFfiImport cs) = cs
    classes _ = []

||| Source files to scan for `Class.member` references when no package file drives the build:
||| the entry files named on the command line plus every `.idr` under their directories (so
||| references in helper modules are seen too); with no entry file, everything under the source
||| directory.
export
jvmFfiScanSources : (sourceDir : Maybe String) -> List CLOpt -> IO (List String)
jvmFfiScanSources sourceDir opts
    = do let inputs = mapMaybe (\o => case o of InputFile f => Just f; _ => Nothing) opts
         let dirs = nub (if null inputs then [fromMaybe "." sourceDir] else map dirOf inputs)
         walked <- concat <$> traverse gatherIdrSources dirs
         pure (nub (inputs ++ walked))

-- Generated sources and their TTCs live under the build directory, never in the source tree.
jvmFfiDir : {auto c : Ref Ctxt Defs} -> Core String
jvmFfiDir = do d <- getDirs
               pure (build_dir d </> "jvm-ffi")

-- Write a generated module, but only when its contents changed: the TTC's timestamp then stays
-- newer than the source and `buildAll` skips the module, which keeps repeated loads (every IDE
-- `:load`, every REPL start) cheap.
writeBindingModule : {auto c : Ref Ctxt Defs} -> (root : String) -> (String, String) -> Core String
writeBindingModule root (relPath, contents)
    = do let path = root </> relPath
         ensureDirectoryExists (dirOf path)
         existing <- coreLift (readFile path)
         let unchanged = case existing of
                           Right current => current == contents
                           Left _ => False
         unless unchanged $ do
           Right () <- coreLift (writeFile path contents)
             | Left err => throw (FileErr path err)
           pure ()
         pure path

||| Reflect `classes` off the session's JVM classpath, render their binding modules for the
||| members referenced from `scanSources` (demand-driven: markers, `Inherits` and the `Java.Lang`
||| infra are always emitted, member bindings only when referenced), compile them under
||| `<build dir>/jvm-ffi` and make them importable. A no-op without classes.
export
generateJvmFfiBindings : {auto c : Ref Ctxt Defs} ->
                         {auto s : Ref Syn SyntaxInfo} ->
                         {auto o : Ref ROpts REPLOpts} ->
                         (classes : List String) -> (scanSources : List String) -> Core ()
generateJvmFfiBindings [] _ = pure ()
generateJvmFfiBindings classes scanSources
    = do cp <- jvmClasspath <$> getSession
         infos <- for (nub classes) $ \cn => do
                    Right ci <- coreLift (reflectClass cp cn)
                      | Left err => throw (UserError ("JVM FFI import failed for " ++ cn ++ ": " ++ err))
                    pure ci
         contents <- for scanSources $ \f => do
                       Right src <- coreLift (readFile f)
                         | Left _ => pure ""
                       pure src
         let classNames = map (\ci => simpleType (binary ci)) infos
         let scans = map (scanReferences classNames) contents
         let refs = MkScanRefs (nub (concatMap qualified scans)) (nub (concatMap bare scans))
         ffiRoot <- jvmFfiDir
         let srcRoot = ffiRoot </> "src"
         paths <- traverse (writeBindingModule srcRoot) (renderAll (Just refs) infos)
         -- Compile the generated modules as their own little source tree: `ctxtPathToNS` derives
         -- module names relative to the source dir and TTCs land in `<build dir>/ttc/<version>`,
         -- so point both at the jvm-ffi dir for the duration and restore afterwards.
         dirs <- getDirs
         setSourceDir (Just srcRoot)
         setBuildDir ffiRoot
         errs <- buildAll paths
         setSourceDir (source_dir dirs)
         setBuildDir (build_dir dirs)
         let [] = errs
           | (err :: _) => throw err
         addExtraDir (ffiRoot </> "ttc")

||| Reflect the named JVM classes and print their callable member catalog (signatures only) to
||| stdout. Unlike binding generation this elaborates nothing — it is a discovery aid (and the
||| data source for IDE completion) so a member can be found *before* it is referenced, breaking
||| the demand-driven importer's chicken-and-egg.
export
generateJvmFfiCatalog : {auto c : Ref Ctxt Defs} -> List String -> Core ()
generateJvmFfiCatalog classes
    = do cp <- jvmClasspath <$> getSession
         infos <- for classes $ \cn => do
                    Right ci <- coreLift (reflectClass cp cn)
                      | Left err => throw (UserError ("JVM FFI list failed for " ++ cn ++ ": " ++ err))
                    pure ci
         coreLift $ putStrLn (renderCatalog infos)
