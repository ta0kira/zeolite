{- -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

module Cli.RunCompiler (
  TraceEntry(..),
  parseTracesFile,
  runCompiler,
) where

import Control.Monad (foldM,when)
import Data.List (intercalate,isSuffixOf,nub)
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Temp (mkdtemp)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.TrackedErrors
import Cli.CompileOptions
import Cli.Compiler
import Cli.Programs
import Module.CompileMetadata
import Module.Paths
import Module.ProcessMetadata
import Parser.Common
import Parser.TextParser


runCompiler :: (PathIOHandler r, CompilerBackend b) => r -> b -> CompileOptions -> TrackedErrorsIO ()
runCompiler resolver backend (CompileOptions _ _ _ ds _ _ p (ExecuteTests tp cl) f) = do
  base <- resolveBaseModule resolver
  ts <- fmap snd $ foldM preloadTests (Map.empty,[]) ds
  checkTestFilters ts
  cl2 <- prepareCallLog cl
  allResults <- fmap concat $ mapCompilerM (runModuleTests resolver backend cl2 base tp) ts
  let passed = sum $ map (fst . fst) allResults
  let failed = sum $ map (snd . fst) allResults
  processResults passed failed (mapCompilerM_ snd allResults) where
    prepareCallLog (Just cl2) = do
      clFull <- errorFromIO $ canonicalizePath (p </> cl2)
      compilerWarningM $ "Logging calls to " ++ clFull
      errorFromIO $ writeFile clFull (intercalate "," (map show tracesLogHeader) ++ "\n")
      return clFull
    prepareCallLog _ = return ""
    preloadTests (ca,ms) d = do
      compilerHash <- getCompilerHash backend
      m <- loadModuleMetadata compilerHash f ca (p </> d)
      let ca2 = ca `Map.union` mapMetadata [m]
      rm <- loadRecompile (p </> d)
      let ca3 = ca2 `Map.union` mapMetadata []
      deps1 <- loadTestingDeps compilerHash f ca3 m
      let ca4 = ca3 `Map.union` mapMetadata deps1
      deps2 <- loadPrivateDeps compilerHash f ca4 ([m]++deps1)
      let ca5 = ca4 `Map.union` mapMetadata deps2
      em <- getExprMap (p </> d) rm
      return (ca5,ms ++ [LoadedTests m em (deps1) deps2])
    checkTestFilters ts = do
      let possibleTests = concat $ map (cmTestFiles . ltMetadata) ts
      let remaining = filter (not . flip any (map (flip isSuffixOf) possibleTests). flip ($)) tp
      case remaining of
          [] -> return ()
          fs -> compilerErrorM $ "Some test files do not occur in the selected modules: " ++
                                intercalate ", " (map show fs) ++ "\n"
    processResults passed failed rs
      | isCompilerError rs =
        (fromTrackedErrors rs) <!!
          "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"
      | otherwise =
        errorFromIO $ hPutStrLn stderr $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"

runCompiler resolver backend (CompileOptions _ is is2 _ _ _ p (CompileFast c fn f2) f) = do
  dir <- errorFromIO $ mkdtemp "/tmp/zfast_"
  absolute <- errorFromIO $ canonicalizePath p
  f2' <- errorFromIO $ canonicalizePath (p </> f2)
  let rm = ModuleConfig {
    mcRoot = "",
    mcPath = ".",
    mcExtra = [],
    mcExprMap = [],
    mcPublicDeps = [],
    mcPrivateDeps = [],
    mcExtraFiles = [],
    mcExtraPaths = [],
    mcMode = CompileUnspecified
  }
  em <- getExprMap p rm
  let spec = ModuleSpec {
    msRoot = absolute,
    msPath = dir,
    msExtra = [],
    msExprMap = em,
    msPublicDeps = is,
    msPrivateDeps = is2,
    msPublicFiles = [],
    msPrivateFiles = [f2'],
    msTestFiles = [],
    msExtraFiles = [],
    msExtraPaths = [],
    msMode = (CompileBinary c fn LinkStatic (absolute </> show c) []),
    msForce = f
  }
  compileModule resolver backend spec <?? "In compilation of \"" ++ f2' ++ "\""
  errorFromIO $ removeDirectoryRecursive dir

runCompiler resolver backend (CompileOptions _ _ _ ds _ _ p CompileRecompileRecursive f) =
  runRecompileCommon resolver backend f True p ds

runCompiler resolver backend (CompileOptions _ _ _ ds _ _ p CompileRecompile f) =
  runRecompileCommon resolver backend f False p ds

runCompiler resolver backend (CompileOptions _ is is2 ds _ _ p CreateTemplates f) = mapM_ compileSingle ds where
  compileSingle d = do
    compilerHash <- getCompilerHash backend
    d' <- errorFromIO $ canonicalizePath (p </> d)
    (ep,is',is2') <- maybeUseConfig d'
    as  <- fmap fixPaths $ mapCompilerM (resolveModule resolver d') is'
    as2 <- fmap fixPaths $ mapCompilerM (resolveModule resolver d') is2'
    isBase <- isBaseModule resolver d'
    deps1 <- if isBase
                then loadPublicDeps compilerHash f Map.empty as
                else do
                  base <- resolveBaseModule resolver
                  loadPublicDeps compilerHash f Map.empty (base:as)
    deps2 <- loadPublicDeps compilerHash f (mapMetadata deps1) as2
    path <- errorFromIO $ canonicalizePath p
    createModuleTemplates resolver path d ep deps1 deps2 <?? "In module \"" ++ d' ++ "\""
  maybeUseConfig d2 = do
    let rm = loadRecompile d2
    isError <- isCompilerErrorM rm
    if isError
       then return ([],is,is2)
       else do
         (ModuleConfig p2 _ ep _ is3 is4 _ _ _) <- rm
         return (map (p2 </>) ep,nub $ is ++ is3,nub $ is2 ++ is4)

runCompiler resolver _ (CompileOptions _ is is2 ds es ep p m f) = mapM_ compileSingle ds where
  compileSingle d = do
    as  <- fmap fixPaths $ mapCompilerM (autoDep (p </> d)) is
    as2 <- fmap fixPaths $ mapCompilerM (autoDep (p </> d)) is2
    isConfigured <- isPathConfigured p d
    when (isConfigured && f == DoNotForce) $ do
      compilerErrorM $ "Module " ++ d ++ " has an existing configuration. " ++
                       "Recompile with -r or use -f to overwrite the config."
    absolute <- errorFromIO $ canonicalizePath p
    let rm = ModuleConfig {
      mcRoot = absolute,
      mcPath = d,
      mcExtra = [],
      mcExprMap = [],
      mcPublicDeps = as,
      mcPrivateDeps = as2,
      mcExtraFiles = es,
      mcExtraPaths = ep,
      mcMode = m
    }
    writeRecompile (p </> d) rm
    config <- getRecompilePath (p </> d)
    errorFromIO $ hPutStrLn stderr $ "*** Setup complete. Please edit " ++ config ++ " and recompile with zeolite -r. ***"
  autoDep p2 i = do
    isSystem <- isSystemModule resolver p2 i
    if isSystem
       then return i
       else resolveModule resolver p2 i

data TraceEntry =
  TraceEntry {
    teMicroseconds :: Integer,
    teProcess :: Integer,
    teFunction :: String,
    teContext :: String
  }

tracesLogHeader :: [String]
tracesLogHeader = ["microseconds","pid","function","context"]

parseTracesFile :: (FilePath,String) -> TrackedErrorsIO [TraceEntry]
parseTracesFile (f,s) = runTextParser (between nullParse endOfDoc tracesFile) f s where
  tracesFile =  do
    parseHeader
    many parseSingle
  parseHeader = do
    sequence_ $ intercalate [string_ ","] $ map ((:[]) . parseColTitle) tracesLogHeader
    some (char '\n' <|> char '\r') >> return ()
  parseSingle = do
    ms <- parseDec
    string_ ","
    pid <- parseDec
    string_ ","
    func <- quotedString
    string_ ","
    c <- quotedString
    some (char '\n' <|> char '\r') >> return ()
    return $ TraceEntry ms pid func c
  parseColTitle expected = do
    title <- quotedString
    when (expected /= title) $ compilerErrorM $ "Expected column named \"" ++ expected ++ "\" but found \"" ++ title ++ "\""

runRecompileCommon :: (PathIOHandler r, CompilerBackend b) => r -> b ->
  ForceMode -> Bool -> FilePath -> [FilePath] -> TrackedErrorsIO ()
runRecompileCommon resolver backend f rec p ds = do
  explicit <- fmap Set.fromList $ mapCompilerM (errorFromIO . canonicalizePath . (p </>)) ds
  foldM (recursive resolver explicit) Set.empty (map ((,) p) ds) >> return () where
    recursive r explicit da (p2,d0) = do
      d <- resolveModule r p2 d0
      isSystem <- isSystemModule r p2 d0
      let process = if rec
                       then d `Set.member` explicit || not isSystem
                       else d `Set.member` explicit
      if not process || d `Set.member` da
         then return da
         else do
           rm <- loadRecompile d
           let ds3 = map ((,) d) (mcPublicDeps rm ++ mcPrivateDeps rm)
           da' <- foldM (recursive r explicit) (d `Set.insert` da) ds3
           recompile d
           return da'
    recompile d = do
      compilerHash <- getCompilerHash backend
      upToDate <- isPathUpToDate compilerHash f d
      if f < ForceAll && upToDate
         then compilerWarningM $ "Path " ++ d ++ " is up to date"
         else do
           rm@(ModuleConfig p2 d2 ee _ is is2 es ep m) <- loadRecompile d
           let fixed = fixPath (d </> p2)
           (ps,xs,ts) <- findSourceFiles fixed (d2:ee)
           em <- getExprMap d rm
           let spec = ModuleSpec {
             msRoot = fixed,
             msPath = d2,
             msExtra = ee,
             msExprMap = em,
             msPublicDeps = is,
             msPrivateDeps = is2,
             msPublicFiles = ps,
             msPrivateFiles = xs,
             msTestFiles = ts,
             msExtraFiles = es,
             msExtraPaths = ep,
             msMode = m,
             msForce = f
           }
           compileModule resolver backend spec <?? "In compilation of module \"" ++ d ++ "\""
