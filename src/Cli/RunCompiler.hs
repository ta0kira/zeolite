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
  runCompiler,
) where

import Control.Monad (foldM,when)
import Data.List (intercalate,nub)
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
      errorFromIO $ writeFile clFull (intercalate "," (map show logHeader) ++ "\n")
      return clFull
    prepareCallLog _ = return ""
    logHeader = ["microseconds","pid","function","context"]
    compilerHash = getCompilerHash backend
    preloadTests (ca,ms) d = do
      m <- loadModuleMetadata compilerHash f ca (p </> d)
      let ca2 = ca `Map.union` mapMetadata [m]
      rm <- loadRecompile (p </> d)
      let ca3 = ca2 `Map.union` mapMetadata []
      deps1 <- loadTestingDeps compilerHash f ca3 m
      let ca4 = ca3 `Map.union` mapMetadata deps1
      deps2 <- loadPrivateDeps compilerHash f ca4 ([m]++deps1)
      let ca5 = ca4 `Map.union` mapMetadata deps2
      em <- getExprMap (p </> d) rm
      return (ca5,ms ++ [LoadedTests p d m em (deps1) deps2])
    checkTestFilters ts = do
      let possibleTests = Set.fromList $ concat $ map (cmTestFiles . ltMetadata) ts
      case Set.toList $ (Set.fromList tp) `Set.difference` possibleTests of
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

runCompiler resolver backend (CompileOptions h _ _ ds _ _ p CompileRecompileRecursive f) = do
  explicit <- fmap Set.fromList $ mapCompilerM (errorFromIO . canonicalizePath . (p </>)) ds
  foldM (recursive resolver explicit) Set.empty (map ((,) p) ds) >> return () where
    recursive r explicit da (p2,d0) = do
      d <- resolveModule r p2 d0
      isSystem <- isSystemModule r p2 d0
      if (isSystem && not (d `Set.member` explicit)) || d `Set.member` da
         then return da
         else do
           rm <- loadRecompile d
           let ds3 = map ((,) d) (mcPublicDeps rm ++ mcPrivateDeps rm)
           da' <- foldM (recursive r explicit) (d `Set.insert` da) ds3
           runCompiler resolver backend (CompileOptions h [] [] [d] [] [] p CompileRecompile f)
           return da'

runCompiler resolver backend (CompileOptions _ _ _ ds _ _ p CompileRecompile f) = do
  mapCompilerM_ recompileSingle ds where
    compilerHash = getCompilerHash backend
    recompileSingle d0 = do
      d <- errorFromIO $ canonicalizePath (p </> d0)
      upToDate <- isPathUpToDate compilerHash f d
      if f < ForceAll && upToDate
         then compilerWarningM $ "Path " ++ d0 ++ " is up to date"
         else do
           rm@(ModuleConfig p2 d2 _ is is2 es ep m) <- loadRecompile d
           -- In case the module is manually configured with a p such as "..",
           -- since the absolute path might not be known ahead of time.
           absolute <- errorFromIO $ canonicalizePath (p </> d0)
           let fixed = fixPath (absolute </> p2)
           (ps,xs,ts) <- findSourceFiles fixed d2
           em <- getExprMap (p </> d0) rm
           let spec = ModuleSpec {
             msRoot = fixed,
             msPath = d2,
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

runCompiler resolver backend (CompileOptions _ is is2 ds _ _ p CreateTemplates f) = mapM_ compileSingle ds where
  compilerHash = getCompilerHash backend
  compileSingle d = do
    d' <- errorFromIO $ canonicalizePath (p </> d)
    (is',is2') <- maybeUseConfig d'
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
    createModuleTemplates resolver path d deps1 deps2 <?? "In module \"" ++ d' ++ "\""
  maybeUseConfig d2 = do
    let rm = loadRecompile d2
    isError <- isCompilerErrorM rm
    if isError
       then return (is,is2)
       else do
         (ModuleConfig _ _ _ is3 is4 _ _ _) <- rm
         return (nub $ is ++ is3,nub $ is2 ++ is4)

runCompiler resolver backend (CompileOptions h is is2 ds es ep p m f) = mapM_ compileSingle ds where
  compileSingle d = do
    as  <- fmap fixPaths $ mapCompilerM (resolveModule resolver (p </> d)) is
    as2 <- fmap fixPaths $ mapCompilerM (resolveModule resolver (p </> d)) is2
    isConfigured <- isPathConfigured p d
    when (isConfigured && f == DoNotForce) $ do
      compilerErrorM $ "Module " ++ d ++ " has an existing configuration. " ++
                       "Recompile with -r or use -f to overwrite the config."
    absolute <- errorFromIO $ canonicalizePath p
    let rm = ModuleConfig {
      mcRoot = absolute,
      mcPath = d,
      mcExprMap = [],
      mcPublicDeps = as,
      mcPrivateDeps = as2,
      mcExtraFiles = es,
      mcExtraPaths = ep,
      mcMode = m
    }
    writeRecompile (p </> d) rm
    runCompiler resolver backend (CompileOptions h [] [] [d] [] [] p CompileRecompile DoNotForce)
