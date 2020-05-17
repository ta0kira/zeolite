{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

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
import Data.List (intercalate)
import System.Directory
import System.FilePath
import System.Posix.Temp (mkdtemp)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.Compiler
import Cli.Paths
import Cli.ProcessMetadata
import Cli.Programs
import Compilation.CompileInfo


runCompiler :: (PathIOHandler r, CompilerBackend b) => r -> b -> CompileOptions -> CompileInfoIO ()
runCompiler resolver backend (CompileOptions _ _ _ ds _ _ p (ExecuteTests tp) f) = do
  base <- resolveBaseModule resolver
  ts <- fmap snd $ foldM (preloadTests base) (Map.empty,[]) ds
  checkTestFilters ts
  allResults <- fmap concat $ mapErrorsM (runModuleTests resolver backend base tp) ts
  let passed = sum $ map (fst . fst) allResults
  let failed = sum $ map (snd . fst) allResults
  processResults passed failed (mergeAll $ map snd allResults) where
    compilerHash = getCompilerHash backend
    preloadTests base (ca,ms) d = do
      m <- lift $ loadMetadata ca (p </> d)
      let ca2 = ca `Map.union` mapMetadata [m]
      fr <- lift $ checkMetadataFreshness (p </> d) m
      lift $ checkAllowedStale fr f
      rm <- lift $ tryLoadRecompile (p </> d)
      rm' <- case rm of
                  Just rm2 -> return rm2
                  Nothing -> compileErrorM $ "Module config for " ++ d ++ " is missing."
      (fr0,deps0) <- lift $ loadPublicDeps compilerHash ca2 [base]
      let ca3 = ca2 `Map.union` mapMetadata deps0
      lift $ checkAllowedStale fr0 f
      (fr1,deps1) <- lift $ loadTestingDeps compilerHash ca3 m
      let ca4 = ca3 `Map.union` mapMetadata deps1
      lift $ checkAllowedStale fr1 f
      (fr2,deps2) <- lift $ loadPrivateDeps compilerHash ca4 (deps0++[m]++deps1)
      let ca5 = ca4 `Map.union` mapMetadata deps2
      lift $ checkAllowedStale fr2 f
      em <- lift $ getExprMap (p </> d) rm'
      return (ca5,ms ++ [LoadedTests p d m em (deps0++[m]++deps1) deps2])
    checkTestFilters ts = do
      let possibleTests = Set.fromList $ concat $ map (cmTestFiles . ltMetadata) ts
      case Set.toList $ (Set.fromList tp) `Set.difference` possibleTests of
          [] -> return ()
          fs -> compileErrorM $ "Some test files do not occur in the selected modules: " ++
                                intercalate ", " (map show fs) ++ "\n"
    processResults passed failed rs
      | isCompileError rs =
        (fromCompileInfo rs >> return ()) `reviseErrorM`
          ("\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)")
      | otherwise =
        compileWarningM $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"

runCompiler resolver backend (CompileOptions _ is is2 _ _ _ p (CompileFast c fn f2) f) = do
  dir <- lift $ mkdtemp "/tmp/zfast_"
  absolute <- lift $ canonicalizePath p
  f2' <- lift $ canonicalizePath (p </> f2)
  let rm = ModuleConfig {
    rmRoot = "",
    rmPath = ".",
    rmExprMap = [],
    rmPublicDeps = [],
    rmPrivateDeps = [],
    rmExtraFiles = [],
    rmExtraPaths = [],
    rmMode = CompileUnspecified
  }
  em <- lift $ getExprMap p rm
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
    msMode = (CompileBinary c fn (absolute </> show c) []),
    msForce = f
  }
  compileModule resolver backend spec
  lift $ removeDirectoryRecursive dir

runCompiler resolver backend (CompileOptions h _ _ ds _ _ p CompileRecompileRecursive f) = do
  foldM (recursive resolver) Set.empty ds >> return () where
    recursive r da d0 = do
      isSystem <- isSystemModule r p d0
      if isSystem
         then do
           compileWarningM $ "Skipping system module " ++ d0 ++ "."
           return da
         else do
           d <- lift $ canonicalizePath (p </> d0)
           rm <- lift $ tryLoadRecompile d
           case rm of
                Nothing -> compileErrorM $ "Path " ++ d ++ " does not have a valid configuration."
                Just m ->
                  if rmPath m `Set.member` da
                     then return da
                     else do
                       let ds3 = map (\d2 -> d </> d2) (rmPublicDeps m ++ rmPrivateDeps m)
                       da' <- foldM (recursive r) (rmPath m `Set.insert` da) ds3
                       runCompiler resolver backend (CompileOptions h [] [] [d] [] [] p CompileRecompile f)
                       return da'

runCompiler resolver backend (CompileOptions _ _ _ ds _ _ p CompileRecompile f) = do
  mergeAllM $ map recompileSingle ds where
    compilerHash = getCompilerHash backend
    recompileSingle d0 = do
      let d = p </> d0
      rm <- lift $ tryLoadRecompile d
      upToDate <- lift $ isPathUpToDate compilerHash d
      maybeCompile rm upToDate where
        maybeCompile Nothing _ = compileErrorM $ "Path " ++ d0 ++ " does not have a valid configuration."
        maybeCompile (Just rm') upToDate
          | f < ForceAll && upToDate = compileWarningM $ "Path " ++ d0 ++ " is up to date."
          | otherwise = do
              let (ModuleConfig p2 d _ is is2 es ep m) = rm'
              -- In case the module is manually configured with a p such as "..",
              -- since the absolute path might not be known ahead of time.
              absolute <- lift $ canonicalizePath (p </> d0)
              let fixed = fixPath (absolute </> p2)
              (ps,xs,ts) <- lift $ findSourceFiles fixed d
              em <- lift $ getExprMap (p </> d0) rm'
              let spec = ModuleSpec {
                msRoot = fixed,
                msPath = d,
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
              compileModule resolver backend spec

runCompiler resolver backend (CompileOptions _ is is2 ds _ _ p CreateTemplates f) = mapM_ compileSingle ds where
  compilerHash = getCompilerHash backend
  compileSingle d = do
    base <- resolveBaseModule resolver
    as  <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is
    as2 <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is2
    (fr1,deps1) <- lift $ loadPublicDeps compilerHash Map.empty (base:as)
    lift $ checkAllowedStale fr1 f
    (fr2,deps2) <- lift $ loadPublicDeps compilerHash (mapMetadata deps1) as2
    lift $ checkAllowedStale fr2 f
    path <- lift $ canonicalizePath p
    createModuleTemplates path d deps1 deps2

runCompiler resolver backend (CompileOptions h is is2 ds es ep p m f) = mapM_ compileSingle ds where
  compileSingle d = do
    as  <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is
    as2 <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is2
    isConfigured <- lift $ isPathConfigured d
    when (isConfigured && f == DoNotForce) $ do
      compileErrorM $ "Module " ++ d ++ " has an existing configuration. " ++
                      "Recompile with -r or use -f to overwrite the config."
    absolute <- lift $ canonicalizePath p
    let rm = ModuleConfig {
      rmRoot = absolute,
      rmPath = d,
      rmExprMap = [],
      rmPublicDeps = as,
      rmPrivateDeps = as2,
      rmExtraFiles = es,
      rmExtraPaths = ep,
      rmMode = m
    }
    lift $ writeRecompile (p </> d) rm
    runCompiler resolver backend (CompileOptions h [] [] [d] [] [] p CompileRecompile DoNotForce)
