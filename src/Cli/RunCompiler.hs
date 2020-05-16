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
import System.Exit
import System.FilePath
import System.Posix.Temp (mkdtemp)
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.Mergeable
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.Compiler
import Cli.ProcessMetadata
import Compilation.CompileInfo
import Config.LoadConfig
import Config.Paths
import Config.Programs


runCompiler :: CompileOptions -> IO ()
runCompiler (CompileOptions _ _ _ ds _ _ p (ExecuteTests tp) f) = do
  (backend,resolver) <- loadConfig
  base <- resolveBaseModule resolver
  ts <- fmap snd $ foldM (preloadTests backend base) (Map.empty,[]) ds
  checkTestFilters ts
  allResults <- fmap concat $ sequence $ map (runModuleTests backend base tp) ts
  let passed = sum $ map (fst . fst) allResults
  let failed = sum $ map (snd . fst) allResults
  processResults passed failed (mergeAll $ map snd allResults) where
    preloadTests b base (ca,ms) d = do
      m <- loadMetadata ca (p </> d)
      let ca2 = ca `Map.union` mapMetadata [m]
      fr <- checkMetadataFreshness (p </> d) m
      checkAllowedStale fr f
      rm <- tryLoadRecompile (p </> d)
      rm' <- case rm of
                  Just rm2 -> return rm2
                  Nothing -> do
                    hPutStr stderr $ "Module config for " ++ d ++ " is missing."
                    exitFailure
      (fr0,deps0) <- loadPublicDeps (getCompilerHash b) ca2 [base]
      let ca3 = ca2 `Map.union` mapMetadata deps0
      checkAllowedStale fr0 f
      (fr1,deps1) <- loadTestingDeps (getCompilerHash b) ca3 m
      let ca4 = ca3 `Map.union` mapMetadata deps1
      checkAllowedStale fr1 f
      (fr2,deps2) <- loadPrivateDeps (getCompilerHash b) ca4 (deps0++[m]++deps1)
      let ca5 = ca4 `Map.union` mapMetadata deps2
      checkAllowedStale fr2 f
      em <- getExprMap (p </> d) rm'
      return (ca5,ms ++ [LoadedTests p d m em (deps0++[m]++deps1) deps2])
    checkTestFilters ts = do
      let possibleTests = Set.fromList $ concat $ map (cmTestFiles . ltMetadata) ts
      case Set.toList $ (Set.fromList tp) `Set.difference` possibleTests of
          [] -> return ()
          fs -> do
            hPutStr stderr $ "Some test files do not occur in the selected modules: " ++
                             intercalate ", " (map show fs) ++ "\n"
            exitFailure
    processResults passed failed rs
      | isCompileError rs = do
          hPutStr stderr $ "\nTest errors:\n" ++ (show $ getCompileError rs)
          hPutStrLn stderr $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"
          hPutStrLn stderr $ "Zeolite tests failed."
          exitFailure
      | otherwise = do
          hPutStrLn stderr $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"
          hPutStrLn stderr $ "Zeolite tests passed."

runCompiler (CompileOptions _ is is2 _ _ _ p (CompileFast c fn f2) f) = do
  dir <- mkdtemp "/tmp/zfast_"
  absolute <- canonicalizePath p
  f2' <- canonicalizePath (p </> f2)
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
    msMode = (CompileBinary c fn (absolute </> show c) []),
    msForce = f
  }
  compileModule spec
  removeDirectoryRecursive dir

runCompiler (CompileOptions h _ _ ds _ _ p CompileRecompileRecursive f) = do
  (_,resolver) <- loadConfig
  foldM (recursive resolver) Set.empty ds >> return () where
    recursive r da d0 = do
      isSystem <- isSystemModule r p d0
      if isSystem
         then do
           hPutStrLn stderr $ "Skipping system module " ++ d0 ++ "."
           exitFailure
         else do
           d <- canonicalizePath (p </> d0)
           rm <- tryLoadRecompile d
           case rm of
                Nothing -> do
                  hPutStrLn stderr $ "Path " ++ d ++ " does not have a valid configuration."
                  exitFailure
                Just m ->
                  if rmPath m `Set.member` da
                     then return da
                     else do
                       let ds3 = map (\d2 -> d </> d2) (rmPublicDeps m ++ rmPrivateDeps m)
                       da' <- foldM (recursive r) (rmPath m `Set.insert` da) ds3
                       runCompiler (CompileOptions h [] [] [d] [] [] p CompileRecompile f)
                       return da'

runCompiler (CompileOptions _ _ _ ds _ _ p CompileRecompile f) = do
  (backend,_) <- loadConfig
  fmap mergeAll $ sequence $ map (recompileSingle $ getCompilerHash backend) ds where
    recompileSingle h2 d0 = do
      let d = p </> d0
      rm <- tryLoadRecompile d
      upToDate <- isPathUpToDate h2 d
      maybeCompile rm upToDate where
        maybeCompile Nothing _ = do
          hPutStrLn stderr $ "Path " ++ d0 ++ " does not have a valid configuration."
          exitFailure
        maybeCompile (Just rm') upToDate
          | f < ForceAll && upToDate = hPutStrLn stderr $ "Path " ++ d0 ++ " is up to date."
          | otherwise = do
              let (ModuleConfig p2 d _ is is2 es ep m) = rm'
              -- In case the module is manually configured with a p such as "..",
              -- since the absolute path might not be known ahead of time.
              absolute <- canonicalizePath (p </> d0)
              let fixed = fixPath (absolute </> p2)
              (ps,xs,ts) <- findSourceFiles fixed d
              em <- getExprMap (p </> d0) rm'
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
              compileModule spec

runCompiler (CompileOptions _ is is2 ds _ _ p CreateTemplates f) = mapM_ compileSingle ds where
  compileSingle d = do
    (backend,resolver) <- loadConfig
    base <- resolveBaseModule resolver
    as  <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is
    as2 <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is2
    (fr1,deps1) <- loadPublicDeps (getCompilerHash backend) Map.empty (base:as)
    checkAllowedStale fr1 f
    (fr2,deps2) <- loadPublicDeps (getCompilerHash backend) (mapMetadata deps1) as2
    checkAllowedStale fr2 f
    path <- canonicalizePath p
    createModuleTemplates path d deps1 deps2

runCompiler (CompileOptions h is is2 ds es ep p m f) = mapM_ compileSingle ds where
  compileSingle d = do
    (_,resolver) <- loadConfig
    as  <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is
    as2 <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is2
    isConfigured <- isPathConfigured d
    when (isConfigured && f == DoNotForce) $ do
      hPutStrLn stderr $ "Module " ++ d ++ " has an existing configuration. " ++
                        "Recompile with -r or use -f to overwrite the config."
      exitFailure
    absolute <- canonicalizePath p
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
    writeRecompile (p </> d) rm
    runCompiler (CompileOptions h [] [] [d] [] [] p CompileRecompile DoNotForce)
