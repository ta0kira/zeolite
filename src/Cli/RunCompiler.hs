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
  processResults passed failed (mapErrorsM_ snd allResults) where
    compilerHash = getCompilerHash backend
    preloadTests base (ca,ms) d = do
      m <- loadModuleMetadata compilerHash f ca (p </> d)
      let ca2 = ca `Map.union` mapMetadata [m]
      rm <- loadRecompile (p </> d)
      deps0 <- loadPublicDeps compilerHash f ca2 [base]
      let ca3 = ca2 `Map.union` mapMetadata deps0
      deps1 <- loadTestingDeps compilerHash f ca3 m
      let ca4 = ca3 `Map.union` mapMetadata deps1
      deps2 <- loadPrivateDeps compilerHash f ca4 (deps0++[m]++deps1)
      let ca5 = ca4 `Map.union` mapMetadata deps2
      em <- getExprMap (p </> d) rm
      return (ca5,ms ++ [LoadedTests p d m em (deps0++[m]++deps1) deps2])
    checkTestFilters ts = do
      let possibleTests = Set.fromList $ concat $ map (cmTestFiles . ltMetadata) ts
      case Set.toList $ (Set.fromList tp) `Set.difference` possibleTests of
          [] -> return ()
          fs -> compileErrorM $ "Some test files do not occur in the selected modules: " ++
                                intercalate ", " (map show fs) ++ "\n"
    processResults passed failed rs
      | isCompileError rs =
        (fromCompileInfo rs) `reviseErrorM`
          ("\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)")
      | otherwise =
        compileWarningM $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"

runCompiler resolver backend (CompileOptions _ is is2 _ _ _ p (CompileFast c fn f2) f) = do
  dir <- errorFromIO $ mkdtemp "/tmp/zfast_"
  absolute <- errorFromIO $ canonicalizePath p
  f2' <- errorFromIO $ canonicalizePath (p </> f2)
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
  compileModule resolver backend spec `reviseErrorM` ("In compilation of \"" ++ f2' ++ "\"")
  errorFromIO $ removeDirectoryRecursive dir

runCompiler resolver backend (CompileOptions h _ _ ds _ _ p CompileRecompileRecursive f) = do
  foldM (recursive resolver) Set.empty ds >> return () where
    recursive r da d0 = do
      isSystem <- isSystemModule r p d0
      if isSystem
         then do
           compileWarningM $ "Skipping system module " ++ d0 ++ "."
           return da
         else do
           d <- errorFromIO $ canonicalizePath (p </> d0)
           rm <- loadRecompile d
           if rmPath rm `Set.member` da
               then return da
               else do
                 let ds3 = map (\d2 -> d </> d2) (rmPublicDeps rm ++ rmPrivateDeps rm)
                 da' <- foldM (recursive r) (rmPath rm `Set.insert` da) ds3
                 runCompiler resolver backend (CompileOptions h [] [] [d] [] [] p CompileRecompile f)
                 return da'

runCompiler resolver backend (CompileOptions _ _ _ ds _ _ p CompileRecompile f) = do
  mergeAllM $ map recompileSingle ds where
    compilerHash = getCompilerHash backend
    recompileSingle d0 = do
      d <- errorFromIO $ canonicalizePath (p </> d0)
      upToDate <- isPathUpToDate compilerHash f d
      if f < ForceAll && upToDate
         then compileWarningM $ "Path " ++ d0 ++ " is up to date"
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
           compileModule resolver backend spec `reviseErrorM` ("In compilation of module \"" ++ d ++ "\"")

runCompiler resolver backend (CompileOptions _ is is2 ds _ _ p CreateTemplates f) = mapM_ compileSingle ds where
  compilerHash = getCompilerHash backend
  compileSingle d = do
    d' <- errorFromIO $ canonicalizePath (p </> d)
    base <- resolveBaseModule resolver
    as  <- fmap fixPaths $ mapErrorsM (resolveModule resolver d') is
    as2 <- fmap fixPaths $ mapErrorsM (resolveModule resolver d') is2
    deps1 <- loadPublicDeps compilerHash f Map.empty (base:as)
    deps2 <- loadPublicDeps compilerHash f (mapMetadata deps1) as2
    path <- errorFromIO $ canonicalizePath p
    createModuleTemplates path d deps1 deps2 `reviseErrorM` ("In module \"" ++ d' ++ "\"")

runCompiler resolver backend (CompileOptions h is is2 ds es ep p m f) = mapM_ compileSingle ds where
  compileSingle d = do
    as  <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is
    as2 <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is2
    isConfigured <- isPathConfigured d
    when (isConfigured && f == DoNotForce) $ do
      compileErrorM $ "Module " ++ d ++ " has an existing configuration. " ++
                      "Recompile with -r or use -f to overwrite the config."
    absolute <- errorFromIO $ canonicalizePath p
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
    runCompiler resolver backend (CompileOptions h [] [] [d] [] [] p CompileRecompile DoNotForce)
