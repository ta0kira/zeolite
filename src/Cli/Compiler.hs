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

module Cli.Compiler (
  runCompiler,
) where

import Control.Monad (when)
import Data.Either (partitionEithers)
import Data.List (intercalate,isSuffixOf,nub,sort)
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Temp (mkdtemp,mkstemps)
import System.IO
import qualified Data.Set as Set

import Base.CompileError
import Base.Mergeable
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.ProcessMetadata
import Cli.TestRunner -- Not safe, due to Text.Regex.TDFA.
import Compilation.CompileInfo
import CompilerCxx.Category
import CompilerCxx.Naming
import Config.LoadConfig
import Config.Paths
import Config.Programs
import Parser.SourceFile
import Types.Builtin
import Types.DefinedCategory
import Types.TypeCategory
import Types.TypeInstance


runCompiler :: CompileOptions -> IO ()
runCompiler (CompileOptions _ _ _ ds _ _ p (ExecuteTests tp) f) = do
  (backend,resolver) <- loadConfig
  base <- resolveBaseModule resolver
  ds' <- sequence $ map (preloadModule backend base) ds
  let possibleTests = Set.fromList $ concat $ map getTestsFromPreload ds'
  case Set.toList $ allowTests `Set.difference` possibleTests of
       [] -> return ()
       ts -> do
         hPutStr stderr $ "Some test files do not occur in the selected modules: " ++
                          intercalate ", " (map show ts) ++ "\n"
         exitFailure
  allResults <- fmap concat $ sequence $ map (runTests backend base) ds'
  let passed = sum $ map (fst . fst) allResults
  let failed = sum $ map (snd . fst) allResults
  processResults passed failed (mergeAllM $ map snd allResults) where
    preloadModule b base d = do
      m <- loadMetadata (p </> d)
      (fr0,deps0) <- loadPublicDeps (getCompilerHash b) [base]
      checkAllowedStale fr0 f
      (fr1,deps1) <- loadTestingDeps (getCompilerHash b) (p </> d)
      checkAllowedStale fr1 f
      (fr2,deps2) <- loadPrivateDeps (getCompilerHash b) (deps0++deps1)
      checkAllowedStale fr2 f
      return (d,m,deps0++deps1,deps2)
    getTestsFromPreload (_,m,_,_) = cmTestFiles m
    allowTests = Set.fromList tp
    isTestAllowed t = if null allowTests then True else t `Set.member` allowTests
    runTests :: CompilerBackend b => b -> String ->
                (String,CompileMetadata,[CompileMetadata],[CompileMetadata]) ->
                IO [((Int,Int),CompileInfo ())]
    runTests b base (d,m,deps1,deps2) = do
      let paths = base:(getIncludePathsForDeps deps1)
      let ss = fixPaths $ getSourceFilesForDeps deps1
      let os = getObjectFilesForDeps deps2
      ss' <- zipWithContents p ss
      ts' <- zipWithContents p (map (d </>) $ filter isTestAllowed $ cmTestFiles m)
      tm <- return $ do
        cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource ss'
        includeNewTypes defaultCategories cs
      if isCompileError tm
         then return [((0,0),tm >> return ())]
         else sequence $ map (runSingleTest b paths (m:deps1) os (getCompileSuccess tm)) ts'
    processResults passed failed rs
      | isCompileError rs = do
          hPutStr stderr $ "\nTest errors:\n" ++ (show $ getCompileError rs)
          hPutStrLn stderr $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"
          hPutStrLn stderr $ "Zeolite tests failed."
          exitFailure
      | otherwise = do
          hPutStrLn stderr $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"
          hPutStrLn stderr $ "Zeolite tests passed."
runCompiler (CompileOptions h is is2 _ _ _ p (CompileFast c fn f2) f) = do
  dir <- mkdtemp "/tmp/zfast_"
  absolute <- canonicalizePath p
  f2' <- canonicalizePath (p </> f2)
  let config = ModuleConfig {
      rmRoot = absolute,
      rmPath = dir,
      rmPublicDeps = is,
      rmPrivateDeps = is2,
      rmExtraFiles = [OtherSource f2'],
      rmExtraPaths = [],
      rmMode = (CompileBinary c fn (absolute </> c) [])
    }
  writeRecompile dir config
  runCompiler (CompileOptions h [] [] [dir] [] [] "" CompileRecompile f)
  removeDirectoryRecursive dir
runCompiler (CompileOptions h _ _ ds _ _ p CompileRecompileRecursive f) = do
  recursiveSequence Set.empty ds where
    recursiveSequence da (d0:ds2) = do
      d <- canonicalizePath (p </> d0)
      rm <- tryLoadRecompile d
      case rm of
           Nothing -> do
             hPutStrLn stderr $ "Path " ++ d ++ " does not have a valid configuration."
             exitFailure
           Just m -> when (not $ rmPath m `Set.member` da) $ do
             let ds3 = map (\d2 -> d </> d2) (rmPublicDeps m ++ rmPrivateDeps m)
             let da' = rmPath m `Set.insert` da
             recursiveSequence da' (ds3 ++ ds2)
             runCompiler (CompileOptions h [] [] [d] [] [] "" CompileRecompile f)
    recursiveSequence _ _ = return ()
runCompiler (CompileOptions h _ _ ds _ _ p CompileRecompile f) = do
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
              let (ModuleConfig p2 d is is2 es ep m) = rm'
              -- In case the module is manually configured with a p such as "..",
              -- since the absolute path might not be known ahead of time.
              absolute <- canonicalizePath (p </> d0)
              let fixed = fixPath (absolute </> p2)
              let recompile = CompileOptions {
                  coHelp = h,
                  coPublicDeps = is,
                  coPrivateDeps = is2,
                  coSources = [d],
                  coExtraFiles = es,
                  coExtraPaths = ep,
                  coSourcePrefix = fixed,
                  coMode = m,
                  coForce = if f == ForceAll then ForceRecompile else AllowRecompile
                }
              runCompiler recompile
runCompiler (CompileOptions _ is is2 [d] es ep p m f) = do
  (backend,resolver) <- loadConfig
  as  <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is
  as2 <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is2
  (fr,deps) <- loadPublicDeps (getCompilerHash backend) (as ++ as2)
  checkAllowedStale fr f
  if isCreateTemplates m
      then do
        base <- resolveBaseModule resolver
        (fr2,bpDeps) <- loadPublicDeps (getCompilerHash backend) [base]
        checkAllowedStale fr2 f
        processTemplates (bpDeps ++ deps)
      else do
        ((paths2,deps2),ms) <- processPath backend resolver deps as as2
        createBinary backend resolver paths2 (deps2:deps) m ms
  hPutStrLn stderr $ "Zeolite compilation succeeded." where
    ep' = fixPaths $ map (p </>) ep
    processPath b r deps as as2 = do
      isConfigured <- isPathConfigured d
      when (isConfigured && f == DoNotForce) $ do
        hPutStrLn stderr $ "Module " ++ d ++ " has an existing configuration. " ++
                           "Recompile with -r or use -f to overwrite the config."
        exitFailure
      eraseCachedData (p </> d)
      absolute <- canonicalizePath p
      let rm = ModuleConfig {
        rmRoot = absolute,
        rmPath = d,
        rmPublicDeps = as,
        rmPrivateDeps = as2,
        rmExtraFiles = es,
        rmExtraPaths = ep,
        rmMode = m
      }
      when (f == DoNotForce || f == ForceAll) $ writeRecompile (p </> d) rm
      (ps,xs,ts) <- findSourceFiles p d
      ps2 <- sequence $ map canonicalizePath $ filter (isSuffixOf ".0rp") $ map ((p </>) . getSourceFile) es
      xs2 <- sequence $ map canonicalizePath $ filter (isSuffixOf ".0rx") $ map ((p </>) . getSourceFile) es
      ts2 <- sequence $ map canonicalizePath $ filter (isSuffixOf ".0rt") $ map ((p </>) . getSourceFile) es
      base <- resolveBaseModule r
      actual <- resolveModule r p d
      isBase <- isBaseModule r actual
      -- Lazy dependency loading, in case we're compiling base.
      deps2 <- if isBase
                  then return deps
                  else do
                    (fr,bpDeps) <- loadPublicDeps (getCompilerHash b) [base]
                    checkAllowedStale fr f
                    return $ bpDeps ++ deps
      let ss = fixPaths $ getSourceFilesForDeps deps2
      ss' <- zipWithContents p ss
      let paths = base:(getIncludePathsForDeps deps2)
      ps' <- zipWithContents p (ps ++ ps2)
      xs' <- zipWithContents p (xs ++ xs2)
      ns0 <- canonicalizePath (p </> d) >>= return . StaticNamespace . publicNamespace
      let ns2 = map StaticNamespace $ filter (not . null) $ getNamespacesForDeps deps
      let fs = compileAll ns0 ns2 ss' ps' xs'
      writeOutput b paths ns0 deps2 as as2
                  (map takeFileName ps ++ ps2)
                  (map takeFileName xs ++ xs2)
                  (map takeFileName ts ++ ts2) fs
    writeOutput b paths ns0 deps as as2 ps xs ts fs
      | isCompileError fs = do
          formatWarnings fs
          hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError fs)
          hPutStrLn stderr $ "Zeolite compilation failed."
          exitFailure
      | otherwise = do
          formatWarnings fs
          let (pc,mf,fs') = getCompileSuccess fs
          let ss = map (\ns -> getCachedPath (p </> d) ns "") $ nub $ filter (not . null) $ map show $ [ns0] ++ map coNamespace fs'
          ss' <- sequence $ map canonicalizePath ss
          s0 <- canonicalizePath $ getCachedPath (p </> d) (show ns0) ""
          let paths' = paths ++ ep' ++ ss'
          let hxx   = filter (isSuffixOf ".hpp" . coFilename)       fs'
          let other = filter (not . isSuffixOf ".hpp" . coFilename) fs'
          os1 <- sequence $ map (writeOutputFile b (show ns0) paths') $ hxx ++ other
          let files = map (\f2 -> getCachedPath (p </> d) (show $ coNamespace f2) (coFilename f2)) fs' ++
                      map (\f2 -> p </> getSourceFile f2) es
          files' <- sequence $ map checkOwnedFile files
          os2 <- fmap concat $ sequence $ map (compileExtraSource b (show ns0) paths') es
          let (hxx',cxx,os') = sortCompiledFiles files'
          let (osCat,osOther) = partitionEithers os2
          path <- canonicalizePath $ p </> d
          let os1' = resolveObjectDeps path (os1 ++ osCat) deps
          let cm = CompileMetadata {
              cmVersionHash = getCompilerHash b,
              cmPath = path,
              cmNamespace = show ns0,
              cmPublicDeps = as,
              cmPrivateDeps = as2,
              cmCategories = sort $ map show pc,
              cmSubdirs = [s0],
              cmPublicFiles = sort ps,
              cmPrivateFiles = sort xs,
              cmTestFiles = sort ts,
              cmHxxFiles = sort hxx',
              cmCxxFiles = sort cxx,
              cmLinkFlags = getLinkFlags m,
              cmObjectFiles = os1' ++ osOther ++ map OtherObjectFile os'
            }
          when (not $ isCreateTemplates m) $ writeMetadata (p </> d) cm
          return ((ss',cm),mf)
    formatWarnings c
      | null $ getCompileWarnings c = return ()
      | otherwise = hPutStr stderr $ "Compiler warnings:\n" ++ (concat $ map (++ "\n") (getCompileWarnings c))
    writeOutputFile b ns0 paths ca@(CxxOutput _ f2 ns _ _ content) = do
      hPutStrLn stderr $ "Writing file " ++ f2
      writeCachedFile (p </> d) (show ns) f2 $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2
         then do
           let f2' = getCachedPath (p </> d) (show ns) f2
           let p0 = getCachedPath (p </> d) "" ""
           let p1 = getCachedPath (p </> d) (show ns) ""
           createCachePath (p </> d)
           let ns' = if isStaticNamespace ns then show ns else show ns0
           let command = CompileToObject f2' (getCachedPath (p </> d) ns' "") dynamicNamespaceName "" (p0:p1:paths) False
           o2 <- runCxxCommand b command
           return $ ([o2],ca)
         else return ([],ca)
    compileExtraSource b ns0 paths (CategorySource f2 cs ds2) = do
      f2' <- compileExtraFile False b ns0 paths f2
      let ds2' = nub $ cs ++ ds2
      case f2' of
           Nothing -> return []
           Just o  -> return $ map (\c -> Left $ ([o],fakeCxxForSource ns0 ds2' c)) cs
    compileExtraSource b ns0 paths (OtherSource f2) = do
      f2' <- compileExtraFile True b ns0 paths f2
      case f2' of
           Just o  -> return [Right $ OtherObjectFile o]
           Nothing -> return []
    fakeCxxForSource ns ds2 c = CxxOutput {
        coCategory = Just (CategoryName c),
        coFilename = "",
        coNamespace = ns',
        coUsesNamespace = [ns'],
        coUsesCategory = map CategoryName ds2,
        coOutput = []
      } where
        ns' = if null ns then NoNamespace else StaticNamespace ns
    checkOwnedFile f2 = do
      exists <- doesFileExist f2
      when (not exists) $ do
        hPutStrLn stderr $ "Owned file \"" ++ f2 ++ "\" does not exist."
        hPutStrLn stderr $ "Zeolite compilation failed."
        exitFailure
      canonicalizePath f2
    compileExtraFile e b ns0 paths f2
      | isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2 = do
          let f2' = p </> f2
          createCachePath (p </> d)
          let command = CompileToObject f2' (getCachedPath (p </> d) "" "") dynamicNamespaceName ns0 paths e
          fmap Just $ runCxxCommand b command
      | isSuffixOf ".a" f2 || isSuffixOf ".o" f2 = return (Just f2)
      | otherwise = return Nothing
    processTemplates deps = do
      (ps,xs,_) <- findSourceFiles p d
      ps' <- zipWithContents p ps
      xs' <- zipWithContents p xs
      let ss = fixPaths $ getSourceFilesForDeps deps
      ss' <- zipWithContents p ss
      let ts = createTemplates ss' ps' xs' :: CompileInfo [CxxOutput]
      if isCompileError ts
         then do
           formatWarnings ts
           hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError ts)
           hPutStrLn stderr $ "Zeolite compilation failed."
           exitFailure
         else do
           formatWarnings ts
           sequence_ $ map writeTemplate $ getCompileSuccess ts
    createTemplates is3 cs ds2 = do
      tm1 <- addIncludes defaultCategories is3
      cs' <- fmap concat $ collectAllOrErrorM $ map parsePublicSource cs
      let cs'' = map (setCategoryNamespace DynamicNamespace) cs'
      tm2 <- includeNewTypes tm1 cs''
      da <- collectAllOrErrorM $ map parseInternalSource ds2
      let ds2' = concat $ map snd da
      let cs2 = concat $ map fst da
      tm3 <- includeNewTypes tm2 cs2
      let ca = Set.fromList $ map getCategoryName $ filter isValueConcrete cs'
      let ca' = foldr Set.delete ca $ map dcName ds2'
      collectAllOrErrorM $ map (compileConcreteTemplate tm3) $ Set.toList ca'
    writeTemplate (CxxOutput _ n _ _ _ content) = do
      let n' = p </> d </> n
      exists <- doesFileExist n'
      if exists && f /= ForceAll
         then hPutStrLn stderr $ "Skipping existing file " ++ n
         else do
           hPutStrLn stderr $ "Writing file " ++ n
           writeFile n' $ concat $ map (++ "\n") content
    compileAll ns0 ns2 is3 cs ds2 = do
      tm1 <- addIncludes defaultCategories is3
      cs' <- fmap concat $ collectAllOrErrorM $ map parsePublicSource cs
      let cs'' = map (setCategoryNamespace ns0) cs'
      xa <- collectAllOrErrorM $ map parsePrivate ds2
      let cm = CategoryModule {
          cnBase = tm1,
          cnNamespaces = ns0:ns2,
          cnPublic = cs'',
          cnPrivate = xa,
          cnExternal = concat $ map getSourceCategories es
        }
      xx <- compileCategoryModule cm
      let pc = map getCategoryName cs''
      ms <- maybeCreateMain cm m
      return (pc,ms,xx)
    parsePrivate ds = do
      let ns1 = StaticNamespace $ privateNamespace (p </> fst ds)
      (cs,ds') <- parseInternalSource ds
      let cs' = map (setCategoryNamespace ns1) cs
      return $ PrivateSource ns1 cs' ds'
    addIncludes tm fs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource fs
      includeNewTypes tm cs
    createBinary b r paths deps (CompileBinary n _ o lf) ms
      | length ms > 1 = do
        hPutStrLn stderr $ "Multiple matches for main category " ++ n ++ "."
        exitFailure
      | length ms == 0 = do
        hPutStrLn stderr $ "Main category " ++ n ++ " not found."
        exitFailure
      | otherwise = do
          f0 <- if null o
                   then canonicalizePath $ p </> d </> n
                   else canonicalizePath $ p </> d </> o
          let (CxxOutput _ _ _ ns2 req content) = head ms
          -- TODO: Create a helper or a constant or something.
          (o',h) <- mkstemps "/tmp/zmain_" ".cpp"
          hPutStr h $ concat $ map (++ "\n") content
          hClose h
          base <- resolveBaseModule r
          (_,bpDeps) <- loadPublicDeps (getCompilerHash b) [base]
          (_,deps2) <- loadPrivateDeps (getCompilerHash b) (bpDeps ++ deps)
          let lf' = lf ++ getLinkFlagsForDeps deps2
          let paths' = fixPaths $ paths ++ base:(getIncludePathsForDeps deps2)
          let os    = getObjectFilesForDeps deps2
          let ofr = getObjectFileResolver os
          let os' = ofr ns2 req
          let command = CompileToBinary o' os' f0 paths' lf'
          hPutStrLn stderr $ "Creating binary " ++ f0
          _ <- runCxxCommand b command
          removeFile o'
    createBinary _ _ _ _ _ _ = return ()
    maybeCreateMain cm (CompileBinary n f2 _ _) =
      fmap (:[]) $ compileModuleMain cm (CategoryName n) (FunctionName f2)
    maybeCreateMain _ _ = return []
runCompiler co = do
  hPutStrLn stderr $ "Unsupported compiler options: " ++ show co
  exitFailure

checkAllowedStale :: Bool -> ForceMode -> IO ()
checkAllowedStale fr f = do
  when (not fr && f < ForceRecompile) $ do
    hPutStrLn stderr $ "Some dependencies are out of date. " ++
                       "Recompile them or use -f to force."
    exitFailure

fixPaths :: [String] -> [String]
fixPaths = nub . map fixPath

zipWithContents :: String -> [String] -> IO [(String,String)]
zipWithContents p fs = fmap (zip $ map fixPath fs) $ sequence $ map (readFile . (p </>)) fs
