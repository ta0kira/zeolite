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
import System.Posix.Temp (mkstemps)
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
  ds' <- sequence $ map (preloadModule backend resolver) ds
  let possibleTests = Set.fromList $ concat $ map getTestsFromPreload ds'
  case Set.toList $ allowTests `Set.difference` possibleTests of
       [] -> return ()
       ts -> do
         hPutStr stderr $ "Some test files do not occur in the selected modules: " ++
                          intercalate ", " (map show ts) ++ "\n"
         exitFailure
  allResults <- fmap concat $ sequence $ map (runTests backend) ds'
  let passed = sum $ map (fst . fst) allResults
  let failed = sum $ map (snd . fst) allResults
  processResults passed failed (mergeAllM $ map snd allResults) where
    preloadModule b r d = do
      m <- loadMetadata (p </> d)
      base <- resolveBaseModule r
      (fr1,deps1) <- loadPublicDeps (getCompilerHash b) [base,p </> d]
      checkAllowedStale fr1 f
      (fr2,deps2) <- loadPrivateDeps (getCompilerHash b) deps1
      checkAllowedStale fr2 f
      return (d,m,deps1,deps2)
    getTestsFromPreload (_,m,_,_) = cmTestFiles m
    allowTests = Set.fromList tp
    isTestAllowed t = if null allowTests then True else t `Set.member` allowTests
    runTests :: CompilerBackend b => b ->
                (String,CompileMetadata,[CompileMetadata],[CompileMetadata]) ->
                IO [((Int,Int),CompileInfo ())]
    runTests b (d,m,deps1,deps2) = do
      let paths = getIncludePathsForDeps deps1
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
runCompiler (CompileOptions h _ _ ds _ _ p CompileRecompile f) = do
  (backend,_) <- loadConfig
  fmap mergeAll $ sequence $ map (recompileSingle $ getCompilerHash backend) ds where
    recompileSingle h2 d0 = do
      let d = p </> d0
      rm <- tryLoadRecompile d
      upToDate <- isPathUpToDate h2 d
      maybeCompile rm upToDate where
        maybeCompile Nothing _ = do
          hPutStrLn stderr $ "Path " ++ d0 ++ " has not been configured or compiled yet."
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
                  coPublicDeps = map ((fixed </> d) </>) is,
                  coPrivateDeps = map ((fixed </> d) </>) is2,
                  coSources = [d],
                  coExtraFiles = es,
                  coExtraPaths = ep,
                  coSourcePrefix = fixed,
                  coMode = m,
                  coForce = if f == ForceAll then ForceRecompile else AllowRecompile
                }
              runCompiler recompile
runCompiler (CompileOptions _ is is2 ds es ep p m f) = do
  (backend,resolver) <- loadConfig
  as  <- fmap fixPaths $ sequence $ map (resolveModule resolver p) is
  as2 <- fmap fixPaths $ sequence $ map (resolveModule resolver p) is2
  (fr,deps) <- loadPublicDeps (getCompilerHash backend) (as ++ as2)
  checkAllowedStale fr f
  if isCreateTemplates m
      then sequence_ $ map (processTemplates deps) ds
      else do
        ma <- sequence $ map (processPath backend resolver deps as as2) ds
        let ms = concat $ map snd ma
        let deps2 = map fst ma
        createBinary backend resolver (deps ++ deps2) m ms
  hPutStrLn stderr $ "Zeolite compilation succeeded." where
    ep' = fixPaths $ map (p </>) ep
    processPath b r deps as as2 d = do
      isConfigured <- isPathConfigured d
      when (isConfigured && f == DoNotForce) $ do
        hPutStrLn stderr $ "Module " ++ d ++ " has already been configured. " ++
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
      let paths = getIncludePathsForDeps deps2
      ps' <- zipWithContents p ps
      xs' <- zipWithContents p xs
      ns0 <- canonicalizePath (p </> d) >>= return . StaticNamespace . publicNamespace
      let ns2 = map StaticNamespace $ filter (not . null) $ getNamespacesForDeps deps
      let fs = compileAll ns0 ns2 ss' ps' xs'
      writeOutput b paths ns0 deps2 d as as2
                  (map takeFileName ps)
                  (map takeFileName xs)
                  (map takeFileName ts) fs
    writeOutput b paths ns0 deps d as as2 ps xs ts fs
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
          let paths' = paths ++ ep' ++ ss'
          let hxx   = filter (isSuffixOf ".hpp" . coFilename)       fs'
          let other = filter (not . isSuffixOf ".hpp" . coFilename) fs'
          os1 <- sequence $ map (writeOutputFile b (show ns0) paths' d) $ hxx ++ other
          let files = map (\f2 -> getCachedPath (p </> d) (show $ coNamespace f2) (coFilename f2)) fs' ++
                      map (\f2 -> p </> getSourceFile f2) es
          files' <- sequence $ map checkOwnedFile files
          os2 <- fmap concat $ sequence $ map (compileExtraSource b (show ns0) paths' d) es
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
              cmSubdirs = sort $ ss' ++ ep',
              cmPublicFiles = sort ps,
              cmPrivateFiles = sort xs,
              cmTestFiles = sort ts,
              cmHxxFiles = sort hxx',
              cmCxxFiles = sort cxx,
              cmLinkFlags = getLinkFlags m,
              cmObjectFiles = os1' ++ osOther ++ map OtherObjectFile os'
            }
          when (not $ isCreateTemplates m) $ writeMetadata (p </> d) cm
          return (cm,mf)
    formatWarnings c
      | null $ getCompileWarnings c = return ()
      | otherwise = hPutStr stderr $ "Compiler warnings:\n" ++ (concat $ map (++ "\n") (getCompileWarnings c))
    writeOutputFile b ns0 paths d ca@(CxxOutput _ f2 ns _ _ content) = do
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
    compileExtraSource b ns0 paths d (CategorySource f2 cs ds2) = do
      f2' <- compileExtraFile False b ns0 paths d f2
      let ds2' = nub $ cs ++ ds2
      case f2' of
           Nothing -> return []
           Just o  -> return $ map (\c -> Left $ ([o],fakeCxxForSource ns0 ds2' c)) cs
    compileExtraSource b ns0 paths d (OtherSource f2) = do
      f2' <- compileExtraFile True b ns0 paths d f2
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
    compileExtraFile e b ns0 paths d f2
      | isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2 = do
          let f2' = p </> f2
          createCachePath (p </> d)
          let command = CompileToObject f2' (getCachedPath (p </> d) "" "") dynamicNamespaceName ns0 paths e
          fmap Just $ runCxxCommand b command
      | isSuffixOf ".a" f2 || isSuffixOf ".o" f2 = return (Just f2)
      | otherwise = return Nothing
    processTemplates deps d = do
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
           sequence $ map (writeTemplate d) $ getCompileSuccess ts
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
    writeTemplate d (CxxOutput _ n _ _ _ content) = do
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
    parsePrivate d = do
      let ns1 = StaticNamespace $ privateNamespace (p </> fst d)
      (cs,ds2) <- parseInternalSource d
      let cs' = map (setCategoryNamespace ns1) cs
      return $ PrivateSource ns1 cs' ds2
    addIncludes tm fs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource fs
      includeNewTypes tm cs
    createBinary b r deps (CompileBinary n _ o lf) ms
      | length ms > 1 = do
        hPutStrLn stderr $ "Multiple matches for main category " ++ n ++ "."
        exitFailure
      | length ms == 0 = do
        hPutStrLn stderr $ "Main category " ++ n ++ " not found."
        exitFailure
      | otherwise = do
          f0 <- if null o
                   then canonicalizePath $ p </> head ds </> n
                   else canonicalizePath $ p </> head ds </> o
          let (CxxOutput _ _ _ ns2 req content) = head ms
          -- TODO: Create a helper or a constant or something.
          (o',h) <- mkstemps "/tmp/zmain_" ".cpp"
          hPutStr h $ concat $ map (++ "\n") content
          hClose h
          base <- resolveBaseModule r
          (_,bpDeps) <- loadPublicDeps (getCompilerHash b) [base]
          (_,deps2) <- loadPrivateDeps (getCompilerHash b) (bpDeps ++ deps)
          let lf' = lf ++ getLinkFlagsForDeps deps2
          let paths = fixPaths $ getIncludePathsForDeps deps2
          let os    = getObjectFilesForDeps deps2
          let ofr = getObjectFileResolver os
          let os' = ofr ns2 req
          let command = CompileToBinary o' os' f0 paths lf'
          hPutStrLn stderr $ "Creating binary " ++ f0
          _ <- runCxxCommand b command
          removeFile o'
    createBinary _ _ _ _ _ = return ()
    maybeCreateMain cm (CompileBinary n f2 _ _) =
      fmap (:[]) $ compileModuleMain cm (CategoryName n) (FunctionName f2)
    maybeCreateMain _ _ = return []

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
