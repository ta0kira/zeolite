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

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (intercalate,isSuffixOf,nub,sort)
import Data.Maybe (isJust)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Posix.Temp (mkstemps)
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

import Builtin
import CompileInfo
import DefinedCategory
import SourceFile
import TypesBase
import TypeCategory
import TypeInstance
import CompilerCxx.Category
import CompilerCxx.Naming
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.CompilerCommand
import Cli.TestRunner
import Cli.ParseCompileOptions -- Not safe, due to Text.Regex.TDFA.


main = do
  args <- getArgs
  let options = parseCompileOptions args >>= validate
  compile options where
    compile co
      | isCompileError co = do
          hPutStr stderr $ show $ getCompileError co
          hPutStrLn stderr "Use the -h option to show help."
          exitFailure
      | otherwise = do
        let co' = getCompileSuccess co
        when (HelpNotNeeded /= (coHelp co')) $ showHelp >> exitFailure
        runCompiler co'
    validate co@(CompileOptions h is is2 ds es ep p m o _)
      | h /= HelpNotNeeded = return co

      | (not $ null o) && (isCompileIncremental m) =
        compileError "Output filename (-o) is not allowed in compile-only mode (-c)."

      | (not $ null o) && (isExecuteTests m) =
        compileError "Output filename (-o) is not allowed in test mode (-t)."
      | (not $ null $ is ++ is2) && (isExecuteTests m) =
        compileError "Include paths (-i/-I) are not allowed in test mode (-t)."
          | (not $ null $ es ++ ep) && (isExecuteTests m) =
        compileError "Extra files (-e) are not allowed in test mode (-t)."

      | (not $ null o) && (isCreateTemplates m) =
        compileError "Output filename (-o) is not allowed in template mode (--templates)."
          | (not $ null $ es ++ ep) && (isCreateTemplates m) =
        compileError "Extra files (-e) are not allowed in template mode (--templates)."

      | (not $ null p) && (isCompileRecompile m) =
        compileError "Path prefix (-p) is not allowed in recompile mode (-r)."
      | (not $ null o) && (isCompileRecompile m) =
        compileError "Output filename (-o) is not allowed in recompile mode (-r)."
      | (not $ null $ is ++ is2) && (isCompileRecompile m) =
        compileError "Include paths (-i/-I) are not allowed in recompile mode (-r)."
          | (not $ null $ es ++ ep) && (isCompileRecompile m) =
        compileError "Extra files (-e) are not allowed in recompile mode (-r)."

      | null ds =
        compileError "Please specify at least one input path."
      | (length ds /= 1) && (isCompileBinary m) =
        compileError "Specify exactly one input path for binary mode (-m)."
      | otherwise = return co

showHelp :: IO ()
showHelp = do
  hPutStrLn stderr "Zeolite CLI Help:"
  mapM_ (hPutStrLn stderr . ("  " ++)) optionHelpText
  hPutStrLn stderr "Also see https://ta0kira.github.io/zeolite for more documentation."

runCompiler :: CompileOptions -> IO ()
runCompiler co@(CompileOptions _ _ _ ds _ _ p (ExecuteTests tp) _ f) = do
  ds' <- sequence $ map preloadModule ds
  let possibleTests = Set.fromList $ concat $ map getTestsFromPreload ds'
  case Set.toList $ allowTests `Set.difference` possibleTests of
       [] -> return ()
       ts -> do
         hPutStr stderr $ "Some test files do not occur in the selected modules: " ++
                          intercalate ", " (map show ts) ++ "\n"
         exitFailure
  allResults <- fmap concat $ sequence $ map runTests ds'
  let passed = sum $ map (fst . fst) allResults
  let failed = sum $ map (snd . fst) allResults
  processResults passed failed (mergeAllM $ map snd allResults) where
    preloadModule d = do
      m <- loadMetadata (p </> d)
      base <- getBasePath
      (fr1,deps1) <- loadPublicDeps [base,p </> d]
      checkAllowedStale fr1 f
      (fr2,deps2) <- loadPrivateDeps deps1
      checkAllowedStale fr2 f
      return (d,m,deps1,deps2)
    getTestsFromPreload (_,m,_,_) = cmTestFiles m
    allowTests = Set.fromList tp
    isTestAllowed t = if null allowTests then True else t `Set.member` allowTests
    runTests :: (String,CompileMetadata,[CompileMetadata],[CompileMetadata]) ->
                IO [((Int,Int),CompileInfo ())]
    runTests (d,m,deps1,deps2) = do
      let paths = getIncludePathsForDeps deps1
      let ss = fixPaths $ getSourceFilesForDeps deps1
      let os = getObjectFilesForDeps deps2
      ss' <- zipWithContents p ss
      ts' <- zipWithContents p (map (d </>) $ filter isTestAllowed $ cmTestFiles m)
      tm <- return $ do
        tm0 <- builtinCategories
        cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource ss'
        includeNewTypes tm0 cs
      if isCompileError tm
         then return [((0,0),tm >> return ())]
         else sequence $ map (runSingleTest paths deps1 os (getCompileSuccess tm)) ts'
    processResults passed failed rs
      | isCompileError rs = do
          hPutStr stderr $ "\nTest errors:\n" ++ (show $ getCompileError rs)
          hPutStrLn stderr $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"
          hPutStrLn stderr $ "Zeolite tests failed."
          exitFailure
      | otherwise = do
          hPutStrLn stderr $ "\nPassed: " ++ show passed ++ " test(s), Failed: " ++ show failed ++ " test(s)"
          hPutStrLn stderr $ "Zeolite tests passed."
runCompiler co@(CompileOptions h _ _ ds _ _ _ CompileRecompile _ f) = do
  fmap mergeAll $ sequence $ map recompileSingle ds where
    recompileSingle d0 = do
      rm <- tryLoadRecompile d0
      upToDate <- isPathUpToDate d0
      maybeCompile rm upToDate where
        maybeCompile Nothing _ = do
          hPutStrLn stderr $ "Path " ++ d0 ++ " has not been configured or compiled yet."
          exitFailure
        maybeCompile (Just rm') upToDate
          | f < ForceAll && upToDate = hPutStrLn stderr $ "Path " ++ d0 ++ " is up to date."
          | otherwise = do
              let (RecompileMetadata p d is is2 es ep m o) = rm'
              -- In case the module is manually configured with a p such as "..",
              -- since the absolute path might not be known ahead of time.
              absolute <- canonicalizePath d0
              let fixed = fixPath (absolute </> p)
              let recompile = CompileOptions {
                  coHelp = h,
                  coPublicDeps = map ((fixed </> d) </>) is,
                  coPrivateDeps = map ((fixed </> d) </>) is2,
                  coSources = [d],
                  coExtraFiles = es,
                  coExtraPaths = ep,
                  coSourcePrefix = fixed,
                  coMode = m,
                  coOutputName = o,
                  coForce = if f == ForceAll then ForceRecompile else AllowRecompile
                }
              runCompiler recompile
runCompiler co@(CompileOptions h is is2 ds es ep p m o f) = do
  (fr,deps) <- loadPublicDeps (is ++ is2)
  checkAllowedStale fr f
  let ss = fixPaths $ getSourceFilesForDeps deps
  as  <- fmap fixPaths $ sequence $ map canonicalizePath is
  as2 <- fmap fixPaths $ sequence $ map canonicalizePath is2
  ss' <- zipWithContents p ss
  if isCreateTemplates m
      then sequence_ $ map (processTemplates ss') ds
      else do
        ma <- sequence $ map (processPath deps as as2 ss') ds
        let ms = concat $ map snd ma
        let deps2 = map fst ma
        createBinary (deps ++ deps2) m ms
  hPutStrLn stderr $ "Zeolite compilation succeeded." where
    ep' = fixPaths $ map (getCachedPath p "") ep
    processPath deps as as2 ss d = do
      isConfigured <- isPathConfigured d
      when (isConfigured && f == DoNotForce) $ do
        hPutStrLn stderr $ "Module " ++ d ++ " has already been configured. " ++
                           "Recompile with -r or use -f to overwrite the config."
        exitFailure
      eraseCachedData (p </> d)
      absolute <- canonicalizePath p
      let rm = RecompileMetadata {
        rmRoot = absolute,
        rmPath = d,
        rmPublicDeps = as,
        rmPrivateDeps = as2,
        rmExtraFiles = sort es,
        rmExtraPaths = sort ep,
        rmMode = m,
        rmOutputName = o
      }
      when (f == DoNotForce && f == ForceAll) $ writeRecompile (p </> d) rm
      (ps,xs,ts) <- findSourceFiles p d
      -- Lazy dependency loading, in case we aren't compiling anything.
      deps2 <- if null ps && null xs
                  then return deps
                  else do
                    base <- getBasePath
                    (fr,bpDeps) <- loadPublicDeps [base]
                    checkAllowedStale fr f
                    return $ bpDeps ++ deps
      let paths = getIncludePathsForDeps deps2
      ps' <- zipWithContents p ps
      xs' <- zipWithContents p xs
      ns0 <- canonicalizePath (p </> d) >>= return . StaticNamespace . publicNamespace
      let ns2 = map StaticNamespace $ filter (not . null) $ getNamespacesForDeps deps
      let fs = compileAll ns0 ns2 ss ps' xs'
      writeOutput (fixPaths $ paths ++ ep') ns0 deps2 d as as2
                  (map takeFileName ps)
                  (map takeFileName xs)
                  (map takeFileName ts) fs
    writeOutput paths ns0 deps d as as2 ps xs ts fs
      | isCompileError fs = do
          formatWarnings fs
          hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError fs)
          hPutStrLn stderr $ "Zeolite compilation failed."
          exitFailure
      | otherwise = do
          formatWarnings fs
          let (pc,mf,fs') = getCompileSuccess fs
          let ss = nub $ filter (not . null) $ map show $ [ns0] ++ map coNamespace fs'
          let paths' = paths ++ map (\ns -> getCachedPath (p </> d) ns "") ss
          let hxx   = filter (isSuffixOf ".hpp" . coFilename)       fs'
          let other = filter (not . isSuffixOf ".hpp" . coFilename) fs'
          os1 <- sequence $ map (writeOutputFile (show ns0) paths' d) $ hxx ++ other
          os2 <- fmap concat $ sequence $ map (compileExtraFile (show ns0) paths' d) es
          let (hxx,cxx,os') = sortCompiledFiles $ map (\f -> show (coNamespace f) </> coFilename f) fs' ++ es
          path <- canonicalizePath $ p </> d
          let os1' = resolveObjectDeps path os1 deps
          let cm = CompileMetadata {
              cmPath = path,
              cmNamespace = show ns0,
              cmPublicDeps = as,
              cmPrivateDeps = as2,
              cmCategories = sort $ map show pc,
              cmSubdirs = sort $ ss ++ ep,
              cmPublicFiles = sort ps,
              cmPrivateFiles = sort xs,
              cmTestFiles = sort ts,
              cmHxxFiles = sort hxx,
              cmCxxFiles = sort cxx,
              cmObjectFiles = os1' ++ os2 ++ map OtherObjectFile os'
            }
          when (not $ isCreateTemplates m) $ writeMetadata (p </> d) cm
          return (cm,mf)
    formatWarnings c
      | null $ getCompileWarnings c = return ()
      | otherwise = hPutStr stderr $ "Compiler warnings:\n" ++ (concat $ map (++ "\n") (getCompileWarnings c))
    writeOutputFile ns0 paths d ca@(CxxOutput c f ns ns2 req content) = do
      hPutStrLn stderr $ "Writing file " ++ f
      writeCachedFile (p </> d) (show ns) f $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f || isSuffixOf ".cc" f
         then do
           let f' = getCachedPath (p </> d) (show ns) f
           let p0 = getCachedPath (p </> d) "" ""
           let p1 = getCachedPath (p </> d) (show ns) ""
           createCachePath (p </> d)
           let ns' = if isStaticNamespace ns then show ns else show ns0
           let command = CompileToObject f' (getCachedPath (p </> d) ns' "") "" (p0:p1:paths) False
           o <- runCxxCommand command
           return $ ([o],ca)
         else return ([],ca)
    compileExtraFile ns0 paths d f
      | isSuffixOf ".cpp" f || isSuffixOf ".cc" f = do
          let f' = getCachedPath (p </> d) "" f
          let p0 = getCachedPath (p </> d) "" ""
          createCachePath (p </> d)
          let command = CompileToObject f' (getCachedPath (p </> d) "" "") ns0 (p0:paths) True
          o <- runCxxCommand command
          return [OtherObjectFile o]
      | otherwise = return []
    processTemplates ss d = do
      (ps,xs,_) <- findSourceFiles p d
      ps' <- zipWithContents p ps
      xs' <- zipWithContents p xs
      let ts = createTemplates ss ps' xs' :: CompileInfo [CxxOutput]
      if isCompileError ts
         then do
           formatWarnings ts
           hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError ts)
           hPutStrLn stderr $ "Zeolite compilation failed."
           exitFailure
         else do
           formatWarnings ts
           sequence $ map (writeTemplate d) $ getCompileSuccess ts
    createTemplates is cs ds = do
      tm0 <- builtinCategories
      tm1 <- addIncludes tm0 is
      cs' <- fmap concat $ collectAllOrErrorM $ map parsePublicSource cs
      let cs'' = map (setCategoryNamespace DynamicNamespace) cs'
      tm2 <- includeNewTypes tm1 cs''
      da <- collectAllOrErrorM $ map parseInternalSource ds
      let ds' = concat $ map snd da
      let cs2 = concat $ map fst da
      tm3 <- includeNewTypes tm2 cs2
      let ca = Set.fromList $ map getCategoryName $ filter isValueConcrete cs'
      let ca' = foldr Set.delete ca $ map dcName ds'
      collectAllOrErrorM $ map (compileConcreteTemplate tm3) $ Set.toList ca'
    writeTemplate d (CxxOutput _ n _ _ _ content) = do
      let n' = p </> d </> n
      exists <- doesPathExist n'
      if exists && f /= ForceAll
         then hPutStrLn stderr $ "Skipping existing file " ++ n
         else do
           hPutStrLn stderr $ "Writing file " ++ n
           writeFile n' $ concat $ map (++ "\n") content
    compileAll ns0 ns2 is cs ds = do
      tm0 <- builtinCategories
      tm1 <- addIncludes tm0 is
      cs' <- fmap concat $ collectAllOrErrorM $ map parsePublicSource cs
      let cs'' = map (setCategoryNamespace ns0) cs'
      xa <- collectAllOrErrorM $ map parsePrivate ds
      let cm = CategoryModule {
          cnBase = tm1,
          cnNamespaces = ns0:ns2,
          cnPublic = cs'',
          cnPrivate = xa
        }
      xx <- compileCategoryModule cm
      let pc = map getCategoryName cs''
      ms <- maybeCreateMain cm m
      return (pc,ms,xx)
    parsePrivate d = do
      let ns1 = StaticNamespace $ privateNamespace (p </> fst d)
      (cs,ds) <- parseInternalSource d
      let cs' = map (setCategoryNamespace ns1) cs
      return $ PrivateSource ns1 cs' ds
    addIncludes tm fs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource fs
      includeNewTypes tm cs
    mergeInternal ds = (concat $ map fst ds,concat $ map snd ds)
    getBinaryName (CompileBinary n _) = canonicalizePath $ if null o then n else o
    getBinaryName _                   = return ""
    createBinary deps ma@(CompileBinary n _) ms
      | length ms > 1 = do
        hPutStrLn stderr $ "Multiple matches for main category " ++ n ++ "."
        exitFailure
      | length ms == 0 = do
        hPutStrLn stderr $ "Main category " ++ n ++ " not found."
        exitFailure
      | otherwise = do
          f0 <- getBinaryName ma
          let f0 = if null o then n else o
          let (CxxOutput _ _ _ ns2 req content) = head ms
          -- TODO: Create a helper or a constant or something.
          (o',h) <- mkstemps "/tmp/zmain_" ".cpp"
          hPutStr h $ concat $ map (++ "\n") content
          hClose h
          base <- getBasePath
          (_,bpDeps) <- loadPublicDeps [base]
          (_,deps2) <- loadPrivateDeps (bpDeps ++ deps)
          let paths = fixPaths $ getIncludePathsForDeps deps2
          let os    = getObjectFilesForDeps deps2
          let ofr = getObjectFileResolver os
          let os' = ofr ns2 req
          let command = CompileToBinary o' os' f0 paths
          hPutStrLn stderr $ "Creating binary " ++ f0
          runCxxCommand command
          removeFile o'
    createBinary _ _ _ = return ()
    maybeCreateMain cm (CompileBinary n f) =
      fmap (:[]) $ compileModuleMain cm (CategoryName n) (FunctionName f)
    maybeCreateMain _ _ = return []

checkAllowedStale :: Bool -> ForceMode -> IO ()
checkAllowedStale fr f = do
  when (not fr && f < ForceRecompile) $ do
    hPutStrLn stderr $ "Some dependencies are out of date. " ++
                       "Recompile them or use -f to force."
    exitFailure

fixPaths :: [String] -> [String]
fixPaths = nub . map fixPath

getBasePath :: IO String
getBasePath = getExecutablePath >>= return . takeDirectory

zipWithContents :: String -> [String] -> IO [(String,String)]
zipWithContents p fs = fmap (zip $ map fixPath fs) $ sequence $ map (readFile . (p </>)) fs
