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
import Data.List (isSuffixOf,nub,sort)
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
  results <- sequence $ map runTests ds'
  processResults $ mergeAllM results where
    preloadModule d = do
      m <- loadMetadata (p </> d)
      base <- getBasePath
      (fr1,deps1) <- loadPublicDeps [base,p </> d]
      checkAllowedStale fr1 f
      (fr2,deps2) <- loadPrivateDeps deps1
      checkAllowedStale fr2 f
      return (d,m,deps1,deps2)
    allowTests = Set.fromList tp
    isTestAllowed t = if null allowTests then True else t `Set.member` allowTests
    runTests :: (String,CompileMetadata,[CompileMetadata],[CompileMetadata]) ->
                IO (CompileInfo ())
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
         then return (tm >> return ())
         else fmap mergeAllM $ sequence $ map (runSingleTest paths deps1 os (getCompileSuccess tm)) ts'
    processResults rs
      | isCompileError rs = do
          hPutStr stderr $ "\nTest errors:\n" ++ (show $ getCompileError rs)
          hPutStrLn stderr $ "\nZeolite tests failed."
          exitFailure
      | otherwise = do
          hPutStrLn stderr $ "\nZeolite tests passed."
runCompiler co@(CompileOptions h _ _ ds _ _ _ CompileRecompile _ f) = do
  fmap mergeAll $ sequence $ map recompileSingle ds where
    recompileSingle d0 = do
      rm <- tryLoadRecompile d0
      if isNotConfigured rm
         then do
           hPutStrLn stderr $ "Path " ++ d0 ++ " has not been configured or compiled yet."
           exitFailure
         else do
           let (RecompileMetadata p d is is2 es ep m o) = rm
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
               coForce = max AllowRecompile f
             }
           runCompiler recompile
runCompiler co@(CompileOptions h is is2 ds es ep p m o f) = do
  (fr,deps) <- loadPublicDeps (is ++ is2)
  checkAllowedStale fr f
  let ss = fixPaths $ getSourceFilesForDeps deps
  as  <- fmap fixPaths $ sequence $ map canonicalizePath is
  as2 <- fmap fixPaths $ sequence $ map canonicalizePath is2
  ss' <- zipWithContents p ss
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
      -- TODO: -f might be used with -r if there are stale dependencies, which
      -- will inadvertently overwrite the config here.
      when (f /= AllowRecompile) $ writeRecompile (p </> d) rm
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
      let fs = compileAll ss ps' xs'
      writeOutput (fixPaths $ paths ++ ep') deps2 d as as2
                  (map takeFileName ps)
                  (map takeFileName xs)
                  (map takeFileName ts) fs
    writeOutput paths deps d as as2 ps xs ts fs
      | isCompileError fs = do
          formatWarnings fs
          hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError fs)
          hPutStrLn stderr $ "Zeolite compilation failed."
          exitFailure
      | otherwise = do
          formatWarnings fs
          let (pc,mf,fs') = getCompileSuccess fs
          let ss = nub $ filter (not . null) $ map coNamespace fs'
          let paths' = paths ++ map (\ns -> getCachedPath (p </> d) ns "") ss
          let hxx   = filter (isSuffixOf ".hpp" . coFilename)       fs'
          let other = filter (not . isSuffixOf ".hpp" . coFilename) fs'
          os1 <- sequence $ map (writeOutputFile paths' d) $ hxx ++ other
          os2 <- fmap concat $ sequence $ map (compileExtraFile paths' d) es
          let (hxx,cxx,os') = sortCompiledFiles $ map (\f -> coNamespace f </> coFilename f) fs' ++ es
          path <- canonicalizePath $ p </> d
          let os1' = resolveObjectDeps path os1 deps
          let cm = CompileMetadata {
              cmPath = path,
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
          writeMetadata (p </> d) cm
          return (cm,mf)
    formatWarnings c
      | null $ getCompileWarnings c = return ()
      | otherwise = hPutStr stderr $ "Compiler warnings:\n" ++ (concat $ map (++ "\n") (getCompileWarnings c))
    writeOutputFile paths d ca@(CxxOutput c f ns ns2 req content) = do
      hPutStrLn stderr $ "Writing file " ++ f
      writeCachedFile (p </> d) ns f $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f || isSuffixOf ".cc" f
         then do
           let f' = getCachedPath (p </> d) ns f
           let p0 = getCachedPath (p </> d) "" ""
           let p1 = getCachedPath (p </> d) ns ""
           createCachePath (p </> d)
           let command = CompileToObject f' (getCachedPath (p </> d) ns "") (p0:p1:paths) False
           o <- runCxxCommand command
           return $ ([o],ca)
         else return ([],ca)
    compileExtraFile paths d f
      | isSuffixOf ".cpp" f || isSuffixOf ".cc" f = do
          let f' = getCachedPath (p </> d) "" f
          let p0 = getCachedPath (p </> d) "" ""
          createCachePath (p </> d)
          let command = CompileToObject f' (getCachedPath (p </> d) "" "") (p0:paths) True
          o <- runCxxCommand command
          return [OtherObjectFile o]
      | otherwise = return []
    compileAll is cs ds = do
      tm0 <- builtinCategories
      tm1 <- addIncludes tm0 is
      (pc,tm2,cf) <- compilePublic tm1 cs
      ds' <- collectAllOrErrorM $ map (compileInternal tm2) ds
      let (mf,df) = mergeInternal ds'
      return $ (pc,mf,cf ++ df)
    addIncludes tm fs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource fs
      includeNewTypes tm cs
    compilePublic tm fs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource fs
      let pc = map getCategoryName cs
      tm' <- includeNewTypes tm cs
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm') cs
      let interfaces = filter (not . isValueConcrete) cs
      cxx <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      return (pc,tm',hxx ++ cxx)
    compileInternal tm d = do
      let namespace = privateNamepace d
      (cs,ds) <- parseInternalSource d
      let cs' = map (setCategoryNamespace namespace) cs
      tm' <- includeNewTypes tm cs'
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm') cs'
      cxx <- collectAllOrErrorM $ map (compileConcreteDefinition tm' [namespace]) ds
      let interfaces = filter (not . isValueConcrete) cs'
      ms <- maybeCreateMain tm' m
      cxx2 <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      return $ (ms,hxx ++ cxx ++ cxx2)
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
    maybeCreateMain tm (CompileBinary n f) = do
      case (CategoryName n) `Map.lookup` tm of
        Nothing -> return []
        Just t -> do
          (ns,main) <- createMainFile tm t f
          return [CxxOutput Nothing mainFilename "" ns [getCategoryName t] main]
    maybeCreateMain _ _ = return []

checkAllowedStale :: Bool -> ForceMode -> IO ()
checkAllowedStale fr f = do
  when (not fr && f < ForceAll) $ do
    hPutStrLn stderr $ "Some dependencies are out of date. " ++
                       "Recompile them or use -f to force."
    exitFailure

fixPaths :: [String] -> [String]
fixPaths = nub . map fixPath

getBasePath :: IO String
getBasePath = getExecutablePath >>= return . takeDirectory

zipWithContents :: String -> [String] -> IO [(String,String)]
zipWithContents p fs = fmap (zip $ map fixPath fs) $ sequence $ map (readFile . (p </>)) fs
