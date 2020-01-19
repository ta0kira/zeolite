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

import Control.Monad (when)
import Data.List (isSuffixOf,nub,sort)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Posix.Temp (mkstemps)
import System.IO
import qualified Data.Map as Map

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
      | otherwise = runCompiler $ getCompileSuccess co
    validate co@(CompileOptions h is ds es ep p m o)
      | h /= HelpNotNeeded = return co
      | (not $ null o) && (isCompileIncremental m) =
        compileError "Output filename (-o) is not allowed in compile-only mode (-c)."
      | (not $ null o) && (isExecuteTests m) =
        compileError "Output filename (-o) is not allowed in test mode (-t)."
      | (not $ null is) && (isExecuteTests m) =
        compileError "Include paths (-i) are not allowed in test mode (-t)."
          | (not $ null $ es ++ ep) && (isExecuteTests m) =
        compileError "Extra files (-e) are not allowed in test mode (-t)."
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
runCompiler co@(CompileOptions _ _ ds _ _ p ExecuteTests _) = do
  results <- sequence $ map runTests ds
  processResults $ mergeAllM results where
    runTests :: String -> IO (CompileInfo ())
    runTests d = do
      m <- loadMetadata (p </> d)
      basePath <- getBasePath
      deps <- loadRecursiveDeps [basePath,p </> d]
      let paths = getIncludePathsForDeps deps
      let ss = fixPaths $ getSourceFilesForDeps deps
      let os = fixPaths $ getObjectFilesForDeps deps
      ss' <- zipWithContents p ss
      ts' <- zipWithContents p (map (d </>) $ cmTestFiles m)
      tm <- return $ do
        tm0 <- builtinCategories
        cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource ss'
        includeNewTypes tm0 cs
      if isCompileError tm
         then return (tm >> return ())
         else fmap mergeAllM $ sequence $ map (runSingleTest paths os (getCompileSuccess tm)) ts'
    processResults rs
      | isCompileError rs = do
          hPutStr stderr $ "Test errors:\n" ++ (show $ getCompileError rs)
          hPutStrLn stderr $ "Zeolite tests failed."
          exitFailure
      | otherwise = do
          hPutStrLn stderr $ "Zeolite tests passed."
runCompiler co@(CompileOptions h is ds es ep p m o) = do
  when (h /= HelpNotNeeded) (showHelp >> exitFailure)
  deps <- loadRecursiveDeps is
  let ss = fixPaths $ getSourceFilesForDeps deps
  let as = fixPaths $ getRealPathsForDeps deps
  basePath <- getBasePath
  ss' <- zipWithContents p ss
  ma <- sequence $ map (processPath basePath deps as ss') ds
  let ms = concat $ map snd ma
  let deps2 = map fst ma
  -- TODO: Stop spamming paths just to find deps for main.cpp.
  writeMain basePath (deps ++ deps2) m ms
  hPutStrLn stderr $ "Zeolite compilation succeeded."
  exitSuccess where
    ep' = fixPaths $ map (getCachedPath p "") ep
    processPath bp deps as ss d = do
      eraseCachedData (p </> d)
      (ps,xs,ts) <- findSourceFiles p d
      -- Lazy dependency loading, in case we aren't compiling anything.
      paths <- if null ps && null xs
                  then return $ getIncludePathsForDeps deps
                  else do
                    bpDeps <- loadRecursiveDeps [bp]
                    return $ getIncludePathsForDeps (bpDeps ++ deps)
      ps' <- zipWithContents p ps
      xs' <- zipWithContents p xs
      let fs = compileAll ss ps' xs'
      writeOutput (fixPaths $ paths ++ ep') d as
                  (map takeFileName ps)
                  (map takeFileName xs)
                  (map takeFileName ts) fs
    writeOutput paths d as ps xs ts fs
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
          os1 <- fmap concat $ sequence $ map (writeOutputFile paths' d) $ hxx ++ other
          os2 <- fmap concat $ sequence $ map (compileExtraFile paths' d) es
          let (hxx,cxx,os') = sortCompiledFiles $ map (\f -> coNamespace f </> coFilename f) fs' ++ (os1 ++ os2) ++ es
          path <- getPath d
          let m = CompileMetadata {
              cmPath = fixPath path,
              cmDepPaths = sort as,
              cmCategories = sort $ map show pc,
              cmSubdirs = sort $ ss ++ ep,
              cmPublicFiles = sort ps,
              cmPrivateFiles = sort xs,
              cmTestFiles = sort ts,
              cmHxxFiles = sort hxx,
              cmCxxFiles = sort cxx,
              cmObjectFiles = sort os'
            }
          writeMetadata (p </> d) m
          return (m,mf)
    getPath d = do
      pwd <- getCurrentDirectory
      return $ fixPath $ pwd </> p </> d
    formatWarnings c
      | null $ getCompileWarnings c = return ()
      | otherwise = hPutStr stderr $ "Compiler warnings:\n" ++ (concat $ map (++ "\n") (getCompileWarnings c))
    writeOutputFile paths d (CxxOutput f ns os) = do
      hPutStrLn stderr $ "Writing file " ++ f
      writeCachedFile (p </> d) ns f $ concat $ map (++ "\n") os
      if isSuffixOf ".cpp" f || isSuffixOf ".cc" f
         then do
           let f' = getCachedPath (p </> d) ns f
           let p0 = getCachedPath (p </> d) "" ""
           let p1 = getCachedPath (p </> d) ns ""
           let o = takeFileName $ dropExtension f ++ ".o"
           createCachePath (p </> d)
           let command = CompileToObject f' (getCachedPath (p </> d) ns o) (p0:p1:paths)
           runCxxCommand command
           return [ns </> o]
         else return []
    compileExtraFile paths d f
      | isSuffixOf ".cpp" f || isSuffixOf ".cc" f = do
          let f' = getCachedPath (p </> d) "" f
          let p0 = getCachedPath (p </> d) "" ""
          let o = takeFileName $ dropExtension f ++ ".o"
          createCachePath (p </> d)
          let command = CompileToObject f' (getCachedPath (p </> d) "" o) (p0:paths)
          runCxxCommand command
          return [o]
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
    writeMain bp deps (CompileBinary n _) ms
      | length ms > 1 = do
        hPutStrLn stderr $ "Multiple matches for main category " ++ n ++ "."
        exitFailure
      | length ms == 0 = do
        hPutStrLn stderr $ "Main category " ++ n ++ " not found."
        exitFailure
      | otherwise = do
          let f0 = if null o then n else o
          let (CxxOutput _ _ os) = head ms
          -- TODO: Create a helper or a constant or something.
          (o',h) <- mkstemps "/tmp/zmain_" ".cpp"
          hPutStr h $ concat $ map (++ "\n") os
          hClose h
          baseDeps <- loadRecursiveDeps [bp]
          let paths = fixPaths $ getIncludePathsForDeps (baseDeps ++ deps)
          let os    = fixPaths $ getObjectFilesForDeps  (baseDeps ++ deps)
          let command = CompileToBinary (o':os) f0 paths
          runCxxCommand command
          removeFile o'
    writeMain _ _ _ _ = return ()
    maybeCreateMain tm (CompileBinary n f) = do
      case (CategoryName n) `Map.lookup` tm of
        Nothing -> return []
        Just t -> do
          contents <- createMainFile tm t f
          return [CxxOutput mainFilename "" contents]
    maybeCreateMain _ _ = return []

fixPaths :: [String] -> [String]
fixPaths = nub . map fixPath

getBasePath :: IO String
getBasePath = getExecutablePath >>= return . takeDirectory

zipWithContents :: String -> [String] -> IO [(String,String)]
zipWithContents p fs = fmap (zip $ map fixPath fs) $ sequence $ map (readFile . (p </>)) fs
