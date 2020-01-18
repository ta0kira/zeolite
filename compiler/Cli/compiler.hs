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

import Control.Arrow (first,second)
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
import Cli.CxxCommand
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
    validate co@(CompileOptions h is ds es ep p m)
      | h /= HelpNotNeeded = return co
      | null ds   = compileError "Please specify at least one input path."
      | otherwise = return co

showHelp :: IO ()
showHelp = do
  hPutStrLn stderr "Zeolite CLI Help:"
  mapM_ (hPutStrLn stderr . ("  " ++)) optionHelpText

runCompiler :: CompileOptions -> IO ()
runCompiler co@(CompileOptions h is ds es ep p m) = do
  when (h /= HelpNotNeeded) (showHelp >> exitFailure)
  (as,is) <- getSourceFilesForDeps is >>= return . first nub >>= return . second nub
  basePath <- getBasePath
  is' <- zipWithContents is
  ms <- fmap concat $ sequence $ map (processPath basePath as is') ds
  -- TODO: Stop spamming paths just to find deps for main.cpp.
  writeMain ([basePath] ++ ds ++ as) m ms
  hPutStrLn stderr $ "Zeolite compilation succeeded."
  exitSuccess where
    ep' = map (getCachedPath p "") ep
    getBasePath = getExecutablePath >>= return . takeDirectory
    processPath bp as is d = do
      eraseMetadata d -- Avoids invalid metadata.
      (ps,xs) <- findSourceFiles p d
      -- Lazy dependency loading, in case we aren't compiling anything.
      paths <- if null ps && null xs
                  then return []
                  else getIncludePathsForDeps (bp:as) >>= return . nub
      ps' <- zipWithContents ps
      xs' <- zipWithContents xs
      let fs = compileAll is ps' xs'
      writeOutput (paths ++ ep') d as (map takeFileName ps) (map takeFileName xs) fs
    zipWithContents fs = fmap (zip fs) $ sequence $ map (readFile . (p </>)) fs
    writeOutput paths d as ps xs fs
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
          writeMetadata (p </> d) $ CompileMetadata {
              cmPath = path,
              cmDepPaths = sort as,
              cmCategories = sort $ map show pc,
              cmSubdirs = sort $ ss ++ ep,
              cmPublicFiles = sort ps,
              cmPrivateFiles = sort xs,
              cmHxxFiles = sort hxx,
              cmCxxFiles = sort cxx,
              cmObjectFiles = sort os'
            }
          return mf
    getPath d = do
      pwd <- getCurrentDirectory
      return $ normalise $ pwd </> p </> d
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
           let command = CompileToObject f' (getCachedPath (p </> d) ns o) (p0:p1:paths)
           runCxxCommand command
           return [ns </> o]
         else return []
    compileExtraFile paths d f
      | isSuffixOf ".cpp" f || isSuffixOf ".cc" f = do
          let f' = getCachedPath (p </> d) "" f
          let p0 = getCachedPath (p </> d) "" ""
          let o = takeFileName $ dropExtension f ++ ".o"
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
    writeMain paths (CompileBinary n f0) ms
      | length ms > 1 = do
        hPutStr stderr $ "Multiple matches for main category " ++ n ++ "."
        exitFailure
      | length ms == 0 = do
        hPutStr stderr $ "Main category " ++ n ++ " not found."
        exitFailure
      | otherwise = do
          let (CxxOutput _ _ os) = head ms
          -- TODO: Create a helper or a constant or something.
          (f1,h) <- mkstemps "/tmp/main" ".cpp"
          hPutStr h $ concat $ map (++ "\n") os
          hClose h
          paths' <- getIncludePathsForDeps paths >>= return . nub
          os     <- getObjectFilesForDeps  paths >>= return . nub
          let command = CompileToBinary (f1:os) f0 paths'
          runCxxCommand command
          removeFile f1
    writeMain _ _ _ = return ()
    maybeCreateMain tm (CompileBinary n _) = do
      case (CategoryName n) `Map.lookup` tm of
        Nothing -> return []
        Just t -> do
          contents <- createMain t
          return [CxxOutput mainFilename "" contents]
    maybeCreateMain _ _ = return []

createMain :: (CompileErrorM m, Monad m) => AnyCategory c -> m [String]
createMain t
  -- TODO: Don't hard code as much here.
  | isValueConcrete t = return [
      "#include \"category-source.hpp\"",
      "",
      "#include \"Category_Runner.hpp\"",
      "#include \"Category_" ++ show (getCategoryName t) ++ ".hpp\"",
      "",
      "int main() {",
      "  SetSignalHandler();",
      "  TRACE_FUNCTION(\"main\")",
      "  " ++ qualifiedTypeGetter t ++ "(T_get()).Call(Function_Runner_run, ParamTuple(), ArgTuple());",
      "}"
    ]
  | otherwise = compileError $ "Main category " ++ show (getCategoryName t) ++ " is not concrete."
