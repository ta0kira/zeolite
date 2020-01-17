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
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import qualified Data.Map as Map

import Builtin
import CompileInfo
import SourceFile
import TypesBase
import TypeCategory
import TypeInstance
import Cli.CompileOptions
import Cli.ParseCompileOptions -- Not safe, due to Text.Regex.TDFA.
import CompilerCxx.Category
import CompilerCxx.Naming


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
    validate co@(CompileOptions h is cs ds p m)
      | h /= HelpNotNeeded = return co
      | null cs && null ds = compileError "Please specify at least one input file."
      | otherwise          = return co

showHelp :: IO ()
showHelp = do
  hPutStrLn stderr "Zeolite CLI Help:"
  mapM_ (hPutStrLn stderr . ("  " ++)) optionHelpText

runCompiler :: CompileOptions -> IO ()
runCompiler co@(CompileOptions h is cs ds p m) = do
  when (h /= HelpNotNeeded) (showHelp >> exitFailure)
  rootPrefix <- guessRootPrefix
  builtinContents <- readFile (rootPrefix </> builtinFilename)
  is' <- zipWithContents is
  cs' <- zipWithContents cs
  ds' <- zipWithContents ds
  let fs = compileAll builtinContents is' cs' ds'
  writeOutput fs where
    -- TODO: Clean up root-prefix inference.
    guessRootPrefix = getExecutablePath >>= return . (</> "../..") . takeDirectory
    zipWithContents fs = fmap (zip fs) $ sequence $ map (readFile . (p </>)) fs
    writeOutput fs
      | isCompileError fs = do
        formatWarnings fs
        hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError fs)
        hPutStrLn stderr $ "Zeolite compilation failed."
        exitFailure
      | otherwise = do
        formatWarnings fs
        mapM_ writeOutputFile $ getCompileSuccess fs
        hPutStrLn stderr $ "Zeolite compilation succeeded."
        exitSuccess
    formatWarnings c
      | null $ getCompileWarnings c = return ()
      | otherwise = hPutStr stderr $ "Compiler warnings:\n" ++ (concat $ map (++ "\n") (getCompileWarnings c))
    writeOutputFile (CxxOutput f os) = do
      writeSingleFile f $ concat $ map (++ "\n") os
    writeSingleFile f c = do
      hPutStrLn stderr $ "Writing file " ++ f
      writeFile f c
    compileAll bs is cs ds = do
      tm0 <- builtinCategories builtinBasename bs
      tm1 <- addIncludes tm0 is
      (tm2,cf) <- compilePublic tm1 cs
      ds' <- collectAllOrErrorM $ map (compileInternal tm2) ds
      (mf,df) <- mergeInternal m ds'
      return $ mf ++ cf ++ df
    addIncludes tm fs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource fs
      includeNewTypes tm cs
    compilePublic tm fs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublicSource fs
      tm' <- includeNewTypes tm cs
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm') cs
      let interfaces = filter (not . isValueConcrete) cs
      cxx <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      return (tm',hxx ++ cxx)
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
    mergeInternal (CompileBinary n _) ds = do
      let matches = length (concat $ map fst ds)
      when (matches > 1)  $ compileError $ "Multiple matches for main category " ++ n ++ "."
      when (matches == 0) $ compileError $ "Main category " ++ n ++ " not found."
      return ([head $ concat $ map fst ds],concat $ map snd ds)
    mergeInternal _ ds = return ([],concat $ map snd ds)
    maybeCreateMain tm (CompileBinary n _) = do
      case (CategoryName n) `Map.lookup` tm of
        Nothing -> return []
        Just t -> do
          contents <- createMain t
          return [CxxOutput mainFilename contents]
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
