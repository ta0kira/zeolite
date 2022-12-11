{- -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

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

module Cli.ParseCompileOptions (
  optionHelpText,
  parseCompileOptions,
  validateCompileOptions,
) where

import Control.Monad (when)
import Lens.Micro
import Text.Regex.TDFA
import qualified Data.Set as Set

import Base.CompilerError
import Cli.CompileOptions
import Module.ProcessMetadata (isPrivateSource,isPublicSource,isTestSource)
import Types.TypeCategory (FunctionName(..))
import Types.TypeInstance (CategoryName(..))


optionHelpText :: [String]
optionHelpText = [
    "",
    "Compilation Modes:",
    "",
    "  zeolite [options...] --fast [category(.function)] [.0rx source]",
    "    Create a binary without needing a config.",
    "",
    "  zeolite [options...] -r [modules...]",
    "    Recompile using each module's .zeolite-module config.",
    "",
    "  zeolite [options...] -R [modules...]",
    "    Recursively recompile using each module's .zeolite-module config.",
    "",
    "  zeolite [options...] -t (--log-traces [filename]) [modules...] (tests...)",
    "    Only execute tests, without other compilation.",
    "",
    "Configuration Modes:",
    "",
    "  zeolite [options...] -c [module]",
    "    Create a new .zeolite-module config for a libary module.",
    "",
    "  zeolite [options...] -m [category(.function)] (-o [binary]) [module]",
    "    Create a new .zeolite-module config for a binary module.",
    "",
    "Special Modes:",
    "",
    "  zeolite [options...] --templates [modules...]",
    "    Only create C++ templates for undefined categories in .0rp sources.",
    "",
    "  zeolite (-p [path]) --clean [modules...]",
    "    Remove all cached data for the modules.",
    "",
    "  zeolite (-p [path]) --missed-lines [filename] [modules...]",
    "    List all lines missed in a log file taken from --log-traces.",
    "",
    "  zeolite (-p [path]) --show-deps [modules...]",
    "    Show category dependencies for the modules.",
    "",
    "  zeolite (-p [path]) --show-traces [modules...]",
    "    Show the possible code traces for the modules.",
    "",
    "Compiler Info:",
    "",
    "  zeolite --get-path",
    "    Show the data path and immediately exit.",
    "",
    "  zeolite --version",
    "    Show the compiler version and immediately exit.",
    "",
    "Options:",
    "  -f: Force an operation that zeolite would otherwise reject.",
    "  -i [module]: A single source module to include as a public dependency.",
    "  -I [module]: A single source module to include as a private dependency.",
    "  -j [# parallel]: Parallel execution of compilation subprocesses.",
    "  -o [binary]: The name of the binary file to create with -m.",
    "  -p [path]: Set a path prefix for finding modules or files.",
    "  --log-traces [filename]: Log call traces to a file when running tests.",
    "",
    "Argument Types:",
    "  category: The name of a concrete category with no params.",
    "  function: The name of a @type function (defaults to \"run\") with no args or params.",
    "  module: Path to a directory containing an existing or to-be-created Zeolite module.",
    "  test: Name of a .0rt file to limit test execution to.",
    "",
    "Examples:",
    "",
    "  # Create a config for a binary in myprogram that calls MyProgram.run() that uses lib/util.",
    "  zeolite -I lib/util -m MyProgram.run myprogram",
    "",
    "  # Compile a binary that calls MyProgram.run() from myprogram.0rx without a config.",
    "  zeolite -I lib/util --fast MyProgram.run myprogram.0rx",
    "",
    "  # Create a config for a library in mylibrary that uses lib/util.",
    "  zeolite -I lib/util -c mylibrary",
    "",
    "  # Recompile myprogram, recursively recompiling its dependencies first.",
    "  zeolite -R myprogram",
    "",
    "  # Execute the tests from .0rt files in mylibrary.",
    "  zeolite -t mylibrary",
    "",
    "  # Display the contexts of all code lines missed by tests for mylibrary.",
    "  zeolite -t --log-traces traces.csv mylibrary",
    "  zeolite --missed-lines traces.csv mylibrary",
    ""
  ]

defaultMainFunc :: String
defaultMainFunc = "run"

optionsWithArgs :: Set.Set Char
optionsWithArgs = Set.fromList "iIjmop"

splitOptionClusters :: [String] -> [String]
splitOptionClusters (o@('-':c:rest):os)
  | c == '-' || null rest          = o : splitOptionClusters os
  | c `Set.member` optionsWithArgs = ('-':c:[]) : rest : splitOptionClusters os
  | otherwise                      = ('-':c:[]) : splitOptionClusters (('-':rest):os)
splitOptionClusters (o:os) = o : splitOptionClusters os
splitOptionClusters [] = []

parseCompileOptions :: CollectErrorsM m => [String] -> m CompileOptions
parseCompileOptions = parseAll emptyCompileOptions . zip ([1..] :: [Int]) . splitOptionClusters where
  parseAll co [] = return co
  parseAll co os = do
    (os',co') <- parseSingle co os
    parseAll co' os'
  argError n o m = compilerErrorM $ "Argument " ++ show n ++ " (\"" ++ o ++ "\"): " ++ m
  checkPathName n f o
    | f =~ "^(/[^/]+|[^-/][^/]*)(/[^/]+)*$" = return ()
    | null o    = argError n f "Invalid file path."
    | otherwise = argError n f $ "Invalid file path for " ++ o ++ "."
  checkCategoryName n c o
    | c =~ "^[A-Z][A-Za-z0-9]+$" = return ()
    | otherwise = argError n c $ "Invalid category name for " ++ o ++ "."
  checkFunctionName n d o
    | d =~ "^[a-z][A-Za-z0-9]+$" = return ()
    | otherwise = argError n d $ "Invalid function name for " ++ o ++ "."

  parseSingle _ [] = undefined

  parseSingle opts ((_,"-h"):os) =
    return (os,opts & coHelp .~ HelpNeeded)

  parseSingle opts ((_,"-f"):os) =
    return (os,opts & coForce .~ ForceAll)

  parseSingle opts ((n,"-c"):os)
    | (opts ^. coMode) /= CompileUnspecified = argError n "-c" "Compiler mode already set."
    | otherwise = return (os,opts & coHelp %~ maybeDisableHelp & coMode .~ (CompileIncremental []))

  parseSingle opts ((n,"-r"):os)
    | (opts ^. coMode) /= CompileUnspecified = argError n "-r" "Compiler mode already set."
    | otherwise = return (os,opts & coHelp %~ maybeDisableHelp & coMode .~ CompileRecompile)

  parseSingle opts ((n,"-R"):os)
    | (opts ^. coMode) /= CompileUnspecified = argError n "-R" "Compiler mode already set."
    | otherwise = return (os,opts & coHelp %~ maybeDisableHelp & coMode .~ CompileRecompileRecursive)

  parseSingle opts ((n,"-t"):os)
    | (opts ^. coMode) /= CompileUnspecified = argError n "-t" "Compiler mode already set."
    | otherwise = return (os,opts & coHelp %~ maybeDisableHelp & coMode .~ (ExecuteTests [] Nothing))

  parseSingle opts ((n,"--templates"):os)
    | (opts ^. coMode) /= CompileUnspecified = argError n "--templates" "Compiler mode already set."
    | otherwise = return (os,opts & coHelp %~ maybeDisableHelp & coMode .~ CreateTemplates)

  parseSingle opts ((n,"-m"):os)
    | (opts ^. coMode) /= CompileUnspecified = argError n "-m" "Compiler mode already set."
    | otherwise = update os where
      update ((n2,c):os2) =  do
        (t,fn) <- check $ break (== '.') c
        checkCategoryName n2 t  "-m"
        checkFunctionName n2 fn "-m"
        let m2 = CompileBinary (CategoryName t) (FunctionName fn) LinkDynamic "" []
        return (os2,opts & coHelp %~ maybeDisableHelp & coMode .~ m2) where
          check (t,"")     = return (t,defaultMainFunc)
          check (t,'.':fn) = return (t,fn)
          check _          = argError n2 c $ "Invalid entry point."
      update _ = argError n "-m" "Requires a category name."

  parseSingle opts ((n,"--fast"):os)
    | (opts ^. coMode) /= CompileUnspecified = argError n "--fast" "Compiler mode already set."
    | otherwise = update os where
      update ((n2,c):(n3,f2):os2) =  do
        (t,fn) <- check $ break (== '.') c
        checkCategoryName n2 t  "--fast"
        checkFunctionName n2 fn "--fast"
        when (not $ isPrivateSource f2) $ argError n3 f2 $ "Must specify a .0rx source file."
        let m2 = CompileFast (CategoryName t) (FunctionName fn) f2
        return (os2,opts & coHelp %~ maybeDisableHelp & coMode .~ m2) where
          check (t,"")     = return (t,defaultMainFunc)
          check (t,'.':fn) = return (t,fn)
          check _          = argError n2 c $ "Invalid entry point."
      update _ = argError n "--fast" "Requires a category name and a .0rx file."

  parseSingle opts ((n,"--log-traces"):os) = update (opts ^. coMode) os where
    update (ExecuteTests tp cl) ((_,cl2):os2)
      | cl /= Nothing = argError n "--log-traces" "Trace-log filename already set."
      | otherwise = return (os2,opts & coHelp %~ maybeDisableHelp & coMode .~ (ExecuteTests tp (Just cl2)))
    update _ [] = argError n "--log-traces" "Requires an output filename."
    update _ _  = argError n "--log-traces" "Set mode to test (-t) first."

  parseSingle opts ((n,"-o"):os) = update (opts ^. coMode) os where
    update (CompileBinary t fn lm o lf) ((n2,o2):os2)
      | not $ null o =  argError n "-o" "Output name already set."
      | otherwise = do
          checkPathName n2 o2 "-o"
          return (os2,opts & coHelp %~ maybeDisableHelp & coMode .~ (CompileBinary t fn lm o2 lf))
    update _ [] = argError n "-o" "Requires an output name."
    update _ _  = argError n "-o" "Set mode to binary (-m) first."

  parseSingle opts ((n,"-i"):os) = update os where
    update ((n2,d):os2)
      | isPublicSource  d = argError n2 d "Cannot directly include .0rp source files."
      | isPrivateSource d = argError n2 d "Cannot directly include .0rx source files."
      | isTestSource    d = argError n2 d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n2 d "-i"
          return (os2,opts & coHelp %~ maybeDisableHelp & coPublicDeps <>~ [d])
    update _ = argError n "-i" "Requires a source path."

  parseSingle opts ((n,"-I"):os) = update os where
    update ((n2,d):os2)
      | isPublicSource  d = argError n2 d "Cannot directly include .0rp source files."
      | isPrivateSource d = argError n2 d "Cannot directly include .0rx source files."
      | isTestSource    d = argError n2 d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n2 d "-i"
          return (os2,opts & coHelp %~ maybeDisableHelp & coPrivateDeps <>~ [d])
    update _ = argError n "-I" "Requires a source path."

  parseSingle opts ((n,"-p"):os)
    | not $ null (opts ^. coSourcePrefix) = argError n "-p" "Path prefix already set."
    | otherwise = update os where
      update ((n2,p2):os2) = do
        checkPathName n2 p2 "-p"
        return (os2,opts & coHelp %~ maybeDisableHelp & coSourcePrefix .~ p2)
      update _ = argError n "-p" "Requires a path prefix."

  parseSingle opts ((n,"-j"):os) = update os where
    update ((n2,k):os2) = do
      case reads k of
           [(pn,[])] -> if pn > 0
                           then return (os2,opts & coHelp %~ maybeDisableHelp & coParallel .~ pn)
                           else argError n2 k "Must be > 0."
           _ -> argError n2 k "Not a valid integer."
    update _ = argError n "-j" "Requires an integer > 0."

  parseSingle _ ((n,o@('-':_)):_) = argError n o "Unknown option."

  parseSingle opts ((n,d):os)
      | isPublicSource  d = argError n d "Cannot directly include .0rp source files."
      | isPrivateSource d = argError n d "Cannot directly include .0rx source files."
      | isTestSource    d = do
        case opts ^. coMode of
             ExecuteTests tp cl -> do
               checkPathName n d ""
               return (os,opts & coHelp %~ maybeDisableHelp & coMode .~ (ExecuteTests (tp ++ [d]) cl))
             _ -> argError n d "Test mode (-t) must be enabled before listing any .0rt test files."
      | otherwise = do
        checkPathName n d ""
        return (os,opts & coHelp %~ maybeDisableHelp & coPaths <>~ [d])

validateCompileOptions :: CollectErrorsM m => CompileOptions -> m CompileOptions
validateCompileOptions opts
  | (opts ^. coHelp) /= HelpNotNeeded = return opts

  | isCompileUnspecified (opts ^. coMode) =
    compilerErrorM "Compiler mode must be specified explicitly."

  | (not $ null $ (opts ^. coPublicDeps) ++ (opts ^. coPrivateDeps)) && (isExecuteTests (opts ^. coMode)) =
    compilerErrorM "Include paths (-i/-I) are not allowed in test mode (-t)."

  | (not $ null $ (opts ^. coPublicDeps) ++ (opts ^. coPrivateDeps)) && (isCompileRecompile (opts ^. coMode)) =
    compilerErrorM "Include paths (-i/-I) are not allowed in recompile mode (-r/-R)."

  | (length (opts ^. coPaths) /= 0) && (isCompileFast (opts ^. coMode)) =
    compilerErrorM "Input path is not allowed with fast mode (--fast)."
  | null (opts ^. coPaths) && (not $ isCompileFast (opts ^. coMode)) =
    compilerErrorM "Please specify at least one input path."
  | (length (opts ^. coPaths) > 1) && (not $ isCompileRecompile (opts ^. coMode)) && (not $ isExecuteTests (opts ^. coMode)) =
    compilerErrorM "Multiple input paths are only allowed with recompile mode (-r/-R) and test mode (-t)."

  | otherwise = do
    when ((opts ^. coParallel) > 0 && not (isCompileRecompile (opts ^. coMode)) && not (isCompileFast (opts ^. coMode))) $
      compilerWarningM "Parallel processing (-j) has no effect outside of recompile mode (-r/-R) and fast mode (--fast)."
    return opts
