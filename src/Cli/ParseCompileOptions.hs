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
import Text.Regex.TDFA

import Base.CompilerError
import Cli.CompileOptions
import Module.ProcessMetadata (isPrivateSource,isPublicSource,isTestSource)
import Types.TypeCategory (FunctionName(..))
import Types.TypeInstance (CategoryName(..))


optionHelpText :: [String]
optionHelpText = [
    "",
    "zeolite [options...] --fast [category(.function)] [.0rx source]",
    "zeolite [options...] -r [modules...]",
    "zeolite [options...] -R [modules...]",
    "zeolite [options...] -t [modules...] (--log-traces [filename])",
    "",
    "zeolite [options...] -c [module]",
    "zeolite [options...] -m [category(.function)] (-o [binary]) [module]",
    "",
    "zeolite [options...] --templates [modules...]",
    "",
    "zeolite (-p [path]) --clean [modules...]",
    "zeolite (-p [path]) --missed-lines [filename] [modules...]",
    "zeolite (-p [path]) --show-deps [modules...]",
    "zeolite (-p [path]) --show-traces [modules...]",
    "",
    "zeolite --get-path",
    "zeolite --version",
    "",
    "Compilation Modes:",
    "  --fast: Create a binary without needing a config.",
    "  -r: Recompile using each module's .zeolite-module config.",
    "  -R: Recursively recompile using each module's .zeolite-module config.",
    "  -t: Only execute tests, without other compilation.",
    "",
    "Configuration Modes:",
    "  -c: Create a new .zeolite-module config for a libary module.",
    "  -m: Create a new .zeolite-module config for a binary module.",
    "",
    "Special Modes:",
    "  --templates: Only create C++ templates for undefined categories in .0rp sources.",
    "  --clean: Remove all cached data for the modules.",
    "  --missed-lines: List all lines missed in a log file taken from --log-traces.",
    "  --show-deps: Show category dependencies for the modules.",
    "  --show-traces: Show the possible code traces for the modules.",
    "  --get-path: Show the data path and immediately exit.",
    "  --version: Show the compiler version and immediately exit.",
    "",
    "Options:",
    "  -f: Force an operation that zeolite would otherwise reject.",
    "  -i [module]: A single source module to include as a public dependency.",
    "  -I [module]: A single source module to include as a private dependency.",
    "  -o [binary]: The name of the binary file to create with -m.",
    "  -p [path]: Set a path prefix for finding modules or files.",
    "  --log-traces [filename]: Log call traces to a file when running tests.",
    "",
    "Argument Types:",
    "  category: The name of a concrete category with no params.",
    "  function: The name of a @type function (defaults to \"run\") with no args or params.",
    "  module: Path to a directory containing an existing or to-be-created Zeolite module.",
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

parseCompileOptions :: CollectErrorsM m => [String] -> m CompileOptions
parseCompileOptions = parseAll emptyCompileOptions . zip ([1..] :: [Int]) where
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

  parseSingle (CompileOptions _ is is2 ds es ep p m f) ((_,"-h"):os) =
    return (os,CompileOptions HelpNeeded is is2 ds es ep p m f)

  parseSingle (CompileOptions h is is2 ds es ep p m _) ((_,"-f"):os) =
    return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p m ForceAll)

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-c"):os)
    | m /= CompileUnspecified = argError n "-c" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (CompileIncremental []) f)

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-r"):os)
    | m /= CompileUnspecified = argError n "-r" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p CompileRecompile f)

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-R"):os)
    | m /= CompileUnspecified = argError n "-r" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p CompileRecompileRecursive f)

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-t"):os)
    | m /= CompileUnspecified = argError n "-t" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (ExecuteTests [] Nothing) f)

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"--templates"):os)
    | m /= CompileUnspecified = argError n "-t" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p CreateTemplates f)

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-m"):os)
    | m /= CompileUnspecified = argError n "-m" "Compiler mode already set."
    | otherwise = update os where
      update ((n2,c):os2) =  do
        (t,fn) <- check $ break (== '.') c
        checkCategoryName n2 t  "-m"
        checkFunctionName n2 fn "-m"
        let m2 = CompileBinary (CategoryName t) (FunctionName fn) LinkDynamic "" []
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep p m2 f) where
          check (t,"")     = return (t,defaultMainFunc)
          check (t,'.':fn) = return (t,fn)
          check _          = argError n2 c $ "Invalid entry point."
      update _ = argError n "-m" "Requires a category name."

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"--fast"):os)
    | m /= CompileUnspecified = argError n "--fast" "Compiler mode already set."
    | otherwise = update os where
      update ((n2,c):(n3,f2):os2) =  do
        (t,fn) <- check $ break (== '.') c
        checkCategoryName n2 t  "--fast"
        checkFunctionName n2 fn "--fast"
        when (not $ isPrivateSource f2) $ argError n3 f2 $ "Must specify a .0rx source file."
        let m2 = CompileFast (CategoryName t) (FunctionName fn) f2
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep p m2 f) where
          check (t,"")     = return (t,defaultMainFunc)
          check (t,'.':fn) = return (t,fn)
          check _          = argError n2 c $ "Invalid entry point."
      update _ = argError n "--fast" "Requires a category name and a .0rx file."

  parseSingle (CompileOptions h is is2 ds es ep p (ExecuteTests tp cl) f) ((n,"--log-traces"):os) = update os where
    update ((_,cl2):os2)
      | cl /= Nothing = argError n "--log-traces" "Trace-log filename already set."
      | otherwise = return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (ExecuteTests tp (Just cl2)) f)
    update _ = argError n "--log-traces" "Requires an output filename."
  parseSingle _ ((n,"--log-traces"):_) = argError n "--log-traces" "Set mode to test (-t) first."

  parseSingle (CompileOptions h is is2 ds es ep p (CompileBinary t fn lm o lf) f) ((n,"-o"):os)
    | not $ null o = argError n "-o" "Output name already set."
    | otherwise = update os where
      update ((n2,o2):os2) = do
        checkPathName n2 o2 "-o"
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (CompileBinary t fn lm o2 lf) f)
      update _ = argError n "-o" "Requires an output name."
  parseSingle _ ((n,"-o"):_) = argError n "-o" "Set mode to binary (-m) first."

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-i"):os) = update os where
    update ((n2,d):os2)
      | isPublicSource  d = argError n2 d "Cannot directly include .0rp source files."
      | isPrivateSource d = argError n2 d "Cannot directly include .0rx source files."
      | isTestSource    d = argError n2 d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n2 d "-i"
          return (os2,CompileOptions (maybeDisableHelp h) (is ++ [d]) is2 ds es ep p m f)
    update _ = argError n "-i" "Requires a source path."

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-I"):os) = update os where
    update ((n2,d):os2)
      | isPublicSource  d = argError n2 d "Cannot directly include .0rp source files."
      | isPrivateSource d = argError n2 d "Cannot directly include .0rx source files."
      | isTestSource    d = argError n2 d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n2 d "-i"
          return (os2,CompileOptions (maybeDisableHelp h) is (is2 ++ [d]) ds es ep p m f)
    update _ = argError n "-I" "Requires a source path."

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-p"):os)
    | not $ null p = argError n "-p" "Path prefix already set."
    | otherwise = update os where
      update ((n2,p2):os2) = do
        checkPathName n2 p2 "-p"
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep p2 m f)
      update _ = argError n "-p" "Requires a path prefix."

  parseSingle _ ((n,o@('-':_)):_) = argError n o "Unknown option."

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,d):os)
      | isPublicSource  d = argError n d "Cannot directly include .0rp source files."
      | isPrivateSource d = argError n d "Cannot directly include .0rx source files."
      | isTestSource    d = do
        when (not $ isExecuteTests m) $
          argError n d "Test mode (-t) must be enabled before listing any .0rt test files."
        checkPathName n d ""
        let (ExecuteTests tp cl) = m
        return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (ExecuteTests (tp ++ [d]) cl) f)
      | otherwise = do
        checkPathName n d ""
        return (os,CompileOptions (maybeDisableHelp h) is is2 (ds ++ [d]) es ep p m f)

validateCompileOptions :: CollectErrorsM m => CompileOptions -> m CompileOptions
validateCompileOptions co@(CompileOptions h is is2 ds _ _ _ m _)
  | h /= HelpNotNeeded = return co

  | isCompileUnspecified m =
    compilerErrorM "Compiler mode must be specified explicitly."

  | (not $ null $ is ++ is2) && (isExecuteTests m) =
    compilerErrorM "Include paths (-i/-I) are not allowed in test mode (-t)."

  | (not $ null $ is ++ is2) && (isCompileRecompile m) =
    compilerErrorM "Include paths (-i/-I) are not allowed in recompile mode (-r/-R)."

  | (length ds /= 0) && (isCompileFast m) =
    compilerErrorM "Input path is not allowed with fast mode (--fast)."
  | null ds && (not $ isCompileFast m) =
    compilerErrorM "Please specify at least one input path."
  | (length ds > 1) && (not $ isCompileRecompile m) && (not $ isExecuteTests m) =
    compilerErrorM "Multiple input paths are only allowed with recompile mode (-r/-R) and test mode (-t)."

  | otherwise = return co
