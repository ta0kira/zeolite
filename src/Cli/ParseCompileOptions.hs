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

module Cli.ParseCompileOptions (
  optionHelpText,
  parseCompileOptions,
  validateCompileOptions,
) where

import Control.Monad (when)
import Data.List (isSuffixOf)
import Text.Regex.TDFA -- Not safe!

import Base.CompileError
import Cli.CompileOptions
import Types.TypeCategory (FunctionName(..))
import Types.TypeInstance (CategoryName(..))


optionHelpText :: [String]
optionHelpText = [
    "",
    "zeolite [options...] -m [category(.function)] -o [binary] [path]",
    "zeolite [options...] --fast [category(.function)] [.0rx source]",
    "zeolite [options...] -c [path]",
    "zeolite [options...] -r [paths...]",
    "zeolite [options...] -R [paths...]",
    "zeolite [options...] -t [paths...]",
    "",
    "zeolite [options...] --templates [paths...]",
    "zeolite --get-path",
    "zeolite --show-deps [paths...]",
    "zeolite --version",
    "",
    "Normal Modes:",
    "  -c: Only compile the individual files. (default)",
    "  -m [category(.function)]: Create a binary that executes the function.",
    "  --fast [category(.function)] [.0rx source]: Create a binary without needing a module.",
    "  -r: Recompile using the previous compilation options.",
    "  -R: Recursively recompile using the previous compilation options.",
    "  -t: Only execute tests, without other compilation.",
    "",
    "Special Modes:",
    "  --templates: Only create C++ templates for undefined categories in .0rp sources.",
    "  --get-path: Show the data path and immediately exit.",
    "  --show-deps: Show category dependencies for the modules.",
    "  --version: Show the compiler version and immediately exit.",
    "",
    "Options:",
    "  -f: Force compilation instead of recompiling with -r/-R.",
    "  -i [path]: A single source path to include as a *public* dependency.",
    "  -I [path]: A single source path to include as a *private* dependency.",
    "  -o [binary]: The name of the binary file to create with -m.",
    "  -p [path]: Set a path prefix for finding the specified source files.",
    "",
    "[category]: The name of a concrete category with no params.",
    "[function]: The name of a @type function (defaults to \"run\") with no args or params.",
    "[path(s...)]: Path(s) containing source or dependency files.",
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
  argError n o m = compileErrorM $ "Argument " ++ show n ++ " (\"" ++ o ++ "\"): " ++ m
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
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (ExecuteTests []) f)

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
        let m2 = CompileBinary (CategoryName t) (FunctionName fn) "" []
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
        when (not $ isSuffixOf ".0rx" f2) $ argError n3 f2 $ "Must specify a .0rx source file."
        let m2 = CompileFast (CategoryName t) (FunctionName fn) f2
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep p m2 f) where
          check (t,"")     = return (t,defaultMainFunc)
          check (t,'.':fn) = return (t,fn)
          check _          = argError n2 c $ "Invalid entry point."
      update _ = argError n "--fast" "Requires a category name and a .0rx file."

  parseSingle (CompileOptions h is is2 ds es ep p (CompileBinary t fn o lf) f) ((n,"-o"):os)
    | not $ null o = argError n "-o" "Output name already set."
    | otherwise = update os where
      update ((n2,o2):os2) = do
        checkPathName n2 o2 "-o"
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (CompileBinary t fn o2 lf) f)
      update _ = argError n "-o" "Requires an output name."
  parseSingle _ ((n,"-o"):_) = argError n "-o" "Set mode to binary (-m) first"

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-i"):os) = update os where
    update ((n2,d):os2)
      | isSuffixOf ".0rp" d = argError n2 d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n2 d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = argError n2 d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n2 d "-i"
          return (os2,CompileOptions (maybeDisableHelp h) (is ++ [d]) is2 ds es ep p m f)
    update _ = argError n "-i" "Requires a source path."

  parseSingle (CompileOptions h is is2 ds es ep p m f) ((n,"-I"):os) = update os where
    update ((n2,d):os2)
      | isSuffixOf ".0rp" d = argError n2 d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n2 d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = argError n2 d "Cannot directly include .0rt test files."
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
      | isSuffixOf ".0rp" d = argError n d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = do
        when (not $ isExecuteTests m) $
          argError n d "Test mode (-t) must be enabled before listing any .0rt test files."
        checkPathName n d ""
        let (ExecuteTests tp) = m
        return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep p (ExecuteTests $ tp ++ [d]) f)
      | otherwise = do
        checkPathName n d ""
        return (os,CompileOptions (maybeDisableHelp h) is is2 (ds ++ [d]) es ep p m f)

validateCompileOptions :: CollectErrorsM m => CompileOptions -> m CompileOptions
validateCompileOptions co@(CompileOptions h is is2 ds _ _ _ m _)
  | h /= HelpNotNeeded = return co

  | (not $ null $ is ++ is2) && (isExecuteTests m) =
    compileErrorM "Include paths (-i/-I) are not allowed in test mode (-t)."

  | (not $ null $ is ++ is2) && (isCompileRecompile m) =
    compileErrorM "Include paths (-i/-I) are not allowed in recompile mode (-r/-R)."

  | (length ds /= 0) && (isCompileFast m) =
    compileErrorM "Input path is not allowed with fast mode (--fast)."
  | null ds && (not $ isCompileFast m) =
    compileErrorM "Please specify at least one input path."
  | (length ds > 1) && (not $ isCompileRecompile m) && (not $ isExecuteTests m) =
    compileErrorM "Multiple input paths are only allowed with recompile mode (-r/-R) and test mode (-t)."

  | otherwise = return co
