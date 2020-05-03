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
  tryFastModes,
  validateCompileOptions,
) where

import Control.Monad (when)
import Data.List (intercalate,isSuffixOf)
import System.Directory
import System.Exit
import System.FilePath (takeExtension)
import System.IO
import Text.Regex.TDFA -- Not safe!

import Base.CompileError
import Cli.CompileOptions
import Cli.ProcessMetadata (allowedExtraTypes)
import Config.LoadConfig (compilerVersion,rootPath)


optionHelpText :: [String]
optionHelpText = [
    "",
    "zeolite [options...] -m [category(.function)] -o [binary] [path]",
    "zeolite [options...] -c [paths...]",
    "zeolite [options...] -r [paths...]",
    "zeolite [options...] -t [paths...]",
    "zeolite [options...] --templates [paths...]",
    "zeolite --get-path",
    "zeolite --version",
    "",
    "Modes:",
    "  -c: Only compile the individual files. (default)",
    "  -m [category(.function)]: Create a binary that executes the function.",
    "  -r: Recompile using the previous compilation options.",
    "  -t: Only execute tests, without other compilation.",
    "  --templates: Only create C++ templates for undefined categories in .0rp sources.",
    "  --get-path: Show the data path and immediately exit.",
    "  --version: Show the compiler version and immediately exit.",
    "",
    "Options:",
    "  -e [path|file]: Include an extra source file or path during compilation.",
    "  -f: Force compilation instead of recompiling with -r.",
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

tryFastModes :: [String] -> IO ()
tryFastModes ("--get-path":os) = do
  when (not $ null os) $ hPutStrLn stderr $ "Ignoring extra arguments: " ++ show os
  p <- rootPath >>= canonicalizePath
  hPutStrLn stdout p
  if null os
     then exitSuccess
     else exitFailure
tryFastModes ("--version":os) = do
  when (not $ null os) $ hPutStrLn stderr $ "Ignoring extra arguments: " ++ show os
  hPutStrLn stdout compilerVersion
  if null os
     then exitSuccess
     else exitFailure
tryFastModes _ = return ()

parseCompileOptions :: CompileErrorM m => [String] -> m CompileOptions
parseCompileOptions = parseAll emptyCompileOptions . zip ([1..] :: [Int]) where
  parseAll co [] = return co
  parseAll co os = do
    (os',co') <- parseSingle co os
    parseAll co' os'
  argError n o m = compileError $ "Argument " ++ show n ++ " (\"" ++ o ++ "\"): " ++ m
  checkPathName n f o
    | f =~ "^(/[^/]+|[^-/][^/]*)(/[^/]+)*$" = return ()
    | null o    = argError n f "Invalid file path."
    | otherwise = argError n f $ "Invalid file path for " ++ o ++ "."
  checkCategoryName n c o
    | c =~ "^[A-Z][A-Za-z0-9]+$" = return ()
    | null o    = argError n c "Invalid category name."
    | otherwise = argError n c $ "Invalid category name for " ++ o ++ "."
  checkFunctionName n d o
    | d =~ "^[a-z][A-Za-z0-9]+$" = return ()
    | null d    = argError n d "Invalid function name."
    | otherwise = argError n d $ "Invalid function name for " ++ o ++ "."

  parseSingle _ [] = undefined

  parseSingle (CompileOptions _ is is2 ds es ep ec p m o f) ((_,"-h"):os) =
    return (os,CompileOptions HelpNeeded is is2 ds es ep ec p m o f)

  parseSingle (CompileOptions h is is2 ds es ep ec p m o _) ((_,"-f"):os) =
    return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p m o ForceAll)

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-c"):os)
    | m /= CompileUnspecified = argError n "-c" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p CompileIncremental o f)

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-r"):os)
    | m /= CompileUnspecified = argError n "-r" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p CompileRecompile o f)

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-t"):os)
    | m /= CompileUnspecified = argError n "-t" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p (ExecuteTests []) o f)

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"--templates"):os)
    | m /= CompileUnspecified = argError n "-t" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p CreateTemplates o f)

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-m"):os)
    | m /= CompileUnspecified = argError n "-m" "Compiler mode already set."
    | otherwise = update os where
      update ((n2,c):os2) =  do
        (t,fn) <- check $ break (== '.') c
        checkCategoryName n2 t  "-m"
        checkFunctionName n2 fn "-m"
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p (CompileBinary t fn) o f) where
          check (t,"")     = return (t,defaultMainFunc)
          check (t,'.':fn) = return (t,fn)
          check _          = argError n2 "-m" $ "Invalid entry point \"" ++ c ++ "\"."
      update _ = argError n "-m" "Requires a category name."

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-o"):os)
    | not $ null o = argError n "-o" "Output name already set."
    | otherwise = update os where
      update ((n2,o2):os2) = do
        checkPathName n2 o2 "-o"
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p m o2 f)
      update _ = argError n "-o" "Requires an output name."

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-i"):os) = update os where
    update ((n2,d):os2)
      | isSuffixOf ".0rp" d = argError n2 d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n2 d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = argError n2 d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n2 d "-i"
          return (os2,CompileOptions (maybeDisableHelp h) (is ++ [d]) is2 ds es ep ec p m o f)
    update _ = argError n "-i" "Requires a source path."

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-I"):os) = update os where
    update ((n2,d):os2)
      | isSuffixOf ".0rp" d = argError n2 d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n2 d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = argError n2 d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n2 d "-i"
          return (os2,CompileOptions (maybeDisableHelp h) is (is2 ++ [d]) ds es ep ec p m o f)
    update _ = argError n "-I" "Requires a source path."

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-e"):os) = update os where
    update ((n2,e):os2)
      | any (flip isSuffixOf e) allowedExtraTypes = do
          checkPathName n2 e "-e"
          return (os2,CompileOptions (maybeDisableHelp h) is is2 ds (es ++ [e]) ep ec p m o f)
      | takeExtension e == "" || e == "." = do
          checkPathName n2 e "-e"
          return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es (ep ++ [e]) ec p m o f)
      | otherwise = argError n2 "-e" $ "Only " ++ intercalate ", " allowedExtraTypes ++
                                       " and directory sources are allowed."
    update _ = argError n "-e" "Requires a source filename."

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,"-p"):os)
    | not $ null p = argError n "-p" "Path prefix already set."
    | otherwise = update os where
      update ((n2,p2):os2) = do
        checkPathName n2 p2 "-p"
        return (os2,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p2 m o f)
      update _ = argError n "-p" "Requires a path prefix."

  parseSingle _ ((n,o@('-':_)):_) = argError n o "Unknown option."

  parseSingle (CompileOptions h is is2 ds es ep ec p m o f) ((n,d):os)
      | isSuffixOf ".0rp" d = argError n d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = do
        when (not $ isExecuteTests m) $
          argError n d "Test mode (-t) must be enabled before listing any .0rt test files."
        checkPathName n d ""
        let (ExecuteTests tp) = m
        return (os,CompileOptions (maybeDisableHelp h) is is2 ds es ep ec p (ExecuteTests $ tp ++ [d]) o f)
      | otherwise = do
        checkPathName n d ""
        return (os,CompileOptions (maybeDisableHelp h) is is2 (ds ++ [d]) es ep ec p m o f)

validateCompileOptions :: CompileErrorM m => CompileOptions -> m CompileOptions
validateCompileOptions co@(CompileOptions h is is2 ds es ep _ _ m o _)
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

  | (not $ null o) && (isCompileRecompile m) =
    compileError "Output filename (-o) is not allowed in recompile mode (-r)."
  | (not $ null $ is ++ is2) && (isCompileRecompile m) =
    compileError "Include paths (-i/-I) are not allowed in recompile mode (-r)."
  | (not $ null $ es ++ ep) && (isCompileRecompile m) =
    compileError "Extra files (-e) are not allowed in recompile mode (-r)."

  | length ds > 1 && length (es ++ ep) > 0 =
    compileError "Extra files and paths (-e) cannot be used with multiple input paths, to avoid ambiguity."

  | null ds =
    compileError "Please specify at least one input path."
  | (length ds /= 1) && (isCompileBinary m) =
    compileError "Specify exactly one input path for binary mode (-m)."
  | otherwise = return co
