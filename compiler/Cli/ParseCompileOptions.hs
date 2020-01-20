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
  parseCompileOptions,
  optionHelpText,
) where

import Data.List (intercalate,isSuffixOf)
import System.FilePath (takeExtension)
import Text.Regex.TDFA -- Not safe!

import TypesBase
import Cli.CompileMetadata (allowedExtraTypes,getCacheRelativePath)
import Cli.CompileOptions


optionHelpText :: [String]
optionHelpText = [
    "",
    "zeolite [options...] -m [category(.function)] -o [binary] [path]",
    "zeolite [options...] -c [paths...]",
    "zeolite [options...] -r [paths...]",
    "zeolite [options...] -t [paths...]",
    "",
    "Modes:",
    "  -c: Only compile the individual files. (default)",
    "  -m [category(.function)]: Create a binary that executes the function.",
    "  -r: Recompile using the previous compilation options.",
    "  -t: Only execute tests, without other compilation.",
    "",
    "Options:",
    "  -e [path|file]: Include an extra source file or path during compilation.",
    "  -f: Force compilation instead of recompiling with -r.",
    "  -i [path]: A single source path to include as a dependency.",
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

parseCompileOptions :: (CompileErrorM m, Monad m) => [String] -> m CompileOptions
parseCompileOptions = parseAll emptyCompileOptions . zip [1..] where
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

  parseSingle (CompileOptions _ is ds es ep p m o f) ((n,"-h"):os) =
    return (os,CompileOptions HelpNeeded is ds es ep p m o f)

  parseSingle (CompileOptions h is ds es ep p m o _) ((n,"-f"):os) =
    return (os,CompileOptions (maybeDisableHelp h) is ds es ep p m o ForceAll)

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-c"):os)
    | m /= CompileUnspecified = argError n "-c" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is ds es ep p CompileIncremental o f)

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-r"):os)
    | m /= CompileUnspecified = argError n "-r" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is ds es ep p CompileRecompile o f)

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-t"):os)
    | m /= CompileUnspecified = argError n "-t" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is ds es ep p ExecuteTests o f)

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-m"):os)
    | m /= CompileUnspecified = argError n "-m" "Compiler mode already set."
    | otherwise = update os where
      update ((n,c):os) =  do
        let (t,fn) = check $ break (== '.') c
        checkCategoryName n t  "-m"
        checkFunctionName n fn "-m"
        return (os,CompileOptions (maybeDisableHelp h) is ds es ep p (CompileBinary t fn) o f) where
          check (t,"")     = (t,defaultMainFunc)
          check (t,'.':fn) = (t,fn)
      update _ = argError n "-m" "Requires a category name."

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-o"):os)
    | not $ null o = argError n "-o" "Output name already set."
    | otherwise = update os where
      update ((n,o):os) = do
        checkPathName n o "-o"
        return (os,CompileOptions (maybeDisableHelp h) is ds es ep p m o f)
      update _ = argError n "-o" "Requires an output name."

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-i"):os) = update os where
    update ((n,d):os)
      | isSuffixOf ".0rp" d = argError n d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = argError n d "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n d "-i"
          return (os,CompileOptions (maybeDisableHelp h) (is ++ [d]) ds es ep p m o f)
    update _ = argError n "-i" "Requires a source path."

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-e"):os) = update os where
    update ((n,e):os)
      | any id $ map (flip isSuffixOf e) allowedExtraTypes = do
          checkPathName n e "-e"
          return (os,CompileOptions (maybeDisableHelp h) is ds (es ++ [getCacheRelativePath e]) ep p m o f)
      | takeExtension e == "" = do
          checkPathName n e "-e"
          return (os,CompileOptions (maybeDisableHelp h) is ds es (ep ++ [getCacheRelativePath e]) p m o f)
      | otherwise = argError n "-e" $ "Only " ++ intercalate ", " allowedExtraTypes ++
                                      " and directory sources are allowed."
    update _ = argError n "-e" "Requires a source filename."

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,"-p"):os)
    | not $ null p = argError n "-p" "Path prefix already set."
    | otherwise = update os where
      update ((n,p):os) = do
        checkPathName n p "-p"
        return (os,CompileOptions (maybeDisableHelp h) is ds es ep p m o f)
      update _ = argError n "-p" "Requires a path prefix."

  parseSingle _ ((n,o@('-':_)):os) = argError n o "Unknown option."

  parseSingle (CompileOptions h is ds es ep p m o f) ((n,d):os)
      | isSuffixOf ".0rp" d = argError n d "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" d = argError n d "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" d = argError n d "Cannot directly include .0rt test files."
      | otherwise = do
        checkPathName n d ""
        return (os,CompileOptions (maybeDisableHelp h) is (ds ++ [d]) es ep p m o f)
