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
    "zeolite [options...] -m [category] -o [binary] [paths...]",
    "zeolite [options...] -c [paths...]",
    "",
    "-m [category]: Compile a specific category into a binary file.",
    "-o [binary]: The name of the binary file to create with -m.",
    "-c: Only compile the individual files. (default)",
    "-i: A single source path to include as a dependency.",
    "-e: Include an extra source file during compilation.",
    "-p: Set a path prefix for finding the specified source files.",
    "[paths...]: Paths containing .0rp and .0rx source files to compile.",
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

  parseSingle (CompileOptions _ is ds es ep p m o) ((n,"-h"):os) =
    return (os,CompileOptions HelpNeeded is ds es ep p m o)

  parseSingle (CompileOptions h is ds es ep p m o) ((n,"-c"):os)
    | m /= CompileUnspecified = argError n "-c" "Compiler mode already set."
    | otherwise = return (os,CompileOptions (maybeDisableHelp h) is ds es ep p CompileIncremental o)

  parseSingle (CompileOptions h is ds es ep p m o) ((n,"-m"):os)
    | m /= CompileUnspecified = argError n "-m" "Compiler mode already set."
    | otherwise = update os where
      update ((n,c):os) =  do
        let (t,f) = check $ break (== '.') c
        checkCategoryName n t "-m"
        checkFunctionName n f "-m"
        return (os,CompileOptions (maybeDisableHelp h) is ds es ep p (CompileBinary t f) o) where
          check (t,"")    = (t,defaultMainFunc)
          check (t,'.':f) = (t,f)
      update _ = argError n "-m" "Requires a category name."

  parseSingle (CompileOptions h is ds es ep p m o) ((n,"-o"):os)
    | not $ null o = argError n "-o" "Output name already set."
    | otherwise = update os where
      update ((n,o):os) = do
        checkPathName n o "-o"
        return (os,CompileOptions (maybeDisableHelp h) is ds es ep p m o)
      update _ = argError n "-o" "Requires an output name."

  parseSingle (CompileOptions h is ds es ep p m o) ((n,"-i"):os) = update os where
    update ((n,f):os)
      | isSuffixOf ".0rp" f = argError n f "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" f = argError n f "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" f = argError n f "Cannot directly include .0rt test files."
      | otherwise = do
          checkPathName n f "-i"
          return (os,CompileOptions (maybeDisableHelp h) (is ++ [f]) ds es ep p m o)
    update _ = argError n "-i" "Requires a source path."

  parseSingle (CompileOptions h is ds es ep p m o) ((n,"-e"):os) = update os where
    update ((n,f):os)
      | any id $ map (flip isSuffixOf f) allowedExtraTypes = do
          checkPathName n f "-e"
          return (os,CompileOptions (maybeDisableHelp h) is ds (es ++ [getCacheRelativePath f]) ep p m o)
      | takeExtension f == "" = do
          checkPathName n f "-e"
          return (os,CompileOptions (maybeDisableHelp h) is ds es (ep ++ [getCacheRelativePath f]) p m o)
      | otherwise = argError n "-e" $ "Only " ++ intercalate ", " allowedExtraTypes ++
                                      " and directory sources are allowed."
    update _ = argError n "-e" "Requires a source filename."

  parseSingle (CompileOptions h is ds es ep p m o) ((n,"-p"):os)
    | not $ null p = argError n "-p" "Path prefix already set."
    | otherwise = update os where
      update ((n,p):os) = do
        checkPathName n p "-p"
        return (os,CompileOptions (maybeDisableHelp h) is ds es ep p m o)
      update _ = argError n "-p" "Requires a path prefix."

  parseSingle _ ((n,o@('-':_)):os) = argError n o "Unknown option."

  parseSingle (CompileOptions h is ds es ep p m o) ((n,f):os)
      | isSuffixOf ".0rp" f = argError n f "Cannot directly include .0rp source files."
      | isSuffixOf ".0rx" f = argError n f "Cannot directly include .0rx source files."
      | isSuffixOf ".0rt" f = argError n f "Cannot directly include .0rt test files."
      | otherwise = do
        checkPathName n f ""
        return (os,CompileOptions (maybeDisableHelp h) is (ds ++ [f]) es ep p m o)
