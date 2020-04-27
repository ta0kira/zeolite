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

{-# LANGUAGE Safe #-}

module Cli.CompilerCommand (
  CxxCommand(..),
  TestCommand(..),
  TestCommandResult(..),
  runCxxCommand,
  runTestCommand,
) where

import Control.Monad (when)
import GHC.IO.Handle
import Data.List (intercalate,isSuffixOf)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process (ProcessStatus(..),executeFile,forkProcess,getProcessStatus)
import System.Posix.Temp (mkstemps)

import CompilerCxx.Naming (dynamicNamespaceName)


data CxxCommand =
  CompileToObject {
    ctoSource :: String,
    ctoPath :: String,
    ctoNamespace :: String,
    ctoPaths :: [String],
    ctoExtra :: Bool
  } |
  CompileToBinary {
    ctbMain :: String,
    ctbSources :: [String],
    ctbOutput :: String,
    ctoPaths :: [String]
  }
  deriving (Show)

data TestCommand =
  TestCommand {
    tcBinary :: String,
    tcPath :: String
  }
  deriving (Show)

data TestCommandResult =
  TestCommandResult {
    tcrSuccess :: Bool,
    tcrOutput :: [String],
    tcrError :: [String]
  }
  deriving (Show)

cxxCompiler = "clang++"
arArchiver = "ar"
cxxBaseOptions = ["-O2", "-std=c++11"]

runCxxCommand :: CxxCommand -> IO String
runCxxCommand (CompileToObject s p ns ps e) = do
  objName <- canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".o")
  executeProcess cxxCompiler $ cxxBaseOptions ++ otherOptions ++ ["-c", s, "-o", objName]
  if e
     then do
       -- Extra files are put into .a since they will be unconditionally
       -- included. This prevents unwanted symbol dependencies.
       arName  <- canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".a")
       executeProcess arArchiver ["-q",arName,objName]
       return arName
     else return objName where
    otherOptions = map (("-I" ++) . normalise) ps ++ nsFlag
    nsFlag
      | null ns = []
      | otherwise = ["-D" ++ dynamicNamespaceName ++ "=" ++ ns]
runCxxCommand (CompileToBinary m ss o ps) = do
  let arFiles    = filter (isSuffixOf ".a")       ss
  let otherFiles = filter (not . isSuffixOf ".a") ss
  executeProcess cxxCompiler $ cxxBaseOptions ++ otherOptions ++ m:otherFiles ++ arFiles ++ ["-o", o]
  return o where
    otherOptions = map ("-I" ++) $ map normalise ps

runTestCommand :: TestCommand -> IO TestCommandResult
runTestCommand (TestCommand b p) = do
  (outF,outH) <- mkstemps "/tmp/ztest_" ".txt"
  (errF,errH) <- mkstemps "/tmp/ztest_" ".txt"
  pid <- forkProcess (execWithCapture outH errH)
  hClose outH
  hClose errH
  status <- getProcessStatus True True pid
  out <- readFile outF
  err <- readFile errF
  let success = case status of
                     Just (Exited ExitSuccess) -> True
                     _ -> False
  return $ TestCommandResult success (lines out) (lines err) where
    execWithCapture h1 h2 = do
      when (not $ null p) $ setCurrentDirectory p
      hDuplicateTo h1 stdout
      hDuplicateTo h2 stderr
      executeFile b True [] Nothing

executeProcess :: String -> [String] -> IO ()
executeProcess c os = do
  hPutStrLn stderr $ "Executing: " ++ intercalate " " (c:os)
  pid <- forkProcess $ executeFile c True os Nothing
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _ -> exitFailure
