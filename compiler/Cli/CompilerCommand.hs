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

import GHC.IO.Handle
import Data.List (intercalate)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process (ProcessStatus(..),executeFile,forkProcess,getProcessStatus)
import System.Posix.Temp (mkstemps)


data CxxCommand =
  CompileToObject {
    ctoSource :: String,
    ctoOutput :: String,
    ctoPaths :: [String]
  } |
  CompileToBinary {
    ctbSources :: [String],
    ctbOutput :: String,
    ctoPaths :: [String]
  }
  deriving (Show)

data TestCommand =
  TestCommand {
    tcBinary :: String
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
cxxBaseOptions = ["-O2", "-std=c++11"]

runCxxCommand :: CxxCommand -> IO ()
runCxxCommand (CompileToObject s o ps) =
  executeProcess cxxCompiler $ cxxBaseOptions ++ otherOptions ++ ["-c", s, "-o", o] where
    otherOptions = map ("-I" ++) $ map normalise ps
runCxxCommand (CompileToBinary ss o ps) =
  executeProcess cxxCompiler $ cxxBaseOptions ++ otherOptions ++ ss ++ ["-o", o] where
    otherOptions = map ("-I" ++) $ map normalise ps

runTestCommand :: TestCommand -> IO TestCommandResult
runTestCommand (TestCommand b) = do
  (outF,outH) <- mkstemps "/tmp/ztest_" ".txt"
  (errF,errH) <- mkstemps "/tmp/ztest_" ".txt"
  pid <- forkProcess (execWithCapture outH errH)
  hClose outH
  hClose errH
  status <- getProcessStatus True True pid
  out <- readFile outF
  err <- readFile errF
  removeFile outF
  removeFile errF
  hPutStr stderr err -- Pass along the captured stderr.
  let success = case status of
                     Just (Exited ExitSuccess) -> True
                     _ -> False
  return $ TestCommandResult success (lines out) (lines err) where
    execWithCapture h1 h2 = do
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
