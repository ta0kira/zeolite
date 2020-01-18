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

module Cli.CxxCommand (
  CxxCommand(..),
  runCxxCommand,
) where

import Data.List (intercalate)
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process (ProcessStatus(..),executeFile,forkProcess,getProcessStatus)


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

cxxCompiler = "clang++"
cxxBaseOptions = ["-O2", "-std=c++11"]

runCxxCommand (CompileToObject s o ps) =
  executeProcess cxxCompiler $ cxxBaseOptions ++ otherOptions ++ ["-c", s, "-o", o] where
    otherOptions = map ("-I" ++) $ map normalise ps
runCxxCommand (CompileToBinary ss o ps) =
  executeProcess cxxCompiler $ cxxBaseOptions ++ otherOptions ++ ss ++ ["-o", o] where
    otherOptions = map ("-I" ++) $ map normalise ps

executeProcess c os = do
  hPutStrLn stderr $ "Executing: " ++ intercalate " " (c:os)
  pid <- forkProcess run
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _ -> exitFailure
  where
    run = executeFile c True os Nothing
