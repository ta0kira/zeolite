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

import CompileInfo
import TypesBase
import Cli.CompileOptions
import Cli.ParseCompileOptions -- Not safe, due to Text.Regex.TDFA.


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
    validate co@(CompileOptions h is cs ds m)
      | h /= HelpNotNeeded = return co
      | null cs && null ds = compileError "Please specify at least one input file."
      | otherwise          = return co

showHelp :: IO ()
showHelp = do
  hPutStrLn stderr "Zeolite CLI Help:"
  mapM_ (hPutStrLn stderr . ("  " ++)) optionHelpText

runCompiler :: CompileOptions -> IO ()
runCompiler co@(CompileOptions h is cs ds m) = do
  when (h /= HelpNotNeeded) (showHelp >> exitFailure)
  hPutStrLn stderr $ show co
  exitSuccess
