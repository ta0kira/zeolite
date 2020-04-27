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
import System.FilePath
import System.IO

import Base.CompileError
import Cli.CompileOptions
import Cli.Compiler
import Cli.ParseCompileOptions -- Not safe, due to Text.Regex.TDFA.
import Compilation.CompileInfo


main = do
  args <- getArgs
  let options = parseCompileOptions args >>= validateCompileOptions
  compile options where
    compile co
      | isCompileError co = do
          hPutStr stderr $ show $ getCompileError co
          hPutStrLn stderr "Use the -h option to show help."
          exitFailure
      | otherwise = do
        let co' = getCompileSuccess co
        when (HelpNotNeeded /= (coHelp co')) $ showHelp >> exitFailure
        runCompiler co'

showHelp :: IO ()
showHelp = do
  hPutStrLn stderr "Zeolite CLI Help:"
  mapM_ (hPutStrLn stderr . ("  " ++)) optionHelpText
  hPutStrLn stderr "Also see https://ta0kira.github.io/zeolite for more documentation."
