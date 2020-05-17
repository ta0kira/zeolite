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
import System.Directory
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map

import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.ProcessMetadata
import Cli.ParseCompileOptions -- Not safe, due to Text.Regex.TDFA.
import Cli.RunCompiler
import Compilation.CompileInfo
import Config.LoadConfig
import Config.LocalConfig


main :: IO ()
main = do
  args <- getArgs
  tryFastModes args
  let options = parseCompileOptions args >>= validateCompileOptions
  compile options where
    compile co
      | isCompileError co = do
          hPutStr stderr $ show $ getCompileError co
          hPutStrLn stderr "Use the -h option to show help."
          exitFailure
      | otherwise = tryCompileInfoIO "Zeolite execution failed." $ do
          let co' = getCompileSuccess co
          (resolver,backend) <- loadConfig
          when (HelpNotNeeded /= (coHelp co')) $ lift $ showHelp >> exitFailure
          runCompiler resolver backend co'
          lift $ hPutStrLn stderr "Zeolite execution succeeded."

showHelp :: IO ()
showHelp = do
  hPutStrLn stderr "Zeolite CLI Help:"
  mapM_ (hPutStrLn stderr . ("  " ++)) optionHelpText
  hPutStrLn stderr "Also see https://ta0kira.github.io/zeolite for more documentation."

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
tryFastModes ("--show-deps":ps) = do
  mapM_ showDeps ps
  exitSuccess where
    showDeps p = do
      p' <- canonicalizePath p
      m <- loadMetadata Map.empty p'
      hPutStrLn stdout $ show p'
      mapM_ showDep (cmObjectFiles m)
    showDep (CategoryObjectFile c ds _) = do
      mapM_ (\d -> hPutStrLn stdout $ "  " ++ show (ciCategory c) ++
                                      " -> " ++ show (ciCategory d) ++
                                      " " ++ show (ciPath d)) ds
    showDep _ = return ()
tryFastModes _ = return ()
