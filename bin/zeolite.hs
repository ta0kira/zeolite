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

import Base.CompilerError
import Base.TrackedErrors
import Cli.CompileOptions
import Cli.ParseCompileOptions
import Cli.Programs
import Cli.RunCompiler
import Module.CompileMetadata
import Module.ProcessMetadata
import Config.LoadConfig
import Config.LocalConfig


main :: IO ()
main = do
  args <- getArgs
  tryFastModes args
  let options = parseCompileOptions args >>= validateCompileOptions
  compile options
  hPutStrLn stderr "Zeolite execution succeeded." where
    compile co
      | isCompilerError co = do
          hPutStr stderr $ show $ getCompilerError co
          hPutStrLn stderr "Use the -h option to show help."
          exitFailure
      | otherwise = tryZeoliteIO $ do
          let co' = getCompilerSuccess co
          (resolver,backend) <- loadConfig
          when (HelpNotNeeded /= (coHelp co')) $ errorFromIO $ showHelp >> exitFailure
          runCompiler resolver backend co'

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
  tryZeoliteIO $ do
    (_,backend) <- loadConfig
    let h = getCompilerHash backend
    mapM_ (showDeps h) ps
  exitSuccess where
    showDeps h p = do
      p' <- errorFromIO $ canonicalizePath p
      m <- loadModuleMetadata h ForceAll Map.empty p'
      errorFromIO $ hPutStrLn stdout $ show p'
      errorFromIO $ mapM_ showDep (cmObjectFiles m)
    showDep (CategoryObjectFile c ds _) = do
      mapM_ (\d -> hPutStrLn stdout $ "  " ++ show (ciCategory c) ++
                                      " -> " ++ show (ciCategory d) ++
                                      " " ++ show (ciPath d)) ds
    showDep _ = return ()
tryFastModes _ = return ()

tryZeoliteIO :: TrackedErrorsIO a -> IO a
tryZeoliteIO = tryTrackedErrorsIO "Warnings (ignored):" "Zeolite execution failed:"
