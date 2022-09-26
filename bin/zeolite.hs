{- -----------------------------------------------------------------------------
Copyright 2020-2022 Kevin P. Barry

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
import Control.Monad.Trans
import System.FilePath
import System.Directory
import System.Environment
import System.Exit
import GHC.IO.Handle
import System.IO
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set

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
          asCompilerWarnings co
          let co' = getCompilerSuccess co
          (resolver,backend) <- loadConfig
          when (HelpNotNeeded /= (_coHelp co')) $ errorFromIO $ showHelp >> exitFailure
          tryCloseStdin
          runCompiler resolver backend co'

tryCloseStdin :: TrackedErrorsIO ()
tryCloseStdin = do
  let result = errorFromIO $ withFile "/dev/null" ReadMode (flip hDuplicateTo stdin)
  -- NOTE: Processing result more than once (e.g., error check followed by
  -- conversion to warnings) could cause the operation to be executed again.
  "In zeolite's attempt to block compiler and test input on stdin" ??>
    (lift (toTrackedErrors result) >>= asCompilerWarnings)

showHelp :: IO ()
showHelp = do
  hPutStrLn stderr $ "Zeolite " ++ compilerVersion ++ " Help:"
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
tryFastModes os0 = maybePath os0 where
  maybePath ("-p":root:os) = tryMode root os
  maybePath os             = tryMode "" os
  tryMode root ("--clean":ps) = do
    tryZeoliteIO $ do
      mapCompilerM_ loadModule ps
      mapCompilerM_ clean ps
    exitSuccess where
      loadModule p = do
        p' <- errorFromIO $ canonicalizePath (root </> p)
        -- Not needed for clean, but forces the caller to only pass real modules.
        loadRecompile p'
      clean p = do
        p' <- errorFromIO $ canonicalizePath (root </> p)
        errorFromIO $ hPutStrLn stderr $ "Clearing cached data for module \"" ++ p' ++ "\"."
        eraseCachedData p'
  tryMode root ("--show-traces":ps) = do
    tryZeoliteIO $ do
      (_,backend) <- loadConfig
      h <- getCompilerHash backend
      mapCompilerM_ (showTraces h) ps
    exitSuccess where
      showTraces h p = do
        p' <- errorFromIO $ canonicalizePath (root </> p)
        -- Not needed to show traces, but causes the error to be about the
        -- module rather than the filename if the module hasn't been compiled.
        _ <- loadModuleMetadata h ForceAll Map.empty p'
        ts <- readPossibleTraces p'
        mapM_ (errorFromIO . hPutStrLn stdout) $ Set.toList ts
  tryMode root ("--missed-lines":l:ps) = do
    tryZeoliteIO $ do
      (_,backend) <- loadConfig
      h <- getCompilerHash backend
      expected <- fmap Set.unions $ mapCompilerM (getTraces h) ps
      actual <- loadTraces
      let difference = expected `Set.difference` actual
      mapM_ (errorFromIO . hPutStrLn stdout) $ Set.toList $ difference
      errorFromIO $ hPutStrLn stdout $ formatCoverage difference expected
    exitSuccess where
      formatCoverage difference expected =
        "Coverage: " ++ show actualSize ++ " of " ++ show expectedSize ++
        " lines (" ++ printf "%.2f" coverage ++ "%)" where
          diffSize = length $ Set.toList $ difference
          expectedSize = length $ Set.toList $ expected
          actualSize = expectedSize - diffSize
          coverage = 100.0 * (fromIntegral actualSize :: Double) / (fromIntegral expectedSize :: Double)
      loadTraces = do
        l' <- errorFromIO $ canonicalizePath (root </> l)
        errorFromIO $ hPutStrLn stderr $ "Loading trace data from \"" ++ l' ++ "\"."
        lc <- errorFromIO $ readFile l'
        fmap (Set.fromList . map teContext) $ parseTracesFile (l,lc)
      getTraces h p = do
        p' <- errorFromIO $ canonicalizePath (root </> p)
        -- Not needed to check traces, but causes the error to be about the
        -- module rather than the filename if the module hasn't been compiled.
        _ <- loadModuleMetadata h ForceAll Map.empty p'
        readPossibleTraces p'
  tryMode _ ("--missed-lines":_) = do
    hPutStrLn stderr $ "Pass a .csv generated by -t --log-traces."
    exitFailure
  tryMode root ("--show-deps":ps) = do
    tryZeoliteIO $ do
      (_,backend) <- loadConfig
      h <- getCompilerHash backend
      mapCompilerM_ (showDeps h) ps
    exitSuccess where
      showDeps h p = do
        p' <- errorFromIO $ canonicalizePath (root </> p)
        m <- loadModuleMetadata h ForceAll Map.empty p'
        errorFromIO $ hPutStrLn stdout $ show p'
        errorFromIO $ mapM_ showDep (cmObjectFiles m)
      showDep (CategoryObjectFile c ds _) = do
        mapM_ (\d -> hPutStrLn stdout $ "  " ++ show (ciCategory c) ++
                                        " -> " ++ show (ciCategory d) ++
                                        " " ++ show (ciPath d)) ds
      showDep _ = return ()
  tryMode _ _ = return ()

tryZeoliteIO :: TrackedErrorsIO a -> IO a
tryZeoliteIO = tryTrackedErrorsIO "Warnings (ignored):" "Zeolite execution failed:"
