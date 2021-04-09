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

import Base.CompilerError
import Base.TrackedErrors
import Cli.CompileOptions
import Cli.RunCompiler
import Config.LoadConfig
import Config.LocalConfig


main :: IO ()
main = do
  args <- getArgs
  when (not $ null args) $ hPutStrLn stderr $ "Ignoring extra arguments: " ++ show args
  f <- localConfigPath
  isFile <- doesFileExist f
  when isFile $ do
    hPutStrLn stderr $ "*** WARNING: Local config " ++ f ++ " will be overwritten. ***"
  config <- createConfig
  hPutStrLn stderr $ "Writing local config to " ++ f ++ "."
  writeFile f (show config ++ "\n")
  initLibraries config
  hPutStrLn stderr "Setup is now complete!"

clangBinary :: String
clangBinary = "clang++"

gccBinary :: String
gccBinary   = "g++"

arBinary :: String
arBinary    = "ar"

libraries :: [String]
libraries = [
    "base",
    "lib/testing",
    "lib/util",
    "lib/container",
    "lib/file",
    "lib/math",
    "lib/thread",
    "tests"
  ]

optionalLibraries :: [String]
optionalLibraries = [
  ]

includePaths :: [String]
includePaths = ["lib"]

cxxFlags :: [String]
cxxFlags = ["-O2", "-std=c++11"]

createConfig :: IO LocalConfig
createConfig = do
  clang <- findExecutables clangBinary
  gcc   <- findExecutables gccBinary
  ar    <- findExecutables arBinary
  compiler <- promptChoice "Which clang-compatible C++ compiler should be used?" (clang ++ gcc)
  archiver <- promptChoice "Which ar-compatible archiver should be used?" ar
  -- Cannot be overridden at this point.
  let options = cxxFlags
  let config = LocalConfig {
      lcBackend = UnixBackend {
        ucCxxBinary = compiler,
        ucCxxOptions = options,
        ucArBinary = archiver
      },
      lcResolver = SimpleResolver {
        srVisibleSystem = includePaths,
        srExtraPaths = []
      }
    }
  return config

promptChoice :: String -> [String] -> IO String
promptChoice p cs = do
  n <- getChoice
  if n <= length cs
     then return $ cs !! (n-1)
     else getResponse
  where
    getChoice = do
      hPutStrLn stderr p
      let cs' = zipWith (\n c -> show n ++ ") " ++ c) ([1..] :: [Int]) $ cs ++ ["other"]
      let cs'' = (head cs' ++ " [default]"):(tail cs')
      mapM_ (hPutStrLn stderr) cs''
      hPutStr stderr "? "
      n <- getInput
      if null n
         then return 1
         else case check (reads n :: [(Int,String)]) of
                   Just n' | n' > 0 && n' <= length cs' -> return n'
                   _ -> getChoice
    getResponse = do
      hPutStr stderr "Enter the full path: "
      getInput
    check [(cm,"")] = Just cm
    check _         = Nothing

getInput :: IO String
getInput = do
  closed <- hIsEOF stdin
  when closed $ do
    hPutStrLn stderr "Canceled."
    exitFailure
  hGetLine stdin

initLibraries :: LocalConfig -> IO ()
initLibraries (LocalConfig backend resolver) = do
  path <- rootPath >>= canonicalizePath
  let options = CompileOptions {
      coHelp = HelpNotNeeded,
      coPublicDeps = [],
      coPrivateDeps = [],
      coPaths = libraries,
      coExtraFiles = [],
      coExtraPaths = [],
      coSourcePrefix = path,
      coMode = CompileRecompileRecursive,
      coForce = ForceAll
    }
  tryTrackedErrorsIO "Warnings:" "Zeolite setup failed:" $ do
    runCompiler resolver backend options
    mapM_ optionalWarning optionalLibraries where
    optionalWarning library = compilerWarningM $ "Optional library " ++ library ++ " must be built manually if needed"
