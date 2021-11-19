{- -----------------------------------------------------------------------------
Copyright 2020-2021 Kevin P. Barry

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
import Config.ParseConfig ()


main :: IO ()
main = tryTrackedErrorsIO "" "Zeolite setup failed:" (lift getArgs >>= handle) where
  handle ("--reuse":_) = do
    config <- loadConfig
    runWith config
  handle args = do
    let (cxxSpec:arSpec:_) = (map Just $ args) ++ repeat Nothing
    f <- lift $ localConfigPath
    isFile <- lift $ doesFileExist f
    when isFile $
      lift $ hPutStrLn stderr $ "*** WARNING: Local config " ++ f ++ " will be overwritten. ***"
    config <- lift $ createConfig cxxSpec arSpec
    saveConfig config
    runWith config
  runWith config = do
    initLibraries config
    lift $ hPutStrLn stderr "Setup is now complete!"

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

compileFlags :: [String]
compileFlags = ["-O2", "-std=c++11", "-fPIC"]

libraryFlags :: [String]
libraryFlags = ["-shared", "-fpic"]

binaryFlags :: [String]
binaryFlags = ["-O2", "-std=c++11"]

intOrString :: String -> Either Int String
intOrString s = handle (reads s :: [(Int, String)]) where
  handle [(n,"")] = Left n
  handle _        = Right s

createConfig :: Maybe String -> Maybe String -> IO (Resolver,Backend)
createConfig cxxSpec arSpec = do
  clang <- findExecutables clangBinary
  gcc   <- findExecutables gccBinary
  ar    <- findExecutables arBinary
  compiler <- promptChoice "Which clang-compatible C++ compiler should be used?" cxxSpec (clang ++ gcc)
  archiver <- promptChoice "Which ar-compatible archiver should be used?"        arSpec  ar
  return (
    SimpleResolver {
      srVisibleSystem = includePaths,
      srExtraPaths = []
    },
    UnixBackend {
      ucCxxBinary    = compiler,
      ucCompileFlags = compileFlags,
      ucLibraryFlags = libraryFlags,
      ucBinaryFlags  = binaryFlags,
      ucArBinary     = archiver
    })

promptChoice :: String -> Maybe String -> [String] -> IO String
promptChoice _ (Just spec) cs = handle $ intOrString spec where
  handle (Right s) = return s
  handle (Left n)
    | n < 1 || n > length cs = do
      hPutStrLn stderr $ "Index " ++ show n ++ " is out of bounds for " ++ show cs
      exitFailure
    | otherwise = return $ cs !! (n-1)
promptChoice p _ cs = do
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

initLibraries :: (Resolver,Backend) -> TrackedErrorsIO ()
initLibraries (resolver,backend) = do
  path <- lift $ rootPath >>= canonicalizePath
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
  runCompiler resolver backend options
  mapM_ optionalWarning optionalLibraries where
  optionalWarning library = compilerWarningM $ "Optional library " ++ library ++ " must be built manually if needed"
