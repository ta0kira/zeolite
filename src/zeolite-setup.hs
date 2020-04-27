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
import System.Exit
import System.FilePath
import System.IO

import Config.LoadConfig
import Config.Paths
import Config.Programs


main = do
  f <- localConfigPath
  isFile <- doesFileExist f
  when isFile $ do
    hPutStrLn stderr $ "*** WARNING: Local config " ++ f ++ " will be overwritten. ***"
  config <- createConfig
  f <- localConfigPath
  hPutStrLn stderr $ "Writing local config to " ++ f ++ "."
  writeFile f (show config ++ "\n")

clangBinary = "clang++"
gccBinary   = "g++"
arBinary    = "ar"

createConfig :: IO LocalConfig
createConfig = do
  clang <- findExecutables clangBinary
  gcc   <- findExecutables gccBinary
  ar    <- findExecutables arBinary
  compiler <- promptChoice "Which clang-compatible C++ compiler should be used?" (clang ++ gcc)
  archiver <- promptChoice "Which ar-compatible archiver should be used?" ar
  -- Cannot be overridden at this point.
  let options = ["-O2", "-std=c++11"]
  let config = LocalConfig {
      lcBackend = UnixBackend {
        ucCxxBinary = compiler,
        ucCxxOptions = options,
        ucArBinary = archiver
      },
      lcResolver = SimpleResolver
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
      let cs' = zipWith (\n c -> show n ++ ") " ++ c) [1..] $ cs ++ ["other"]
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
      hPutStrLn stderr "Enter the full path: "
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
