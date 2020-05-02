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

module Config.LoadConfig (
  Backend(..),
  LocalConfig(..),
  Resolver(..),
  compilerVersion,
  localConfigPath,
  loadConfig,
  rootPath,
) where

import Config.Paths
import Config.Programs

import Control.Monad (when)
import Data.Hashable (hash)
import Data.List (intercalate,isSuffixOf)
import Data.Version (showVersion)
import GHC.IO.Handle
import Numeric (showHex)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process (ProcessStatus(..),executeFile,forkProcess,getProcessStatus)
import System.Posix.Temp (mkstemps)

import Paths_zeolite_lang (getDataFileName,version)


loadConfig :: IO (Backend,Resolver)
loadConfig = do
  f <- localConfigPath
  isFile <- doesFileExist f
  when (not isFile) $ do
    hPutStrLn stderr "Zeolite has not been configured. Please run zeolite-setup."
    exitFailure
  c <- readFile f
  lc <- check $ (reads c :: [(LocalConfig,String)])
  return (lcBackend lc,lcResolver lc) where
    check [(cm,"")] = return cm
    check [(cm,"\n")] = return cm
    check _ = do
      hPutStrLn stderr "Zeolite configuration is corrupt. Please rerun zeolite-setup."
      exitFailure

rootPath :: IO FilePath
rootPath = getDataFileName ""

compilerVersion :: String
compilerVersion = showVersion version

data Backend =
  UnixBackend {
    ucCxxBinary :: String,
    ucCxxOptions :: [String],
    ucArBinary :: String
  }
  deriving (Read,Show)

data Resolver = SimpleResolver deriving (Read,Show)

data LocalConfig =
  LocalConfig {
    lcBackend :: Backend,
    lcResolver :: Resolver
  }
  deriving (Read,Show)

localConfigFilename :: String
localConfigFilename = "local-config.txt"

localConfigPath :: IO FilePath
localConfigPath = getDataFileName localConfigFilename >>= canonicalizePath

instance CompilerBackend Backend where
  runCxxCommand (UnixBackend cb co ab) (CompileToObject s p nm ns ps e) = do
    objName <- canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".o")
    executeProcess cb $ co ++ otherOptions ++ ["-c", s, "-o", objName]
    if e
      then do
        -- Extra files are put into .a since they will be unconditionally
        -- included. This prevents unwanted symbol dependencies.
        arName  <- canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".a")
        executeProcess ab ["-q",arName,objName]
        return arName
      else return objName where
      otherOptions = map (("-I" ++) . normalise) ps ++ nsFlag
      nsFlag
        | null ns = []
        | otherwise = ["-D" ++ nm ++ "=" ++ ns]
  runCxxCommand (UnixBackend cb co _) (CompileToBinary m ss o ps) = do
    let arFiles    = filter (isSuffixOf ".a")       ss
    let otherFiles = filter (not . isSuffixOf ".a") ss
    executeProcess cb $ co ++ otherOptions ++ m:otherFiles ++ arFiles ++ ["-o", o]
    return o where
      otherOptions = map ("-I" ++) $ map normalise ps
  runTestCommand _ (TestCommand b p) = do
    (outF,outH) <- mkstemps "/tmp/ztest_" ".txt"
    (errF,errH) <- mkstemps "/tmp/ztest_" ".txt"
    pid <- forkProcess (execWithCapture outH errH)
    hClose outH
    hClose errH
    status <- getProcessStatus True True pid
    out <- readFile outF
    err <- readFile errF
    let success = case status of
                      Just (Exited ExitSuccess) -> True
                      _ -> False
    return $ TestCommandResult success (lines out) (lines err) where
      execWithCapture h1 h2 = do
        when (not $ null p) $ setCurrentDirectory p
        hDuplicateTo h1 stdout
        hDuplicateTo h2 stderr
        executeFile b True [] Nothing
  getCompilerHash b = flip showHex "" $ abs $ hash $ compilerVersion ++ show b

executeProcess :: String -> [String] -> IO ()
executeProcess c os = do
  hPutStrLn stderr $ "Executing: " ++ intercalate " " (c:os)
  pid <- forkProcess $ executeFile c True os Nothing
  status <- getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _ -> exitFailure

instance PathResolver Resolver where
  resolveModule SimpleResolver p m = do
    m' <- getDataFileName m
    firstExisting m [p</>m,m']
  resolveBaseModule _ = do
    let m = "base"
    m' <- getDataFileName m
    firstExisting m [m']
  resolveBinary SimpleResolver = canonicalizePath
  isBaseModule r@SimpleResolver f = do
    b <- resolveBaseModule r
    return (f == b)

firstExisting :: FilePath -> [FilePath] -> IO FilePath
firstExisting n [] = do
  -- TODO: Allow error recovery here.
  hPutStrLn stderr $ "Could not find path " ++ n
  exitFailure
firstExisting n (p:ps) = do
  isDir <- doesDirectoryExist p
  if isDir
     then canonicalizePath p
     else firstExisting n ps
