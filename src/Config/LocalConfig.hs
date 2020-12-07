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

module Config.LocalConfig (
  Backend(..),
  LocalConfig(..),
  Resolver(..),
  rootPath,
  compilerVersion,
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Hashable (hash)
import Data.List (intercalate,isPrefixOf,isSuffixOf)
import Data.Maybe (isJust)
import Data.Version (showVersion,versionBranch)
import GHC.IO.Handle
import Numeric (showHex)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process (ProcessStatus(..),executeFile,forkProcess,getProcessStatus)
import System.Posix.Temp (mkstemps)

import Base.CompilerError
import Cli.Programs
import Module.Paths

import Paths_zeolite_lang (getDataFileName,version)


data Backend =
  UnixBackend {
    ucCxxBinary :: FilePath,
    ucCxxOptions :: [String],
    ucArBinary :: FilePath
  }
  deriving (Read,Show)

data Resolver =
  SimpleResolver {
    srVisibleSystem :: [FilePath],
    srExtraPaths :: [FilePath]
  }
  deriving (Read,Show)

data LocalConfig =
  LocalConfig {
    lcBackend :: Backend,
    lcResolver :: Resolver
  }
  deriving (Read,Show)

rootPath :: IO FilePath
rootPath = getDataFileName ""

compilerVersion :: String
compilerVersion = showVersion version

instance CompilerBackend Backend where
  runCxxCommand (UnixBackend cb co ab) (CompileToObject s p ms ps e) = do
    objName <- errorFromIO $ canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".o")
    executeProcess cb (co ++ otherOptions ++ ["-c", s, "-o", objName]) <?? "In compilation of " ++ s
    if e
      then do
        -- Extra files are put into .a since they will be unconditionally
        -- included. This prevents unwanted symbol dependencies.
        arName  <- errorFromIO $ canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".a")
        executeProcess ab ["-q",arName,objName] <?? "In packaging of " ++ objName
        return arName
      else return objName where
      otherOptions = map (("-I" ++) . normalise) ps ++ map macro ms
      macro (n,Just v)  = "-D" ++ n ++ "=" ++ v
      macro (n,Nothing) = "-D" ++ n
  runCxxCommand (UnixBackend cb co _) (CompileToBinary m ss o ps lf) = do
    let arFiles    = filter (isSuffixOf ".a")       ss
    let otherFiles = filter (not . isSuffixOf ".a") ss
    executeProcess cb (co ++ otherOptions ++ m:otherFiles ++ arFiles ++ ["-o", o]) <?? "In linking of " ++ o
    return o where
      otherOptions = lf ++ map ("-I" ++) (map normalise ps)
  runTestCommand _ (TestCommand b p as) = errorFromIO $ do
    (outF,outH) <- mkstemps "/tmp/ztest_" ".txt"
    (errF,errH) <- mkstemps "/tmp/ztest_" ".txt"
    pid <- forkProcess (execWithCapture outH errH)
    hClose outH
    hClose errH
    status <- getProcessStatus True True pid
    out <- readFile outF
    removeFile outF
    err <- readFile errF
    removeFile errF
    let success = case status of
                       Just (Exited ExitSuccess) -> True
                       _ -> False
    return $ TestCommandResult success (lines out) (lines err) where
      execWithCapture h1 h2 = do
        when (not $ null p) $ setCurrentDirectory p
        hDuplicateTo h1 stdout
        hDuplicateTo h2 stderr
        executeFile b True as Nothing
  getCompilerHash b = VersionHash $ flip showHex "" $ abs $ hash $ minorVersion ++ show b where
    minorVersion = show $ take 3 $ versionBranch version

executeProcess :: (MonadIO m, ErrorContextM m) => String -> [String] -> m ()
executeProcess c os = do
  errorFromIO $ hPutStrLn stderr $ "Executing: " ++ intercalate " " (c:os)
  pid    <- errorFromIO $ forkProcess $ executeFile c True os Nothing
  status <- errorFromIO $ getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _ -> do
         errorFromIO $ hPutStrLn stderr $ "Execution of " ++ c ++ " failed"
         compilerErrorM $ "Execution of " ++ c ++ " failed"

instance PathIOHandler Resolver where
  resolveModule r p m = do
    ps2 <- errorFromIO $ potentialSystemPaths r m
    firstExisting m $ [p</>m] ++ ps2
  isSystemModule r p m = do
    isDir <- errorFromIO $ doesDirectoryExist (p</>m)
    if isDir
       then return False
       else do
         ps2 <- errorFromIO $ potentialSystemPaths r m
         path <- errorFromIO (findModule ps2)
         return $ isJust path
  resolveBaseModule _ = do
    let m = "base"
    m0 <- errorFromIO $ getDataFileName m
    firstExisting m [m0]
  isBaseModule r f = do
    b <- resolveBaseModule r
    return (f == b)
  zipWithContents _ p fs = fmap (zip $ map fixPath fs) $ mapM (errorFromIO . readFile . (p </>)) fs

potentialSystemPaths :: Resolver -> FilePath -> IO [FilePath]
potentialSystemPaths (SimpleResolver ls ps) m = do
  let allowGlobal = not (".." `elem` components)
  m0 <- if allowGlobal && any (\l -> isPrefixOf (l ++ "/") m) ls
           then getDataFileName m >>= return . (:[])
           else return []
  let m2 = if allowGlobal
              then map (</> m) ps
              else []
  return $ m0 ++ m2 where
    components = map stripSlash $ splitPath m
    stripSlash = reverse . dropWhile (== '/') . reverse

firstExisting :: (MonadIO m, ErrorContextM m) => FilePath -> [FilePath] -> m FilePath
firstExisting m ps = do
  p <- errorFromIO $ findModule ps
  case p of
       Nothing -> compilerErrorM $ "Could not find path " ++ m
       Just p2 -> return p2

findModule :: [FilePath] -> IO (Maybe FilePath)
findModule [] = return Nothing
findModule (p:ps) = do
  isDir <- doesDirectoryExist p
  if isDir
     then fmap Just $ canonicalizePath p
     else findModule ps
