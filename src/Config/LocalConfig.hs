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

{-# LANGUAGE TypeFamilies #-}

module Config.LocalConfig (
  Backend(..),
  LocalConfig(..),
  PendingProcess,
  Resolver(..),
  rootPath,
  compilerVersion,
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Hashable (hash)
import Data.List (intercalate,isPrefixOf,isSuffixOf,nub)
import Data.Maybe (isJust)
import Data.Version (showVersion,versionBranch)
import GHC.IO.Handle
import Numeric (showHex)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Temp (mkstemps)
import System.Posix.Types (ProcessID)

import Base.CompilerError
import Cli.Programs
import Config.CompilerConfig
import Config.ParseConfig ()
import Module.ParseMetadata
import Module.Paths

import Paths_zeolite_lang (getDataFileName,version)


rootPath :: IO FilePath
rootPath = getDataFileName ""

compilerVersion :: String
compilerVersion = showVersion version

data PendingProcess =
  PendingProcess {
    pcContext :: String,
    pcProcess :: ProcessID,
    pcNext :: Either (IO PendingProcess) (FilePath,CxxCommand)
  }

instance CompilerBackend Backend where
  type AsyncWait Backend = PendingProcess
  syncCxxCommand b compile = asyncCxxCommand b compile >>= waitAll where
    waitAll (PendingProcess context pid next) = do
      blockProcess pid <?? context
      case next of
           Left process -> errorFromIO process >>= waitAll
           Right (path,_) -> return path
  asyncCxxCommand = run where
    run (UnixBackend cb ff _ _ ab) ca@(CompileToObject s p ms ps e) = do
      objName <- errorFromIO $ canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".o")
      arName  <- errorFromIO $ canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".a")
      let otherOptions = map (("-I" ++) . normalise) ps ++ map macro ms
      let next = if e
                    then Right (objName,ca)
                    else Left $ do
                      pid <- executeProcess ab ["-q",arName,objName]
                      return $ PendingProcess {
                          pcContext = "Archiving of " ++ objName,
                          pcProcess = pid,
                          pcNext = Right (arName,ca)
                        }
      pid <- errorFromIO $ executeProcess cb (ff ++ otherOptions ++ ["-c", s, "-o", objName])
      return $ PendingProcess {
          pcContext = "Compilation of " ++ s,
          pcProcess = pid,
          pcNext = next
        }
    run (UnixBackend cb _ ff _ _) ca@(CompileToShared ss o lf) = do
      let arFiles      = filter (isSuffixOf ".a")       ss
      let otherFiles   = filter (not . isSuffixOf ".a") ss
      let flags = nub lf
      let args = ff ++ otherFiles ++ arFiles ++ ["-o", o] ++ flags
      pid <- errorFromIO $ executeProcess cb args
      return $ PendingProcess {
          pcContext = "In linking of " ++ o,
          pcProcess = pid,
          pcNext = Right (o,ca)
        }
    run (UnixBackend cb _ _ ff _) ca@(CompileToBinary m ss ms o ps lf) = do
      let arFiles      = filter (isSuffixOf ".a")       ss
      let otherFiles   = filter (not . isSuffixOf ".a") ss
      let otherOptions = map (("-I" ++) . normalise) ps ++ map macro ms
      let flags = nub lf
      let args = ff ++ otherOptions ++ m:otherFiles ++ arFiles ++ ["-o", o] ++ flags
      pid <- errorFromIO $ executeProcess cb args
      return $ PendingProcess {
          pcContext = "In linking of " ++ o,
          pcProcess = pid,
          pcNext = Right (o,ca)
        }
    macro (n,Just v)  = "-D" ++ n ++ "=" ++ v
    macro (n,Nothing) = "-D" ++ n
  waitCxxCommand _ p@(PendingProcess context pid next) = do
    status <- waitProcess pid <?? context
    if status
       then case next of
                 Left process -> fmap Left $ errorFromIO process
                 Right result -> return $ Right result  -- Not the same Either.
       else return $ Left p
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
  getCompilerHash b = do
    let minorVersion = show $ take 3 $ versionBranch version
    serialized <- autoWriteConfig b
    return $ VersionHash $ flip showHex "" $ abs $ hash $ minorVersion ++ serialized

executeProcess :: String -> [String] -> IO ProcessID
executeProcess c os = do
  hPutStrLn stderr $ "Executing: " ++ intercalate " " (c:os)
  forkProcess $ executeFile c True os Nothing

waitProcess :: (MonadIO m, ErrorContextM m) => ProcessID -> m Bool
waitProcess pid = do
  status <- errorFromIO $ getProcessStatus False True pid
  case status of
       Nothing -> return False
       Just (Exited ExitSuccess) -> return True
       _ -> do
         errorFromIO $ hPutStrLn stderr $ "Command execution failed"
         compilerErrorM $ "Command execution failed"

blockProcess :: (MonadIO m, ErrorContextM m) => ProcessID -> m ()
blockProcess pid = do
  status <- errorFromIO $ getProcessStatus True True pid
  case status of
       Just (Exited ExitSuccess) -> return ()
       _ -> do
         errorFromIO $ hPutStrLn stderr $ "Command execution failed"
         compilerErrorM $ "Command execution failed"

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
