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
import Data.List (intercalate,isPrefixOf,isSuffixOf,nub)
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
import Config.CompilerConfig
import Config.ParseConfig ()
import Module.ParseMetadata
import Module.Paths

import Paths_zeolite_lang (getDataFileName,version)


rootPath :: IO FilePath
rootPath = getDataFileName ""

compilerVersion :: String
compilerVersion = showVersion version

instance CompilerBackend Backend where
  runCxxCommand = run where
    run (UnixBackend cb ff _ _ ab) (CompileToObject s p ms ps e) = do
      objName <- errorFromIO $ canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".o")
      let otherOptions = map (("-I" ++) . normalise) ps ++ map macro ms
      executeProcess cb (ff ++ otherOptions ++ ["-c", s, "-o", objName]) <?? "In compilation of " ++ s
      if e
         then do
           -- Extra files are put into .a since they will be unconditionally
           -- included. This prevents unwanted symbol dependencies.
           arName  <- errorFromIO $ canonicalizePath $ p </> (takeFileName $ dropExtension s ++ ".a")
           executeProcess ab ["-q",arName,objName] <?? "In packaging of " ++ objName
           return arName
         else return objName
    run (UnixBackend cb _ ff _ _) (CompileToShared ss o lf) = do
      let arFiles      = filter (isSuffixOf ".a")       ss
      let otherFiles   = filter (not . isSuffixOf ".a") ss
      let flags = nub lf
      let args = ff ++ otherFiles ++ arFiles ++ ["-o", o] ++ flags
      executeProcess cb args <?? "In linking of " ++ o
      return o
    run (UnixBackend cb _ _ ff _) (CompileToBinary m ss ms o ps lf) = do
      let arFiles      = filter (isSuffixOf ".a")       ss
      let otherFiles   = filter (not . isSuffixOf ".a") ss
      let otherOptions = map (("-I" ++) . normalise) ps ++ map macro ms
      let flags = nub lf
      let args = ff ++ otherOptions ++ m:otherFiles ++ arFiles ++ ["-o", o] ++ flags
      executeProcess cb args <?? "In linking of " ++ o
      return o
    macro (n,Just v)  = "-D" ++ n ++ "=" ++ v
    macro (n,Nothing) = "-D" ++ n
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
