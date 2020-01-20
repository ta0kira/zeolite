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

module Cli.CompileMetadata (
  CompileMetadata(..),
  RecompileMetadata(..),
  allowedExtraTypes,
  createCachePath,
  eraseCachedData,
  findSourceFiles,
  fixPath,
  getCachedPath,
  getCacheRelativePath,
  getIncludePathsForDeps,
  getObjectFilesForDeps,
  getRealPathsForDeps,
  getSourceFilesForDeps,
  isNotCompiled,
  isPathCompiled,
  loadRecursiveDeps,
  loadMetadata,
  sortCompiledFiles,
  writeCachedFile,
  writeMetadata,
) where

import Control.Monad (when)
import Data.List (isSuffixOf)
import System.Directory
import System.Environment
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import qualified Data.Set as Set

import Cli.CompileOptions (CompileMode)


data CompileMetadata =
  CompileMetadata {
    cmPath :: String,
    cmRecompile :: RecompileMetadata,
    cmDepPaths :: [String],
    cmCategories :: [String],
    cmSubdirs :: [String],
    cmPublicFiles :: [String],
    cmPrivateFiles :: [String],
    cmTestFiles :: [String],
    cmHxxFiles :: [String],
    cmCxxFiles :: [String],
    cmObjectFiles :: [String]
  }
  deriving (Show,Read)

data RecompileMetadata =
  RecompileMetadata {
    rmRoot :: String,
    rmPath :: String,
    rmDepPaths :: [String],
    rmExtraFiles :: [String],
    rmExtraPaths :: [String],
    rmMode :: CompileMode,
    rmOutputName :: String
  } | NotCompiled
  deriving (Show,Read)

cachedDataPath = ".zeolite-cache"
metadataFilename = "metadata.txt"
allowedExtraTypes = [".hpp",".cpp",".h",".cc",".a",".o"]

isNotCompiled :: RecompileMetadata -> Bool
isNotCompiled NotCompiled = True
isNotCompiled _           = False

resetMetadata :: CompileMetadata -> CompileMetadata
resetMetadata (CompileMetadata p rm _ _ _ _ _ _ _ _ _) =
  CompileMetadata {
    cmPath = p,
    cmRecompile = rm,
    cmDepPaths = [],
    cmCategories = [],
    cmSubdirs = [],
    cmPublicFiles = [],
    cmPrivateFiles = [],
    cmTestFiles = [],
    cmHxxFiles = [],
    cmCxxFiles = [],
    cmObjectFiles = []
  }

emptyMetadata :: String -> CompileMetadata
emptyMetadata p =
  CompileMetadata {
    cmPath = p,
    cmRecompile = NotCompiled,
    cmDepPaths = [],
    cmCategories = [],
    cmSubdirs = [],
    cmPublicFiles = [],
    cmPrivateFiles = [],
    cmTestFiles = [],
    cmHxxFiles = [],
    cmCxxFiles = [],
    cmObjectFiles = []
  }

loadMetadata :: String -> IO CompileMetadata
loadMetadata p = do
  let f = p </> cachedDataPath </> metadataFilename
  isFile <- doesFileExist p
  when isFile $ do
    hPutStrLn stderr $ "Path \"" ++ p ++ "\" is not a directory."
    exitFailure
  isDir <- doesDirectoryExist p
  when (not isDir) $ do
    hPutStrLn stderr $ "Path \"" ++ p ++ "\" does not exist."
    exitFailure
  filePresent <- doesFileExist f
  when (not filePresent) $ do
    hPutStrLn stderr $ "Module \"" ++ p ++ "\" has not been compiled yet."
    exitFailure
  c <- readFile f
  m <- check $ (reads c :: [(CompileMetadata,String)])
  when (isNotCompiled $ cmRecompile m) $ do
    hPutStrLn stderr $ "Module \"" ++ p ++ "\" has not been compiled yet."
    exitFailure
  return m where
    check [(cm,"")] = return cm
    check [(cm,"\n")] = return cm
    check _ = do
      hPutStrLn stderr $ "Could not parse metadata from \"" ++ p ++ "\"; please recompile."
      exitFailure

tryLoadMetadata :: String -> IO CompileMetadata
tryLoadMetadata p = do
  let f = p </> cachedDataPath </> metadataFilename
  def <- canonicalizePath p >>= return . emptyMetadata
  isDir <- doesDirectoryExist p
  if not isDir
     then return def
     else do
       filePresent <- doesFileExist f
       if not filePresent
          then return def
          else do
            c <- readFile f
            check def $ (reads c :: [(CompileMetadata,String)]) where
              check _ [(cm,"")]   = return cm
              check _ [(cm,"\n")] = return cm
              check d _           = return d

isPathCompiled :: String -> IO Bool
isPathCompiled p = do
  m <- tryLoadMetadata p
  return $ not $ isNotCompiled $ cmRecompile m

writeMetadata :: String -> CompileMetadata -> IO ()
writeMetadata p m = do
  p' <- canonicalizePath p
  hPutStrLn stderr $ "Writing metadata for \"" ++ p' ++ "\"."
  writeCachedFile p' "" metadataFilename (show m ++ "\n")
  fresh <- checkModuleFreshness p' m
  when (not fresh) $ do
    hPutStrLn stderr $ "Error writing metadata for \"" ++ p' ++ "\"."
    exitFailure

eraseCachedData :: String -> IO ()
eraseCachedData p = do
  let d  = p </> cachedDataPath
  let md = getCachedPath p "" metadataFilename
  dirExists <- doesDirectoryExist d
  fileExists <- doesFileExist md
  -- Preserve the recompilation parts, in case there is an error later on.
  m <- tryLoadMetadata p
  when dirExists $ removeDirectoryRecursive d
  writeCachedFile p "" metadataFilename (show m ++ "\n")

createCachePath :: String -> IO ()
createCachePath p = do
  let f = p </> cachedDataPath
  exists <- doesDirectoryExist f
  when (not exists) $ createDirectoryIfMissing False f

writeCachedFile :: String -> String -> String -> String -> IO ()
writeCachedFile p ns f c = do
  createCachePath p
  createDirectoryIfMissing False $ p </> cachedDataPath </> ns
  writeFile (getCachedPath p ns f) c

getCachedPath :: String -> String -> String -> String
getCachedPath p ns f = fixPath $ p </> cachedDataPath </> ns </> f

getCacheRelativePath :: String -> String
getCacheRelativePath f = ".." </> f

findSourceFiles :: String -> String -> IO ([String],[String],[String])
findSourceFiles p0 p = do
  let absolute = p0 </> p
  isFile <- doesFileExist absolute
  when isFile $ do
    hPutStrLn stderr $ "Path \"" ++ absolute ++ "\" is not a directory."
    exitFailure
  isDir <- doesDirectoryExist absolute
  when (not isDir) $ do
    hPutStrLn stderr $ "Path \"" ++ absolute ++ "\" does not exist."
    exitFailure
  ds <- getDirectoryContents absolute >>= return . map (p </>)
  let ps = filter (isSuffixOf ".0rp") ds
  let xs = filter (isSuffixOf ".0rx") ds
  let ts = filter (isSuffixOf ".0rt") ds
  return (ps,xs,ts)

getRealPathsForDeps :: [CompileMetadata] -> [String]
getRealPathsForDeps = map cmPath

getSourceFilesForDeps :: [CompileMetadata] -> [String]
getSourceFilesForDeps = concat . map extract where
  extract m = map (cmPath m </>) (cmPublicFiles m)

getIncludePathsForDeps :: [CompileMetadata] -> [String]
getIncludePathsForDeps = concat . map extract where
  extract m = (cmPath m </> cachedDataPath):(map ((cmPath m </> cachedDataPath) </>) $ cmSubdirs m)

getObjectFilesForDeps :: [CompileMetadata] -> [String]
getObjectFilesForDeps = concat . map extract where
  extract m = map ((cmPath m </> cachedDataPath) </>) $ cmObjectFiles m

loadRecursiveDeps :: [String] -> IO (Bool,[CompileMetadata])
loadRecursiveDeps ps = fmap snd $ fixedPaths >>= run (Set.empty,(True,[])) where
  fixedPaths = sequence $ map canonicalizePath ps
  run xa@(pa,(fr,xs)) (p:ps)
    | p `Set.member` pa = run xa ps
    | otherwise = do
        hPutStrLn stderr $ "Loading metadata for dependency \"" ++ p ++ "\"."
        m <- loadMetadata p
        fresh <- checkModuleFreshness p m
        when (not fresh) $
          hPutStrLn stderr $ "Module \"" ++ p ++ "\" is out of date and should be recompiled."
        run (p `Set.insert` pa,(fresh && fr,xs ++ [m])) (ps ++ cmDepPaths m)
  run xa _ = return xa

fixPath :: String -> String
fixPath = foldl (</>) "" . process [] . map dropSlash . splitPath where
  dropSlash "/" = "/"
  dropSlash d
    | isSuffixOf "/" d = reverse $ tail $ reverse d
    | otherwise        = d
  process rs        (".":ds)  = process rs ds
  process ("..":rs) ("..":ds) = process ("..":"..":rs) ds
  process ("/":[])  ("..":ds) = process ("/":[]) ds
  process (_:rs)    ("..":ds) = process rs ds
  process rs        (d:ds)    = process (d:rs) ds
  process rs        _         = reverse rs

sortCompiledFiles :: [String] -> ([String],[String],[String])
sortCompiledFiles = foldl split ([],[],[]) where
  split fs@(hxx,cxx,os) f
    | isSuffixOf ".hpp" f = (hxx++[f],cxx,os)
    | isSuffixOf ".h"   f = (hxx++[f],cxx,os)
    | isSuffixOf ".cpp" f = (hxx,cxx++[f],os)
    | isSuffixOf ".cc"  f = (hxx,cxx++[f],os)
    | isSuffixOf ".a"   f = (hxx,cxx,os++[f])
    | isSuffixOf ".o"   f = (hxx,cxx,os++[f])
    | otherwise = fs

checkModuleFreshness :: String -> CompileMetadata -> IO Bool
checkModuleFreshness p (CompileMetadata p2 _ is _ _ ps xs ts hxx cxx os) = do
  time <- getModificationTime $ getCachedPath p "" metadataFilename
  (ps2,xs2,ts2) <- findSourceFiles p ""
  let e1 = checkMissing ps ps2
  let e2 = checkMissing xs xs2
  let e3 = checkMissing ts ts2
  f1 <- sequence $ map (\p2 -> check time $ getCachedPath p2 "" metadataFilename) is
  f2 <- sequence $ map (check time . (p2 </>)) $ ps ++ xs
  f3 <- sequence $ map (check time . getCachedPath p2 "") $ hxx ++ cxx ++ os
  let fresh = not $ any id $ [e1,e2,e3] ++ f1 ++ f2 ++ f3
  return fresh where
    check time f = do
      time2 <- getModificationTime f
      return (time2 > time)
    checkMissing s0 s1 = not $ null $ (Set.fromList s1) `Set.difference` (Set.fromList s0)
