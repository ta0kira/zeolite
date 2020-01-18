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

module Cli.CompileMetadata (
  CompileMetadata(..),
  allowedExtraTypes,
  eraseMetadata,
  findSourceFiles,
  fixPath,
  getCachedPath,
  getCacheRelativePath,
  getIncludePathsForDeps,
  getObjectFilesForDeps,
  getSourceFilesForDeps,
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


data CompileMetadata =
  CompileMetadata {
    cmPath :: String,
    cmDepPaths :: [String],
    cmCategories :: [String],
    cmSubdirs :: [String],
    cmPublicFiles :: [String],
    cmPrivateFiles :: [String],
    cmHxxFiles :: [String],
    cmCxxFiles :: [String],
    cmObjectFiles :: [String]
  }
  deriving (Show,Read)

emptyCompileMetadata =
  CompileMetadata {
    cmPath = "",
    cmDepPaths = [],
    cmCategories = [],
    cmSubdirs = [],
    cmPublicFiles = [],
    cmPrivateFiles = [],
    cmHxxFiles = [],
    cmCxxFiles = [],
    cmObjectFiles = []
  }

cachedDataPath = ".zeolite-cache"
metadataFilename = "metadata.txt"
allowedExtraTypes = [".hpp",".cpp",".h",".cc",".a",".o"]

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
  check $ (reads c :: [(CompileMetadata,String)]) where
    check [(cm,"")] = return cm
    check [(cm,"\n")] = return cm
    check _ = do
      hPutStrLn stderr $ "Could not parse metadata from \"" ++ p ++ "\"; please recompile."
      exitFailure

writeMetadata :: String -> CompileMetadata -> IO ()
writeMetadata p m = do
  writeCachedFile p "" metadataFilename (show m ++ "\n")
  fresh <- checkModuleFreshness p m
  when (not fresh) $ do
    hPutStrLn stderr $ "Error writing metadata for \"" ++ p ++ "\"."
    exitFailure

eraseMetadata :: String -> IO ()
eraseMetadata p = do
  let f = p </> cachedDataPath </> metadataFilename
  exists <- doesPathExist f
  when exists $ removeFile f

writeCachedFile :: String -> String -> String -> String -> IO ()
writeCachedFile p ns f c = do
  createDirectoryIfMissing False $ p </> cachedDataPath
  createDirectoryIfMissing False $ p </> cachedDataPath </> ns
  writeFile (getCachedPath p ns f) c

getCachedPath :: String -> String -> String -> String
getCachedPath p ns f = fixPath $ p </> cachedDataPath </> ns </> f

getCacheRelativePath :: String -> String
getCacheRelativePath f = ".." </> f

findSourceFiles :: String -> String -> IO ([String],[String])
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
  return (ps,xs)

getSourceFilesForDeps :: [CompileMetadata] -> ([String],[String])
getSourceFilesForDeps = foldl extract ([],[]) where
  extract fs m = (fst fs ++ [cmPath m],snd fs ++ map (cmPath m </>) (cmPublicFiles m))

getIncludePathsForDeps :: [CompileMetadata] -> [String]
getIncludePathsForDeps = concat . map extract where
  extract m = (cmPath m </> cachedDataPath):(map ((cmPath m </> cachedDataPath) </>) $ cmSubdirs m)

getObjectFilesForDeps :: [CompileMetadata] -> [String]
getObjectFilesForDeps = concat . map extract where
  extract m = map ((cmPath m </> cachedDataPath) </>) $ cmObjectFiles m

loadRecursiveDeps :: [String] -> IO [CompileMetadata]
loadRecursiveDeps ps = fmap snd $ run (Set.empty,[]) ps where
  run xa@(pa,xs) (p:ps) = do
    p' <- canonicalizePath p
    if p' `Set.member` pa
       then run xa ps
       else do
         hPutStrLn stderr $ "Loading metadata for dependency \"" ++ p' ++ "\"."
         m <- loadMetadata p'
         fresh <- checkModuleFreshness p' m
         when (not fresh) $
           hPutStrLn stderr $ "Module \"" ++ p' ++ "\" is out of date and should be recompiled."
         run (p' `Set.insert` pa,xs ++ [m]) (ps ++ cmDepPaths m)
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
checkModuleFreshness p (CompileMetadata p2 is _ _ ps xs hxx cxx os) = do
  time <- getModificationTime $ getCachedPath p "" metadataFilename
  c1 <- sequence $ map (\p2 -> check time $ getCachedPath p2 "" metadataFilename) is
  c2 <- sequence $ map (check time . (p2 </>)) $ ps ++ xs
  c2 <- sequence $ map (check time . getCachedPath p2 "") $ hxx ++ cxx ++ os
  let fresh = not $ any id $ c1 ++ c2
  return fresh where
    check time f = do
      time2 <- getModificationTime f
      return (time2 > time)
