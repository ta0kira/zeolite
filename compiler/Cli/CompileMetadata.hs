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
  getIncludePathsForDeps,
  getObjectFilesForDeps,
  getSourceFilesForDeps,
  loadMetadata,
  sortCompiledFiles,
  writeCachedFile,
  writeMetadata,
) where

import Control.Monad (when)
import Data.List (isSuffixOf)
import System.Directory
import System.Environment
import System.FilePath
import System.IO


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
allowedExtraTypes = [".hpp",".cpp",".h",".cc",".o"]

loadMetadata :: String -> IO CompileMetadata
loadMetadata p = do
  -- TODO: This needs error handling.
  let f = p </> cachedDataPath </> metadataFilename
  c <- readFile f
  return $ read c

writeMetadata :: String -> CompileMetadata -> IO ()
writeMetadata p m = writeCachedFile p "" metadataFilename (show m ++ "\n")

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

findSourceFiles :: String -> String -> IO ([String],[String])
findSourceFiles p0 p = do
  -- TODO: This needs error handling.
  ds <- getDirectoryContents (p0 </> p) >>= return . map (p </>)
  let ps = filter (isSuffixOf ".0rp") ds
  let xs = filter (isSuffixOf ".0rx") ds
  return (ps,xs)

getSourceFilesForDeps :: [String] -> IO ([String],[String])
getSourceFilesForDeps = fmap merge . sequence . map loadSingle where
  loadSingle p = do
    -- TODO: This needs error handling.
    m <- loadMetadata p
    let p' = cmPath m
    let direct = ([p'],map (p' </>) $ cmPublicFiles m)
    -- TODO: This will cause issues if there is a dependency cycle!
    indirect <- getSourceFilesForDeps $ cmDepPaths m
    return (fst direct ++ fst indirect,snd direct ++ snd indirect)
  merge fs = (concat $ map fst fs,concat $ map snd fs)

getIncludePathsForDeps :: [String] -> IO [String]
getIncludePathsForDeps = fmap concat . sequence . map loadSingle where
  loadSingle p = do
    -- TODO: This needs error handling.
    m <- loadMetadata p
    let p' = cmPath m
    let direct = (p' </> cachedDataPath):(map ((p' </> cachedDataPath) </>) $ cmSubdirs m)
    -- TODO: This will cause issues if there is a dependency cycle!
    indirect <- getIncludePathsForDeps $ cmDepPaths m
    return $ direct ++ indirect

getObjectFilesForDeps :: [String] -> IO [String]
getObjectFilesForDeps = fmap concat . sequence . map loadSingle where
  loadSingle p = do
    -- TODO: This needs error handling.
    m <- loadMetadata p
    let p' = cmPath m
    let direct = map ((p' </> cachedDataPath) </>) $ cmObjectFiles m
    -- TODO: This will cause issues if there is a dependency cycle!
    indirect <- getObjectFilesForDeps $ cmDepPaths m
    return $ direct ++ indirect

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
    | isSuffixOf ".o"   f = (hxx,cxx,os++[f])
    | otherwise = fs
