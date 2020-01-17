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
  findSourceFiles,
  getSourceFilesForDeps,
  loadMetadata,
  sortCompiledFiles,
  writeCachedFile,
  writeMetadata,
) where

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
    cmNamespace :: String,
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
    cmNamespace = "",
    cmSubdirs = [],
    cmPublicFiles = [],
    cmPrivateFiles = [],
    cmHxxFiles = [],
    cmCxxFiles = [],
    cmObjectFiles = []
  }

cachedDataPath = ".zeolite-cache"
metadataFilename = "metadata.txt"
allowedExtraTypes = [".hpp",".cpp",".o"]

loadMetadata :: String -> IO CompileMetadata
loadMetadata p = do
  -- TODO: This needs error handling.
  let f = p </> cachedDataPath </> metadataFilename
  c <- readFile f
  return $ read c

writeMetadata :: String -> CompileMetadata -> IO ()
writeMetadata p m = writeCachedFile p "" metadataFilename (show m)

writeCachedFile :: String -> String -> String -> String -> IO ()
writeCachedFile p ns f c = do
  createDirectoryIfMissing False $ p </> cachedDataPath
  createDirectoryIfMissing False $ p </> cachedDataPath </> ns
  writeFile (p </> cachedDataPath </> ns </> f) c

findSourceFiles :: String -> String -> IO ([String],[String])
findSourceFiles p0 p = do
  -- TODO: This needs error handling.
  ds <- getDirectoryContents (p0 </> p) >>= return . map (p </>)
  let ps = filter (isSuffixOf ".0rp") ds
  let xs = filter (isSuffixOf ".0rx") ds
  return (ps,xs)

getSourceFilesForDeps :: [String] -> IO [String]
getSourceFilesForDeps = fmap concat . sequence . map loadSingle where
  loadSingle p = do
    -- TODO: This needs error handling.
    m <- loadMetadata p
    return $ map (p </>) $ cmPublicFiles m

sortCompiledFiles :: [String] -> ([String],[String],[String])
sortCompiledFiles = foldl split ([],[],[]) where
  split fs@(hxx,cxx,os) f
    | isSuffixOf ".hpp" f = (hxx++[f],cxx,os)
    | isSuffixOf ".cpp" f = (hxx,cxx++[f],os)
    | isSuffixOf ".o"   f = (hxx,cxx,os++[f])
    | otherwise = fs
