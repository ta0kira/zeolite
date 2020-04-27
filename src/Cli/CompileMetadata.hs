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
  CategoryIdentifier(..),
  CompileMetadata(..),
  ObjectFile(..),
  RecompileMetadata(..),
  allowedExtraTypes,
  createCachePath,
  eraseCachedData,
  findSourceFiles,
  fixPath,
  getCachedPath,
  getCacheRelativePath,
  getIncludePathsForDeps,
  getNamespacesForDeps,
  getObjectFilesForDeps,
  getObjectFileResolver,
  getRealPathsForDeps,
  getRequiresFromDeps,
  getSourceFilesForDeps,
  isCategoryObjectFile,
  isPathConfigured,
  isPathUpToDate,
  loadPrivateDeps,
  loadPublicDeps,
  loadMetadata,
  mergeObjectFiles,
  resolveCategoryDeps,
  resolveObjectDeps,
  sortCompiledFiles,
  tryLoadRecompile,
  writeCachedFile,
  writeMetadata,
  writeRecompile,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (nub,isSuffixOf)
import Data.Maybe (isJust)
import System.Directory
import System.Environment
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

import Cli.CompileOptions (CompileMode)
import CompilerCxx.Category (CxxOutput(..))
import Types.TypeCategory
import Types.TypeInstance


data CompileMetadata =
  CompileMetadata {
    cmPath :: String,
    cmNamespace :: String, -- TODO: Use Namespace here?
    cmPublicDeps :: [String],
    cmPrivateDeps :: [String],
    cmExtraRequires :: [CategoryIdentifier],
    cmCategories :: [String],
    cmSubdirs :: [String],
    cmPublicFiles :: [String],
    cmPrivateFiles :: [String],
    cmTestFiles :: [String],
    cmHxxFiles :: [String],
    cmCxxFiles :: [String],
    cmObjectFiles :: [ObjectFile]
  }
  deriving (Show,Read)

data ObjectFile =
  CategoryObjectFile {
    cofCategory :: CategoryIdentifier,
    cofRequires :: [CategoryIdentifier],
    cofFiles :: [String]
  } |
  OtherObjectFile {
    oofFile :: String
  }
  deriving (Show,Read)

data CategoryIdentifier =
  CategoryIdentifier {
    ciPath :: String,
    ciCategory :: String,
    ciNamespace :: String
  } |
  UnresolvedCategory {
    ucCategory :: String
  }
  deriving (Eq,Ord,Show,Read)

mergeObjectFiles :: ObjectFile -> ObjectFile -> ObjectFile
mergeObjectFiles (CategoryObjectFile c rs1 fs1) (CategoryObjectFile _ rs2 fs2) =
  CategoryObjectFile c (rs1 ++ rs2) (fs1 ++ fs2)
mergeObjectFiles o _ = o

isCategoryObjectFile :: ObjectFile -> Bool
isCategoryObjectFile (CategoryObjectFile _ _ _) = True
isCategoryObjectFile (OtherObjectFile _)        = False

data RecompileMetadata =
  RecompileMetadata {
    rmRoot :: String,
    rmPath :: String,
    rmPublicDeps :: [String],
    rmPrivateDeps :: [String],
    rmExtraFiles :: [String],
    rmExtraPaths :: [String],
    rmExtraRequires :: [String],
    rmMode :: CompileMode,
    rmOutputName :: String
  }
  deriving (Show,Read)

cachedDataPath = ".zeolite-cache"
recompileFilename = ".zeolite-module"
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
  m <- check $ (reads c :: [(CompileMetadata,String)])
  return m where
    check [(cm,"")] = return cm
    check [(cm,"\n")] = return cm
    check _ = do
      hPutStrLn stderr $ "Could not parse metadata from \"" ++ p ++ "\"; please recompile."
      exitFailure

tryLoadMetadata :: String -> IO (Maybe CompileMetadata)
tryLoadMetadata p = tryLoadData $ (p </> cachedDataPath </> metadataFilename)

tryLoadRecompile :: String -> IO (Maybe RecompileMetadata)
tryLoadRecompile p = tryLoadData $ (p </> recompileFilename)

tryLoadData :: Read a => String -> IO (Maybe a)
tryLoadData f = do
  filePresent <- doesFileExist f
  if not filePresent
    then return Nothing
    else do
      c <- readFile f
      check (reads c) where
        check [(cm,"")]   = return (Just cm)
        check [(cm,"\n")] = return (Just cm)
        check _           = return Nothing

isPathUpToDate :: String -> IO Bool
isPathUpToDate p = do
  m <- tryLoadMetadata p
  case m of
       Nothing -> return False
       Just m'-> do
         (fr,_) <- loadDepsCommon True (\m -> cmPublicDeps m ++ cmPrivateDeps m) [p]
         return fr

isPathConfigured :: String -> IO Bool
isPathConfigured p = tryLoadRecompile p >>= return . isJust

writeMetadata :: String -> CompileMetadata -> IO ()
writeMetadata p m = do
  p' <- canonicalizePath p
  hPutStrLn stderr $ "Writing metadata for \"" ++ p' ++ "\"."
  writeCachedFile p' "" metadataFilename (show m ++ "\n")

writeRecompile :: String -> RecompileMetadata -> IO ()
writeRecompile p m = do
  p' <- canonicalizePath p
  hPutStrLn stderr $ "Updating config for \"" ++ p' ++ "\"."
  writeFile (p </> recompileFilename) (show m ++ "\n")

eraseCachedData :: String -> IO ()
eraseCachedData p = do
  let d  = p </> cachedDataPath
  dirExists <- doesDirectoryExist d
  when dirExists $ removeDirectoryRecursive d

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

getRequiresFromDeps :: [CompileMetadata] -> [CategoryIdentifier]
getRequiresFromDeps = concat . map cmExtraRequires

getNamespacesForDeps :: [CompileMetadata] -> [String]
getNamespacesForDeps = filter (not . null) . map cmNamespace

getIncludePathsForDeps :: [CompileMetadata] -> [String]
getIncludePathsForDeps = concat . map cmSubdirs

getObjectFilesForDeps :: [CompileMetadata] -> [ObjectFile]
getObjectFilesForDeps = concat . map cmObjectFiles

loadPublicDeps :: [String] -> IO (Bool,[CompileMetadata])
loadPublicDeps = loadDepsCommon False cmPublicDeps

loadPrivateDeps :: [CompileMetadata] -> IO (Bool,[CompileMetadata])
loadPrivateDeps ms = do
  (fr,new) <- loadDepsCommon False (\m -> cmPublicDeps m ++ cmPrivateDeps m) toFind
  return (fr,ms ++ existing ++ new) where
    paths = concat $ map (\m -> cmPublicDeps m ++ cmPrivateDeps m) ms
    (existing,toFind) = foldl splitByExisting ([],[]) $ nub paths
    byPath = Map.fromList $ map (\m -> (cmPath m,m)) ms
    splitByExisting (es,fs) p =
      case p `Map.lookup` byPath of
          Just m  -> (es ++ [m],fs)
          Nothing -> (es,fs ++ [p])

loadDepsCommon :: Bool -> (CompileMetadata -> [String]) -> [String] -> IO (Bool,[CompileMetadata])
loadDepsCommon s f ps = fmap snd $ fixedPaths >>= collect (Set.empty,(True,[])) where
  fixedPaths = sequence $ map canonicalizePath ps
  collect xa@(pa,(fr,xs)) (p:ps)
    | p `Set.member` pa = collect xa ps
    | otherwise = do
        when (not s) $ hPutStrLn stderr $ "Loading metadata for dependency \"" ++ p ++ "\"."
        m <- loadMetadata p
        fresh <- checkModuleFreshness p m
        when (not s && not fresh) $
          hPutStrLn stderr $ "Module \"" ++ p ++ "\" is out of date and should be recompiled."
        collect (p `Set.insert` pa,(fresh && fr,xs ++ [m])) (ps ++ f m)
  collect xa _ = return xa

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
checkModuleFreshness p (CompileMetadata p2 _ is is2 _ _ _ ps xs ts hxx cxx _) = do
  time <- getModificationTime $ getCachedPath p "" metadataFilename
  (ps2,xs2,ts2) <- findSourceFiles p ""
  let e1 = checkMissing ps ps2
  let e2 = checkMissing xs xs2
  let e3 = checkMissing ts ts2
  rm <- check time (p </> recompileFilename)
  f1 <- sequence $ map (\p2 -> check time $ getCachedPath p2 "" metadataFilename) $ is ++ is2
  f2 <- sequence $ map (check time . (p2 </>)) $ ps ++ xs
  f3 <- sequence $ map (check time . getCachedPath p2 "") $ hxx ++ cxx
  let fresh = not $ any id $ [rm,e1,e2,e3] ++ f1 ++ f2 ++ f3
  return fresh where
    check time f = do
      exists <- doesPathExist f
      if not exists
         then return True
         else do
           time2 <- getModificationTime f
           return (time2 > time)
    checkMissing s0 s1 = not $ null $ (Set.fromList s1) `Set.difference` (Set.fromList s0)

getObjectFileResolver :: [CategoryIdentifier] -> [ObjectFile] -> [Namespace] -> [CategoryName] -> [String]
getObjectFileResolver ce os ns ds = resolved ++ nonCategories where
  categories    = filter isCategoryObjectFile os
  nonCategories = map oofFile $ filter (not . isCategoryObjectFile) os
  categoryMap = Map.fromList $ map keyByCategory categories
  keyByCategory o = ((ciCategory $ cofCategory o,ciNamespace $ cofCategory o),o)
  objectMap = Map.fromList $ map keyBySpec categories
  keyBySpec o = (cofCategory o,o)
  directDeps = concat $ map (resolveDep . show) ds
  directResolved = map cofCategory directDeps ++ ce
  resolveDep d = unwrap $ foldl (<|>) Nothing allChecks <|> Just [] where
    allChecks = map (\n -> (d,n) `Map.lookup` categoryMap >>= return . (:[])) (map show ns ++ [""])
    unwrap (Just xs) = xs
    unwrap _         = []
  resolved = reverse $ nub $ reverse $ collectAll Set.empty directResolved
  collectAll ca = concat . map (collect ca)
  -- NOTE: Object files are collected with deps following things that depend on
  -- them, without skipping over deps that have already been seen. This is so
  -- that a dep strictly follows everything that depends on it. This is
  -- is necessary when linking .a files.
  collect ca c
    | c `Set.member` ca = []
    | otherwise =
      case c `Map.lookup` objectMap of
           Just (CategoryObjectFile _ ds fs) -> fs ++ collectAll (c `Set.insert` ca) ds
           Nothing -> []

resolveObjectDeps :: String -> [([String],CxxOutput)] -> [CompileMetadata] -> [ObjectFile]
resolveObjectDeps p os deps = resolvedCategories ++ nonCategories where
  categories = filter (isJust . coCategory . snd) os
  publicNamespaces = getNamespacesForDeps deps
  nonCategories = map OtherObjectFile $ concat $ map fst $ filter (not . isJust . coCategory . snd) os
  resolvedCategories = Map.elems $ Map.fromListWith mergeObjectFiles $ map resolveCategory categories
  categoryMap = Map.fromList $ directCategories ++ depCategories
  directCategories = map (keyByCategory . cxxToId) $ map snd categories
  depCategories = map keyByCategory $ concat $ map categoriesToIds deps
  categoriesToIds dep = map (\c -> CategoryIdentifier (cmPath dep) c (cmNamespace dep)) (cmCategories dep)
  cxxToId (CxxOutput (Just c) _ ns _ _ _) = CategoryIdentifier p (show c) (show ns)
  resolveCategory (fs,ca@(CxxOutput _ _ _ ns2 ds _)) =
    (cxxToId ca,CategoryObjectFile (cxxToId ca) rs fs) where
      rs = concat $ map (resolveDep categoryMap (map show ns2 ++ publicNamespaces) . show) ds

resolveCategoryDeps :: [String] -> [CompileMetadata] -> [CategoryIdentifier]
resolveCategoryDeps cs deps = resolvedCategories where
  publicNamespaces = getNamespacesForDeps deps
  resolvedCategories = concat $ map (resolveDep categoryMap publicNamespaces) cs
  categoryMap = Map.fromList depCategories
  depCategories = map (keyByCategory . cofCategory) $ filter isCategoryObjectFile $ concat $ map cmObjectFiles deps

keyByCategory :: CategoryIdentifier -> ((String,String),CategoryIdentifier)
keyByCategory c = ((ciCategory c,ciNamespace c),c)

resolveDep :: Map.Map (String,String) CategoryIdentifier -> [String] -> String -> [CategoryIdentifier]
resolveDep cm ns d = unwrap $ foldl (<|>) Nothing allChecks where
  allChecks = map (\n -> (d,n) `Map.lookup` cm >>= return . (:[])) ns
  unwrap (Just xs) = xs
  unwrap _         = [UnresolvedCategory d]
