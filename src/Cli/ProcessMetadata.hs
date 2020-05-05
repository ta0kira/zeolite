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

module Cli.ProcessMetadata (
  allowedExtraTypes,
  createCachePath,
  eraseCachedData,
  findSourceFiles,
  fixPath,
  getCachedPath,
  getCacheRelativePath,
  getIncludePathsForDeps,
  getLinkFlagsForDeps,
  getNamespacesForDeps,
  getObjectFilesForDeps,
  getObjectFileResolver,
  getRealPathsForDeps,
  getSourceFilesForDeps,
  isPathConfigured,
  isPathUpToDate,
  loadPrivateDeps,
  loadPublicDeps,
  loadMetadata,
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
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Cli.CompileMetadata
import Cli.ParseMetadata -- Not safe, due to Text.Regex.TDFA.
import Compilation.CompileInfo
import CompilerCxx.Category (CxxOutput(..))
import Types.TypeCategory
import Types.TypeInstance


cachedDataPath :: String
cachedDataPath = ".zeolite-cache"

recompileFilename :: String
recompileFilename = ".zeolite-module"

metadataFilename :: String
metadataFilename = "compile-metadata"

allowedExtraTypes :: [String]
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
  let m = autoReadConfig f c
  if isCompileError m
     then do
       hPutStrLn stderr $ "Could not parse metadata from \"" ++ p ++ "\"; please recompile."
       hPutStrLn stderr $ show (getCompileError m)
       exitFailure
     else return (getCompileSuccess m)

tryLoadMetadata :: String -> IO (Maybe CompileMetadata)
tryLoadMetadata p = tryLoadData $ (p </> cachedDataPath </> metadataFilename)

tryLoadRecompile :: String -> IO (Maybe ModuleConfig)
tryLoadRecompile p = tryLoadData $ (p </> recompileFilename)

tryLoadData :: ConfigFormat a => String -> IO (Maybe a)
tryLoadData f = do
  filePresent <- doesFileExist f
  if not filePresent
    then return Nothing
    else do
      c <- readFile f
      let m = autoReadConfig f c
      if isCompileError m
         then do
           hPutStrLn stderr $ "Could not parse config file:"
           hPutStrLn stderr $ show (getCompileError m)
           return Nothing
         else return $ Just (getCompileSuccess m)

isPathUpToDate :: String -> String -> IO Bool
isPathUpToDate h p = do
  m <- tryLoadMetadata p
  case m of
       Nothing -> return False
       Just _ -> do
         (fr,_) <- loadDepsCommon True h (\m2 -> cmPublicDeps m2 ++ cmPrivateDeps m2) [p]
         return fr

isPathConfigured :: String -> IO Bool
isPathConfigured p = tryLoadRecompile p >>= return . isJust

writeMetadata :: String -> CompileMetadata -> IO ()
writeMetadata p m = do
  p' <- canonicalizePath p
  hPutStrLn stderr $ "Writing metadata for \"" ++ p' ++ "\"."
  let m' = autoWriteConfig m
  if isCompileError m'
     then do
       hPutStrLn stderr $ "Could not serialize metadata."
       hPutStrLn stderr $ show (getCompileError m')
       exitFailure
     else writeCachedFile p' "" metadataFilename (getCompileSuccess m')

writeRecompile :: String -> ModuleConfig -> IO ()
writeRecompile p m = do
  p' <- canonicalizePath p
  let f = p </> recompileFilename
  hPutStrLn stderr $ "Updating config for \"" ++ p' ++ "\"."
  let m' = autoWriteConfig m
  if isCompileError m'
     then do
       hPutStrLn stderr $ "Could not serialize module config."
       hPutStrLn stderr $ show (getCompileError m')
       exitFailure
     else do
       exists <- doesFileExist f
       when exists $ removeFile f
       writeFile f (getCompileSuccess m')

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

getNamespacesForDeps :: [CompileMetadata] -> [String]
getNamespacesForDeps = filter (not . null) . map cmNamespace

getIncludePathsForDeps :: [CompileMetadata] -> [String]
getIncludePathsForDeps = concat . map cmSubdirs

getLinkFlagsForDeps :: [CompileMetadata] -> [String]
getLinkFlagsForDeps = concat . map cmLinkFlags

getObjectFilesForDeps :: [CompileMetadata] -> [ObjectFile]
getObjectFilesForDeps = concat . map cmObjectFiles

loadPublicDeps :: String -> [String] -> IO (Bool,[CompileMetadata])
loadPublicDeps h = loadDepsCommon False h cmPublicDeps

loadPrivateDeps :: String -> [CompileMetadata] -> IO (Bool,[CompileMetadata])
loadPrivateDeps h ms = do
  (fr,new) <- loadDepsCommon False h (\m -> cmPublicDeps m ++ cmPrivateDeps m) toFind
  return (fr,ms ++ existing ++ new) where
    paths = concat $ map (\m -> cmPublicDeps m ++ cmPrivateDeps m) ms
    (existing,toFind) = foldl splitByExisting ([],[]) $ nub paths
    byPath = Map.fromList $ map (\m -> (cmPath m,m)) ms
    splitByExisting (es,fs) p =
      case p `Map.lookup` byPath of
          Just m  -> (es ++ [m],fs)
          Nothing -> (es,fs ++ [p])

loadDepsCommon :: Bool -> String -> (CompileMetadata -> [String]) -> [String] -> IO (Bool,[CompileMetadata])
loadDepsCommon s h f ps = fmap snd $ fixedPaths >>= collect (Set.empty,(True,[])) where
  fixedPaths = sequence $ map canonicalizePath ps
  collect xa@(pa,(fr,xs)) (p:ps2)
    | p `Set.member` pa = collect xa ps2
    | otherwise = do
        when (not s) $ hPutStrLn stderr $ "Loading metadata for dependency \"" ++ p ++ "\"."
        m <- loadMetadata p
        fresh <- checkModuleFreshness s p m
        when (not s && not fresh) $
          hPutStrLn stderr $ "Module \"" ++ p ++ "\" is out of date and should be recompiled."
        let sameVersion = checkModuleVersionHash h m
        when (not s && not sameVersion) $
          hPutStrLn stderr $ "Module \"" ++ p ++ "\" was compiled with a different compiler setup."
        collect (p `Set.insert` pa,(sameVersion && fresh && fr,xs ++ [m])) (ps2 ++ f m)
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

checkModuleVersionHash :: String -> CompileMetadata -> Bool
checkModuleVersionHash h m = cmVersionHash m == h

checkModuleFreshness :: Bool -> String -> CompileMetadata -> IO Bool
checkModuleFreshness s p (CompileMetadata _ p2 _ is is2 _ _ ps xs ts hxx cxx _ _) = do
  time <- getModificationTime $ getCachedPath p "" metadataFilename
  (ps2,xs2,ts2) <- findSourceFiles p ""
  let e1 = checkMissing ps ps2
  let e2 = checkMissing xs xs2
  let e3 = checkMissing ts ts2
  rm <- check time (p </> recompileFilename)
  f1 <- sequence $ map (\p3 -> check time $ getCachedPath p3 "" metadataFilename) $ is ++ is2
  f2 <- sequence $ map (check time . (p2 </>)) $ ps ++ xs
  f3 <- sequence $ map (check time . getCachedPath p2 "") $ hxx ++ cxx
  let fresh = not $ any id $ [rm,e1,e2,e3] ++ f1 ++ f2 ++ f3
  return fresh where
    check time f = do
      exists <- doesFileOrDirExist f
      if not exists
         then do
           when (not s) $ hPutStrLn stderr $ "Required path \"" ++ f ++ "\" is missing."
           return True
         else do
           time2 <- getModificationTime f
           if time2 > time
              then do
                when (not s) $ hPutStrLn stderr $ "Required path \"" ++ f ++ "\" is out of date."
                return True
              else return False
    checkMissing s0 s1 = not $ null $ (Set.fromList s1) `Set.difference` (Set.fromList s0)
    doesFileOrDirExist f2 = do
      existF <- doesFileExist f2
      if existF
        then return True
        else doesDirectoryExist f2

getObjectFileResolver :: [ObjectFile] -> [Namespace] -> [CategoryName] -> [String]
getObjectFileResolver os ns ds = resolved ++ nonCategories where
  categories    = filter isCategoryObjectFile os
  nonCategories = map oofFile $ filter (not . isCategoryObjectFile) os
  categoryMap = Map.fromList $ map keyByCategory2 categories
  keyByCategory2 o = ((ciCategory $ cofCategory o,ciNamespace $ cofCategory o),o)
  objectMap = Map.fromList $ map keyBySpec categories
  keyBySpec o = (cofCategory o,o)
  directDeps = concat $ map (resolveDep2 . show) ds
  directResolved = map cofCategory directDeps
  resolveDep2 d = unwrap $ foldl (<|>) Nothing allChecks <|> Just [] where
    allChecks = map (\n -> (d,n) `Map.lookup` categoryMap >>= return . (:[])) (map show ns ++ [""])
    unwrap (Just xs) = xs
    unwrap _         = []
  (_,_,resolved) = collectAll Set.empty Set.empty directResolved
  collectAll ca fa [] = (ca,fa,[])
  collectAll ca fa (c:cs)
    | c `Set.member` ca = collectAll ca fa cs
    | otherwise =
      case c `Map.lookup` objectMap of
           Just (CategoryObjectFile _ ds2 fs) -> (ca',fa'',fs') where
             (ca',fa',fs0) = collectAll (c `Set.insert` ca) fa (ds2 ++ cs)
             fa'' = fa' `Set.union` (Set.fromList fs)
             fs' = (filter (not . flip elem fa') fs) ++ fs0
           _ -> collectAll ca fa cs

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
  cxxToId _                               = undefined
  resolveCategory (fs,ca@(CxxOutput _ _ _ ns2 ds _)) =
    (cxxToId ca,CategoryObjectFile (cxxToId ca) (filter (/= cxxToId ca) rs) fs) where
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
resolveDep cm (n:ns) d =
  case (d,n) `Map.lookup` cm of
       Just xs -> [xs]
       Nothing -> resolveDep cm ns d
resolveDep _ _ d = [UnresolvedCategory d]
