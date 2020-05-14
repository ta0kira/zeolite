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
  checkAllowedStale,
  checkMetadataFreshness,
  createCachePath,
  eraseCachedData,
  findSourceFiles,
  fixPath,
  fixPaths,
  getCachedPath,
  getCacheRelativePath,
  getExprMap,
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
  loadTestingDeps,
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
import Text.Parsec (SourcePos)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.ParseMetadata -- Not safe, due to Text.Regex.TDFA.
import Compilation.CompileInfo
import Compilation.ProcedureContext (ExprMap)
import CompilerCxx.Category (CxxOutput(..))
import Config.Programs (VersionHash(..))
import Types.Procedure (Expression(Literal),ValueLiteral(..))
import Types.TypeCategory
import Types.TypeInstance


cachedDataPath :: FilePath
cachedDataPath = ".zeolite-cache"

moduleFilename :: FilePath
moduleFilename = ".zeolite-module"

metadataFilename :: FilePath
metadataFilename = "compile-metadata"

checkAllowedStale :: Bool -> ForceMode -> IO ()
checkAllowedStale fr f = do
  when (not fr && f < ForceAll) $ do
    hPutStrLn stderr $ "Some dependencies are out of date. " ++
                       "Recompile them or use -f to force."
    exitFailure

loadMetadata :: FilePath -> IO CompileMetadata
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

checkMetadataFreshness :: FilePath -> CompileMetadata -> IO Bool
checkMetadataFreshness = checkModuleFreshness False

tryLoadMetadata :: FilePath -> IO (Maybe CompileMetadata)
tryLoadMetadata p = tryLoadData $ (p </> cachedDataPath </> metadataFilename)

tryLoadRecompile :: FilePath -> IO (Maybe ModuleConfig)
tryLoadRecompile p = tryLoadData $ (p </> moduleFilename)

tryLoadData :: ConfigFormat a => FilePath -> IO (Maybe a)
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
           hPutStr stderr $ show (getCompileError m)
           return Nothing
         else return $ Just (getCompileSuccess m)

isPathUpToDate :: VersionHash -> FilePath -> IO Bool
isPathUpToDate h p = do
  m <- tryLoadMetadata p
  case m of
       Nothing -> return False
       Just _ -> do
         (fr,_) <- loadDepsCommon True h Set.empty (\m2 -> cmPublicDeps m2 ++ cmPrivateDeps m2) [p]
         return fr

isPathConfigured :: FilePath -> IO Bool
isPathConfigured p = do
  -- Just for error messages.
  _ <- tryLoadRecompile p
  doesFileExist (p </> moduleFilename)

writeMetadata :: FilePath -> CompileMetadata -> IO ()
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

writeRecompile :: FilePath -> ModuleConfig -> IO ()
writeRecompile p m = do
  p' <- canonicalizePath p
  let f = p </> moduleFilename
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

eraseCachedData :: FilePath -> IO ()
eraseCachedData p = do
  let d  = p </> cachedDataPath
  dirExists <- doesDirectoryExist d
  when dirExists $ removeDirectoryRecursive d

createCachePath :: FilePath -> IO ()
createCachePath p = do
  let f = p </> cachedDataPath
  exists <- doesDirectoryExist f
  when (not exists) $ createDirectoryIfMissing False f

writeCachedFile :: FilePath -> String -> FilePath -> String -> IO ()
writeCachedFile p ns f c = do
  createCachePath p
  createDirectoryIfMissing False $ p </> cachedDataPath </> ns
  writeFile (getCachedPath p ns f) c

getCachedPath :: FilePath -> String -> FilePath -> FilePath
getCachedPath p ns f = fixPath $ p </> cachedDataPath </> ns </> f

getCacheRelativePath :: FilePath -> FilePath
getCacheRelativePath f = ".." </> f

findSourceFiles :: FilePath -> FilePath -> IO ([FilePath],[FilePath],[FilePath])
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

getExprMap :: FilePath -> ModuleConfig -> IO (ExprMap SourcePos)
getExprMap p m = do
  path <- canonicalizePath (p </> rmRoot m </> rmPath m)
  let defaults = [("MODULE_PATH",Literal (StringLiteral [] path))]
  return $ Map.fromList $ rmExprMap m ++ defaults

getRealPathsForDeps :: [CompileMetadata] -> [FilePath]
getRealPathsForDeps = map cmPath

getSourceFilesForDeps :: [CompileMetadata] -> [FilePath]
getSourceFilesForDeps = concat . map extract where
  extract m = map (cmPath m </>) (cmPublicFiles m)

getNamespacesForDeps :: [CompileMetadata] -> [Namespace]
getNamespacesForDeps = filter (not . isNoNamespace) . map cmNamespace

getIncludePathsForDeps :: [CompileMetadata] -> [FilePath]
getIncludePathsForDeps = concat . map cmSubdirs

getLinkFlagsForDeps :: [CompileMetadata] -> [String]
getLinkFlagsForDeps = concat . map cmLinkFlags

getObjectFilesForDeps :: [CompileMetadata] -> [ObjectFile]
getObjectFilesForDeps = concat . map cmObjectFiles

loadPublicDeps :: VersionHash -> [FilePath] -> IO (Bool,[CompileMetadata])
loadPublicDeps h = loadDepsCommon False h Set.empty cmPublicDeps

loadTestingDeps :: VersionHash -> CompileMetadata -> IO (Bool,[CompileMetadata])
loadTestingDeps h m = loadDepsCommon False h (Set.fromList [cmPath m]) cmPublicDeps (cmPublicDeps m ++ cmPrivateDeps m)

loadPrivateDeps :: VersionHash -> [CompileMetadata] -> IO (Bool,[CompileMetadata])
loadPrivateDeps h ms = do
  (fr,new) <- loadDepsCommon False h pa (\m -> cmPublicDeps m ++ cmPrivateDeps m) paths
  return (fr,ms ++ new) where
    paths = concat $ map (\m -> cmPublicDeps m ++ cmPrivateDeps m) ms
    pa = Set.fromList $ map cmPath ms

loadDepsCommon :: Bool -> VersionHash -> Set.Set FilePath ->
  (CompileMetadata -> [FilePath]) -> [FilePath] -> IO (Bool,[CompileMetadata])
loadDepsCommon s h pa0 f ps = fmap snd $ fixedPaths >>= collect (pa0,(True,[])) where
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

fixPath :: FilePath -> FilePath
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

fixPaths :: [FilePath] -> [FilePath]
fixPaths = nub . map fixPath

sortCompiledFiles :: [FilePath] -> ([FilePath],[FilePath],[FilePath])
sortCompiledFiles = foldl split ([],[],[]) where
  split fs@(hxx,cxx,os) f
    | isSuffixOf ".hpp" f = (hxx++[f],cxx,os)
    | isSuffixOf ".h"   f = (hxx++[f],cxx,os)
    | isSuffixOf ".cpp" f = (hxx,cxx++[f],os)
    | isSuffixOf ".cc"  f = (hxx,cxx++[f],os)
    | isSuffixOf ".a"   f = (hxx,cxx,os++[f])
    | isSuffixOf ".o"   f = (hxx,cxx,os++[f])
    | otherwise = fs

checkModuleVersionHash :: VersionHash -> CompileMetadata -> Bool
checkModuleVersionHash h m = cmVersionHash m == h

checkModuleFreshness :: Bool -> FilePath -> CompileMetadata -> IO Bool
checkModuleFreshness s p (CompileMetadata _ p2 _ is is2 _ _ _ ps xs ts hxx cxx bs _ _) = do
  time <- getModificationTime $ getCachedPath p "" metadataFilename
  (ps2,xs2,ts2) <- findSourceFiles p ""
  let e1 = checkMissing ps ps2
  let e2 = checkMissing xs xs2
  let e3 = checkMissing ts ts2
  rm <- checkInput time (p </> moduleFilename)
  f1 <- sequence $ map (\p3 -> checkInput time $ getCachedPath p3 "" metadataFilename) $ is ++ is2
  f2 <- sequence $ map (checkInput time . (p2 </>)) $ ps ++ xs
  f3 <- sequence $ map (checkInput time . getCachedPath p2 "") $ hxx ++ cxx
  f4 <- sequence $ map checkOutput bs
  let fresh = not $ any id $ [rm,e1,e2,e3] ++ f1 ++ f2 ++ f3 ++ f4
  return fresh where
    checkInput time f = do
      exists <- doesFileOrDirExist f
      if not exists
         then do
           when (not s) $ hPutStrLn stderr $ "Required path \"" ++ f ++ "\" is missing."
           return True
         else do
           time2 <- getModificationTime f
           if time2 > time
              then do
                when (not s) $ hPutStrLn stderr $ "Required path \"" ++ f ++ "\" is newer than cached data."
                return True
              else return False
    checkOutput f = do
      exists <- doesFileExist f
      if not exists
         then do
           when (not s) $ hPutStrLn stderr $ "Output file \"" ++ f ++ "\" is missing."
           return True
         else return False
    checkMissing s0 s1 = not $ null $ (Set.fromList s1) `Set.difference` (Set.fromList s0)
    doesFileOrDirExist f2 = do
      existF <- doesFileExist f2
      if existF
        then return True
        else doesDirectoryExist f2

getObjectFileResolver :: [ObjectFile] -> [Namespace] -> [CategoryName] -> [FilePath]
getObjectFileResolver os ns ds = resolved ++ nonCategories where
  categories    = filter isCategoryObjectFile os
  nonCategories = map oofFile $ filter (not . isCategoryObjectFile) os
  categoryMap = Map.fromList $ map keyByCategory2 categories
  keyByCategory2 o = ((ciCategory $ cofCategory o,ciNamespace $ cofCategory o),o)
  objectMap = Map.fromList $ map keyBySpec categories
  keyBySpec o = (cofCategory o,o)
  directDeps = concat $ map resolveDep2 ds
  directResolved = map cofCategory directDeps
  resolveDep2 d = unwrap $ foldl (<|>) Nothing allChecks <|> Just [] where
    allChecks = map (\n -> (d,n) `Map.lookup` categoryMap >>= return . (:[])) (ns ++ [NoNamespace])
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

resolveObjectDeps :: [CompileMetadata] -> FilePath -> FilePath -> [([FilePath],CxxOutput)] -> [ObjectFile]
resolveObjectDeps deps p d os = resolvedCategories ++ nonCategories where
  categories = filter (isJust . coCategory . snd) os
  publicNamespaces = getNamespacesForDeps deps
  nonCategories = map OtherObjectFile $ concat $ map fst $ filter (not . isJust . coCategory . snd) os
  resolvedCategories = Map.elems $ Map.fromListWith mergeObjectFiles $ map resolveCategory categories
  categoryMap = Map.fromList $ directCategories ++ depCategories
  directCategories = map (keyByCategory . cxxToId) $ map snd categories
  depCategories = map keyByCategory $ concat (map categoriesToIds deps)
  getCats dep
    -- Allow ModuleOnly when the path is the same. Only needed for tests.
    | cmPath dep == p = cmPrivateCategories dep ++ cmPublicCategories dep
    | otherwise       = cmPublicCategories dep
  categoriesToIds dep = map (\c -> CategoryIdentifier (cmPath dep) c (cmNamespace dep)) $ getCats dep
  cxxToId (CxxOutput (Just c) _ ns _ _ _) = CategoryIdentifier d c ns
  cxxToId _                               = undefined
  resolveCategory (fs,ca@(CxxOutput _ _ _ ns2 ds _)) =
    (cxxToId ca,CategoryObjectFile (cxxToId ca) (filter (/= cxxToId ca) rs) fs) where
      rs = concat $ map (resolveDep categoryMap (ns2 ++ publicNamespaces)) ds

resolveCategoryDeps :: [CategoryName] -> [CompileMetadata] -> [CategoryIdentifier]
resolveCategoryDeps cs deps = resolvedCategories where
  publicNamespaces = getNamespacesForDeps deps
  resolvedCategories = concat $ map (resolveDep categoryMap publicNamespaces) cs
  categoryMap = Map.fromList depCategories
  depCategories = map (keyByCategory . cofCategory) $ filter isCategoryObjectFile $ concat $ map cmObjectFiles deps

keyByCategory :: CategoryIdentifier -> ((CategoryName,Namespace),CategoryIdentifier)
keyByCategory c = ((ciCategory c,ciNamespace c),c)

resolveDep :: Map.Map (CategoryName,Namespace) CategoryIdentifier ->
  [Namespace] -> CategoryName -> [CategoryIdentifier]
resolveDep cm (n:ns) d =
  case (d,n) `Map.lookup` cm of
       Just xs -> [xs]
       Nothing -> resolveDep cm ns d
resolveDep _ _ d = [UnresolvedCategory d]
