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
  MetadataMap,
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
  loadModuleMetadata,
  loadPrivateDeps,
  loadPublicDeps,
  loadRecompile,
  loadTestingDeps,
  mapMetadata,
  resolveCategoryDeps,
  resolveObjectDeps,
  sortCompiledFiles,
  writeCachedFile,
  writeMetadata,
  writeRecompile,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.List (nub,isSuffixOf)
import Data.Maybe (isJust)
import System.Directory
import System.FilePath
import System.IO
import Text.Parsec (SourcePos)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.ParseMetadata -- Not safe, due to Text.Regex.TDFA.
import Cli.Programs (VersionHash(..))
import Compilation.CompileInfo
import Compilation.ProcedureContext (ExprMap)
import CompilerCxx.Category (CxxOutput(..))
import Types.Procedure (Expression(Literal),ValueLiteral(..))
import Types.TypeCategory
import Types.TypeInstance


cachedDataPath :: FilePath
cachedDataPath = ".zeolite-cache"

moduleFilename :: FilePath
moduleFilename = ".zeolite-module"

metadataFilename :: FilePath
metadataFilename = "compile-metadata"

type MetadataMap = Map.Map FilePath CompileMetadata

mapMetadata :: [CompileMetadata] -> MetadataMap
mapMetadata cs = Map.fromList $ zip (map cmPath cs) cs

loadRecompile :: FilePath -> CompileInfoIO ModuleConfig
loadRecompile p = do
  let f = p </> moduleFilename
  isFile <- errorFromIO $ doesFileExist p
  when isFile $ compileErrorM $ "Path \"" ++ p ++ "\" is not a directory"
  isDir <- errorFromIO $ doesDirectoryExist p
  when (not isDir) $ compileErrorM $ "Path \"" ++ p ++ "\" does not exist"
  filePresent <- errorFromIO $ doesFileExist f
  when (not filePresent) $ compileErrorM $ "Module \"" ++ p ++ "\" has not been configured yet"
  c <- errorFromIO $ readFile f
  (autoReadConfig f c) `reviseErrorM`
    ("Could not parse metadata from \"" ++ p ++ "\"; please reconfigure")

isPathUpToDate :: VersionHash -> ForceMode -> FilePath -> CompileInfoIO Bool
isPathUpToDate h f p = do
  m <- errorFromIO $ toCompileInfo $ loadDepsCommon f h Map.empty Set.empty (\m2 -> cmPublicDeps m2 ++ cmPrivateDeps m2) [p]
  return $ not $ isCompileError m

isPathConfigured :: FilePath -> CompileInfoIO Bool
isPathConfigured p = do
  m <- errorFromIO $ toCompileInfo $ loadRecompile p
  return $ not $ isCompileError m

writeMetadata :: FilePath -> CompileMetadata -> CompileInfoIO ()
writeMetadata p m = do
  p' <- errorFromIO $ canonicalizePath p
  errorFromIO $ hPutStrLn stderr $ "Writing metadata for \"" ++ p' ++ "\"."
  m' <- autoWriteConfig m `reviseErrorM` ("In data for " ++ p)
  writeCachedFile p' "" metadataFilename m'

writeRecompile :: FilePath -> ModuleConfig -> CompileInfoIO ()
writeRecompile p m = do
  p' <- errorFromIO $ canonicalizePath p
  let f = p </> moduleFilename
  errorFromIO $ hPutStrLn stderr $ "Updating config for \"" ++ p' ++ "\"."
  m' <- autoWriteConfig m `reviseErrorM` ("In data for " ++ p)
  errorFromIO $ writeFile f m'

eraseCachedData :: FilePath -> CompileInfoIO ()
eraseCachedData p = do
  let d  = p </> cachedDataPath
  dirExists <- errorFromIO $ doesDirectoryExist d
  when dirExists $ errorFromIO $ removeDirectoryRecursive d

createCachePath :: FilePath -> CompileInfoIO ()
createCachePath p = do
  let f = p </> cachedDataPath
  exists <- errorFromIO $ doesDirectoryExist f
  when (not exists) $ errorFromIO $ createDirectoryIfMissing False f

writeCachedFile :: FilePath -> String -> FilePath -> String -> CompileInfoIO ()
writeCachedFile p ns f c = do
  createCachePath p
  errorFromIO $ createDirectoryIfMissing False $ p </> cachedDataPath </> ns
  errorFromIO $ writeFile (getCachedPath p ns f) c

getCachedPath :: FilePath -> String -> FilePath -> FilePath
getCachedPath p ns f = fixPath $ p </> cachedDataPath </> ns </> f

getCacheRelativePath :: FilePath -> FilePath
getCacheRelativePath f = ".." </> f

findSourceFiles :: FilePath -> FilePath -> CompileInfoIO ([FilePath],[FilePath],[FilePath])
findSourceFiles p0 p = do
  let absolute = p0 </> p
  isFile <- errorFromIO $ doesFileExist absolute
  when isFile $ compileErrorM $ "Path \"" ++ absolute ++ "\" is not a directory"
  isDir <- errorFromIO $ doesDirectoryExist absolute
  when (not isDir) $ compileErrorM $ "Path \"" ++ absolute ++ "\" does not exist"
  ds <- errorFromIO $ getDirectoryContents absolute >>= return . map (p </>)
  let ps = filter (isSuffixOf ".0rp") ds
  let xs = filter (isSuffixOf ".0rx") ds
  let ts = filter (isSuffixOf ".0rt") ds
  return (ps,xs,ts)

getExprMap :: FilePath -> ModuleConfig -> CompileInfoIO (ExprMap SourcePos)
getExprMap p m = do
  path <- errorFromIO $ canonicalizePath (p </> rmRoot m </> rmPath m)
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

loadModuleMetadata :: VersionHash -> ForceMode -> MetadataMap -> FilePath ->
  CompileInfoIO CompileMetadata
loadModuleMetadata h f ca = fmap head . loadDepsCommon f h ca Set.empty (const []) . (:[])

loadPublicDeps :: VersionHash -> ForceMode -> MetadataMap -> [FilePath] ->
  CompileInfoIO [CompileMetadata]
loadPublicDeps h f ca = loadDepsCommon f h ca Set.empty cmPublicDeps

loadTestingDeps :: VersionHash -> ForceMode -> MetadataMap -> CompileMetadata ->
  CompileInfoIO [CompileMetadata]
loadTestingDeps h f ca m = loadDepsCommon f h ca (Set.fromList [cmPath m]) cmPublicDeps (cmPublicDeps m ++ cmPrivateDeps m)

loadPrivateDeps :: VersionHash -> ForceMode -> MetadataMap -> [CompileMetadata] ->
  CompileInfoIO [CompileMetadata]
loadPrivateDeps h f ca ms = do
  new <- loadDepsCommon f h ca pa (\m -> cmPublicDeps m ++ cmPrivateDeps m) paths
  return $ ms ++ new where
    paths = concat $ map (\m -> cmPublicDeps m ++ cmPrivateDeps m) ms
    pa = Set.fromList $ map cmPath ms

loadDepsCommon :: ForceMode -> VersionHash -> MetadataMap -> Set.Set FilePath ->
  (CompileMetadata -> [FilePath]) -> [FilePath] -> CompileInfoIO [CompileMetadata]
loadDepsCommon f h ca pa0 getDeps ps = do
  (_,processed) <- fixedPaths >>= collect (pa0,[])
  mapErrorsM check processed where
    enforce = f /= ForceAll
    fixedPaths = mapM (errorFromIO . canonicalizePath) ps
    collect xa@(pa,xs) (p:ps2)
      | p `Set.member` pa = collect xa ps2
      | otherwise = do
          let continue m ds = collect (p `Set.insert` pa,xs ++ [m]) (ps2 ++ ds)
          case p `Map.lookup` ca of
               Just m2 -> continue m2 []
               Nothing -> do
                 errorFromIO $ hPutStrLn stderr $ "Loading metadata for dependency \"" ++ p ++ "\"."
                 m2 <- loadMetadata ca p
                 let ds = getDeps m2
                 continue m2 ds
    collect xa _ = return xa
    check m
      | cmPath m `Map.member` ca = return m
      | otherwise = do
          fresh <- checkModuleFreshness (cmPath m) m
          let sameVersion = checkModuleVersionHash h m
          when (enforce && not fresh) $
            compileErrorM $ "Module \"" ++ cmPath m ++ "\" is out of date and should be recompiled"
          when (enforce && not sameVersion) $
            compileErrorM $ "Module \"" ++ cmPath m ++ "\" was compiled with a different compiler setup"
          return m

loadMetadata :: MetadataMap -> FilePath -> CompileInfoIO CompileMetadata
loadMetadata ca p = do
  path <- errorFromIO $ canonicalizePath p
  case path `Map.lookup` ca of
       Just cm -> return cm
       Nothing -> do
         let f = p </> cachedDataPath </> metadataFilename
         isFile <- errorFromIO $ doesFileExist p
         when isFile $ compileErrorM $ "Path \"" ++ p ++ "\" is not a directory"
         isDir <- errorFromIO $ doesDirectoryExist p
         when (not isDir) $ compileErrorM $ "Path \"" ++ p ++ "\" does not exist"
         filePresent <- errorFromIO $ doesFileExist f
         when (not filePresent) $ compileErrorM $ "Module \"" ++ p ++ "\" has not been compiled yet"
         c <- errorFromIO $ readFile f
         (autoReadConfig f c) `reviseErrorM`
            ("Could not parse metadata from \"" ++ p ++ "\"; please recompile")

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

checkModuleFreshness :: FilePath -> CompileMetadata -> CompileInfoIO Bool
checkModuleFreshness p (CompileMetadata _ p2 _ is is2 _ _ _ ps xs ts hxx cxx bs _ _) = do
  time <- errorFromIO $ getModificationTime $ getCachedPath p "" metadataFilename
  (ps2,xs2,ts2) <- findSourceFiles p ""
  let e1 = checkMissing ps ps2
  let e2 = checkMissing xs xs2
  let e3 = checkMissing ts ts2
  rm <- checkInput time (p </> moduleFilename)
  f1 <- mapErrorsM (\p3 -> checkInput time $ getCachedPath p3 "" metadataFilename) $ is ++ is2
  f2 <- mapErrorsM (checkInput time . (p2 </>)) $ ps ++ xs
  f3 <- mapErrorsM (checkInput time . getCachedPath p2 "") $ hxx ++ cxx
  f4 <- mapErrorsM checkOutput bs
  let fresh = not $ any id $ [rm,e1,e2,e3] ++ f1 ++ f2 ++ f3 ++ f4
  return fresh where
    checkInput time f = do
      exists <- doesFileOrDirExist f
      if not exists
         then do
           compileWarningM $ "Required path \"" ++ f ++ "\" is missing"
           return True
         else do
           time2 <- errorFromIO $ getModificationTime f
           if time2 > time
              then do
                compileWarningM $ "Required path \"" ++ f ++ "\" is newer than cached data"
                return True
              else return False
    checkOutput f = do
      exists <- errorFromIO $ doesFileExist f
      if not exists
         then do
           compileWarningM $ "Output file \"" ++ f ++ "\" is missing"
           return True
         else return False
    checkMissing s0 s1 = not $ null $ (Set.fromList s1) `Set.difference` (Set.fromList s0)
    doesFileOrDirExist f2 = do
      existF <- errorFromIO $ doesFileExist f2
      if existF
        then return True
        else errorFromIO $ doesDirectoryExist f2

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
