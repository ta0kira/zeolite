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

module Cli.Compiler (
  LoadedTests(..),
  ModuleSpec(..),
  compileModule,
  createModuleTemplates,
  runModuleTests,
) where

import Control.Monad (foldM,when)
import Data.Either (partitionEithers)
import Data.List (isSuffixOf,nub,sort)
import System.Directory
import System.FilePath
import System.Posix.Temp (mkstemps)
import System.IO
import Text.Parsec (SourcePos)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompileError
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.Paths
import Cli.ProcessMetadata
import Cli.Programs
import Cli.TestRunner -- Not safe, due to Text.Regex.TDFA.
import Compilation.CompileInfo
import Compilation.ProcedureContext (ExprMap)
import CompilerCxx.Category
import CompilerCxx.Naming
import Parser.SourceFile
import Types.Builtin
import Types.DefinedCategory
import Types.Pragma
import Types.TypeCategory
import Types.TypeInstance


data ModuleSpec =
  ModuleSpec {
    msRoot :: FilePath,
    msPath :: FilePath,
    msExprMap :: ExprMap SourcePos,
    msPublicDeps :: [FilePath],
    msPrivateDeps :: [FilePath],
    msPublicFiles :: [FilePath],
    msPrivateFiles :: [FilePath],
    msTestFiles :: [FilePath],
    msExtraFiles :: [ExtraSource],
    msExtraPaths :: [FilePath],
    msMode :: CompileMode,
    msForce :: ForceMode
  }
  deriving (Show)

data LoadedTests =
  LoadedTests {
    ltRoot :: FilePath,
    ltPath :: FilePath,
    ltMetadata :: CompileMetadata,
    ltExprMap :: ExprMap SourcePos,
    ltPublicDeps :: [CompileMetadata],
    ltPrivateDeps :: [CompileMetadata]
  }
  deriving (Show)

compileModule :: (PathIOHandler r, CompilerBackend b) => r -> b -> ModuleSpec -> CompileInfoIO ()
compileModule resolver backend (ModuleSpec p d em is is2 ps xs ts es ep m f) = do
  as  <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is
  as2 <- fmap fixPaths $ mapErrorsM (resolveModule resolver (p </> d)) is2
  let ca0 = Map.empty
  deps1 <- loadPublicDeps compilerHash f ca0 as
  let ca1 = ca0 `Map.union` mapMetadata deps1
  deps2 <- loadPublicDeps compilerHash f ca1 as2
  let ca2 = ca1 `Map.union` mapMetadata deps2
  base <- resolveBaseModule resolver
  actual <- resolveModule resolver p d
  isBase <- isBaseModule resolver actual
  -- Lazy dependency loading, in case we're compiling base.
  deps1' <- if isBase
               then return deps1
               else do
                 bpDeps <- loadPublicDeps compilerHash f ca2 [base]
                 return $ bpDeps ++ deps1
  ns0 <- createPublicNamespace p d
  let ex = concat $ map getSourceCategories es
  (cm,(pc,tc)) <- loadLanguageModule p ns0 ex em ps deps1' deps2
  xa <- mapErrorsM (loadPrivateSource p) xs
  (xx1,xx2) <- compileLanguageModule cm xa
  mf <- maybeCreateMain cm xa m
  let fs' = xx1++xx2
  eraseCachedData (p </> d)
  let ps2 = map takeFileName ps
  let xs2 = map takeFileName xs
  let ts2 = map takeFileName ts
  let paths = map (\ns -> getCachedPath (p </> d) ns "") $ nub $ filter (not . null) $ map show $ [ns0] ++ map coNamespace fs'
  paths' <- mapM (errorFromIO . canonicalizePath) paths
  s0 <- errorFromIO $ canonicalizePath $ getCachedPath (p </> d) (show ns0) ""
  let paths2 = base:(getIncludePathsForDeps (deps1' ++ deps2)) ++ ep' ++ paths'
  let hxx   = filter (isSuffixOf ".hpp" . coFilename)       fs'
  let other = filter (not . isSuffixOf ".hpp" . coFilename) fs'
  os1 <- mapErrorsM (writeOutputFile (show ns0) paths2) $ hxx ++ other
  let files = map (\f2 -> getCachedPath (p </> d) (show $ coNamespace f2) (coFilename f2)) fs' ++
              map (\f2 -> p </> getSourceFile f2) es
  files' <- mapErrorsM checkOwnedFile files
  os2 <- fmap concat $ mapErrorsM (compileExtraSource (show ns0) paths2) es
  let (hxx',cxx,os') = sortCompiledFiles files'
  let (osCat,osOther) = partitionEithers os2
  path <- errorFromIO $ canonicalizePath $ p </> d
  let os1' = resolveObjectDeps (deps1' ++ deps2) path path (os1 ++ osCat)
  let cm2 = CompileMetadata {
      cmVersionHash = compilerHash,
      cmPath = path,
      cmNamespace = ns0,
      cmPublicDeps = as,
      cmPrivateDeps = as2,
      cmPublicCategories = sort pc,
      cmPrivateCategories = sort tc,
      cmSubdirs = [s0],
      cmPublicFiles = sort ps2,
      cmPrivateFiles = sort xs2,
      cmTestFiles = sort ts2,
      cmHxxFiles = sort hxx',
      cmCxxFiles = sort cxx,
      cmBinaries = [],
      cmLinkFlags = getLinkFlags m,
      cmObjectFiles = os1' ++ osOther ++ map OtherObjectFile os'
    }
  bs <- createBinary paths' (cm2:(deps1' ++ deps2)) m mf
  let cm2' = CompileMetadata {
      cmVersionHash = cmVersionHash cm2,
      cmPath = cmPath cm2,
      cmNamespace = cmNamespace cm2,
      cmPublicDeps = cmPublicDeps cm2,
      cmPrivateDeps = cmPrivateDeps cm2,
      cmPublicCategories = cmPublicCategories cm2,
      cmPrivateCategories = cmPrivateCategories cm2,
      cmSubdirs = cmSubdirs cm2,
      cmPublicFiles = cmPublicFiles cm2,
      cmPrivateFiles = cmPrivateFiles cm2,
      cmTestFiles = cmTestFiles cm2,
      cmHxxFiles = cmHxxFiles cm2,
      cmCxxFiles = cmCxxFiles cm2,
      cmBinaries = bs,
      cmLinkFlags = cmLinkFlags cm2,
      cmObjectFiles = cmObjectFiles cm2
    }
  writeMetadata (p </> d) cm2' where
    compilerHash = getCompilerHash backend
    ep' = fixPaths $ map (p </>) ep
    writeOutputFile ns0 paths ca@(CxxOutput _ f2 ns _ _ content) = do
      errorFromIO $ hPutStrLn stderr $ "Writing file " ++ f2
      writeCachedFile (p </> d) (show ns) f2 $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2
         then do
           let f2' = getCachedPath (p </> d) (show ns) f2
           let p0 = getCachedPath (p </> d) "" ""
           let p1 = getCachedPath (p </> d) (show ns) ""
           createCachePath (p </> d)
           let ns' = if isStaticNamespace ns then show ns else show ns0
           let command = CompileToObject f2' (getCachedPath (p </> d) ns' "") dynamicNamespaceName "" (p0:p1:paths) False
           o2 <- runCxxCommand backend command
           return $ ([o2],ca)
         else return ([],ca)
    compileExtraSource ns0 paths (CategorySource f2 cs ds2) = do
      f2' <- compileExtraFile False ns0 paths f2
      let ds2' = nub $ cs ++ ds2
      case f2' of
           Nothing -> return []
           Just o  -> return $ map (\c -> Left $ ([o],fakeCxxForSource ns0 ds2' c)) cs
    compileExtraSource ns0 paths (OtherSource f2) = do
      f2' <- compileExtraFile True ns0 paths f2
      case f2' of
           Just o  -> return [Right $ OtherObjectFile o]
           Nothing -> return []
    fakeCxxForSource ns ds2 c = CxxOutput {
        coCategory = Just c,
        coFilename = "",
        coNamespace = ns',
        coUsesNamespace = [ns'],
        coUsesCategory = ds2,
        coOutput = []
      } where
        ns' = if null ns then NoNamespace else StaticNamespace ns
    checkOwnedFile f2 = do
      exists <- errorFromIO $ doesFileExist f2
      when (not exists) $ compileErrorM $ "Owned file " ++ f2 ++ " does not exist."
      errorFromIO $ canonicalizePath f2
    compileExtraFile e ns0 paths f2
      | isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2 = do
          let f2' = p </> f2
          createCachePath (p </> d)
          let command = CompileToObject f2' (getCachedPath (p </> d) "" "") dynamicNamespaceName ns0 paths e
          fmap Just $ runCxxCommand backend command
      | isSuffixOf ".a" f2 || isSuffixOf ".o" f2 = return (Just f2)
      | otherwise = return Nothing
    createBinary paths deps (CompileBinary n _ o lf) ms
      | length ms >  1 = compileErrorM $ "Multiple matches for main category " ++ show n ++ "."
      | length ms == 0 = compileErrorM $ "Main category " ++ show n ++ " not found."
      | otherwise = do
          f0 <- if null o
                   then errorFromIO $ canonicalizePath $ p </> d </> show n
                   else errorFromIO $ canonicalizePath $ p </> d </> o
          let (CxxOutput _ _ _ ns2 req content) = head ms
          -- TODO: Create a helper or a constant or something.
          (o',h) <- errorFromIO $ mkstemps "/tmp/zmain_" ".cpp"
          errorFromIO $ hPutStr h $ concat $ map (++ "\n") content
          errorFromIO $ hClose h
          base <- resolveBaseModule resolver
          deps2  <- loadPrivateDeps compilerHash f (mapMetadata deps) deps
          let lf' = lf ++ getLinkFlagsForDeps deps2
          let paths' = fixPaths $ paths ++ base:(getIncludePathsForDeps deps)
          let os     = getObjectFilesForDeps deps2
          let ofr = getObjectFileResolver os
          let os' = ofr ns2 req
          let command = CompileToBinary o' os' f0 paths' lf'
          errorFromIO $ hPutStrLn stderr $ "Creating binary " ++ f0
          f1 <- runCxxCommand backend command
          errorFromIO $ removeFile o'
          return [f1]
    createBinary _ _ _ _ = return []
    maybeCreateMain cm2 xs2 (CompileBinary n f2 _ _) =
      fmap (:[]) $ compileModuleMain cm2 xs2 n f2
    maybeCreateMain _ _ _ = return []

createModuleTemplates :: FilePath -> FilePath -> [CompileMetadata] -> [CompileMetadata] -> CompileInfoIO ()
createModuleTemplates p d deps1 deps2 = do
  ns0 <- createPublicNamespace p d
  (ps,xs,_) <- findSourceFiles p d
  (LanguageModule _ _ _ cs0 ps0 ts0 cs1 ps1 ts1 _ _,_) <-
    fmap (fmap fst) $ loadLanguageModule p ns0 [] Map.empty ps deps1 deps2
  xs' <- zipWithContents p xs
  ds <- mapErrorsM parseInternalSource xs'
  let ds2 = concat $ map (\(_,_,d2) -> d2) ds
  tm <- foldM includeNewTypes defaultCategories [cs0,cs1,ps0,ps1,ts0,ts1]
  let cs = filter isValueConcrete $ cs1++ps1++ts1
  let ca = Set.fromList $ map getCategoryName $ filter isValueConcrete cs
  let ca' = foldr Set.delete ca $ map dcName ds2
  ts <- mapErrorsM (compileConcreteTemplate tm) $ Set.toList ca'
  mapErrorsM_ writeTemplate ts where
  writeTemplate (CxxOutput _ n _ _ _ content) = do
    let n' = p </> d </> n
    exists <- errorFromIO $ doesFileExist n'
    if exists
        then compileWarningM $ "Skipping existing file " ++ n
        else do
          errorFromIO $ hPutStrLn stderr $ "Writing file " ++ n
          errorFromIO $ writeFile n' $ concat $ map (++ "\n") content

runModuleTests :: (PathIOHandler r, CompilerBackend b) => r -> b -> FilePath ->
  [FilePath] -> LoadedTests -> CompileInfoIO [((Int,Int),CompileInfo ())]
runModuleTests _ backend base tp (LoadedTests p d m em deps1 deps2) = do
  let paths = base:(getIncludePathsForDeps deps1)
  mapErrorsM_ showSkipped $ filter (not . isTestAllowed) $ cmTestFiles m
  ts' <- zipWithContents p $ map (d </>) $ filter isTestAllowed $ cmTestFiles m
  path <- errorFromIO $ canonicalizePath (p </> d)
  (cm,_) <- loadLanguageModule path NoNamespace [] em [] deps1 []
  mapErrorsM (runSingleTest backend cm path paths (m:deps2)) ts' where
    allowTests = Set.fromList tp
    isTestAllowed t = if null allowTests then True else t `Set.member` allowTests
    showSkipped f = compileWarningM $ "Skipping tests in " ++ f ++ " due to explicit test filter."

createPublicNamespace :: FilePath -> FilePath -> CompileInfoIO Namespace
createPublicNamespace p d = (errorFromIO $ canonicalizePath (p </> d)) >>= return . StaticNamespace . publicNamespace

createPrivateNamespace :: FilePath -> FilePath -> CompileInfoIO Namespace
createPrivateNamespace p f = (errorFromIO $ canonicalizePath (p </> f)) >>= return . StaticNamespace . publicNamespace

zipWithContents :: FilePath -> [FilePath] -> CompileInfoIO [(FilePath,String)]
zipWithContents p fs = fmap (zip $ map fixPath fs) $ mapM (errorFromIO . readFile . (p </>)) fs

loadPrivateSource :: FilePath -> FilePath -> CompileInfoIO (PrivateSource SourcePos)
loadPrivateSource p f = do
  [f'] <- zipWithContents p [f]
  ns <- createPrivateNamespace p f
  (pragmas,cs,ds) <- parseInternalSource f'
  let cs' = map (setCategoryNamespace ns) cs
  let testing = any isTestsOnly pragmas
  return $ PrivateSource ns testing cs' ds

loadLanguageModule :: FilePath -> Namespace -> [CategoryName] ->
  ExprMap SourcePos -> [FilePath] -> [CompileMetadata] -> [CompileMetadata] ->
  CompileInfoIO (LanguageModule SourcePos,([CategoryName],[CategoryName]))
loadLanguageModule p ns2 ex em fs deps1 deps2 = do
  let public = Set.fromList $ map cmPath deps1
  let deps2' = filter (\cm -> not $ cmPath cm `Set.member` public) deps2
  let ns0 = filter (not . isNoNamespace) $ getNamespacesForDeps deps1
  let ns1 = filter (not . isNoNamespace) $ getNamespacesForDeps deps2'
  (ps0,_,  tsA0,_)    <- fmap merge $ mapErrorsM processAll deps1
  (ps1,_,  tsA1,_)    <- fmap merge $ mapErrorsM processAll deps2'
  (ps2,xs2,tsA2,tsB2) <- loadAllPublic "" fs
  let cm = LanguageModule {
      lmPublicNamespaces = ns0,
      lmPrivateNamespaces = ns1,
      lmLocalNamespaces = [ns2],
      lmPublicDeps = ps0,
      lmPrivateDeps = ps1,
      lmTestingDeps = tsA0++tsA1,
      lmPublicLocal = map (setCategoryNamespace ns2) ps2,
      lmPrivateLocal = map (setCategoryNamespace ns2) xs2,
      lmTestingLocal = map (setCategoryNamespace ns2) $ tsA2 ++ tsB2,
      lmExternal = ex,
      lmExprMap = em
    }
  return (cm,(map getCategoryName $ ps2++tsA2,map getCategoryName $ xs2++tsB2)) where
    loadPublic p2 p3 = parsePublicSource p3 >>= return . uncurry (partition p2)
    partition p2 pragmas cs
      -- Allow ModuleOnly when the path is the same. Only needed for tests.
      | p2 == p && (any isTestsOnly pragmas) = ([],[],cs,[])
      | p2 == p                              = (cs,[],[],[])
      | (any isModuleOnly pragmas) && (any isTestsOnly pragmas) = ([],[],[],cs)
      | (any isTestsOnly pragmas)                               = ([],[],cs,[])
      | (any isModuleOnly pragmas)                              = ([],cs,[],[])
      | otherwise                                               = (cs,[],[],[])
    processAll dep = do
      let dep' = getSourceFilesForDeps [dep]
      loadAllPublic (cmPath dep) dep'
    loadAllPublic p2 fs2 = do
      fs2' <- zipWithContents p fs2
      as <- mapErrorsM (loadPublic p2) fs2'
      return $ merge as
    merge = foldl merge4 ([],[],[],[])
    merge4 (ps1,xs1,tsA1,tsB1) (ps2,xs2,tsA2,tsB2) = (ps1++ps2,xs1++xs2,tsA1++tsA2,tsB1++tsB2)
