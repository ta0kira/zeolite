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
import Data.Time.LocalTime (getZonedTime)
import System.Directory
import System.FilePath
import System.Posix.Temp (mkstemps)
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

import Base.CompilerError
import Base.TrackedErrors
import Cli.CompileOptions
import Cli.Programs
import Cli.TestRunner
import Compilation.ProcedureContext (ExprMap)
import CompilerCxx.CxxFiles
import CompilerCxx.LanguageModule
import CompilerCxx.Naming
import Module.CompileMetadata
import Module.Paths
import Module.ProcessMetadata
import Parser.SourceFile
import Parser.TextParser (SourceContext)
import Types.Builtin
import Types.DefinedCategory
import Types.Procedure (isLiteralCategory)
import Types.TypeCategory
import Types.TypeInstance


data ModuleSpec =
  ModuleSpec {
    msRoot :: FilePath,
    msPath :: FilePath,
    msExprMap :: ExprMap SourceContext,
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
    ltExprMap :: ExprMap SourceContext,
    ltPublicDeps :: [CompileMetadata],
    ltPrivateDeps :: [CompileMetadata]
  }
  deriving (Show)

compileModule :: (PathIOHandler r, CompilerBackend b) => r -> b -> ModuleSpec -> TrackedErrorsIO ()
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
  time <- errorFromIO getZonedTime
  path <- errorFromIO $ canonicalizePath $ p </> d
  -- NOTE: Making the public namespace deterministic allows freshness checks to
  -- skip checking all inputs/outputs for each dependency.
  let ns0 = StaticNamespace $ publicNamespace  $ show compilerHash ++ path
  let ns1 = StaticNamespace . privateNamespace $ show time ++ show compilerHash ++ path
  let ex = concat $ map getSourceCategories es
  let ss = filter (not . isLiteralCategory) ex
  cs <- loadModuleGlobals resolver p (ns0,ns1) ps Nothing deps1' deps2
  let cm = createLanguageModule ex ss em cs
  let cs2 = filter (not . hasCodeVisibility FromDependency) cs
  let pc = map (getCategoryName . wvData) $ filter (not . hasCodeVisibility ModuleOnly) cs2
  let tc = map (getCategoryName . wvData) $ filter (hasCodeVisibility ModuleOnly)       cs2
  let dc = map (getCategoryName . wvData) $ filter (hasCodeVisibility FromDependency) $ filter (not . hasCodeVisibility ModuleOnly) cs
  xa <- mapErrorsM (loadPrivateSource resolver compilerHash p) xs
  fs <- compileLanguageModule cm xa
  mf <- maybeCreateMain cm xa m
  eraseCachedData (p </> d)
  let ps2 = map takeFileName ps
  let xs2 = map takeFileName xs
  let ts2 = map takeFileName ts
  let paths = map (\ns -> getCachedPath (p </> d) ns "") $ nub $ filter (not . null) $ map show $ map coNamespace fs
  paths' <- mapM (errorFromIO . canonicalizePath) paths
  s0 <- errorFromIO $ canonicalizePath $ getCachedPath (p </> d) (show ns0) ""
  s1 <- errorFromIO $ canonicalizePath $ getCachedPath (p </> d) (show ns1) ""
  let paths2 = base:s0:s1:(getIncludePathsForDeps (deps1' ++ deps2)) ++ ep' ++ paths'
  let hxx   = filter (isSuffixOf ".hpp" . coFilename)       fs
  let other = filter (not . isSuffixOf ".hpp" . coFilename) fs
  os1 <- mapErrorsM (writeOutputFile paths2) $ hxx ++ other
  let files = map (\f2 -> getCachedPath (p </> d) (show $ coNamespace f2) (coFilename f2)) fs ++
              map (\f2 -> p </> getSourceFile f2) es
  files' <- mapErrorsM checkOwnedFile files
  let ca = Map.fromList $ map (\c -> (getCategoryName c,getCategoryNamespace c)) $ map wvData cs2
  os2 <- fmap concat $ mapErrorsM (compileExtraSource (ns0,ns1) ca paths2) es
  let (hxx',cxx,os') = sortCompiledFiles files'
  let (osCat,osOther) = partitionEithers os2
  let os1' = resolveObjectDeps (deps1' ++ deps2) path path (os1 ++ osCat)
  warnPublic resolver (p </> d) pc dc os1' is
  let cm2 = CompileMetadata {
      cmVersionHash = compilerHash,
      cmPath = path,
      cmPublicNamespace = ns0,
      cmPrivateNamespace = ns1,
      cmPublicDeps = as,
      cmPrivateDeps = as2,
      cmPublicCategories = sort pc,
      cmPrivateCategories = sort tc,
      cmPublicSubdirs = [s0],
      cmPrivateSubdirs = [s1],
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
      cmPublicNamespace = cmPublicNamespace cm2,
      cmPrivateNamespace = cmPrivateNamespace cm2,
      cmPublicDeps = cmPublicDeps cm2,
      cmPrivateDeps = cmPrivateDeps cm2,
      cmPublicCategories = cmPublicCategories cm2,
      cmPrivateCategories = cmPrivateCategories cm2,
      cmPublicSubdirs = cmPublicSubdirs cm2,
      cmPrivateSubdirs = cmPrivateSubdirs cm2,
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
    writeOutputFile paths ca@(CxxOutput _ f2 ns _ _ content) = do
      errorFromIO $ hPutStrLn stderr $ "Writing file " ++ f2
      writeCachedFile (p </> d) (show ns) f2 $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2
         then do
           let f2' = getCachedPath (p </> d) (show ns) f2
           let p0 = getCachedPath (p </> d) "" ""
           let p1 = getCachedPath (p </> d) (show ns) ""
           createCachePath (p </> d)
           let ms = []
           let command = CompileToObject f2' (getCachedPath (p </> d) (show ns) "") ms (p0:p1:paths) False
           o2 <- runCxxCommand backend command
           return $ ([o2],ca)
         else return ([],ca)
    compileExtraSource (ns0,ns1) ca paths (CategorySource f2 cs ds2) = do
      f2' <- compileExtraFile False (ns0,ns1) paths f2
      case f2' of
           Nothing -> return []
           Just o  -> return $ map (\c -> Left $ ([o],fakeCxx c)) cs
      where
        fakeCxx c = CxxOutput {
            coCategory = Just c,
            coFilename = "",
            coNamespace = case c `Map.lookup` ca of
                               Just ns2 -> ns2
                               Nothing  -> NoNamespace,
            coUsesNamespace = Set.fromList [ns0,ns1],
            coUsesCategory = Set.fromList ds2,
            coOutput = []
          }
    compileExtraSource _ _ paths (OtherSource f2) = do
      f2' <- compileExtraFile True (NoNamespace,NoNamespace) paths f2
      case f2' of
           Just o  -> return [Right $ OtherObjectFile o]
           Nothing -> return []
    checkOwnedFile f2 = do
      exists <- errorFromIO $ doesFileExist f2
      when (not exists) $ compilerErrorM $ "Owned file " ++ f2 ++ " does not exist."
      errorFromIO $ canonicalizePath f2
    compileExtraFile e (ns0,ns1) paths f2
      | isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2 = do
          let f2' = p </> f2
          createCachePath (p </> d)
          let ms = [(publicNamespaceMacro,Just $ show ns0),(privateNamespaceMacro,Just $ show ns1)]
          let command = CompileToObject f2' (getCachedPath (p </> d) "" "") ms paths e
          fmap Just $ runCxxCommand backend command
      | isSuffixOf ".a" f2 || isSuffixOf ".o" f2 = return (Just f2)
      | otherwise = return Nothing
    createBinary paths deps (CompileBinary n _ o lf) ms
      | length ms >  1 = compilerErrorM $ "Multiple matches for main category " ++ show n ++ "."
      | length ms == 0 = compilerErrorM $ "Main category " ++ show n ++ " not found."
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

createModuleTemplates :: PathIOHandler r => r -> FilePath -> FilePath ->
  [CompileMetadata] -> [CompileMetadata] -> TrackedErrorsIO ()
createModuleTemplates resolver p d deps1 deps2 = do
  (ps,xs,_) <- findSourceFiles p d
  (LanguageModule _ _ _ cs0 ps0 ts0 cs1 ps1 ts1 _ _ _) <-
    fmap (createLanguageModule [] [] Map.empty) $ loadModuleGlobals resolver p (PublicNamespace,PrivateNamespace) ps Nothing deps1 deps2
  xs' <- zipWithContents resolver p xs
  ds <- mapErrorsM parseInternalSource xs'
  let ds2 = concat $ map (\(_,_,d2) -> d2) ds
  tm <- foldM includeNewTypes defaultCategories [cs0,cs1,ps0,ps1,ts0,ts1]
  let cs = filter isValueConcrete $ cs1++ps1++ts1
  let ca = Set.fromList $ map getCategoryName $ filter isValueConcrete cs
  let ca' = foldr Set.delete ca $ map dcName ds2
  let testingCats = Set.fromList $ map getCategoryName ts1
  ts <- mapErrorsM (\n -> compileConcreteTemplate (n `Set.member` testingCats) tm n) $ Set.toList ca'
  mapErrorsM_ writeTemplate ts where
  writeTemplate (CxxOutput _ n _ _ _ content) = do
    let n' = p </> d </> n
    exists <- errorFromIO $ doesFileExist n'
    if exists
        then compilerWarningM $ "Skipping existing file " ++ n
        else do
          errorFromIO $ hPutStrLn stderr $ "Writing file " ++ n
          errorFromIO $ writeFile n' $ concat $ map (++ "\n") content

runModuleTests :: (PathIOHandler r, CompilerBackend b) => r -> b -> FilePath ->
  [FilePath] -> LoadedTests -> TrackedErrorsIO [((Int,Int),TrackedErrors ())]
runModuleTests resolver backend base tp (LoadedTests p d m em deps1 deps2) = do
  let paths = base:(cmPublicSubdirs m ++ cmPrivateSubdirs m ++ getIncludePathsForDeps deps1)
  mapErrorsM_ showSkipped $ filter (not . isTestAllowed) $ cmTestFiles m
  ts' <- zipWithContents resolver p $ map (d </>) $ filter isTestAllowed $ cmTestFiles m
  path <- errorFromIO $ canonicalizePath (p </> d)
  cm <- fmap (createLanguageModule [] [] em) $ loadModuleGlobals resolver path (NoNamespace,NoNamespace) [] (Just m) deps1 []
  mapErrorsM (runSingleTest backend cm path paths (m:deps2)) ts' where
    allowTests = Set.fromList tp
    isTestAllowed t = if null allowTests then True else t `Set.member` allowTests
    showSkipped f = compilerWarningM $ "Skipping tests in " ++ f ++ " due to explicit test filter."

loadPrivateSource :: PathIOHandler r => r -> VersionHash -> FilePath -> FilePath -> TrackedErrorsIO (PrivateSource SourceContext)
loadPrivateSource resolver h p f = do
  [f'] <- zipWithContents resolver p [f]
  time <- errorFromIO getZonedTime
  path <- errorFromIO $ canonicalizePath (p </> f)
  let ns = StaticNamespace $ privateNamespace $ show time ++ show h ++ path
  (pragmas,cs,ds) <- parseInternalSource f'
  let cs' = map (setCategoryNamespace ns) cs
  let testing = any isTestsOnly pragmas
  return $ PrivateSource ns testing cs' ds

createLanguageModule :: [CategoryName] -> [CategoryName] -> ExprMap c ->
  [WithVisibility (AnyCategory c)] -> LanguageModule c
createLanguageModule ex ss em cs = lm where
  lm = LanguageModule {
      lmPublicNamespaces  = Set.fromList $ map wvData $ apply ns [with    FromDependency,without ModuleOnly],
      lmPrivateNamespaces = Set.fromList $ map wvData $ apply ns [with    FromDependency,with    ModuleOnly],
      lmLocalNamespaces   = Set.fromList $ map wvData $ apply ns [without FromDependency],
      lmPublicDeps        = map wvData $ apply cs [with    FromDependency,without ModuleOnly,without TestsOnly],
      lmPrivateDeps       = map wvData $ apply cs [with    FromDependency,with    ModuleOnly,without TestsOnly],
      lmTestingDeps       = map wvData $ apply cs [with    FromDependency,with TestsOnly],
      lmPublicLocal       = map wvData $ apply cs [without FromDependency,without ModuleOnly,without TestsOnly],
      lmPrivateLocal      = map wvData $ apply cs [without FromDependency,with    ModuleOnly,without TestsOnly],
      lmTestingLocal      = map wvData $ apply cs [without FromDependency,with TestsOnly],
      lmExternal = ex,
      lmStreamlined = ss,
      lmExprMap  = em
    }
  ns = map (mapCodeVisibility getCategoryNamespace) cs
  with    v = hasCodeVisibility v
  without v = not . hasCodeVisibility v
  apply = foldr filter

warnPublic :: PathIOHandler r => r -> FilePath -> [CategoryName] ->
  [CategoryName] -> [ObjectFile] -> [FilePath] -> TrackedErrorsIO ()
warnPublic resolver p pc dc os = mapErrorsM_ checkPublic where
  checkPublic d = do
    d2 <- resolveModule resolver p d
    when (not $ d2 `Set.member` neededPublic) $ compilerWarningM $ "Dependency \"" ++ d ++ "\" does not need to be public"
  pc' = Set.fromList pc
  dc' = Set.fromList dc
  neededPublic = Set.fromList $ concat $ map checkDep os
  checkDep (CategoryObjectFile (CategoryIdentifier _ n _) ds _)
    | n `Set.member` pc' = map ciPath $ filter ((`Set.member` dc') . ciCategory) ds
  checkDep _ = []
