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

import Control.Monad (when)
import Data.Either (partitionEithers)
import Data.List (isSuffixOf,nub,sort)
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Temp (mkstemps)
import System.IO
import Text.Parsec (SourcePos)
import qualified Data.Set as Set

import Base.CompileError
import Cli.CompileMetadata
import Cli.CompileOptions
import Cli.ProcessMetadata
import Cli.TestRunner -- Not safe, due to Text.Regex.TDFA.
import Compilation.CompileInfo
import CompilerCxx.Category
import CompilerCxx.Naming
import Config.LoadConfig
import Config.Paths
import Config.Programs
import Parser.SourceFile
import Types.Builtin
import Types.DefinedCategory
import Types.TypeCategory
import Types.TypeInstance


data ModuleSpec =
  ModuleSpec {
    msRoot :: FilePath,
    msPath :: FilePath,
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
    ltPublicDeps :: [CompileMetadata],
    ltPrivateDeps :: [CompileMetadata]
  }
  deriving (Show)

compileModule :: ModuleSpec -> IO ()
compileModule (ModuleSpec p d is is2 ps xs ts es ep m f) = do
  (backend,resolver) <- loadConfig
  let hash = getCompilerHash backend
  as  <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is
  as2 <- fmap fixPaths $ sequence $ map (resolveModule resolver (p </> d)) is2
  (fr,deps) <- loadPublicDeps hash (as ++ as2)
  checkAllowedStale fr f
  base <- resolveBaseModule resolver
  actual <- resolveModule resolver p d
  isBase <- isBaseModule resolver actual
  -- Lazy dependency loading, in case we're compiling base.
  deps2 <- if isBase
              then return deps
              else do
                (fr2,bpDeps) <- loadPublicDeps hash [base]
                checkAllowedStale fr2 f
                return $ bpDeps ++ deps
  let ss = fixPaths $ getSourceFilesForDeps deps2
  ss' <- zipWithContents p ss
  ps' <- zipWithContents p ps
  xs' <- zipWithContents p xs
  ns0 <- canonicalizePath (p </> d) >>= return . StaticNamespace . publicNamespace
  let ns2 = map StaticNamespace $ filter (not . null) $ getNamespacesForDeps deps
  let fs = compileAll ns0 ns2 ss' ps' xs'
  when (isCompileError fs) $ do
    formatWarnings fs
    hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError fs)
    hPutStrLn stderr $ "Zeolite compilation failed."
    exitFailure
  formatWarnings fs
  let (pc,mf,fs') = getCompileSuccess fs
  let ps2 = map takeFileName ps
  let xs2 = map takeFileName xs
  let ts2 = map takeFileName ts
  let paths = map (\ns -> getCachedPath (p </> d) ns "") $ nub $ filter (not . null) $ map show $ [ns0] ++ map coNamespace fs'
  paths' <- sequence $ map canonicalizePath paths
  s0 <- canonicalizePath $ getCachedPath (p </> d) (show ns0) ""
  let paths2 = base:(getIncludePathsForDeps deps2) ++ ep' ++ paths'
  let hxx   = filter (isSuffixOf ".hpp" . coFilename)       fs'
  let other = filter (not . isSuffixOf ".hpp" . coFilename) fs'
  os1 <- sequence $ map (writeOutputFile backend (show ns0) paths2) $ hxx ++ other
  let files = map (\f2 -> getCachedPath (p </> d) (show $ coNamespace f2) (coFilename f2)) fs' ++
              map (\f2 -> p </> getSourceFile f2) es
  files' <- sequence $ map checkOwnedFile files
  os2 <- fmap concat $ sequence $ map (compileExtraSource backend (show ns0) paths2) es
  let (hxx',cxx,os') = sortCompiledFiles files'
  let (osCat,osOther) = partitionEithers os2
  path <- canonicalizePath $ p </> d
  let os1' = resolveObjectDeps path (os1 ++ osCat) deps2
  let cm = CompileMetadata {
      cmVersionHash = hash,
      cmPath = path,
      cmNamespace = show ns0,
      cmPublicDeps = as,
      cmPrivateDeps = as2,
      cmCategories = sort $ map show pc,
      cmSubdirs = [s0],
      cmPublicFiles = sort ps2,
      cmPrivateFiles = sort xs2,
      cmTestFiles = sort ts2,
      cmHxxFiles = sort hxx',
      cmCxxFiles = sort cxx,
      cmLinkFlags = getLinkFlags m,
      cmObjectFiles = os1' ++ osOther ++ map OtherObjectFile os'
    }
  writeMetadata (p </> d) cm
  createBinary backend resolver paths' (cm:deps) m mf
  hPutStrLn stderr $ "Zeolite compilation succeeded." where
    ep' = fixPaths $ map (p </>) ep
    writeOutputFile b ns0 paths ca@(CxxOutput _ f2 ns _ _ content) = do
      hPutStrLn stderr $ "Writing file " ++ f2
      writeCachedFile (p </> d) (show ns) f2 $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2
         then do
           let f2' = getCachedPath (p </> d) (show ns) f2
           let p0 = getCachedPath (p </> d) "" ""
           let p1 = getCachedPath (p </> d) (show ns) ""
           createCachePath (p </> d)
           let ns' = if isStaticNamespace ns then show ns else show ns0
           let command = CompileToObject f2' (getCachedPath (p </> d) ns' "") dynamicNamespaceName "" (p0:p1:paths) False
           o2 <- runCxxCommand b command
           return $ ([o2],ca)
         else return ([],ca)
    compileExtraSource b ns0 paths (CategorySource f2 cs ds2) = do
      f2' <- compileExtraFile False b ns0 paths f2
      let ds2' = nub $ cs ++ ds2
      case f2' of
           Nothing -> return []
           Just o  -> return $ map (\c -> Left $ ([o],fakeCxxForSource ns0 ds2' c)) cs
    compileExtraSource b ns0 paths (OtherSource f2) = do
      f2' <- compileExtraFile True b ns0 paths f2
      case f2' of
           Just o  -> return [Right $ OtherObjectFile o]
           Nothing -> return []
    fakeCxxForSource ns ds2 c = CxxOutput {
        coCategory = Just (CategoryName c),
        coFilename = "",
        coNamespace = ns',
        coUsesNamespace = [ns'],
        coUsesCategory = map CategoryName ds2,
        coOutput = []
      } where
        ns' = if null ns then NoNamespace else StaticNamespace ns
    checkOwnedFile f2 = do
      exists <- doesFileExist f2
      when (not exists) $ do
        hPutStrLn stderr $ "Owned file " ++ f2 ++ " does not exist."
        hPutStrLn stderr $ "Zeolite compilation failed."
        exitFailure
      canonicalizePath f2
    compileExtraFile e b ns0 paths f2
      | isSuffixOf ".cpp" f2 || isSuffixOf ".cc" f2 = do
          let f2' = p </> f2
          createCachePath (p </> d)
          let command = CompileToObject f2' (getCachedPath (p </> d) "" "") dynamicNamespaceName ns0 paths e
          fmap Just $ runCxxCommand b command
      | isSuffixOf ".a" f2 || isSuffixOf ".o" f2 = return (Just f2)
      | otherwise = return Nothing
    compileAll ns0 ns2 is3 cs ds2 = do
      tm1 <- addIncludes defaultCategories is3
      cs' <- collectAllOrErrorM $ map parsePublicSource cs
      let cs'' = map (setCategoryNamespace ns0) (concat $ map snd cs')
      xa <- collectAllOrErrorM $ map parsePrivate ds2
      let cm = CategoryModule {
          cnBase = tm1,
          cnNamespaces = ns0:ns2,
          cnPublic = cs'',
          cnPrivate = xa,
          cnExternal = concat $ map getSourceCategories es
        }
      xx <- compileCategoryModule cm
      let pc = map getCategoryName cs''
      ms <- maybeCreateMain cm m
      return (pc,ms,xx)
    parsePrivate ds = do
      let ns1 = StaticNamespace $ privateNamespace (p </> fst ds)
      (_,cs,ds') <- parseInternalSource ds
      let cs' = map (setCategoryNamespace ns1) cs
      return $ PrivateSource ns1 cs' ds'
    createBinary b r paths deps (CompileBinary n _ o lf) ms
      | length ms > 1 = do
        hPutStrLn stderr $ "Multiple matches for main category " ++ n ++ "."
        exitFailure
      | length ms == 0 = do
        hPutStrLn stderr $ "Main category " ++ n ++ " not found."
        exitFailure
      | otherwise = do
          f0 <- if null o
                   then canonicalizePath $ p </> d </> n
                   else canonicalizePath $ p </> d </> o
          let (CxxOutput _ _ _ ns2 req content) = head ms
          -- TODO: Create a helper or a constant or something.
          (o',h) <- mkstemps "/tmp/zmain_" ".cpp"
          hPutStr h $ concat $ map (++ "\n") content
          hClose h
          base <- resolveBaseModule r
          (_,bpDeps) <- loadPublicDeps (getCompilerHash b) [base]
          (_,deps2) <- loadPrivateDeps (getCompilerHash b) (bpDeps ++ deps)
          let lf' = lf ++ getLinkFlagsForDeps deps2
          let paths' = fixPaths $ paths ++ base:(getIncludePathsForDeps deps2)
          let os    = getObjectFilesForDeps deps2
          let ofr = getObjectFileResolver os
          let os' = ofr ns2 req
          let command = CompileToBinary o' os' f0 paths' lf'
          hPutStrLn stderr $ "Creating binary " ++ f0
          _ <- runCxxCommand b command
          removeFile o'
    createBinary _ _ _ _ _ _ = return ()
    maybeCreateMain cm (CompileBinary n f2 _ _) =
      fmap (:[]) $ compileModuleMain cm (CategoryName n) (FunctionName f2)
    maybeCreateMain _ _ = return []

createModuleTemplates :: FilePath -> FilePath -> [CompileMetadata] -> IO ()
createModuleTemplates p d deps = do
  (ps,xs,_) <- findSourceFiles p d
  let ss = fixPaths $ getSourceFilesForDeps deps
  ps' <- zipWithContents p ps
  xs' <- zipWithContents p xs
  ss' <- zipWithContents p ss
  let ts = createTemplates ss' ps' xs' :: CompileInfo [CxxOutput]
  if isCompileError ts
      then do
        formatWarnings ts
        hPutStr stderr $ "Compiler errors:\n" ++ (show $ getCompileError ts)
        hPutStrLn stderr $ "Zeolite compilation failed."
        exitFailure
      else do
        formatWarnings ts
        sequence_ $ map writeTemplate $ getCompileSuccess ts where
  createTemplates is cs ds2 = do
    tm <- addIncludes defaultCategories is
    cs' <- collectAllOrErrorM $ map parsePublicSource cs
    let cs'' = map (setCategoryNamespace DynamicNamespace) (concat $ map snd cs')
    tm2 <- includeNewTypes tm cs''
    da <- collectAllOrErrorM $ map parseInternalSource ds2
    let ds2' = concat $ map (\(_,_,d2) -> d2) da
    let cs2 = concat $ map (\(_,c,_) -> c) da
    tm3 <- includeNewTypes tm2 cs2
    let ca = Set.fromList $ map getCategoryName $ filter isValueConcrete (concat $ map snd cs')
    let ca' = foldr Set.delete ca $ map dcName ds2'
    collectAllOrErrorM $ map (compileConcreteTemplate tm3) $ Set.toList ca'
  writeTemplate (CxxOutput _ n _ _ _ content) = do
    let n' = p </> d </> n
    exists <- doesFileExist n'
    if exists
        then hPutStrLn stderr $ "Skipping existing file " ++ n
        else do
          hPutStrLn stderr $ "Writing file " ++ n
          writeFile n' $ concat $ map (++ "\n") content

runModuleTests :: CompilerBackend b => b -> FilePath -> [FilePath] -> LoadedTests -> IO [((Int,Int),CompileInfo ())]
runModuleTests b base tp (LoadedTests p d m deps1 deps2) = do
  let paths = base:(getIncludePathsForDeps deps1)
  let ss = fixPaths $ getSourceFilesForDeps deps1
  let os = getObjectFilesForDeps deps2
  ss' <- zipWithContents p ss
  mapM_ showSkipped $ filter (not . isTestAllowed) $ cmTestFiles m
  ts' <- zipWithContents p $ map (d </>) $ filter isTestAllowed $ cmTestFiles m
  tm <- return $ do
    cs <- collectAllOrErrorM $ map parsePublicSource ss'
    includeNewTypes defaultCategories (concat $ map snd cs)
  if isCompileError tm
      then return [((0,0),tm >> return ())]
      else sequence $ map (runSingleTest b paths (m:deps1) os (getCompileSuccess tm)) ts' where
    allowTests = Set.fromList tp
    isTestAllowed t = if null allowTests then True else t `Set.member` allowTests
    showSkipped f = do
      hPutStrLn stderr $ "Skipping tests in " ++ f ++ " due to explicit test filter."

formatWarnings :: CompileInfo a -> IO ()
formatWarnings c
  | null $ getCompileWarnings c = return ()
  | otherwise = hPutStr stderr $ "Compiler warnings:\n" ++ (concat $ map (++ "\n") (getCompileWarnings c))

addIncludes :: CategoryMap SourcePos -> [(FilePath,String)] -> CompileInfo (CategoryMap SourcePos)
addIncludes tm fs = do
  cs <- collectAllOrErrorM $ map parsePublicSource fs
  includeNewTypes tm (concat $ map snd cs)

zipWithContents :: FilePath -> [FilePath] -> IO [(FilePath,String)]
zipWithContents p fs = fmap (zip $ map fixPath fs) $ sequence $ map (readFile . (p </>)) fs
