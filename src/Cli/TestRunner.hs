{- -----------------------------------------------------------------------------
Copyright 2020-2021,2023 Kevin P. Barry

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

module Cli.TestRunner (
  runSingleTest,
) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.List (intercalate,isSuffixOf,nub,sort)
import System.Directory
import System.IO
import System.Posix.Temp (mkdtemp)
import System.FilePath
import Text.Regex.TDFA
import qualified Data.Map as Map

import Base.CompilerError
import Base.TrackedErrors
import Cli.Programs
import CompilerCxx.CxxFiles
import CompilerCxx.LanguageModule
import CompilerCxx.Naming
import Module.CompileMetadata
import Module.Paths
import Module.ProcessMetadata
import Parser.SourceFile
import Parser.TextParser (SourceContext)
import Types.IntegrationTest
import Types.Procedure
import Types.TypeCategory


runSingleTest :: CompilerBackend b => b -> FilePath ->
  LanguageModule SourceContext -> [FilePath] -> [CompileMetadata] ->
  (String,String) -> TrackedErrorsIO ((Int,Int),TrackedErrors ())
runSingleTest b cl cm paths deps (f,s) = do
  errorFromIO $ hPutStrLn stderr $ "\nExecuting tests from " ++ f
  allResults <- checkAndRun (parseTestSource (f,s))
  return $ second (("\nIn test file " ++ f) ??>) allResults where
    checkAndRun ts
      | isCompilerError ts = do
        errorFromIO $ hPutStrLn stderr $ "Failed to parse tests in " ++ f
        return ((0,1),ts >> return ())
      | otherwise = do
          let (_,ts') = getCompilerSuccess ts
          allResults <- mapCompilerM runSingle ts'
          let passed = sum $ map (fst . fst) allResults
          let failed = sum $ map (snd . fst) allResults
          let result = collectAllM_ $ map snd allResults
          return ((passed,failed),result)

    runSingle t = do
      let name = "\"" ++ ithTestName (itHeader t) ++ "\" (from " ++ f ++ ")"
      let context = formatFullContextBrace (ithContext $ itHeader t)
      let scope = "\nIn testcase \"" ++ ithTestName (itHeader t) ++ "\"" ++ context
      warnUnused (itHeader t) <?? scope
      errorFromIO $ hPutStrLn stderr $ "\n*** Executing testcase " ++ name ++ " ***"
      result <- toTrackedErrors $ run (ithResult $ itHeader t)
                                      (ithArgs $ itHeader t)
                                      (ithTimeout $ itHeader t)
                                      (itCategory t) (itDefinition t) (itTests t)
      if isCompilerError result
         then return ((0,1),scope ??> (result >> return ()))
         else do
           let allResults = getCompilerSuccess result
           let passed = length $ filter (not . isCompilerError) allResults
           let failed = length $ filter isCompilerError allResults
           let combined = scope ??> collectAllM_ allResults
           if failed > 0
             then errorFromIO $ hPutStrLn stderr $ "*** Some tests in testcase " ++ name ++ " failed ***"
             else errorFromIO $ hPutStrLn stderr $ "*** All tests in testcase " ++ name ++ " passed ***"
           return ((passed,failed),combined)

    warnUnused (IntegrationTestHeader _ _ args timeout ex) = check where
      check =
        case ex of
             (ExpectCompilerError _ _ _) -> do
               warnArgs    "error"
               warnTimeout "error"
             (ExpectCompiles _ _ _) -> do
               warnArgs    "compiles"
               warnTimeout "compiles"
             _ -> return ()
      warnArgs ex2 =
        case args of
             [] -> return ()
             _  -> compilerWarningM $ "Explicit args are ignored in " ++ ex2 ++ " tests: " ++ intercalate ", " (map show args)
      warnTimeout ex2 =
        case timeout of
             Nothing -> return ()
             Just  t -> compilerWarningM $ "Explicit timeouts are ignored in " ++ ex2 ++ " tests: " ++ show t

    run (ExpectCompilerError _ rs es) _ _ cs ds ts = do
      result <- toTrackedErrors $ compileAll Nothing [] cs ds ts
      if not $ isCompilerError result
         then compilerErrorM "Expected compilation failure"
         else fmap (:[]) $ return $ do
           let warnings = show $ getCompilerWarnings result
           let errors   = show $ getCompilerError result
           checkContent rs es (lines warnings ++ lines errors) [] []

    run (ExpectCompiles _ rs es) _ _ cs ds ts = do
      result <- toTrackedErrors $ compileAll Nothing [] cs ds ts
      if isCompilerError result
         then fromTrackedErrors result >> return []
         else fmap (:[]) $ return $ do
           let warnings = show $ getCompilerWarnings result
           checkContent rs es (lines warnings) [] []

    run (ExpectRuntimeError _ t rs es) args timeout cs ds ts = do
      when (length ts /= 1) $ compilerErrorM "Exactly one unittest is required when failure is expected"
      uniqueTestNames ts
      execute False t rs es args timeout cs ds ts

    run (ExpectRuntimeSuccess _ t rs es) args timeout cs ds ts = do
      when (null ts) $ compilerErrorM "At least one unittest is required when success is expected"
      uniqueTestNames ts
      execute True t rs es args timeout cs ds ts

    checkContent rs es comp err out = do
      let cr = checkRequired rs comp err out
      let ce = checkExcluded es comp err out
      let compError = if null comp
                         then return ()
                         else (mapCompilerM_ compilerErrorM comp) <?? "\nOutput from compiler:"
      let errError = if null err
                        then return ()
                        else (mapCompilerM_ compilerErrorM err) <?? "\nOutput to stderr from test:"
      let outError = if null out
                        then return ()
                        else (mapCompilerM_ compilerErrorM out) <?? "\nOutput to stdout from test:"
      if isCompilerError cr || isCompilerError ce
         then collectAllM_ [cr,ce,compError,errError,outError]
         else collectAllM_ [cr,ce]

    uniqueTestNames ts = do
      let ts' = Map.fromListWith (++) $ map (\t -> (tpName t,[t])) ts
      mapCompilerM_ testClash $ Map.toList ts'
    testClash (_,[_]) = return ()
    testClash (n,ts) = "unittest " ++ show n ++ " is defined multiple times" !!>
      (mapCompilerM_ (compilerErrorM . ("Defined at " ++) . formatFullContext) $ sort $ map tpContext ts)

    execute s2 t rs es args timeout cs ds ts = do
      result <- toTrackedErrors $ compileAll t args cs ds ts
      if isCompilerError result
         then return [result >> return ()]
         else do
           let (xx,main,fs) = getCompilerSuccess result
           (dir,binaryName) <- createBinary main timeout xx
           results <- liftIO $ sequence $ map (toTrackedErrors . executeTest binaryName rs es result s2) fs
           when (not $ any isCompilerError results) $ errorFromIO $ removeDirectoryRecursive dir
           return results

    executeTest binary rs es res s2 (f2,c) = printOutcome $ context ??> do
      let command = TestCommand binary (takeDirectory binary) [show f2,cl]
      resetBackgroundM $ do
        compilerBackgroundM $ "See output files for testcase in " ++ (takeDirectory binary)
        (TestCommandResult s2' out err) <- runTestCommand b command
        case (s2,s2') of
             (True,False) -> collectAllM_ $ (asCompilerError res):(map compilerErrorM $ err ++ out)
             (False,True) -> collectAllM_ [compilerErrorM "Expected runtime failure",
                                           asCompilerError res <?? "\nOutput from compiler:"]
             _ -> fromTrackedErrors $ checkContent rs es (lines $ show $ getCompilerWarnings res) err out
      where
        context = "\nIn unittest " ++ show f2 ++ formatFullContextBrace c
        printOutcome outcome =
          ifElseSuccessT outcome
            (hPutStrLn stderr $ "--- unittest " ++ show f2 ++ " passed ---")
            (hPutStrLn stderr $ "--- unittest " ++ show f2 ++ " failed ---")

    compileAll t args cs ds ts = do
      let ns1 = StaticNamespace $ privateNamespace s
      let cs' = map (setCategoryNamespace ns1) cs
      fromTrackedErrors $ compileTestsModule cm ns1 args t cs' ds ts

    checkRequired rs comp err out = mapCompilerM_ (checkSubsetForRegex True  comp err out) rs
    checkExcluded es comp err out = mapCompilerM_ (checkSubsetForRegex False comp err out) es
    checkSubsetForRegex expected comp err out (OutputPattern OutputAny r) =
      checkForRegex expected (comp ++ err ++ out) r "compiler output or test output"
    checkSubsetForRegex expected comp _ _ (OutputPattern OutputCompiler r) =
      checkForRegex expected comp r "compiler output"
    checkSubsetForRegex expected _ err _ (OutputPattern OutputStderr r) =
      checkForRegex expected err r "test stderr"
    checkSubsetForRegex expected _ _ out (OutputPattern OutputStdout r) =
      checkForRegex expected out r "test stdout"
    checkForRegex :: Bool -> [String] -> String -> String -> TrackedErrors ()
    checkForRegex expected ms r n = do
      let found = any (=~ r) ms
      when (found && not expected) $ compilerErrorM $ "Pattern \"" ++ r ++ "\" present in " ++ n
      when (not found && expected) $ compilerErrorM $ "Pattern \"" ++ r ++ "\" missing from " ++ n
    createBinary (CxxOutput _ f2 _ _ _ _ content) timeout xx = do
      dir <- errorFromIO $ mkdtemp "/tmp/ztest_"
      errorFromIO $ hPutStrLn stderr $ "Writing temporary files to " ++ dir
      sources <- mapCompilerM (writeSingleFile dir) xx
      let main   = dir </> f2
      let binary = dir </> "testcase"
      errorFromIO $ writeFile main $ concat $ map (++ "\n") content
      let flags = getLinkFlagsForDeps deps
      let paths' = nub $ map fixPath (dir:paths)
      let libraries = getLibrariesForDeps deps
      macro <- timeoutMacro timeout
      let command = CompileToBinary main (concat (map fst sources) ++ libraries) macro binary paths' flags
      file <- syncCxxCommand b command
      return (dir,file)
    timeoutMacro (Just 0) = return []  -- No timeout.
    timeoutMacro Nothing  = return [(testTimeoutMacro,Just "30")]  -- Default timeout.
    timeoutMacro (Just t)
      | t < 0 || t > 65535 = compilerErrorM $ "Invalid testcase timeout " ++ show t ++ " => use timeout 0 for unlimited time"
      | otherwise = return [(testTimeoutMacro,Just (show t))]  -- Custom timeout.
    writeSingleFile d ca@(CxxOutput _ f2 _ _ _ _ content) = do
      errorFromIO $ writeFile (d </> f2) $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f2
         then return ([d </> f2],ca)
         else return ([],ca)
