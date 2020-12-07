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

module Cli.TestRunner (
  runSingleTest,
) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.List (isSuffixOf,nub,sort)
import System.Directory
import System.IO
import System.Posix.Temp (mkdtemp)
import System.FilePath
import Text.Parsec
import Text.Regex.TDFA -- Not safe!
import qualified Data.Map as Map

import Base.CompilerError
import Base.TrackedErrors
import Cli.Programs
import CompilerCxx.Category
import CompilerCxx.Naming
import Module.CompileMetadata
import Module.Paths
import Module.ProcessMetadata
import Parser.SourceFile
import Types.IntegrationTest
import Types.Procedure
import Types.TypeCategory


runSingleTest :: CompilerBackend b => b -> LanguageModule SourcePos ->
  FilePath -> [FilePath] -> [CompileMetadata] -> (String,String) ->
  TrackedErrorsIO ((Int,Int),TrackedErrors ())
runSingleTest b cm p paths deps (f,s) = do
  errorFromIO $ hPutStrLn stderr $ "\nExecuting tests from " ++ f
  allResults <- checkAndRun (parseTestSource (f,s))
  return $ second (("\nIn test file " ++ f) ??>) allResults where
    checkAndRun ts
      | isCompilerError ts = do
        errorFromIO $ hPutStrLn stderr $ "Failed to parse tests in " ++ f
        return ((0,1),ts >> return ())
      | otherwise = do
          let (_,ts') = getCompilerSuccess ts
          allResults <- mapErrorsM runSingle ts'
          let passed = sum $ map (fst . fst) allResults
          let failed = sum $ map (snd . fst) allResults
          let result = collectAllM_ $ map snd allResults
          return ((passed,failed),result)

    runSingle t = do
      let name = "\"" ++ ithTestName (itHeader t) ++ "\" (from " ++ f ++ ")"
      let context = formatFullContextBrace (ithContext $ itHeader t)
      let scope = "\nIn testcase \"" ++ ithTestName (itHeader t) ++ "\"" ++ context
      errorFromIO $ hPutStrLn stderr $ "\n*** Executing testcase " ++ name ++ " ***"
      result <- toTrackedErrors $ run (ithResult $ itHeader t) (ithArgs $ itHeader t) (itCategory t) (itDefinition t) (itTests t)
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

    run (ExpectCompilerError _ rs es) args cs ds ts = do
      let result = compileAll args cs ds ts
      if not $ isCompilerError result
         then compilerErrorM "Expected compilation failure"
         else fmap (:[]) $ return $ do
           let warnings = show $ getCompileWarnings result
           let errors   = show $ getCompilerError result
           checkContent rs es (lines warnings ++ lines errors) [] []

    run (ExpectCompiles _ rs es) args cs ds ts = do
      let result = compileAll args cs ds ts
      if isCompilerError result
         then fromTrackedErrors result >> return []
         else fmap (:[]) $ return $ do
           let warnings = show $ getCompileWarnings result
           checkContent rs es (lines warnings) [] []

    run (ExpectRuntimeError _ rs es) args cs ds ts = do
      when (length ts /= 1) $ compilerErrorM "Exactly one unittest is required when crash is expected"
      uniqueTestNames ts
      execute False rs es args cs ds ts

    run (ExpectRuntimeSuccess _ rs es) args cs ds ts = do
      when (null ts) $ compilerErrorM "At least one unittest is required when success is expected"
      uniqueTestNames ts
      execute True rs es args cs ds ts

    checkContent rs es comp err out = do
      let cr = checkRequired rs comp err out
      let ce = checkExcluded es comp err out
      let compError = if null comp
                         then return ()
                         else (mapErrorsM_ compilerErrorM comp) <?? "\nOutput from compiler:"
      let errError = if null err
                        then return ()
                        else (mapErrorsM_ compilerErrorM err) <?? "\nOutput to stderr from test:"
      let outError = if null out
                        then return ()
                        else (mapErrorsM_ compilerErrorM out) <?? "\nOutput to stdout from test:"
      if isCompilerError cr || isCompilerError ce
         then collectAllM_ [cr,ce,compError,errError,outError]
         else collectAllM_ [cr,ce]

    uniqueTestNames ts = do
      let ts' = Map.fromListWith (++) $ map (\t -> (tpName t,[t])) ts
      mapErrorsM_ testClash $ Map.toList ts'
    testClash (_,[_]) = return ()
    testClash (n,ts) = "unittest " ++ show n ++ " is defined multiple times" !!>
      (mapErrorsM_ (compilerErrorM . ("Defined at " ++) . formatFullContext) $ sort $ map tpContext ts)

    execute s2 rs es args cs ds ts = do
      let result = compileAll args cs ds ts
      if isCompilerError result
         then return [result >> return ()]
         else do
           let (xx,main,fs) = getCompilerSuccess result
           (dir,binaryName) <- createBinary main xx
           results <- liftIO $ sequence $ map (toTrackedErrors . executeTest binaryName rs es result s2) fs
           when (not $ any isCompilerError results) $ errorFromIO $ removeDirectoryRecursive dir
           return results

    executeTest binary rs es res s2 (f2,c) = printOutcome $ "\nIn unittest " ++ show f2 ++ formatFullContextBrace c ??> do
      let command = TestCommand binary (takeDirectory binary) [show f2]
      (TestCommandResult s2' out err) <- runTestCommand b command
      case (s2,s2') of
           (True,False) -> collectAllM_ $ (asCompilerError res):(map compilerErrorM $ err ++ out)
           (False,True) -> collectAllM_ [compilerErrorM "Expected runtime failure",
                                         asCompilerError res <?? "\nOutput from compiler:"]
           _ -> fromTrackedErrors $ checkContent rs es (lines $ show $ getCompileWarnings res) err out
      where
        printOutcome outcome = do
          failed <- isCompilerErrorM outcome
          if failed
             then errorFromIO $ hPutStrLn stderr $ "--- unittest " ++ show f2 ++ " failed ---"
             else errorFromIO $ hPutStrLn stderr $ "--- unittest " ++ show f2 ++ " passed ---"
          outcome

    compileAll args cs ds ts = do
      let ns1 = StaticNamespace $ privateNamespace s
      let cs' = map (setCategoryNamespace ns1) cs
      compileTestsModule cm ns1 args cs' ds ts

    checkRequired rs comp err out = mapErrorsM_ (checkSubsetForRegex True  comp err out) rs
    checkExcluded es comp err out = mapErrorsM_ (checkSubsetForRegex False comp err out) es
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
    createBinary (CxxOutput _ f2 _ ns req content) xx = do
      dir <- errorFromIO $ mkdtemp "/tmp/ztest_"
      errorFromIO $ hPutStrLn stderr $ "Writing temporary files to " ++ dir
      sources <- mapErrorsM (writeSingleFile dir) xx
      -- TODO: Cache CompileMetadata here for debugging failures.
      let sources' = resolveObjectDeps deps p dir sources
      let main   = dir </> f2
      let binary = dir </> "testcase"
      errorFromIO $ writeFile main $ concat $ map (++ "\n") content
      let flags = getLinkFlagsForDeps deps
      let paths' = nub $ map fixPath (dir:paths)
      let os  = getObjectFilesForDeps deps
      let ofr = getObjectFileResolver (sources' ++ os)
      let os' = ofr ns req
      let command = CompileToBinary main os' binary paths' flags
      file <- runCxxCommand b command
      return (dir,file)
    writeSingleFile d ca@(CxxOutput _ f2 _ _ _ content) = do
      errorFromIO $ writeFile (d </> f2) $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f2
         then return ([d </> f2],ca)
         else return ([],ca)
