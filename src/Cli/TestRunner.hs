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
import Data.List (isSuffixOf,nub)
import System.Directory
import System.IO
import System.Posix.Temp (mkdtemp)
import System.FilePath
import Text.Parsec
import Text.Regex.TDFA -- Not safe!

import Base.CompileError
import Base.Mergeable
import Cli.CompileMetadata
import Cli.ProcessMetadata
import Compilation.CompileInfo
import CompilerCxx.Category
import CompilerCxx.Naming
import Config.Programs
import Parser.SourceFile
import Types.IntegrationTest
import Types.TypeCategory


runSingleTest :: CompilerBackend b => b -> LanguageModule SourcePos ->
  [FilePath] -> [CompileMetadata] -> (String,String) -> IO ((Int,Int),CompileInfo ())
runSingleTest b cm paths deps (f,s) = do
  hPutStrLn stderr $ "\nExecuting tests from " ++ f
  allResults <- checkAndRun (parseTestSource (f,s))
  return $ second (flip reviseError $ "\nIn test file " ++ f) allResults where
    checkAndRun ts
      | isCompileError ts = do
        hPutStrLn stderr $ "Failed to parse tests in " ++ f
        return ((0,1),ts >> return ())
      | otherwise = do
          let (_,ts') = getCompileSuccess ts
          allResults <- sequence $ map runSingle ts'
          let passed = length $ filter (not . isCompileError) allResults
          let failed = length $ filter isCompileError allResults
          return ((passed,failed),mergeAllM allResults)
    runSingle t = do
      let name = ithTestName $ itHeader t
      let context = formatFullContextBrace (ithContext $ itHeader t)
      hPutStrLn stderr $ "\n*** Executing test \"" ++ name ++ "\" ***"
      outcome <- fmap (flip reviseError ("\nIn test \"" ++ name ++ "\"" ++ context)) $
                   run (ithResult $ itHeader t) (itCategory t) (itDefinition t)
      if isCompileError outcome
         then hPutStrLn stderr $ "*** Test \"" ++ name ++ "\" failed ***"
         else hPutStrLn stderr $ "*** Test \"" ++ name ++ "\" passed ***"
      return outcome

    run (ExpectCompileError _ rs es) cs ds = do
      let result = compileAll Nothing cs ds
      if not $ isCompileError result
         then undefined  -- Should be caught in compileAll.
         else return $ do
           let warnings = getCompileWarnings result
           let errors = show $ getCompileError result
           checkContent rs es (warnings ++ lines errors) [] []

    run (ExpectRuntimeError   _ e rs es) cs ds = execute False e rs es cs ds
    run (ExpectRuntimeSuccess _ e rs es) cs ds = execute True  e rs es cs ds

    checkContent rs es comp err out = do
      let cr = checkRequired rs comp err out
      let ce = checkExcluded es comp err out
      let compError = if null comp
                         then return ()
                         else (mergeAllM $ map compileError comp) `reviseError` "\nOutput from compiler:"
      let errError = if null err
                        then return ()
                        else (mergeAllM $ map compileError err) `reviseError` "\nOutput to stderr from test:"
      let outError = if null out
                        then return ()
                        else (mergeAllM $ map compileError out) `reviseError` "\nOutput to stdout from test:"
      if isCompileError cr || isCompileError ce
         then mergeAllM [cr,ce,compError,errError,outError]
         else mergeAllM [cr,ce]

    execute s2 e rs es cs ds = do
      let result = compileAll (Just e) cs ds
      if isCompileError result
         then return $ result >> return ()
         else do
           let warnings = getCompileWarnings result
           let (xx,main) = getCompileSuccess result
           (dir,binaryName) <- createBinary main xx
           let command = TestCommand binaryName (takeDirectory binaryName)
           (TestCommandResult s2' out err) <- runTestCommand b command
           case (s2,s2') of
                (True,False) -> return $ mergeAllM $ map compileError $ warnings ++ err ++ out
                (False,True) -> return $ compileError "Expected runtime failure"
                _ -> do
                  let result2 = checkContent rs es warnings err out
                  when (not $ isCompileError result) $ removeDirectoryRecursive dir
                  return result2

    compileAll e cs ds = do
      let ns1 = StaticNamespace $ privateNamespace s
      let cs' = map (setCategoryNamespace ns1) cs
      let xs = PrivateSource {
          psNamespace = ns1,
          psTesting = True,
          psCategory = cs',
          psDefine = ds
        }
      xx <- compileLanguageModule cm [xs]
      main <- case e of
                   Just e2 -> compileTestMain cm xs e2
                   Nothing -> compileError "Expected compiler error"
      return (xx,main)

    checkRequired rs comp err out = mergeAllM $ map (checkSubsetForRegex True  comp err out) rs
    checkExcluded es comp err out = mergeAllM $ map (checkSubsetForRegex False comp err out) es
    checkSubsetForRegex expected comp err out (OutputPattern OutputAny r) =
      checkForRegex expected (comp ++ err ++ out) r "compiler output or test output"
    checkSubsetForRegex expected comp _ _ (OutputPattern OutputCompiler r) =
      checkForRegex expected comp r "compiler output"
    checkSubsetForRegex expected _ err _ (OutputPattern OutputStderr r) =
      checkForRegex expected err r "test stderr"
    checkSubsetForRegex expected _ _ out (OutputPattern OutputStdout r) =
      checkForRegex expected out r "test stdout"
    checkForRegex :: Bool -> [String] -> String -> String -> CompileInfo ()
    checkForRegex expected ms r n = do
      let found = any (=~ r) ms
      when (found && not expected) $ compileError $ "Pattern \"" ++ r ++ "\" present in " ++ n
      when (not found && expected) $ compileError $ "Pattern \"" ++ r ++ "\" missing from " ++ n
    createBinary (CxxOutput _ f2 _ ns req content) xx = do
      dir <- mkdtemp "/tmp/ztest_"
      hPutStrLn stderr $ "Writing temporary files to " ++ dir
      sources <- sequence $ map (writeSingleFile dir) xx
      -- TODO: Cache CompileMetadata here for debugging failures.
      let sources' = resolveObjectDeps deps dir sources
      let main   = dir </> f2
      let binary = dir </> "testcase"
      writeFile main $ concat $ map (++ "\n") content
      let flags = getLinkFlagsForDeps deps
      let paths' = nub $ map fixPath (dir:paths)
      let os  = getObjectFilesForDeps deps
      let ofr = getObjectFileResolver (sources' ++ os)
      let os' = ofr ns req
      let command = CompileToBinary main os' binary paths' flags
      file <- runCxxCommand b command
      return (dir,file)
    writeSingleFile d ca@(CxxOutput _ f2 _ _ _ content) = do
      writeFile (d </> f2) $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f2
         then return ([d </> f2],ca)
         else return ([],ca)
