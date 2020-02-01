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
import System.Directory (setCurrentDirectory)
import System.IO
import System.Posix.Temp (mkdtemp)
import System.FilePath
import Text.Parsec
import Text.Regex.TDFA -- Not safe!

import Builtin
import CompileInfo
import IntegrationTest
import SourceFile
import TypeCategory
import TypeInstance
import TypesBase
import CompilerCxx.Category
import CompilerCxx.Naming
import Cli.CompileMetadata
import Cli.CompilerCommand


runSingleTest :: [String] -> [CompileMetadata] -> [ObjectFile] ->
                 CategoryMap SourcePos -> (String,String) ->
                 IO ((Int,Int),CompileInfo ())
runSingleTest paths deps os tm (f,s) = do
  hPutStrLn stderr $ "\nExecuting tests from " ++ f
  allResults <- checkAndRun (parseTestSource (f,s))
  return $ second (flip reviseError $ "\nIn test file " ++ f) allResults where
    checkAndRun ts
      | isCompileError ts = do
        hPutStrLn stderr $ "Failed to parse tests in " ++ f
        return ((0,1),ts >> return ())
      | otherwise = do
        allResults <- sequence $ map runSingle $ getCompileSuccess ts
        let passed = length $ filter (not . isCompileError) allResults
        let failed = length $ filter isCompileError allResults
        return ((passed,failed),mergeAllM allResults)
    runSingle t = do
      let name = ithTestName $ itHeader t
      let context = "[" ++ formatFullContext (ithContext $ itHeader t) ++ "]"
      hPutStrLn stderr $ "\n*** Executing test \"" ++ name ++ "\" ***"
      outcome <- fmap (flip reviseError ("\nIn test \"" ++ name ++ "\" " ++ context)) $
                   run name (ithResult $ itHeader t) (itCategory t) (itDefinition t)
      if isCompileError outcome
         then hPutStrLn stderr $ "*** Test \"" ++ name ++ "\" failed ***"
         else hPutStrLn stderr $ "*** Test \"" ++ name ++ "\" passed ***"
      return outcome

    run n (ExpectCompileError _ rs es) cs ds = do
      let result = compileAll Nothing cs ds :: CompileInfo ([CategoryName],[String],Namespace,[CxxOutput])
      if not $ isCompileError result
         then return $ compileError "Expected compiler error"
         else return $ do
           let warnings = getCompileWarnings result
           let errors = show $ getCompileError result
           checkContent rs es (warnings ++ lines errors) [] []

    run n (ExpectRuntimeError   _ e rs es) cs ds = execute False n e rs es cs ds
    run n (ExpectRuntimeSuccess _ e rs es) cs ds = execute True  n e rs es cs ds

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

    execute s n e rs es cs ds = do
      let result = compileAll (Just e) cs ds :: CompileInfo ([CategoryName],[String],Namespace,[CxxOutput])
      if isCompileError result
         then return $ result >> return ()
         else do
           let warnings = getCompileWarnings result
           let (req,main,ns,fs) = getCompileSuccess result
           binaryName <- createBinary main req [ns] fs
           let command = TestCommand binaryName (takeDirectory binaryName)
           (TestCommandResult s' out err) <- runTestCommand command
           case (s,s') of
                (True,False) -> return $ mergeAllM $ map compileError $ warnings ++ err ++ out
                (False,True) -> return $ compileError "Expected runtime failure"
                _ -> return $ checkContent rs es warnings err out

    -- TODO: Combine this with the logic in runCompiler.
    compileAll e cs ds = do
      let ns0 = map (StaticNamespace . cmNamespace) deps
      let ns1 = StaticNamespace $ privateNamespace s
      let cs' = map (setCategoryNamespace ns1) cs
      tm' <- includeNewTypes tm cs'
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm') cs'
      cxx <- collectAllOrErrorM $ map (compileConcreteDefinition tm' (ns1:ns0)) ds
      let interfaces = filter (not . isValueConcrete) cs'
      cxx2 <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      (req,main) <- case e of
                         Just e -> createTestFile tm' e
                         Nothing -> return ([],[])
      return (req,main,ns1,hxx ++ cxx ++ cxx2)
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
    createBinary c req ns fs = do
      dir <- mkdtemp "/tmp/ztest_"
      hPutStrLn stderr $ "Writing temporary files to " ++ dir
      sources <- sequence $ map (writeSingleFile dir) fs
      let sources' = resolveObjectDeps dir sources deps
      let main   = dir </> "testcase.cpp"
      let binary = dir </> "testcase"
      writeFile main $ concat $ map (++ "\n") c
      let paths' = nub $ map fixPath (dir:paths)
      let ofr = getObjectFileResolver (sources' ++ os)
      let os' = ofr ns req
      let command = CompileToBinary main os' binary paths'
      runCxxCommand command
      return binary
    writeSingleFile d ca@(CxxOutput _ f _ _ _ content) = do
      writeFile (d </> f) $ concat $ map (++ "\n") content
      if isSuffixOf ".cpp" f
         then return ([d </> f],ca)
         else return ([],ca)
