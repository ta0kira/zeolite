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

{-# LANGUAGE Safe #-}

module Test.CompileInfo (tests) where

import Base.CompilerError
import Base.CompileInfo


tests :: [IO (CompileInfo ())]
tests = [
    checkSuccess 'a' (return 'a'),
    checkError "error\n" (compilerErrorM "error" :: CompileInfoIO Char),
    checkError "" (compilerErrorM "" :: CompileInfoIO Char),

    checkSuccess ['a','b']          (collectAllM [return 'a',return 'b']),
    checkSuccess []                 (collectAllM [] :: CompileInfoIO [Char]),
    checkError   "error1\nerror2\n" (collectAllM [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccess "ab" (collectAnyM [return 'a',return 'b']),
    checkSuccess ""   (collectAnyM [] :: CompileInfoIO [Char]),
    checkSuccess "b"  (collectAnyM [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccess 'a' (collectFirstM [return 'a',return 'b']),
    checkError   ""  (collectFirstM [] :: CompileInfoIO Char),
    checkSuccess 'b' (collectFirstM [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccessAndWarnings "warning1\nwarning2\n" ()
      (compilerWarningM "warning1" >> return () >> compilerWarningM "warning2"),
    checkErrorAndWarnings "warning1\n" "error\n"
      (compilerWarningM "warning1" >> compilerErrorM "error" >> compilerWarningM "warning2" :: CompileInfoIO ()),

    checkSuccess ['a','b']  (sequence [return 'a',return 'b']),
    checkSuccess []         (sequence [] :: CompileInfoIO [Char]),
    checkError   "error1\n" (sequence [compilerErrorM "error1",return 'b',compilerErrorM "error2"]),

    checkSuccess 'a' (return 'a' `withContextM` "message"),
    checkError "message\n  error\n" (compilerErrorM "error" `withContextM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "message\n  warning\n" ()
      (compilerWarningM "warning" `withContextM` "message" :: CompileInfoIO ()),
    checkErrorAndWarnings "message\n  warning\n" "message\n  error\n"
      ((compilerWarningM "warning" >> compilerErrorM "error") `withContextM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "" () (return () `withContextM` "message"),
    checkErrorAndWarnings "" "message\n" (compilerErrorM "" `withContextM` "message" :: CompileInfoIO ()),

    checkSuccess 'a' (return 'a' `summarizeErrorsM` "message"),
    checkError "message\n  error\n" (compilerErrorM "error" `summarizeErrorsM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "warning\n" ()
      (compilerWarningM "warning" `summarizeErrorsM` "message" :: CompileInfoIO ()),
    checkErrorAndWarnings "warning\n" "message\n  error\n"
      ((compilerWarningM "warning" >> compilerErrorM "error") `summarizeErrorsM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "" () (return () `summarizeErrorsM` "message"),
    checkErrorAndWarnings "" "message\n" (compilerErrorM "" `summarizeErrorsM` "message" :: CompileInfoIO ()),

    checkSuccessAndWarnings "error\n" ()
      (asCompileWarnings $ compilerErrorM "error" :: CompileInfoIO ()),
    checkErrorAndWarnings "" "warning\n"
      (asCompilerError $ compilerWarningM "warning" :: CompileInfoIO ()),

    checkSuccess 'a' (compilerBackgroundM "background" >> return 'a'),
    checkError "error\n  background\n"
      (compilerBackgroundM "background" >> compilerErrorM "error" :: CompileInfoIO ()),
    checkError "error\n  background\n"
      (collectAllM [compilerBackgroundM "background"] >> compilerErrorM "error" :: CompileInfoIO [()]),
    checkError "error\n  background\n"
      (collectFirstM [compilerBackgroundM "background"] >> compilerErrorM "error" :: CompileInfoIO ()),

    checkSuccess 'a' ((resetBackgroundM $ compilerBackgroundM "background") >> return 'a'),
    checkError "error\n"
      ((resetBackgroundM $ compilerBackgroundM "background") >> compilerErrorM "error" :: CompileInfoIO ())
  ]

checkSuccess :: (Eq a, Show a) => a -> CompileInfoIO a -> IO (CompileInfo ())
checkSuccess x y = do
  y' <- toCompileInfo y
  if isCompilerError y' || getCompilerSuccess y' == x
     then return $ y' >> return ()
     else return $ compilerErrorM $ "Expected value " ++ show x ++ " but got value " ++ show (getCompilerSuccess y')

checkError :: (Eq a, Show a) => String -> CompileInfoIO a -> IO (CompileInfo ())
checkError e y = do
  y' <- toCompileInfo y
  if not (isCompilerError y')
     then return $ compilerErrorM $ "Expected error \"" ++ e ++ "\" but got value " ++ show (getCompilerSuccess y')
     else if show (getCompilerError y') == e
          then return $ return ()
          else return $ compilerErrorM $ "Expected error \"" ++ e ++ "\" but got error \"" ++ show (getCompilerError y') ++ "\""

checkSuccessAndWarnings :: (Eq a, Show a) => String -> a -> CompileInfoIO a -> IO (CompileInfo ())
checkSuccessAndWarnings w x y = do
  y' <- toCompileInfo y
  outcome <- checkSuccess x y
  if show (getCompileWarnings y') == w
     then return $ outcome >> return ()
     else return $ compilerErrorM $ "Expected warnings " ++ show w ++ " but got warnings \"" ++ show (getCompileWarnings y') ++ "\""

checkErrorAndWarnings :: (Eq a, Show a) => String -> String -> CompileInfoIO a -> IO (CompileInfo ())
checkErrorAndWarnings w e y = do
  y' <- toCompileInfo y
  outcome <- checkError e y
  if show (getCompileWarnings y') == w
     then return $ outcome >> return ()
     else return $ compilerErrorM $ "Expected warnings " ++ show w ++ " but got warnings \"" ++ show (getCompileWarnings y') ++ "\""
