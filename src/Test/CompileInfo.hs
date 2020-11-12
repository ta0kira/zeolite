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

import Base.CompileError
import Base.CompileInfo


tests :: [IO (CompileInfo ())]
tests = [
    checkSuccess 'a' (return 'a'),
    checkError "error\n" (compileErrorM "error" :: CompileInfoIO Char),
    checkError "" (compileErrorM "" :: CompileInfoIO Char),

    checkSuccess ['a','b']          (collectAllM [return 'a',return 'b']),
    checkSuccess []                 (collectAllM [] :: CompileInfoIO [Char]),
    checkError   "error1\nerror2\n" (collectAllM [compileErrorM "error1",return 'b',compileErrorM "error2"]),

    checkSuccess "ab" (collectAnyM [return 'a',return 'b']),
    checkSuccess ""   (collectAnyM [] :: CompileInfoIO [Char]),
    checkSuccess "b"  (collectAnyM [compileErrorM "error1",return 'b',compileErrorM "error2"]),

    checkSuccess 'a' (collectFirstM [return 'a',return 'b']),
    checkError   ""  (collectFirstM [] :: CompileInfoIO Char),
    checkSuccess 'b' (collectFirstM [compileErrorM "error1",return 'b',compileErrorM "error2"]),

    checkSuccessAndWarnings "warning1\nwarning2\n" ()
      (compileWarningM "warning1" >> return () >> compileWarningM "warning2"),
    checkErrorAndWarnings "warning1\n" "error\n"
      (compileWarningM "warning1" >> compileErrorM "error" >> compileWarningM "warning2" :: CompileInfoIO ()),

    checkSuccess ['a','b']  (sequence [return 'a',return 'b']),
    checkSuccess []         (sequence [] :: CompileInfoIO [Char]),
    checkError   "error1\n" (sequence [compileErrorM "error1",return 'b',compileErrorM "error2"]),

    checkSuccess 'a' (return 'a' `withContextM` "message"),
    checkError "message\n  error\n" (compileErrorM "error" `withContextM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "message\n  warning\n" ()
      (compileWarningM "warning" `withContextM` "message" :: CompileInfoIO ()),
    checkErrorAndWarnings "message\n  warning\n" "message\n  error\n"
      ((compileWarningM "warning" >> compileErrorM "error") `withContextM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "" () (return () `withContextM` "message"),
    checkErrorAndWarnings "" "message\n" (compileErrorM "" `withContextM` "message" :: CompileInfoIO ()),

    checkSuccess 'a' (return 'a' `summarizeErrorsM` "message"),
    checkError "message\n  error\n" (compileErrorM "error" `summarizeErrorsM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "warning\n" ()
      (compileWarningM "warning" `summarizeErrorsM` "message" :: CompileInfoIO ()),
    checkErrorAndWarnings "warning\n" "message\n  error\n"
      ((compileWarningM "warning" >> compileErrorM "error") `summarizeErrorsM` "message" :: CompileInfoIO ()),
    checkSuccessAndWarnings "" () (return () `summarizeErrorsM` "message"),
    checkErrorAndWarnings "" "message\n" (compileErrorM "" `summarizeErrorsM` "message" :: CompileInfoIO ()),

    checkSuccessAndWarnings "error\n" ()
      (asCompileWarnings $ compileErrorM "error" :: CompileInfoIO ()),
    checkErrorAndWarnings "" "warning\n"
      (asCompileError $ compileWarningM "warning" :: CompileInfoIO ()),

    checkSuccess 'a' (compileBackgroundM "background" >> return 'a'),
    checkError "error\n  background\n"
      (compileBackgroundM "background" >> compileErrorM "error" :: CompileInfoIO ()),
    checkError "error\n  background\n"
      (collectAllM [compileBackgroundM "background"] >> compileErrorM "error" :: CompileInfoIO [()]),
    checkError "error\n  background\n"
      (collectFirstM [compileBackgroundM "background"] >> compileErrorM "error" :: CompileInfoIO ()),

    checkSuccess 'a' ((resetBackgroundM $ compileBackgroundM "background") >> return 'a'),
    checkError "error\n"
      ((resetBackgroundM $ compileBackgroundM "background") >> compileErrorM "error" :: CompileInfoIO ())
  ]

checkSuccess :: (Eq a, Show a) => a -> CompileInfoIO a -> IO (CompileInfo ())
checkSuccess x y = do
  y' <- toCompileInfo y
  if isCompileError y' || getCompileSuccess y' == x
     then return $ y' >> return ()
     else return $ compileErrorM $ "Expected value " ++ show x ++ " but got value " ++ show (getCompileSuccess y')

checkError :: (Eq a, Show a) => String -> CompileInfoIO a -> IO (CompileInfo ())
checkError e y = do
  y' <- toCompileInfo y
  if not (isCompileError y')
     then return $ compileErrorM $ "Expected error \"" ++ e ++ "\" but got value " ++ show (getCompileSuccess y')
     else if show (getCompileError y') == e
          then return $ return ()
          else return $ compileErrorM $ "Expected error \"" ++ e ++ "\" but got error \"" ++ show (getCompileError y') ++ "\""

checkSuccessAndWarnings :: (Eq a, Show a) => String -> a -> CompileInfoIO a -> IO (CompileInfo ())
checkSuccessAndWarnings w x y = do
  y' <- toCompileInfo y
  outcome <- checkSuccess x y
  if show (getCompileWarnings y') == w
     then return $ outcome >> return ()
     else return $ compileErrorM $ "Expected warnings " ++ show w ++ " but got warnings \"" ++ show (getCompileWarnings y') ++ "\""

checkErrorAndWarnings :: (Eq a, Show a) => String -> String -> CompileInfoIO a -> IO (CompileInfo ())
checkErrorAndWarnings w e y = do
  y' <- toCompileInfo y
  outcome <- checkError e y
  if show (getCompileWarnings y') == w
     then return $ outcome >> return ()
     else return $ compileErrorM $ "Expected warnings " ++ show w ++ " but got warnings \"" ++ show (getCompileWarnings y') ++ "\""
