{- -----------------------------------------------------------------------------
Copyright 2019-2020 Kevin P. Barry

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

import Data.List
import Text.Parsec
import Text.Parsec.String
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map

import Builtin
import CompileInfo
import DefinedCategory
import TypeCategory
import ParseCategory
import ParseDefinition
import ParserBase
import TypesBase
import CompilerCxx.Category


-- $ compiler [root prefix] [path prefix] [abs. existing sources] -- [rel. new sources]
main = do
  (root:prefix:files) <- getArgs
  builtinContents <- readFile $ setPrefix root builtinFilename
  let bs = builtinCategories builtinBasename builtinContents
  let (fs0,fs) = case break (== "--") files of
                      (fs,[]) -> ([],fs)
                      (fs0,_:fs) -> (fs0,fs)
  contents0 <- sequence $ map (readFile . setPrefix prefix) fs0
  let ps0 = zip fs0 contents0
  contents <- sequence $ map (readFile . setPrefix prefix) fs
  let namedContents = zip fs contents
  let (ps,xs) = partition ((".0rp" `isSuffixOf`) . fst) namedContents
  results <- return $ do
    bs0 <- bs
    tm0 <- processExisting bs0 ps0
    -- Everything in .0rp is available to all other files.
    (tm0',ps') <- processPublic tm0 ps
    -- All other files are considered internal.
    -- TODO: There will be an output filename clash if two files use the same
    -- name for an internal-only category.
    xs' <- fmap concat $ collectAllOrErrorM $  map (processInternal tm0') xs
    return $ ps' ++ xs'
  hPutStr stderr $ format results
  writeResults results
  exit results
  where
    setPrefix "" f = f
    setPrefix p f@('/':_) = f
    setPrefix p f = p ++ "/" ++ f
    exit c = if isCompileError c
                then exitFailure
                else exitSuccess
    processExisting :: (CategoryMap SourcePos) -> [(String,String)] ->
                       CompileInfo (CategoryMap SourcePos)
    processExisting bs cs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublic cs
      includeNewTypes bs cs
    processPublic :: CategoryMap SourcePos -> [(String,String)] ->
                     CompileInfo (CategoryMap SourcePos,[CxxOutput])
    processPublic tm0 cs = do
      cs <- fmap concat $ collectAllOrErrorM $ map parsePublic cs
      tm <- includeNewTypes tm0 cs
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm) cs
      let interfaces = filter (not . isValueConcrete) cs
      cxx <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      return (tm,hxx ++ cxx)
    processInternal :: CategoryMap SourcePos -> (String,String) -> CompileInfo [CxxOutput]
    processInternal tm0 c = do
      (cs,ds) <- parseInternal c
      tm <- includeNewTypes tm0 cs
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration tm) cs
      cxx <- collectAllOrErrorM $ map (compileConcreteDefinition  tm) ds
      let interfaces = filter (not . isValueConcrete) cs
      cxx2 <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      return $ hxx ++ cxx ++ cxx2
    format c
      | isCompileError c = show $ getCompileError c
      | otherwise = ""
    writeResults c
      | isCompileError c = return ()
      | otherwise = mapM_ (\(CxxOutput f os) -> writeSingleFile f $ concat $ map (++ "\n") os) $ getCompileSuccess c
    writeSingleFile f c = do
      hPutStrLn stderr $ "Writing file " ++ f
      writeFile f c

parseInternal :: (String,String) -> CompileInfo ([AnyCategory SourcePos],[DefinedCategory SourcePos])
parseInternal (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc parseAny) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t

parsePublic :: (String,String) -> CompileInfo [AnyCategory SourcePos]
parsePublic (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc (sepBy sourceParser optionalSpace)) f s
  unwrap (Left e)  = compileError (show e)
  unwrap (Right t) = return t

parseAny :: Parser ([AnyCategory SourcePos],[DefinedCategory SourcePos])
parseAny = parsed >>= return . foldr merge empty where
  empty = ([],[])
  merge (cs1,ds1) (cs2,ds2) = (cs1++cs2,ds1++ds2)
  parsed = sepBy anyType optionalSpace
  anyType = singleCategory <|> singleDefine
  singleCategory = do
    c <- sourceParser
    return ([c],[])
  singleDefine = do
    d <- sourceParser
    return ([],[d])
