import Text.Parsec
import Text.Parsec.String
import System.Environment
import System.IO
import qualified Data.Map as Map

import CompileInfo
import CompilerCxx
import DefinedCategory
import TypeCategory
import ParseCategory
import ParseDefinition
import ParserBase
import TypesBase


main = do
  files <- getArgs
  allContents <- sequence $ map readFile files
  let namedContents = zip files allContents
  results <- return $ processContents namedContents
  hPutStr stderr $ format results
  where
    processContents :: [(String,String)] -> CompileInfo [CxxOutput]
    processContents cs = do
      parsed <- collectAllOrErrorM $ map parseContents cs
      let (cs,ds) = foldr merge empty parsed
      cm <- includeNewTypes Map.empty cs
      collectAllOrErrorM $ map (compileCategoryDefinition cm) ds
    empty = ([],[])
    merge (cs1,ds1) (cs2,ds2) = (cs1++cs2,ds1++ds2)
    format (Left e) = show e
    format (Right fs) = concat $ map formatFile fs
    formatFile (CxxOutput f fs os) =
      concat $ map (++ "\n") $ ["/* " ++ f ++ " */"] ++ cdOutput os

parseContents :: (String,String) -> CompileInfo ([AnyCategory SourcePos],[DefinedCategory SourcePos])
parseContents (f,s) = unwrap parsed where
  parsed = parse (between optionalSpace endOfDoc parseAny) f s
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
