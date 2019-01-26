import Text.Parsec
import Text.Parsec.String
import System.Environment
import System.Exit
import System.IO
import qualified Data.Map as Map

import CompileInfo
import DefinedCategory
import TypeCategory
import ParseCategory
import ParseDefinition
import ParserBase
import TypesBase
import CompilerCxx.Category


main = do
  files <- getArgs
  allContents <- sequence $ map readFile files
  let namedContents = zip files allContents
  results <- return $ processContents namedContents
  hPutStr stdout $ format results
  exit results
  where
    exit c = if isCompileError c
                then exitFailure
                else exitSuccess
    processContents :: [(String,String)] -> CompileInfo [CxxOutput]
    processContents cs = do
      parsed <- collectAllOrErrorM $ map parseContents cs
      let (cs,ds) = foldr merge empty parsed
      cm <- includeNewTypes Map.empty cs
      hxx <- collectAllOrErrorM $ map (compileCategoryDeclaration cm) cs
      cxx <- collectAllOrErrorM $ map (compileConcreteDefinition  cm) ds
      let interfaces = filter (not . isValueConcrete) cs
      cxx2 <- collectAllOrErrorM $ map compileInterfaceDefinition interfaces
      return $ hxx ++ cxx ++ cxx2
    empty = ([],[])
    merge (cs1,ds1) (cs2,ds2) = (cs1++cs2,ds1++ds2)
    format c
      | isCompileError c = show $ getCompileError c
      | otherwise = concat $ map formatFile $ getCompileSuccess c
    formatFile (CxxOutput f os) =
      concat $ map (++ "\n") $ ["/* " ++ f ++ " >>> */"] ++ os ++ ["/* <<< " ++ f ++ " */"]

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
