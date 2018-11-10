module TypeCategory (
) where

import Data.List (group, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CompileInfo
import TypeInstance
import TypesBase


newtype CategoryConnect a =
  CategoryConnect {
    ccMap :: Map.Map TypeName a
  }

data CategoryRefine =
  CategoryRefine {
    crName :: TypeName,
    crRefines :: [GeneralInstance]
  }

type CategoryMain = CategoryConnect (Set.Set TypeName)

type Refinements = CategoryConnect CategoryRefine

categoryLookup :: (CompileError (m a), Monad m) => CategoryConnect a -> TypeName -> m a
categoryLookup (CategoryConnect cs) n = resolve $ n `Map.lookup` cs where
  resolve (Just x) = return x
  resolve _        = compileError $ "Category " ++ show n ++ " not found"

checkCategory :: (Mergeable b) => (TypeName -> a -> b) -> CategoryConnect a -> b
checkCategory f (CategoryConnect cs) = mergeAll $ map (\(k,v) -> f k v) (Map.toList cs)


checkCycles :: (Mergeable (m ()), CompileError (m ()),
                CompileError (m (Set.Set TypeName)), Monad m) => CategoryMain -> m ()
checkCycles ca@(CategoryConnect cs) = checkCategory (checker []) ca where
  checker ns n ts
    | n `Set.member` (Set.fromList ns) =
      compileError $ "Cycle found: " ++ intercalate " -> " (map show (ns ++ [n]))
    | otherwise =
      mergeAll $ map (\(n2,ts2) -> ts2 >>= checker (ns ++ [n]) n2) (map find $ Set.toList ts) where
        find t = (t,categoryLookup ca t)
