module TypesBase (
  CompileError(..),
  Mergeable(..),
  MergeType(..),
  ParamSet(..),
  TypeInstance(..),
  checkParamsMatch,
  checkTypeInstance,
) where


class Mergeable a where
  mergeAny :: [a] -> a
  mergeAll :: [a] -> a
  mergeNested :: a -> a -> a

class CompileError a where
  compileError :: String -> a

data MergeType = MergeUnion | MergeIntersect deriving (Eq,Ord,Show)

data TypeInstance a =
  TypeInstance {
    tiType :: a
  } |
  TypeMerge {
    tmMerge :: MergeType,
    tmTypes :: [TypeInstance a]
  }
  deriving (Eq)

data ParamSet a =
  ParamSet {
    psParams :: [a]
  }
  deriving (Eq)

checkTypeInstance :: Mergeable c => (a -> b -> c) -> TypeInstance a -> TypeInstance b -> c
checkTypeInstance f ti1 ti2 = singleCheck ti1 ti2 where
  singleCheck (TypeInstance t1) (TypeInstance t2) = t1 `f` t2
  singleCheck (TypeMerge MergeUnion     t1) ti2 = mergeAny $ map (`singleCheck` ti2) t1
  singleCheck (TypeMerge MergeIntersect t1) ti2 = mergeAll $ map (`singleCheck` ti2) t1
  singleCheck ti1 (TypeMerge MergeUnion     t2) = mergeAll $ map (ti1 `singleCheck`) t2
  singleCheck ti1 (TypeMerge MergeIntersect t2) = mergeAny $ map (ti1 `singleCheck`) t2

checkParamsMatch :: (Mergeable c, CompileError c) => (a -> b -> c) -> ParamSet a -> ParamSet b -> c
checkParamsMatch f (ParamSet ps1) (ParamSet ps2) = checkedMerge ps1 ps2 where
  checkedMerge []      []      = mergeAll $ map (\(p1,p2) -> (p1 `f` p2)) (zip ps1 ps2)
  checkedMerge (_:ps1) (_:ps2) = checkedMerge ps1 ps2
  checkedMerge _       _       = compileError "Parameter count mismatch"
