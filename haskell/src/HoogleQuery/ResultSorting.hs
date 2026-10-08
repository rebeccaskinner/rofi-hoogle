{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
module HoogleQuery.ResultSorting where
import Hoogle
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map

data PackageType
  = PackageTypeBase
  | PackageTypeCoreLibrary
  | PackageTypeGHCLibrary
  | PackageTypePopularLibrary
  | PackageTypeOtherLibrary
  deriving (Eq, Ord, Enum, Show)

newtype PackageClassification = PackageClassification
  { getClassifications :: HashMap.HashMap String PackageType }
  deriving newtype (Semigroup, Monoid)

defaultPackageClassification :: PackageClassification
defaultPackageClassification =
  PackageClassification . HashMap.fromList $
  [ ("base", PackageTypeBase)
  -- core libraries
  , ("array", PackageTypeCoreLibrary)
  , ("deepseq", PackageTypeCoreLibrary)
  , ("directory", PackageTypeCoreLibrary)
  , ("filepath", PackageTypeCoreLibrary)
  , ("mtl", PackageTypeCoreLibrary)
  , ("primitive", PackageTypeCoreLibrary)
  , ("process", PackageTypeCoreLibrary)
  , ("stm", PackageTypeCoreLibrary)
  , ("template-haskell", PackageTypeCoreLibrary)
  , ("unix", PackageTypeCoreLibrary)
  , ("vector", PackageTypeCoreLibrary)
  , ("Win32", PackageTypeCoreLibrary)
  -- Non-Core libraries that ship with GHC
  , ("containers", PackageTypeGHCLibrary)
  , ("hoopl", PackageTypeGHCLibrary)
  , ("pretty", PackageTypeGHCLibrary)
  , ("time", PackageTypeGHCLibrary)
  , ("xhtml", PackageTypeGHCLibrary)
  , ("ghc-prim", PackageTypeGHCLibrary)
  , ("hpc", PackageTypeGHCLibrary)
  -- Popular libraries that should be prioritized in search results,
  -- not based on any particular strong evidence, but with a slight
  -- bias toward "low-level" things, things with a lot of operators,
  -- or things I happen to be using lately. Not necessarily an
  -- endorsement.
  , ("aeson", PackageTypePopularLibrary)
  , ("bytestring", PackageTypePopularLibrary)
  , ("text", PackageTypePopularLibrary)
  , ("network", PackageTypePopularLibrary)
  , ("attoparsec", PackageTypePopularLibrary)
  , ("megaparsec", PackageTypePopularLibrary)
  , ("rio", PackageTypePopularLibrary)
  , ("relude", PackageTypePopularLibrary)
  , ("mono-traversable", PackageTypePopularLibrary)
  , ("warp", PackageTypePopularLibrary)
  , ("servant", PackageTypePopularLibrary)
  , ("pandoc", PackageTypePopularLibrary)
  , ("random", PackageTypePopularLibrary)
  , ("lens", PackageTypePopularLibrary)
  , ("cryptonite", PackageTypePopularLibrary)
  , ("HTTP", PackageTypePopularLibrary)
  , ("optparse-applicative", PackageTypePopularLibrary)
  , ("transformers", PackageTypePopularLibrary)
  , ("http-types", PackageTypePopularLibrary)
  , ("foundation", PackageTypePopularLibrary)
  , ("wai", PackageTypePopularLibrary)
  , ("parsec", PackageTypePopularLibrary)
  , ("parallel", PackageTypePopularLibrary)
  , ("persistent", PackageTypePopularLibrary)
  , ("esqueleto", PackageTypePopularLibrary)
  , ("unliftio", PackageTypePopularLibrary)
  , ("unliftio-core", PackageTypePopularLibrary)
  ]

classifyPackage :: PackageClassification -> String -> PackageType
classifyPackage (PackageClassification classifications) pkgName =
  fromMaybe PackageTypeOtherLibrary $ HashMap.lookup pkgName classifications

sortTargetsByClassification :: [Target] -> [Target]
sortTargetsByClassification =
  sortOn (classifyPackage defaultPackageClassification . maybe "" fst . targetPackage)

-- | Groups targets that refer to the same item in different locations (e.g. a
-- function and its re-exports), as Hoogle's web UI does. Within a group, the
-- target from the highest-priority package comes first and becomes the primary
-- result. Groups are ordered by that package's classification, with ties kept
-- in Hoogle's relevance order.
sortTargets :: [Target] -> [[Target]]
sortTargets =
  map snd
  . sortOn fst
  . map rankGroup
  . Map.elems
  . Map.fromListWith mergeGroups
  . zipWith (\index target -> (locationless target, (index, [target]))) [0 :: Int ..]
  where
    locationless :: Target -> Target
    locationless target =
      target { targetURL = "", targetPackage = Nothing, targetModule = Nothing }

    -- fromListWith passes the later entry first; keep the earliest index and
    -- the original relative order of the group's targets.
    mergeGroups :: (Int, [Target]) -> (Int, [Target]) -> (Int, [Target])
    mergeGroups (_, later) (firstIndex, earlier) = (firstIndex, earlier <> later)

    rankGroup :: (Int, [Target]) -> ((PackageType, Int), [Target])
    rankGroup (firstIndex, group) =
      let sorted = sortTargetsByClassification group
      in ((classifyTargetSet sorted, firstIndex), sorted)

    classifyTargetSet :: [Target] -> PackageType
    classifyTargetSet [] = PackageTypeOtherLibrary
    classifyTargetSet(t:_) =
      let n = maybe "" fst (targetPackage t)
      in classifyPackage defaultPackageClassification n
