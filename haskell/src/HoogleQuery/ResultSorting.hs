{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
module HoogleQuery.ResultSorting where
import Hoogle
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.HashMap.Strict as HashMap

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
  , ("random", PackageTypeCoreLibrary)
  , ("stm", PackageTypeCoreLibrary)
  , ("template-haskell", PackageTypeCoreLibrary)
  , ("unix", PackageTypeCoreLibrary)
  , ("vector", PackageTypeCoreLibrary)
  , ("Win32", PackageTypeCoreLibrary)
  -- Non-Core libraries that ship with GHC
  , ("containers", PackageTypeGHCLibrary)
  , ("hoopl", PackageTypeGHCLibrary)
  , ("parallel", PackageTypeGHCLibrary)
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

sortTargets :: [Target] -> [[Target]]
sortTargets =
  HashMap.elems
  . HashMap.map sortTargetsByClassification
  . foldr insertTarget HashMap.empty
  where
    insertTarget :: Target -> HashMap.HashMap String [Target] -> HashMap.HashMap String [Target]
    insertTarget target accumulatorMap =
      let key = targetItem target
      in HashMap.insertWith (<>) key [target] accumulatorMap