-- | Hedgehog generators. Names are drawn from small pools so that generated
-- result lists contain plenty of duplicate items and overlapping packages.
module Gen
  ( packageName
  , target
  , targets
  , packageSet
  , config
  , mkTarget
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hoogle (Target(..))
import HoogleQuery.Config

packageName :: Gen String
packageName = Gen.element ["base", "containers", "text", "rio", "relude", "vector", "lens"]

-- | A target for @item@ in module @modName@ of @pkg@ (or a package-less
-- target, as Hoogle returns for packages themselves).
mkTarget :: Maybe String -> String -> String -> Target
mkTarget pkg modName item = Target
  { targetURL     = "https://hackage.haskell.org/" <> maybe "" (<> "/") pkg <> modName <> "#" <> item
  , targetPackage = fmap (\p -> (p, "https://hackage.haskell.org/package/" <> p)) pkg
  , targetModule  = Just (modName, "https://hackage.haskell.org/" <> modName)
  , targetType    = ""
  , targetItem    = item
  , targetDocs    = ""
  }

target :: Gen Target
target =
  mkTarget
    <$> Gen.frequency [(9, Just <$> packageName), (1, pure Nothing)]
    <*> Gen.element ["Data.Map", "Prelude", "Data.List"]
    <*> Gen.element ["map", "lookup", "insert", "foldr", "mapM_", "(!?)"]

targets :: Gen [Target]
targets = Gen.list (Range.linear 0 60) target

packageSet :: Gen (Set String)
packageSet = Set.fromList <$> Gen.list (Range.linear 0 3) packageName

config :: Gen RofiHoogleConfig
config =
  RofiHoogleConfig
    <$> Gen.int (Range.linear 1 30)
    <*> Gen.maybe (Gen.int (Range.linear 1 60))
    <*> packageSet
    <*> packageSet
