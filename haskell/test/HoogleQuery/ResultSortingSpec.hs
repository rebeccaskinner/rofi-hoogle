module HoogleQuery.ResultSortingSpec (spec) where

import Data.Foldable (toList)
import Data.List (delete, find, nub, sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Hoogle (Target (..))
import Test.Hspec
import Test.Hspec.Hedgehog (assert, forAll, hedgehog, (===))

import Gen (mkTarget)
import qualified Gen
import HoogleQuery.Config
import HoogleQuery.ResultSorting

spec :: Spec
spec = describe "HoogleQuery.ResultSorting" $ do
  describe "groupTargets" $ do
    it "contains every target exactly once" $ hedgehog $ do
      ts <- forAll Gen.targets
      sort (concatMap toList (groupTargets ts)) === sort ts

    it "orders groups by where their item first appears" $ hedgehog $ do
      ts <- forAll Gen.targets
      map groupKey (groupTargets ts) === nub (map locationless ts)

    it "groups every copy of an item, in Hoogle's order" $ hedgehog $ do
      ts <- forAll Gen.targets
      let sameItemAs g = filter ((== groupKey g) . locationless) ts
      mapM_ (\g -> toList g === sameItemAs g) (groupTargets ts)

  describe "rankResults" $ do
    it "is grouping plus truncation when nothing is pinned or hidden" $ hedgehog $ do
      cfg <- forAll Gen.config
      ts <- forAll Gen.targets
      let plain = cfg{configPinnedPackages = Set.empty, configHiddenPackages = Set.empty}
          limit = min (configMaxResults plain) (effectiveRelevanceWindow plain)
      rankResults plain ts === take limit (groupTargets ts)

    it "never shows a target from a hidden package" $ hedgehog $ do
      cfg <- forAll Gen.config
      ts <- forAll Gen.targets
      assert $ not (any (isHidden cfg) (concatMap toList (rankResults cfg ts)))

    it "shows pinned candidates first, then the rest, each in Hoogle's order" $ hedgehog $ do
      cfg <- forAll Gen.config
      ts <- forAll Gen.targets
      let cands = candidates cfg ts
          (pinned, rest) = (filter (any (isPinned cfg)) cands, filter (not . any (isPinned cfg)) cands)
      map groupKey (rankResults cfg ts)
        === take (configMaxResults cfg) (map groupKey (pinned <> rest))

    it "makes a pinned target the primary, keeping the rest of the group in order" $ hedgehog $ do
      cfg <- forAll Gen.config
      ts <- forAll Gen.targets
      let candidateFor g = find ((== groupKey g) . groupKey) (candidates cfg ts)
      mapM_
        ( \g -> do
            let primary = NonEmpty.head g
            assert $ not (any (isPinned cfg) g) || isPinned cfg primary
            Just (NonEmpty.tail g) === fmap (delete primary . toList) (candidateFor g)
        )
        (rankResults cfg ts)

    describe "examples" $ do
      let base = mkTarget (Just "base") "Prelude"
          containers = mkTarget (Just "containers") "Data.Map"
          rio = mkTarget (Just "rio") "RIO.Map"
          relude = mkTarget (Just "relude") "Relude"
          text = mkTarget (Just "text") "Data.Text"
          items = map (map targetItem . toList)

      it "nudges a pinned package ahead of more relevant results" $
        items
          ( rankResults
              defaultConfig{configPinnedPackages = Set.fromList ["base"]}
              [containers "lookup", text "pack", base "map"]
          )
          `shouldBe` [["map"], ["lookup"], ["pack"]]

      it "does not promote pinned results from outside the relevance window" $ do
        let ts = map (text . show) [1 .. 6 :: Int] <> [base "map"]
            cfg =
              defaultConfig
                { configMaxResults = 3
                , configRelevanceWindow = Just 6
                , configPinnedPackages = Set.fromList ["base"]
                }
        items (rankResults cfg ts) `shouldBe` [["1"], ["2"], ["3"]]

      it "makes the pinned copy of a re-exported item the primary" $
        map
          (map targetPackage . toList)
          ( rankResults
              defaultConfig{configPinnedPackages = Set.fromList ["containers"]}
              [rio "lookup", containers "lookup"]
          )
          `shouldBe` [map targetPackage [containers "lookup", rio "lookup"]]

      it "keeps an item whose other copies are in a hidden package" $
        rankResults
          defaultConfig{configHiddenPackages = Set.fromList ["relude"]}
          [relude "mapM_", base "mapM_"]
          `shouldBe` [pure (base "mapM_")]

      it "hides a package that is also pinned" $
        rankResults
          defaultConfig
            { configPinnedPackages = Set.fromList ["relude"]
            , configHiddenPackages = Set.fromList ["relude"]
            }
          [relude "mapM_", text "pack"]
          `shouldBe` [pure (text "pack")]

      it "caps the default config at 50 results" $
        length (rankResults defaultConfig (map (text . show) [1 .. 80 :: Int]))
          `shouldBe` 50

groupKey :: NonEmpty Target -> Target
groupKey = locationless . NonEmpty.head

inPackages :: Set.Set String -> Target -> Bool
inPackages packages = maybe False (`Set.member` packages) . targetPackageName

isPinned, isHidden :: RofiHoogleConfig -> Target -> Bool
isPinned = inPackages . configPinnedPackages
isHidden = inPackages . configHiddenPackages

-- | The groups pinning may choose from: Hoogle's groups with hidden targets
-- removed, limited to the relevance window.
candidates :: RofiHoogleConfig -> [Target] -> [NonEmpty Target]
candidates cfg =
  take (effectiveRelevanceWindow cfg)
    . mapMaybe (NonEmpty.nonEmpty . filter (not . isHidden cfg) . toList)
    . groupTargets
