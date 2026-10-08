-- | Turns Hoogle's results into the rows shown in rofi. Hoogle's relevance
-- order is the baseline; the user's config only hides packages and nudges
-- pinned packages up within the most relevant results.
module HoogleQuery.ResultSorting
  ( locationless
  , targetPackageName
  , groupTargets
  , rankResults
  ) where

import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Hoogle
import HoogleQuery.Config

-- | A target with its location removed. Targets with the same 'locationless'
-- value are the same item found in different places (e.g. re-exports); this
-- matches how Hoogle's web UI groups duplicates.
locationless :: Target -> Target
locationless target =
  target{targetURL = "", targetPackage = Nothing, targetModule = Nothing}

-- | The package a target belongs to; 'Nothing' for results that are packages.
targetPackageName :: Target -> Maybe String
targetPackageName = fmap fst . targetPackage

-- | Groups targets by 'locationless'. Groups are ordered by where their first
-- target appeared, and targets within a group keep their relative order.
groupTargets :: [Target] -> [NonEmpty Target]
groupTargets =
  map (NonEmpty.reverse . snd)
    . sortOn fst
    . Map.elems
    . Map.fromListWith mergeGroups
    . zipWith (\index target -> (locationless target, (index, target :| []))) [0 :: Int ..]
 where
  -- fromListWith passes the later entry first. Keep the earliest index and
  -- build each group in reverse so that adding a target is O(1).
  mergeGroups :: (Int, NonEmpty Target) -> (Int, NonEmpty Target) -> (Int, NonEmpty Target)
  mergeGroups (_, later) (firstIndex, earlier) = (firstIndex, later <> earlier)

-- | Applies the config to Hoogle's results:
--
-- 1. keep only the first 'effectiveRawResultLimit' targets, so that Hoogle
--    doesn't have to produce the rest; copies of an item beyond this point
--    are not grouped with it
-- 2. group duplicates ('groupTargets'), keeping Hoogle's order
-- 3. drop targets from hidden packages, and any groups left empty
-- 4. keep the first 'effectiveRelevanceWindow' groups
-- 5. move groups containing a pinned package to the front, keeping Hoogle's
--    order otherwise; the first pinned target in a group becomes its primary
-- 6. keep the first 'configMaxResults' groups
rankResults :: RofiHoogleConfig -> [Target] -> [NonEmpty Target]
rankResults cfg =
  take (configMaxResults cfg)
    . pinToFront
    . take (effectiveRelevanceWindow cfg)
    . mapMaybe (NonEmpty.nonEmpty . NonEmpty.filter (not . isHidden))
    . groupTargets
    . take (effectiveRawResultLimit cfg)
 where
  inPackages packages = maybe False (`Set.member` packages) . targetPackageName
  isPinned = inPackages (configPinnedPackages cfg)
  isHidden = inPackages (configHiddenPackages cfg)

  pinToFront groups =
    let (pinned, rest) = partition (any isPinned) groups
    in map promotePinned pinned <> rest

  promotePinned group =
    case break isPinned (NonEmpty.toList group) of
      (before, p : after) -> p :| (before <> after)
      (_, []) -> group
