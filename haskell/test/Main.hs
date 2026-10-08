module Main (main) where

import HoogleQuery.ConfigSpec qualified
import HoogleQuery.ResultSortingSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HoogleQuery.ConfigSpec.spec
  HoogleQuery.ResultSortingSpec.spec
