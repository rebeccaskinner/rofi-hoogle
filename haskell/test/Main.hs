module Main (main) where

import Test.Hspec (hspec)
import qualified HoogleQuery.ConfigSpec
import qualified HoogleQuery.ResultSortingSpec

main :: IO ()
main = hspec $ do
  HoogleQuery.ConfigSpec.spec
  HoogleQuery.ResultSortingSpec.spec
