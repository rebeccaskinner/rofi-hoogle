module Main (main) where

import qualified HoogleQuery.ConfigSpec
import qualified HoogleQuery.ResultSortingSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  HoogleQuery.ConfigSpec.spec
  HoogleQuery.ResultSortingSpec.spec
