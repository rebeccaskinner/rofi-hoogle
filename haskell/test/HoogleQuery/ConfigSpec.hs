{-# LANGUAGE OverloadedStrings #-}

module HoogleQuery.ConfigSpec (spec) where

import Data.Aeson (encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Set qualified as Set
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Test.Hspec.Hedgehog (forAll, hedgehog, tripping)

import Gen qualified
import HoogleQuery.Config

spec :: Spec
spec = describe "HoogleQuery.Config" $ do
  describe "parseConfig" $ do
    it "uses the defaults for an empty object" $
      parseConfig "{}" `shouldBe` Right defaultConfig

    it "uses the defaults for an empty results section" $
      parseConfig "{\"results\": {}}" `shouldBe` Right defaultConfig

    it "parses a full config" $
      parseConfig
        ( BS.concat
            [ "{\"results\": {"
            , "  \"max-results\": 10,"
            , "  \"relevance-window\": 40,"
            , "  \"pinned-packages\": [\"base\", \"containers\"],"
            , "  \"hidden-packages\": [\"relude\"]"
            , "}}"
            ]
        )
        `shouldBe` Right
          RofiHoogleConfig
            { configMaxResults = 10
            , configRelevanceWindow = Just 40
            , configPinnedPackages = Set.fromList ["base", "containers"]
            , configHiddenPackages = Set.fromList ["relude"]
            }

    it "keeps defaults for fields that are left out" $
      parseConfig "{\"results\": {\"pinned-packages\": [\"base\"]}}"
        `shouldBe` Right defaultConfig{configPinnedPackages = Set.fromList ["base"]}

    it "rejects unknown keys, naming them" $ do
      parseConfig "{\"results\": {\"pinned-pacakges\": []}}"
        `shouldSatisfy` either ("pinned-pacakges" `isInfixOf`) (const False)
      parseConfig "{\"result\": {}}"
        `shouldSatisfy` either ("result" `isInfixOf`) (const False)

    it "rejects non-positive counts" $ do
      parseConfig "{\"results\": {\"max-results\": 0}}" `shouldSatisfy` isLeft
      parseConfig "{\"results\": {\"max-results\": -3}}" `shouldSatisfy` isLeft
      parseConfig "{\"results\": {\"relevance-window\": 0}}" `shouldSatisfy` isLeft

    it "rejects values of the wrong type" $ do
      parseConfig "{\"results\": {\"max-results\": 2.5}}" `shouldSatisfy` isLeft
      parseConfig "{\"results\": {\"pinned-packages\": \"base\"}}" `shouldSatisfy` isLeft
      parseConfig "[]" `shouldSatisfy` isLeft

    it "round-trips through JSON" $ hedgehog $ do
      cfg <- forAll Gen.config
      tripping cfg encode (parseConfig . LBS.toStrict)

  describe "effectiveRelevanceWindow" $ do
    it "defaults to five times max-results" $
      effectiveRelevanceWindow defaultConfig{configMaxResults = 7} `shouldBe` 35

    it "uses an explicit window" $
      effectiveRelevanceWindow defaultConfig{configRelevanceWindow = Just 3} `shouldBe` 3

    it "saturates instead of overflowing" $
      effectiveRelevanceWindow defaultConfig{configMaxResults = maxBound} `shouldBe` maxBound

  describe "loadConfigFrom" $ do
    it "uses the defaults without a warning when the file is missing" $
      withSystemTempDirectory "rofi-hoogle" $ \dir ->
        loadConfigFrom (dir </> "config.json") `shouldReturn` (defaultConfig, Nothing)

    it "loads a valid file" $
      withSystemTempDirectory "rofi-hoogle" $ \dir -> do
        let path = dir </> "config.json"
        BS.writeFile path "{\"results\": {\"max-results\": 5}}"
        loadConfigFrom path `shouldReturn` (defaultConfig{configMaxResults = 5}, Nothing)

    it "falls back to the defaults with a warning naming the file when it is invalid" $
      withSystemTempDirectory "rofi-hoogle" $ \dir -> do
        let path = dir </> "config.json"
        BS.writeFile path "{\"results\": "
        (cfg, warning) <- loadConfigFrom path
        cfg `shouldBe` defaultConfig
        warning `shouldSatisfy` maybe False (path `isInfixOf`)
