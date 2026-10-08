{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- | Runtime configuration.
--
-- At start time, rofi-hoogle will read configuration from
-- @$XDG_CONFIG_HOME/rofi-hoogle/config.json@.

module HoogleQuery.Config
  ( RofiHoogleConfig(..)
  , defaultConfig
  , effectiveRelevanceWindow
  , parseConfig
  , configFilePath
  , loadConfig
  , loadConfigFrom
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory (XdgDirectory(..), getXdgDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

data RofiHoogleConfig = RofiHoogleConfig
  { -- | Maximum number of results to show.
    -- Default: 50
    configMaxResults      :: Int
  , -- | Size of the relevance window to use when promoting pinned packages.
    -- Default: 5 * 'configMaxResults'
    configRelevanceWindow :: Maybe Int
  , -- | Packages to promote in the result set, if they are found
    -- Default: 'Set.empty'
    configPinnedPackages  :: Set String
  , -- | Packages whose results are never shown; takes precedence over pinning
    -- Default: 'Set.empty'
    configHiddenPackages  :: Set String
  } deriving (Eq, Show)

-- | Default configuration values
defaultConfig :: RofiHoogleConfig
defaultConfig = RofiHoogleConfig
  { configMaxResults      = 50
  , configRelevanceWindow = Nothing
  , configPinnedPackages  = Set.empty
  , configHiddenPackages  = Set.empty
  }

-- | The relevance window, defaulting to five times 'configMaxResults'
-- (saturating rather than overflowing).
effectiveRelevanceWindow :: RofiHoogleConfig -> Int
effectiveRelevanceWindow cfg =
  fromMaybe defaultWindow (configRelevanceWindow cfg)
  where
    defaultWindow =
      fromInteger $ min (toInteger (maxBound @Int)) (5 * toInteger (configMaxResults cfg))

instance FromJSON RofiHoogleConfig where
  parseJSON = withKnownObject "config" ["results"] $ \top ->
    maybe (pure defaultConfig) parseResults =<< top .:? "results"
    where
      parseResults = withKnownObject "results"
        ["max-results", "relevance-window", "pinned-packages", "hidden-packages"] $ \o -> do
        maxResults <-
          positive "max-results" =<< o .:? "max-results" .!= configMaxResults defaultConfig
        window <- traverse (positive "relevance-window") =<< o .:? "relevance-window"
        pinned <- o .:? "pinned-packages" .!= Set.empty
        hidden <- o .:? "hidden-packages" .!= Set.empty
        pure RofiHoogleConfig
          { configMaxResults      = maxResults
          , configRelevanceWindow = window
          , configPinnedPackages  = pinned
          , configHiddenPackages  = hidden
          }

instance ToJSON RofiHoogleConfig where
  toJSON cfg = object
    [ "results" .= object
        ( [ "max-results"     .= configMaxResults cfg
          , "pinned-packages" .= configPinnedPackages cfg
          , "hidden-packages" .= configHiddenPackages cfg
          ]
          <> maybe [] (\w -> ["relevance-window" .= w]) (configRelevanceWindow cfg)
        )
    ]

withKnownObject :: String -> [Key] -> (Object -> Parser a) -> Value -> Parser a
withKnownObject obj knownKeys parser = withObject obj $ \w -> do
  let unknownCfg = KeyMap.difference w (KeyMap.fromList $ (,()) <$> knownKeys)
  unless (KeyMap.null unknownCfg) $
    fail $ "unknown key(s) in "
      <> obj
      <> ": "
      <> intercalate ", " (Key.toString <$> KeyMap.keys unknownCfg)
  parser w

positive :: String -> Int -> Parser Int
positive name n = do
  unless (n > 0) $ fail (name <> " must be positive, got " <> show n)
  pure n

parseConfig :: BS.ByteString -> Either String RofiHoogleConfig
parseConfig = eitherDecodeStrict

configFilePath :: IO FilePath
configFilePath = (</> "config.json") <$> getXdgDirectory XdgConfig "rofi-hoogle"

-- | Loads the config from 'configFilePath'. Never fails: problems fall back to
-- 'defaultConfig' and are reported as a warning to show the user.
loadConfig :: IO (RofiHoogleConfig, Maybe String)
loadConfig = do
  path <- try @IOException configFilePath
  case path of
    Left err -> pure (defaultConfig, Just ("rofi-hoogle: cannot locate config: " <> show err))
    Right p  -> loadConfigFrom p

-- | Like 'loadConfig', for an explicit path. A missing file is not an error.
loadConfigFrom :: FilePath -> IO (RofiHoogleConfig, Maybe String)
loadConfigFrom path = do
  contents <- try @IOException (BS.readFile path)
  pure $ case contents of
    Left err
      | isDoesNotExistError err -> (defaultConfig, Nothing)
      | otherwise               -> (defaultConfig, Just (warning (show err)))
    Right bytes -> case parseConfig bytes of
      Left err  -> (defaultConfig, Just (warning err))
      Right cfg -> (cfg, Nothing)
  where
    warning err = "rofi-hoogle: ignoring " <> path <> ": " <> err
