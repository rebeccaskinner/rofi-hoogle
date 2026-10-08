{-# LANGUAGE ImportQualifiedPost #-}

module PangoUtils where

import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder
import Debug.Trace
import HTMLEntities.Decoder

removeHTML :: String -> String
removeHTML [] = []
removeHTML ('<' : xs) = removeHTML . drop 1 . dropWhile (/= '>') $ xs
removeHTML (x : xs) = x : removeHTML xs

cleanupHTML :: String -> String
cleanupHTML = LazyText.unpack . Builder.toLazyText . htmlEncodedText . Text.pack . removeHTML
