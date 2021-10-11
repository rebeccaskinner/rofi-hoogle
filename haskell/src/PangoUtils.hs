{-# LANGUAGE ImportQualifiedPost #-}
module PangoUtils where
import HTMLEntities.Decoder
import Debug.Trace
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder

removeHTML :: String -> String
removeHTML [] = []
removeHTML ('<':xs) = removeHTML . drop 1 . dropWhile (/='>') $ xs
removeHTML (x:xs) = x : removeHTML xs

cleanupHTML :: String -> String
cleanupHTML = LazyText.unpack . Builder.toLazyText . htmlEncodedText . Text.pack . removeHTML
