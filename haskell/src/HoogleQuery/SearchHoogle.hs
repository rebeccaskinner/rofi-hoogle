{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HoogleQuery.SearchHoogle where

import Data.List
import Hoogle

hoogleLookup :: String -> IO [Target]
hoogleLookup query = do
  db <- defaultDatabaseLocation
  withDatabase db (pure . (`searchDatabase` query))

searchHoogle :: String -> IO String
searchHoogle input = do
  results <- hoogleLookup input
  pure . unlines . map (show . targetItem) $ results

shouldSearchString :: String -> Bool
shouldSearchString input =
  let
    (parenBalance, bracketBalance, trailingSpaces, totalLen) = foldl' (flip updateCharState) (0, 0, 0, 0) input
  in
    (parenBalance == 0 && bracketBalance == 0) && ((trailingSpaces >= 2) || (totalLen >= 15))
 where
  updateCharState :: Char -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
  updateCharState c st@(parens, brackets, trailingSpaces, totalLen) =
    case c of
      '(' -> (parens + 1, brackets, 0, totalLen + 1)
      ')' -> (parens - 1, brackets, 0, totalLen + 1)
      '[' -> (parens, brackets + 1, 0, totalLen + 1)
      ']' -> (parens, brackets - 1, 0, totalLen + 1)
      ' ' -> (parens, brackets, trailingSpaces + 1, totalLen + 1)
      _otherwise -> (parens, brackets, 0, totalLen + 1)
