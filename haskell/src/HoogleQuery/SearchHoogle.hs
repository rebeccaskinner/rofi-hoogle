{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HoogleQuery.SearchHoogle where

import Hoogle
import Data.Set qualified as Set
import Control.Monad
import Control.Concurrent.STM.TBQueue
import Control.Concurrent
import Control.Monad.STM
import Control.Exception
import Data.IORef
import Data.List
import Debug.Trace

hoogleLookup :: String -> IO [Target]
hoogleLookup query = do
  db <- defaultDatabaseLocation
  withDatabase db (pure . (`searchDatabase` query))

searchHoogle :: String -> IO String
searchHoogle input = do
  results <- hoogleLookup input
  pure . unlines . map (show . targetItem) $ results

spawnSearchWorker :: IO (String -> IO String)
spawnSearchWorker = do
  dbLoc <- defaultDatabaseLocation
  db <- withDatabase dbLoc newIORef
  queryQueue <- newTBQueueIO 1
  resultsQueue <- newTBQueueIO 1
  _thread <- forkIO . forever $ do
    query <- atomically $ readTBQueue queryQueue
    results <- withDatabase dbLoc $ \hoogleDB -> do
      pure $ searchDatabase hoogleDB query
    atomically $ writeTBQueue resultsQueue (show results)
  pure $ \query -> do
    atomically $ writeTBQueue queryQueue query
    results <- atomically $ readTBQueue resultsQueue
    putStrLn $ query <> " => " <> results
    pure results

shouldSearchString :: String -> Bool
shouldSearchString input =
  let
    (parenBalance, bracketBalance, trailingSpaces, totalLen) = foldl' (flip updateCharState) (0,0,0,0) input
  in (parenBalance == 0 && bracketBalance == 0) && ((trailingSpaces >= 2) || (totalLen >= 15))
  where
    updateCharState :: Char -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
    updateCharState c st@(parens, brackets, trailingSpaces, totalLen) =
      case c of
        '(' -> (parens + 1, brackets, 0, totalLen + 1)
        ')' -> (parens - 1, brackets, 0, totalLen + 1)
        '[' -> (parens, brackets + 1, 0, totalLen + 1)
        ']' -> (parens, brackets - 1, 0, totalLen + 1)
        ' ' -> (parens, brackets, trailingSpaces + 1, totalLen + 1)
        _otherwise -> (parens, brackets, 0, totalLen + 1)
