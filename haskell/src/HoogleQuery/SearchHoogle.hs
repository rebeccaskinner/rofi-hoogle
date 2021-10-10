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
