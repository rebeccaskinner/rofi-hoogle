{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HoogleQuery.Native where
import HoogleQuery.SearchHoogle

import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString qualified as BS
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

foreign export ccall "search_hoogle" searchHoogleNative :: CString -> IO CString

{-# NOINLINE searchQueue #-}
searchQueue :: IORef (String -> IO String)
searchQueue = unsafePerformIO $ spawnSearchWorker >>= newIORef

searchHoogleNative :: CString -> IO CString
searchHoogleNative input =
  peekCString input >>= searchHoogle >>= newCString
--   searchF <- readIORef searchQueue
--   input' <- peekCString input
--   result <- searchF input'
--   putStrLn result
-- --  result <- searchHoogle =<< peekCString input
-- --  putStrLn result
--   newCString result
