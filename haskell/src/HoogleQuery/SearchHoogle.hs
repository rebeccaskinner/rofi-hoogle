{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HoogleQuery.SearchHoogle where

import Hoogle
import Data.ByteString qualified as BS

searchHoogle :: BS.ByteString -> IO BS.ByteString
searchHoogle input = pure (input <> " => " <> input)
