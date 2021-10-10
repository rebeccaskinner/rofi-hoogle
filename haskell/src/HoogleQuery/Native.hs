{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module HoogleQuery.Native where
import HoogleQuery.SearchHoogle

import Foreign.C
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString qualified as BS

foreign export ccall search_hoogle :: CString -> IO CString

search_hoogle :: CString -> IO CString
search_hoogle input = pure input
