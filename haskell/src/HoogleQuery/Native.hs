{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeApplications         #-}


module HoogleQuery.Native where
import           HoogleQuery.SearchHoogle
import HoogleQuery.ResultSorting

import           Control.Monad
import qualified Data.ByteString          as BS
import           Data.Foldable
import           Data.IORef
import Data.List
import           Data.Maybe
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Hoogle
import           System.IO.Unsafe         (unsafePerformIO)
import Data.Ord.HT (comparing)

data HoogleSearchResult = HoogleSearchResult
  { searchResultURL        :: CString
  , searchResultName       :: CString
  , searchResultType       :: CString
  , searchResultItem       :: CString
  , searchResultDocPreview :: CString
  }

instance Storable HoogleSearchResult where
  sizeOf _ = 5 * sizeOf @CString undefined
  alignment _ = alignment @CString undefined
  peek resultPtr = HoogleSearchResult
    <$> peekElemOff (castPtr resultPtr) 0
    <*> peekElemOff (castPtr resultPtr) 1
    <*> peekElemOff (castPtr resultPtr) 2
    <*> peekElemOff (castPtr resultPtr) 3
    <*> peekElemOff (castPtr resultPtr) 4
  poke outPtr HoogleSearchResult{..} = do
    pokeElemOff (castPtr outPtr) 0 searchResultURL
    pokeElemOff (castPtr outPtr) 1 searchResultName
    pokeElemOff (castPtr outPtr) 2 searchResultType
    pokeElemOff (castPtr outPtr) 3 searchResultItem
    pokeElemOff (castPtr outPtr) 4 searchResultDocPreview

data HoogleResultSet = HoogleResultSet
  { hoogleResultValue :: HoogleSearchResult
  , hoogleResultNext  :: Ptr HoogleResultSet
  }

instance Storable HoogleResultSet where
  sizeOf _ = (5 * sizeOf @CString undefined) + sizeOf @(Ptr HoogleResultSet) undefined
  alignment _ = max (alignment @CString undefined) (alignment @(Ptr HoogleResultSet) undefined)
  peek resultPtr = HoogleResultSet
    <$> peek (castPtr resultPtr)
    <*> peekByteOff  resultPtr (sizeOf @HoogleSearchResult undefined)

  poke outPtr HoogleResultSet{..} = do
    poke (castPtr outPtr) hoogleResultValue
    pokeByteOff outPtr (sizeOf @HoogleSearchResult undefined) hoogleResultNext

data HoogleSearchState = HoogleSearchState
  { hoogleStateResults     :: Ptr HoogleResultSet
  , hoogleStateResultCount :: Int
  }

instance Storable HoogleSearchState where
  sizeOf _ = sizeOf @(Ptr HoogleResultSet) undefined + sizeOf @Int undefined
  alignment _ = max (alignment @(Ptr HoogleResultSet) undefined) (alignment @Int undefined)
  peek inPtr = HoogleSearchState
    <$> peek (castPtr inPtr)
    <*> peekByteOff inPtr (sizeOf @(Ptr HoogleResultSet) undefined)
  poke outPtr HoogleSearchState{..} = do
    poke (castPtr outPtr) hoogleStateResults
    pokeByteOff outPtr (sizeOf @(Ptr HoogleResultSet) undefined) hoogleStateResultCount

hoogleSearchResultSetToList :: HoogleResultSet -> IO [HoogleSearchResult]
hoogleSearchResultSetToList HoogleResultSet{..} =
  fmap (hoogleResultValue :) getTail
  where
    getTail =
      if hoogleResultNext == nullPtr
      then pure []
      else peek hoogleResultNext >>= hoogleSearchResultSetToList

hoogleSearchResultSetFromList :: [HoogleSearchResult] -> IO (Ptr HoogleResultSet)
hoogleSearchResultSetFromList [] = pure nullPtr
hoogleSearchResultSetFromList (r:rs) = do
  tailPtr <- hoogleSearchResultSetFromList rs
  p <- malloc
  poke p (HoogleResultSet r tailPtr)
  pure p

freeHoogleSearchResult :: HoogleSearchResult -> IO ()
freeHoogleSearchResult r@HoogleSearchResult{..} = do
  free searchResultURL
  free searchResultName
  free searchResultType
  free searchResultItem
  free searchResultDocPreview

freeHoogleResultSet :: Ptr HoogleResultSet -> IO ()
freeHoogleResultSet resultPtr
  | resultPtr == nullPtr = pure ()
  | otherwise = do
      HoogleResultSet{..} <- peek resultPtr
      freeHoogleSearchResult hoogleResultValue
      freeHoogleResultSet hoogleResultNext
      free resultPtr
      pure ()

freeHoogleSearchState :: Ptr HoogleSearchState -> IO ()
freeHoogleSearchState p
  | p == nullPtr = pure ()
  | otherwise = do
      HoogleSearchState{..} <- peek p
      freeHoogleResultSet hoogleStateResults
      free p

hoogleSearchStateFromList :: [HoogleSearchResult] -> IO (Ptr HoogleSearchState)
hoogleSearchStateFromList [] = pure nullPtr
hoogleSearchStateFromList results = do
  p <- malloc
  resultSet <- hoogleSearchResultSetFromList results
  poke p (HoogleSearchState resultSet (length results))
  pure p

lastResults :: IORef (Ptr HoogleSearchState)
lastResults = unsafePerformIO $ newIORef nullPtr
{-# NOINLINE lastResults #-}

updateResults :: Ptr HoogleSearchState -> IO ()
updateResults newResults = do
  oldResults <- readIORef lastResults
  when (oldResults /= nullPtr) $
    freeHoogleSearchState oldResults
  writeIORef lastResults newResults

updateResults' :: Ptr HoogleSearchState -> IO (Ptr HoogleSearchState)
updateResults' p = updateResults p >> pure p

foreign export ccall "search_hoogle" searchHoogleNative :: CString -> IO CString

searchHoogleNative :: CString -> IO CString
searchHoogleNative input =
  peekCString input >>= searchHoogle >>= newCString

foreign export ccall "hs_preprocess_input" preprocessInput :: CString -> IO (Ptr HoogleSearchState)

preprocessInput :: CString -> IO (Ptr HoogleSearchState)
preprocessInput input = do
  dbName <- defaultDatabaseLocation
  input' <- peekCString input
  if shouldSearchString input'
    then withDatabase dbName (searchUpdateResults input')
    else pure nullPtr

sortTargetsByClassification :: [Target] -> [Target]
sortTargetsByClassification =
  sortOn (classifyPackage defaultPackageClassification . maybe "" fst . targetPackage)

searchUpdateResults :: String -> Database -> IO (Ptr HoogleSearchState)
searchUpdateResults query db = updateSearchResults $ searchDatabase db query

updateSearchResults :: [Target] -> IO (Ptr HoogleSearchState)
updateSearchResults targets =
  traverse targetToSearchResult targets >>= hoogleSearchStateFromList >>= updateResults'
  where
    targetToSearchResult Target{..} =
      let
        packageName = fst <$> targetPackage
        moduleName = do
          n <- fst <$> targetModule
          pure $ "(" <> n <> ")"
        name = unwords . catMaybes $ [packageName, moduleName]
      in HoogleSearchResult
      <$> newCString targetURL
      <*> newCString name
      <*> newCString targetType
      <*> newCString targetItem
      <*> newCString ""-- targetDocs

testSearchResults ::
  String ->
  String ->
  String ->
  String ->
  String ->
  IO HoogleSearchResult
testSearchResults url name typ item doc = HoogleSearchResult
  <$> newCString url
  <*> newCString name
  <*> newCString typ
  <*> newCString item
  <*> newCString doc

exampleSearchState :: IO (Ptr HoogleSearchState)
exampleSearchState = do
  results <- sequenceA
             [ testSearchResults "url1" "name1" "typ1" "item1" "doc1"
             , testSearchResults "url2" "name2" "typ2" "item2" "doc2"
             , testSearchResults "url3" "name3" "typ3" "item3" "doc3"
             , testSearchResults "url4" "name4" "typ4" "item4" "doc4"
             , testSearchResults "url5" "name5" "typ5" "item5" "doc5"
             ]
  hoogleSearchStateFromList results
