{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeApplications         #-}


module HoogleQuery.Native where
import PangoUtils
import           HoogleQuery.ResultSorting
import           HoogleQuery.SearchHoogle
import Data.Text.Lazy qualified as LazyText

import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.Foldable
import           Data.IORef
import           Data.List
import Data.List.NonEmpty qualified as NonEmpty
import           Data.Maybe
import           Data.Ord               (comparing)
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Hoogle
import           System.IO.Unsafe          (unsafePerformIO)
import qualified GHC.Pack as LazyText

data HoogleSecondaryResult = HoogleSecondaryResult
  { secondaryResultURL     :: CString
  , secondaryResultPackage :: CString
  , secondaryResultModule  :: CString
  , secondaryResultNext    :: Ptr HoogleSecondaryResult
  }

instance Storable HoogleSecondaryResult where
  sizeOf _ = (3 * sizeOf @CString undefined)
             + sizeOf @(Ptr HoogleSecondaryResult) undefined
  alignment _ = max (alignment @CString undefined)
                (alignment @(Ptr HoogleSecondaryResult) undefined)
  peek inPtr = HoogleSecondaryResult
    <$> peek (castPtr inPtr)
    <*> peekElemOff (castPtr inPtr) 1
    <*> peekElemOff (castPtr inPtr) 2
    <*> peekByteOff inPtr (3 * sizeOf @CString undefined)

  poke outPtr HoogleSecondaryResult{..} = do
    poke (castPtr outPtr) secondaryResultURL
    pokeElemOff (castPtr outPtr) 1 secondaryResultPackage
    pokeElemOff (castPtr outPtr) 2 secondaryResultModule
    pokeByteOff outPtr (3 * sizeOf @CString undefined) secondaryResultNext

freeHoogleSecondaryResult :: Ptr HoogleSecondaryResult -> IO ()
freeHoogleSecondaryResult p
  | p == nullPtr = pure ()
  | otherwise = do
      HoogleSecondaryResult{..} <- peek p
      free secondaryResultURL
      when (secondaryResultPackage /= nullPtr) $
        free secondaryResultPackage
      when (secondaryResultModule /= nullPtr) $
        free secondaryResultModule
      freeHoogleSecondaryResult secondaryResultNext
      free p

data HoogleSearchResult = HoogleSearchResult
  { -- | The actual (html) name of the result
    searchResultName                 :: CString
  ,  -- | The URL of the primary location of this result
    searchResultPrimaryURL           :: CString
  ,  -- | The package name for the primary result; may be null
    searchResultPrimaryPackage       :: CString
  , -- | The module name for the primary result; may be null
    searchResultPrimaryModule        :: CString
  , -- | The number of additional results that we've found
    searchResultSecondaryResultCount :: Int
  , -- | The secondary results, if any (nullPtr if 'searchResultSecondaryResultCount' is 0)
    searchResultSecondaryResults     :: Ptr HoogleSecondaryResult
  }

instance Storable HoogleSearchResult where
  sizeOf _ = (4 * sizeOf @CString undefined)
             + sizeOf @Int undefined
             + sizeOf @(Ptr HoogleSecondaryResult) undefined
  alignment _ = maximum [ alignment @CString undefined
                        , alignment @Int undefined
                        , alignment @(Ptr HoogleSecondaryResult) undefined
                        ]
  peek inPtr = HoogleSearchResult
    <$> peek (castPtr inPtr)
    <*> peekElemOff (castPtr inPtr) 1
    <*> peekElemOff (castPtr inPtr) 2
    <*> peekElemOff (castPtr inPtr) 3
    <*> peekByteOff inPtr (4 * sizeOf @CString undefined)
    <*> peekByteOff inPtr ((4 * sizeOf @CString undefined) + sizeOf @Int undefined)

  poke outPtr HoogleSearchResult{..} = do
    poke (castPtr outPtr) searchResultName
    pokeElemOff (castPtr outPtr) 1 searchResultPrimaryURL
    pokeElemOff (castPtr outPtr) 2 searchResultPrimaryPackage
    pokeElemOff (castPtr outPtr) 3 searchResultPrimaryModule
    pokeByteOff outPtr (4 * sizeOf @CString undefined) searchResultSecondaryResultCount
    pokeByteOff outPtr ((4 * sizeOf @CString undefined) + sizeOf @Int undefined) searchResultSecondaryResults

freeHoogleSearchResult :: HoogleSearchResult -> IO ()
freeHoogleSearchResult HoogleSearchResult{..} = do
  free searchResultName
  free searchResultPrimaryURL
  when (searchResultPrimaryPackage /= nullPtr) $
    free searchResultPrimaryPackage
  when (searchResultPrimaryModule /= nullPtr) $
    free searchResultPrimaryModule
  freeHoogleSecondaryResult searchResultSecondaryResults

freeHoogleSearchResultPtr :: Ptr HoogleSearchResult -> IO ()
freeHoogleSearchResultPtr p =
  if p == nullPtr
  then pure ()
  else peek p >>= freeHoogleSearchResult >> free p

maybeCString :: Maybe String -> IO CString
maybeCString Nothing = pure nullPtr
maybeCString (Just s) = newCString s

hoogleSearchResultFromList :: NonEmpty.NonEmpty Target -> IO HoogleSearchResult
hoogleSearchResultFromList (primary NonEmpty.:| rest) = HoogleSearchResult
  <$> newCString (cleanupHTML $ targetItem primary)
  <*> newCString (targetURL primary)
  <*> maybeCString (fst <$> targetPackage primary)
  <*> maybeCString (fst <$> targetModule primary)
  <*> pure (length rest)
  <*> secondaryResultsFromList rest
  where
    secondaryResultsFromList [] = pure nullPtr
    secondaryResultsFromList (result:results) = do
      p <- malloc
      secondaryResult <- HoogleSecondaryResult
                         <$> newCString (targetURL result)
                         <*> maybeCString (fst <$> targetPackage result)
                         <*> maybeCString (fst <$> targetModule result)
                         <*> secondaryResultsFromList results
      poke p secondaryResult
      pure p

hoogleSearchResultFromListPtr :: [Target] -> IO (Ptr HoogleSearchResult)
hoogleSearchResultFromListPtr [] = pure nullPtr
hoogleSearchResultFromListPtr (primary:rest) = do
  result <- malloc
  searchResult <- hoogleSearchResultFromList (primary NonEmpty.:| rest)
  poke result searchResult
  pure result

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

freeHoogleResultSet :: Ptr HoogleResultSet -> IO ()
freeHoogleResultSet resultPtr
  | resultPtr == nullPtr = pure ()
  | otherwise = do
      HoogleResultSet{..} <- peek resultPtr
      freeHoogleSearchResult hoogleResultValue
      freeHoogleResultSet hoogleResultNext
      free resultPtr
      pure ()

hoogleSearchResultSetFromTargetGroups :: [[Target]] -> IO (Ptr HoogleResultSet)
hoogleSearchResultSetFromTargetGroups [] = pure nullPtr
hoogleSearchResultSetFromTargetGroups (target:targets) =
  case target of
    [] -> hoogleSearchResultSetFromTargetGroups targets
    (t:ts) -> do
      p <- malloc
      poke p =<< HoogleResultSet
        <$> hoogleSearchResultFromList (t NonEmpty.:| ts)
        <*> hoogleSearchResultSetFromTargetGroups targets
      pure p

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

freeHoogleSearchState :: Ptr HoogleSearchState -> IO ()
freeHoogleSearchState p
  | p == nullPtr = pure ()
  | otherwise = do
      HoogleSearchState{..} <- peek p
      freeHoogleResultSet hoogleStateResults
      free p

hoogleSearchStateFromList :: [Target] -> IO (Ptr HoogleSearchState)
hoogleSearchStateFromList [] = pure nullPtr
hoogleSearchStateFromList targets = do
  let
    targetGroups = sortTargets targets
    resultCount = length targetGroups
  p <- malloc
  resultSet <- hoogleSearchResultSetFromTargetGroups targetGroups
  poke p $ HoogleSearchState resultSet resultCount
  pure p

lastResults :: IORef (Ptr HoogleSearchState)
lastResults = unsafePerformIO $ newIORef nullPtr
{-# NOINLINE lastResults #-}

lastQuery :: IORef String
lastQuery = unsafePerformIO $ newIORef ""
{-# NOINLINE lastQuery #-}

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
  lastQueryInput <- readIORef lastQuery
  if input' == lastQueryInput
    then readIORef lastResults
    else (do
             writeIORef lastQuery input'
             if shouldSearchString input'
               then withDatabase dbName (searchUpdateResults input')
               else pure nullPtr)

searchUpdateResults :: String -> Database -> IO (Ptr HoogleSearchState)
searchUpdateResults query db = updateSearchResults $ searchDatabase db query

updateSearchResults :: [Target] -> IO (Ptr HoogleSearchState)
updateSearchResults targets =
  hoogleSearchStateFromList targets >>= updateResults'
