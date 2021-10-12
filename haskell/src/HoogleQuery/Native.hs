{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeApplications         #-}


module HoogleQuery.Native where
import qualified Data.Text.Lazy            as LazyText
import           HoogleQuery.ResultSorting
import           PangoUtils

import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.Foldable
import           Data.IORef
import           Data.List
import qualified Data.List.NonEmpty        as NonEmpty
import Data.Int
import           Data.Maybe
import           Data.Ord                  (comparing)
import           Data.Word
import           Foreign                   (Storable (peekElemOff))
import           Foreign.C
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified GHC.Pack                  as LazyText
import           Hoogle
import           System.IO                 (NewlineMode (inputNL))
import           System.IO.Unsafe          (unsafePerformIO)

foreign export ccall "hs_preprocess_input" preprocessInput :: CString -> IO (Ptr HoogleSearchState)

preprocessInput :: CString -> IO (Ptr HoogleSearchState)
preprocessInput input = do
  dbName <- defaultDatabaseLocation
  input' <- peekCString input
  lastQueryInput <- readIORef lastQuery

  let
    processNewInput = do
      let qInfo = stringScan input'
      writeIORef lastQuery input'
      if shouldSearchString qInfo
        then withDatabase dbName (searchUpdateResults qInfo input')
        else pure nullPtr -- hoogleSearchStateFromList qInfo [] >>= updateResults'

  if input' == lastQueryInput
    then readIORef lastResults
    else processNewInput


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
maybeCString Nothing  = pure nullPtr
maybeCString (Just s) = newCString s

hoogleSearchResultFromList :: NonEmpty.NonEmpty Target -> IO HoogleSearchResult
hoogleSearchResultFromList (primary NonEmpty.:| rest) = HoogleSearchResult
  <$> newCString (cleanupHTML $ targetItem primary)
  <*> newCString (targetURL primary)
  <*> maybeCString (fst <$> targetPackage primary)
  <*> maybeCString (fst <$> targetModule primary)
  <*> pure (fromIntegral $ length rest)
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
--  sizeOf _ = sizeOf @HoogleSearchResult undefined + sizeOf @(Ptr HoogleResultSet) undefined
--  alignment _ = max (alignment @HoogleSearchResult undefined) (alignment @(Ptr HoogleResultSet) undefined)
  sizeOf _ = (5 * sizeOf @CString undefined) + sizeOf @(Ptr HoogleResultSet) undefined
  alignment _ = max (alignment @CString undefined) (alignment @(Ptr HoogleResultSet) undefined)
  peek resultPtr = HoogleResultSet
    <$> peek (castPtr resultPtr)
    <*> peekByteOff resultPtr (sizeOf @HoogleSearchResult undefined)

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

data QueryInfo = QueryInfo
  { queryParenCount         :: Int16
  , queryBracketCount       :: Int16
  , queryTrailingSpaceCount :: Int16
  , queryTotalLength        :: Int16
  } deriving (Eq, Show)

instance Storable QueryInfo where
  sizeOf _ = 4 * sizeOf @Int16 undefined
  alignment _ = alignment @Int16 undefined
  peek inPtr = QueryInfo
    <$> peekElemOff (castPtr inPtr) 0
    <*> peekElemOff (castPtr inPtr) 1
    <*> peekElemOff (castPtr inPtr) 2
    <*> peekElemOff (castPtr inPtr) 3
  poke outPtr QueryInfo{..} = do
    pokeElemOff (castPtr outPtr) 0 queryParenCount
    pokeElemOff (castPtr outPtr) 1 queryBracketCount
    pokeElemOff (castPtr outPtr) 2 queryTrailingSpaceCount
    pokeElemOff (castPtr outPtr) 3 queryTotalLength

emptyQueryInfo :: QueryInfo
emptyQueryInfo = QueryInfo 0 0 0 0

stringScan :: String -> QueryInfo
stringScan =
  foldl' (flip updateCharState) (QueryInfo 0 0 0 0)
  where
    updateCharState :: Char -> QueryInfo -> QueryInfo
    updateCharState c (QueryInfo parens brackets trailingSpaces totalLen) =
      case c of
        '('        -> QueryInfo (parens + 1) brackets 0 (totalLen + 1)
        ')'        -> QueryInfo (parens - 1) brackets 0 (totalLen + 1)
        '['        -> QueryInfo parens (brackets + 1) 0 (totalLen + 1)
        ']'        -> QueryInfo parens (brackets - 1) 0 (totalLen + 1)
        ' '        -> QueryInfo parens brackets (trailingSpaces + 1) (totalLen + 1)
        _otherwise -> QueryInfo parens brackets 0 (totalLen + 1)


{-# INLINE shouldSearchString #-}
shouldSearchString :: QueryInfo -> Bool
shouldSearchString (QueryInfo parenBalance bracketBalance trailingSpaces totalLen) =
  (parenBalance == 0 && bracketBalance == 0) && ((trailingSpaces >= 2) || (totalLen >= 15))

data HoogleSearchState = HoogleSearchState
  { hoogleStateResults     :: Ptr HoogleResultSet
  , hoogleStateResultCount :: CUInt
--  , hoogleStateLastQuery   :: QueryInfo
  } deriving Show

instance Storable HoogleSearchState where
  sizeOf _ = sizeOf @(Ptr HoogleResultSet) undefined + sizeOf @CUInt undefined-- + sizeOf @QueryInfo undefined
  alignment _ = maximum [ alignment @(Ptr HoogleResultSet) undefined
                        , alignment @CUInt undefined
--                        , alignment @QueryInfo undefined
                        ]
  peek inPtr = HoogleSearchState
    <$> peek (castPtr inPtr)
    <*> peekByteOff inPtr (sizeOf @(Ptr HoogleResultSet) undefined)
--    <*> peekByteOff inPtr (sizeOf @(Ptr HoogleResultSet) undefined + sizeOf @Int undefined)

  poke outPtr HoogleSearchState{..} = do
    poke (castPtr outPtr) hoogleStateResults
    pokeByteOff outPtr (sizeOf @(Ptr HoogleResultSet) undefined) hoogleStateResultCount
--    pokeByteOff outPtr (sizeOf @(Ptr HoogleResultSet) undefined + sizeOf @Int undefined) hoogleStateLastQuery

freeHoogleSearchState :: Ptr HoogleSearchState -> IO ()
freeHoogleSearchState p
  | p == nullPtr = pure ()
  | otherwise = do
      HoogleSearchState{..} <- peek p
      freeHoogleResultSet hoogleStateResults
      free p

hoogleSearchStateFromList :: QueryInfo -> [Target] -> IO (Ptr HoogleSearchState)
hoogleSearchStateFromList qInfo [] = pure nullPtr
-- do
--   p <- malloc
--   poke p $ HoogleSearchState nullPtr 0-- qInfo
--   pure p
hoogleSearchStateFromList queryInfo targets = do
  let
    targetGroups = sortTargets targets
    resultCount = length targetGroups
  p <- malloc
  resultSet <- hoogleSearchResultSetFromTargetGroups targetGroups
  poke p $ HoogleSearchState resultSet (fromIntegral resultCount) -- queryInfo
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

searchUpdateResults :: QueryInfo -> String -> Database -> IO (Ptr HoogleSearchState)
searchUpdateResults qInfo query db = updateSearchResults qInfo $ searchDatabase db query

updateSearchResults :: QueryInfo -> [Target] -> IO (Ptr HoogleSearchState)
updateSearchResults queryInfo targets =
  hoogleSearchStateFromList queryInfo targets >>= updateResults'
