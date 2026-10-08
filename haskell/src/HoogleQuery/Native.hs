{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TypeApplications         #-}


module HoogleQuery.Native where
import PangoUtils
import           HoogleQuery.Config
import           HoogleQuery.ResultSorting
import           HoogleQuery.SearchHoogle
import Data.Text.Lazy qualified as LazyText

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBQueue
import           Control.Exception
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
    searchResultSecondaryResultCount :: CInt
  , -- | The secondary results, if any (nullPtr if 'searchResultSecondaryResultCount' is 0)
    searchResultSecondaryResults     :: Ptr HoogleSecondaryResult
  }

searchResultCountOffset :: Int
searchResultCountOffset = 4 * sizeOf @CString undefined

searchResultSecondaryResultsOffset :: Int
searchResultSecondaryResultsOffset =
  let a = alignment @(Ptr HoogleSecondaryResult) undefined
      rawEnd = searchResultCountOffset + sizeOf @CInt undefined
  in ((rawEnd + a - 1) `div` a) * a

instance Storable HoogleSearchResult where
  sizeOf _ = searchResultSecondaryResultsOffset
             + sizeOf @(Ptr HoogleSecondaryResult) undefined
  alignment _ = maximum [ alignment @CString undefined
                        , alignment @CInt undefined
                        , alignment @(Ptr HoogleSecondaryResult) undefined
                        ]
  peek inPtr = HoogleSearchResult
    <$> peek (castPtr inPtr)
    <*> peekElemOff (castPtr inPtr) 1
    <*> peekElemOff (castPtr inPtr) 2
    <*> peekElemOff (castPtr inPtr) 3
    <*> peekByteOff inPtr searchResultCountOffset
    <*> peekByteOff inPtr searchResultSecondaryResultsOffset

  poke outPtr HoogleSearchResult{..} = do
    poke (castPtr outPtr) searchResultName
    pokeElemOff (castPtr outPtr) 1 searchResultPrimaryURL
    pokeElemOff (castPtr outPtr) 2 searchResultPrimaryPackage
    pokeElemOff (castPtr outPtr) 3 searchResultPrimaryModule
    pokeByteOff outPtr searchResultCountOffset searchResultSecondaryResultCount
    pokeByteOff outPtr searchResultSecondaryResultsOffset searchResultSecondaryResults

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
  <*> pure (fromIntegral (length rest))
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

-- Phantom type tag for Ptr HoogleSearchState. The C struct is:
--   struct hoogle_search_state {
--     unsigned int result_count;
--     char *message;                 // NULL if there is nothing to show
--     hoogle_search_result_t results[];
--   };
-- It's variable-size, so there's no Storable instance; allocation and access
-- go through the helpers below.
data HoogleSearchState

hoogleSearchStateMessageOffset :: Int
hoogleSearchStateMessageOffset =
  let a = alignment @CString undefined
      rawEnd = sizeOf @CUInt undefined
  in ((rawEnd + a - 1) `div` a) * a

hoogleSearchStateResultsOffset :: Int
hoogleSearchStateResultsOffset =
  let a = alignment @HoogleSearchResult undefined
      rawEnd = hoogleSearchStateMessageOffset + sizeOf @CString undefined
  in ((rawEnd + a - 1) `div` a) * a

hoogleSearchStateAllocSize :: Int -> Int
hoogleSearchStateAllocSize n =
  hoogleSearchStateResultsOffset + n * sizeOf @HoogleSearchResult undefined

hoogleSearchStateResults :: Ptr HoogleSearchState -> Ptr HoogleSearchResult
hoogleSearchStateResults p = castPtr (p `plusPtr` hoogleSearchStateResultsOffset)

freeHoogleSearchState :: Ptr HoogleSearchState -> IO ()
freeHoogleSearchState p
  | p == nullPtr = pure ()
  | otherwise = do
      count <- peek (castPtr p) :: IO CUInt
      msgStr <- peekByteOff p hoogleSearchStateMessageOffset :: IO CString
      when (msgStr /= nullPtr) $ free msgStr
      let arr = hoogleSearchStateResults p
      for_ [0 .. fromIntegral count - 1] $ \i ->
        peekElemOff arr i >>= freeHoogleSearchResult
      free p

-- Builds a search state from already-ranked result groups. The message (e.g.
-- a DB-load error or a config warning) is read by plugin.c in _get_message
-- and rendered in place of the usage hint.
--
-- Always returns a non-null pointer (even for an empty result set) so the C
-- side can swap private_data in lockstep with us freeing the previous state.
newHoogleSearchState :: Maybe String -> [NonEmpty.NonEmpty Target] -> IO (Ptr HoogleSearchState)
newHoogleSearchState message groups = do
  let resultCount = length groups
  p <- mallocBytes (hoogleSearchStateAllocSize resultCount)
  poke (castPtr p :: Ptr CUInt) (fromIntegral resultCount)
  msgStr <- maybeCString message
  pokeByteOff p hoogleSearchStateMessageOffset msgStr
  let arr = hoogleSearchStateResults (castPtr p)
  for_ (zip [0..] groups) $ \(i, group) -> do
    sr <- hoogleSearchResultFromList group
    pokeElemOff arr i sr
  pure (castPtr p)

-- Holds the active search state pointer; not freed at hs_exit because the
-- RTS only shuts down when rofi is exiting and the OS reclaims it.
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

newtype DBLoadError = DBLoadError { dbLoadErrorMessage :: String }

newtype SearchQueue = SearchQueue (TBQueue (String, MVar [Target]))

-- Either the DB-load error from hs_search_init, or a handle to the worker
-- thread that owns the open Hoogle database. Set once by initSearchWorker.
searchHandle :: IORef (Either DBLoadError SearchQueue)
searchHandle = unsafePerformIO $
  newIORef (Left (DBLoadError "search worker not initialized"))
{-# NOINLINE searchHandle #-}

-- The user's config, and a warning to show if it couldn't be loaded (in which
-- case the config is the default). Set once by initSearchWorker.
searchConfig :: IORef (RofiHoogleConfig, Maybe String)
searchConfig = unsafePerformIO $ newIORef (defaultConfig, Nothing)
{-# NOINLINE searchConfig #-}

-- Loads the config, spawns the worker thread inside withDatabase, then blocks
-- until the worker either reports the queue handle (DB loaded) or reports a
-- load error. The results are stashed in searchConfig and searchHandle for
-- preprocessInput / initialState to consult.
initSearchWorker :: IO ()
initSearchWorker = do
  loadConfig >>= writeIORef searchConfig
  resultMVar <- newEmptyMVar
  let runWorker :: Database -> IO ()
      runWorker db = do
        queries <- newTBQueueIO 1
        putMVar resultMVar (Right (SearchQueue queries))
        forever $ do
          (q, reply) <- atomically $ readTBQueue queries
          putMVar reply (searchDatabase db q)
  void $ forkIO $ do
    outcome <- try @SomeException $ do
      dbLoc <- defaultDatabaseLocation
      withDatabase dbLoc runWorker
    case outcome of
      Left err -> void $ tryPutMVar resultMVar (Left (DBLoadError (show err)))
      Right _  -> pure ()  -- unreachable; the loop never returns
  result <- takeMVar resultMVar
  writeIORef searchHandle result

foreign export ccall "hs_search_init" initSearchWorker :: IO ()

queryViaWorker :: SearchQueue -> String -> IO [Target]
queryViaWorker (SearchQueue qs) q = do
  reply <- newEmptyMVar
  atomically $ writeTBQueue qs (q, reply)
  takeMVar reply

-- A state with no results that shows the DB-load error, if any.
dbErrorState :: DBLoadError -> IO (Ptr HoogleSearchState)
dbErrorState (DBLoadError msg) = newHoogleSearchState (Just msg) [] >>= updateResults'

-- Called from mode_init so a DB-load error or config warning is visible in
-- the rofi prompt before the user types anything. Returns nullPtr when there
-- is nothing to report.
initialState :: IO (Ptr HoogleSearchState)
initialState = do
  handle <- readIORef searchHandle
  (_, configWarning) <- readIORef searchConfig
  case (handle, configWarning) of
    (Left err, _)     -> dbErrorState err
    (Right _, Just w) -> newHoogleSearchState (Just w) [] >>= updateResults'
    (Right _, Nothing) -> pure nullPtr

foreign export ccall "hs_initial_state" initialState :: IO (Ptr HoogleSearchState)

foreign export ccall "search_hoogle" searchHoogleNative :: CString -> IO CString

searchHoogleNative :: CString -> IO CString
searchHoogleNative input =
  peekCString input >>= searchHoogle >>= newCString

foreign export ccall "hs_preprocess_input" preprocessInput :: CString -> IO (Ptr HoogleSearchState)

preprocessInput :: CString -> IO (Ptr HoogleSearchState)
preprocessInput input = do
  input' <- peekCString input
  lastQueryInput <- readIORef lastQuery
  if input' == lastQueryInput
    then readIORef lastResults
    else do
      writeIORef lastQuery input'
      handle <- readIORef searchHandle
      case handle of
        Left err -> dbErrorState err
        Right q
          | shouldSearchString input' -> do
              targets <- queryViaWorker q input'
              updateSearchResults targets
          | otherwise -> pure nullPtr

updateSearchResults :: [Target] -> IO (Ptr HoogleSearchState)
updateSearchResults targets = do
  (cfg, configWarning) <- readIORef searchConfig
  newHoogleSearchState configWarning (rankResults cfg targets) >>= updateResults'
