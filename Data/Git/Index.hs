module Data.Git.Index (
  Index,
  IndexEntry,
  open,
  clear,
  read,
  write,
  find,
  unique,
  add,
  addIndexEntry,
  append,
  appendIndexEntry,
  remove,
  get,
  entryCount,
  unmergedEntryCount,
  getUnmergedByPath,
  getUnmergedByIndex,
  entryStage
  ) where
import Prelude hiding (read)
import Data.Git.Errors
import Data.Git.Helpers
import Data.Git.Types
import Bindings.Libgit2.Index
import Control.Exception
import System.FilePath
import Foreign.Marshal
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr

{-| Create a new bare Git index object as a memory representation
of the Git index file in 'index_path', without a repository
to back it.

Since there is no ODB or working directory behind this index,
any Index methods which rely on these (e.g. index_add) will
fail with a BareIndex error.

If you need to access the index of an actual repository,
use the \Data.Git.Repository.index\ wrapper.
-}

open :: FilePath -> IO Index
open fp = withCString fp $ \fp' -> ptrFunc (\pp -> c'git_index_open pp fp') toIndex

clear :: Index -> IO ()
clear = stateMod' c'git_index_clear . ixPrim


read :: Index -> IO ()
read = stateMod c'git_index_read . ixPrim

write :: Index -> IO ()
write = stateMod c'git_index_write . ixPrim

find :: Index -> FilePath -> IO Bool
find ix fp = withCString fp $ \fp' -> withForeignPtr (ixPrim ix) $ \ix' -> do
  retVal <- c'git_index_find ix' fp'
  return $ toBool retVal

unique :: Index -> IO ()
unique = stateMod' c'git_index_uniq . ixPrim

add :: Index -> FilePath -> Int -> IO ()
add ix fp stage = withCString fp $ \fp' -> 
  stateMod (\p -> c'git_index_add p fp' (fromIntegral stage)) (ixPrim ix)
  
addIndexEntry :: Index -> IndexEntry -> IO ()
addIndexEntry ix ixe = stateMod (\p -> c'git_index_add2 p (ixePrim ixe)) (ixPrim ix)

append :: Index -> FilePath -> Int -> IO ()
append ix fp stage = withCString fp $ \fp' -> 
  stateMod (\p -> c'git_index_append p fp' (fromIntegral stage)) (ixPrim ix)

appendIndexEntry :: Index -> IndexEntry -> IO ()
appendIndexEntry ix ixe = stateMod (\p -> c'git_index_append2 p (ixePrim ixe)) (ixPrim ix)

remove :: Index -> Int -> IO ()
remove ix position = stateMod (\p -> c'git_index_remove p (fromIntegral position)) (ixPrim ix)

get :: Index -> Int -> IO (Maybe IndexEntry)
get ix position = withForeignPtr (ixPrim ix) $ \p -> do
  ixe <- c'git_index_get p (fromIntegral position)
  return $ ptrMaybe IndexEntry ixe

entryCount :: Index -> IO Int
entryCount = stateMod' (fmap fromIntegral . c'git_index_entrycount) . ixPrim

unmergedEntryCount :: Index -> IO Int
unmergedEntryCount = stateMod' (fmap fromIntegral . c'git_index_entrycount_unmerged) . ixPrim

getUnmergedByPath :: Index -> FilePath -> IO (Maybe IndexEntryUnmerged)
getUnmergedByPath ix fp = withCString fp $ \fp' -> withForeignPtr (ixPrim ix) $ \p -> do
  retPtr <- c'git_index_get_unmerged_bypath p fp'
  return $ ptrMaybe IndexEntryUnmerged retPtr
  
getUnmergedByIndex :: Index -> Int -> IO (Maybe IndexEntryUnmerged)
getUnmergedByIndex ix pos = withForeignPtr (ixPrim ix) $ \p -> do
  retPtr <- c'git_index_get_unmerged_byindex p (fromIntegral pos)
  return $ ptrMaybe IndexEntryUnmerged retPtr
  
entryStage :: IndexEntry -> IO Int
entryStage = fmap fromIntegral . c'git_index_entry_stage . ixePrim 