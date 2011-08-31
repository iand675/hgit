module Data.Git.Index (
  Index,
  IndexEntry,
  open,
  clear,
 -- free,
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
use the Data.Git.Repository.index wrapper.
-}

open :: FilePath -> IO Index
open fp = withCString fp $ \fp' -> ptrFunc (\pp -> c'git_index_open pp fp') toIndex

clear :: Index -> IO ()
clear = stateMod' c'git_index_clear . ixPrim
-- free :: Index -> IO ()
-- free = undefined
read :: Index -> IO ()
read = stateMod c'git_index_read . ixPrim

write :: Index -> IO ()
write = stateMod c'git_index_write . ixPrim

find :: Index -> FilePath -> IO Bool
find = undefined

unique :: Index -> IO ()
unique = stateMod' c'git_index_uniq . ixPrim

add :: Index -> FilePath -> Int -> IO ()
add = undefined

addIndexEntry :: Index -> IndexEntry -> IO ()
addIndexEntry = undefined
append :: Index -> FilePath -> Int -> IO ()
append = undefined
appendIndexEntry :: Index -> IndexEntry -> IO ()
appendIndexEntry = undefined
remove :: Index -> Int -> IO ()
remove = undefined
get :: Index -> Int -> IO IndexEntry
get = undefined
entryCount :: Index -> IO Int
entryCount = undefined
unmergedEntryCount :: Index -> IO Int
unmergedEntryCount = undefined
getUnmergedByPath :: Index -> IO String
getUnmergedByPath = undefined
getUnmergedByIndex :: Index -> IO Int
getUnmergedByIndex = undefined
entryStage :: IndexEntry -> IO Int
entryStage = undefined