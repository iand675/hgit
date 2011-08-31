module Data.Git.Index (
  Index,
  IndexEntry,
  open,
  clear,
  free,
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

open :: FilePath -> IO Index
open = undefined

clear :: Index -> IO ()
clear = undefined
free :: Index -> IO ()
free = undefined
read :: Index -> IO ()
read = undefined
write :: Index -> IO ()
write = undefined
find :: Index -> FilePath -> IO Bool
find = undefined
unique :: Index -> IO ()
unique = undefined
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