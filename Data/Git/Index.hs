module Data.Git.Index where
import Data.Git.Errors
import Data.Git.Helpers
import Data.Git.Types
import Bindings.Libgit2.Index
import Control.Exception
import System.FilePath

open :: FilePath -> IO Index
clear :: Index -> IO ()
free :: Index -> IO ()
read :: Index -> IO ()
write :: Index -> IO ()
find :: Index -> FilePath -> IO Bool
unique :: Index -> IO ()
add :: Index -> FilePath -> Int -> IO ()
add2 :: Index -> IndexEntry -> IO ()
append :: Index -> FilePath -> Int -> IO ()
append2 :: Index -> IndexEntry -> IO ()
remove :: Index -> Int -> IO ()
get :: Index -> Int -> IndexEntry
entryCount
remove
entryCountUnmerged
unmergedEntryCount
getUnmergedByPath
getUnmergedByIndex
entryStage