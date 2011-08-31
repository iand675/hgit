module Data.Git.Repository (
  Repository (), -- | A git repository handle.
  GitDir,
  ObjectDir,
  IndexFile,
  WorkTree,
  StartPath,
  open,
  openWithPaths,
  database,
  index,
  init,
  initBare,
  isHeadDetached,
  isHeadOrphan,
  isEmpty,
  isBare,
  path,
  indexPath,
  objectDatabasePath,
  workingDirectoryPath ) where
import Data.Git.Common
import Data.Git.Errors
import Data.Git.Helpers
import Bindings.Libgit2.Repository 
import System.FilePath
import Foreign.C.String
import Data.Git.Types
import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal
import Control.Exception
import Prelude hiding (init)
type GitDir    = FilePath
type ObjectDir = FilePath
type IndexFile = FilePath
type WorkTree  = FilePath
type StartPath = FilePath

open :: FilePath -> IO Repository
open fp = withCString fp $ \str -> ptrFunc (\pp -> c'git_repository_open pp str) toRepository
    
openWithPaths :: GitDir -> Maybe ObjectDir -> Maybe IndexFile -> Maybe WorkTree -> IO Repository
openWithPaths fp od ixf wt = withCString fp $ \str -> do
  odcs  <- maybeCStr od
  ixfcs <- maybeCStr ixf
  wtcs  <- maybeCStr wt
  repo <- ptrFunc (\ptr -> c'git_repository_open2 ptr str odcs ixfcs wtcs) toRepository
  mapM_ free [odcs, ixfcs, wtcs]
  return repo
    
-- ObjectDBs have weird semantics in the C code since the odb ref is managed on the C side.
-- Need to figure out how to sanely deal with this before implementing something that's wrong.
openWithPathsAndObjectDatabase :: GitDir -> ObjectDB -> Maybe IndexFile -> Maybe WorkTree -> IO Repository
openWithPathsAndObjectDatabase gd odb mif mwt = throw NotImplemented
    
fromBool False = 0
fromBool True  = 1

discover ::  StartPath -> Bool -> [FilePath] -> IO (Maybe FilePath)
discover fp across ceils = throw NotImplemented
  
database :: Repository -> IO ObjectDB
database repo = withForeignPtr (repoPrim repo) $ \r -> do
  odb <- c'git_repository_database r
  toObjectDB odb

index :: Repository -> IO Index
index repo = withForeignPtr (repoPrim repo) $ \r -> ptrFunc (\ix -> c'git_repository_index ix r) toIndex

initHelper i fp = withCString fp $ \str -> ptrFunc (\ptr -> c'git_repository_init ptr str i) toRepository

init :: FilePath -> IO Repository
init = initHelper 0

initBare :: FilePath -> IO Repository
initBare = initHelper 1

isHeadDetached :: Repository -> IO Bool
isHeadDetached = boolHelper c'git_repository_head_detached . repoPrim

isHeadOrphan :: Repository -> IO Bool
isHeadOrphan = boolHelper c'git_repository_head_orphan . repoPrim

isEmpty :: Repository -> IO Bool
isEmpty = boolHelper c'git_repository_is_empty . repoPrim

isBare :: Repository -> IO Bool
isBare = boolHelper c'git_repository_is_bare . repoPrim

-- don't need to worry about handling CString here. It's a struct member.
pathHelper num repo = withForeignPtr (repoPrim repo) (\ptr -> c'git_repository_path ptr num) >>= peekCString
  
path :: Repository -> IO FilePath
path = pathHelper 0
  
indexPath :: Repository -> IO FilePath
indexPath = pathHelper 1

objectDatabasePath :: Repository -> IO FilePath
objectDatabasePath = pathHelper 2

workingDirectoryPath :: Repository -> IO FilePath
workingDirectoryPath = pathHelper 3

config :: Repository -> FilePath -> FilePath -> IO Config
config = throw NotImplemented