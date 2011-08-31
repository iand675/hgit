module Data.Git.Repository (
  Repository (), -- | A git repository handle.
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

maybeCStr Nothing  = return nullPtr
maybeCStr (Just s) = newCString s

maybePtr Nothing  = return nullPtr
maybePtr (Just s) = malloc

open :: FilePath -> IO Repository
open fp = withCString fp $ \str -> do
  writeVal <- malloc
  result   <- c'git_repository_open writeVal str
  retVal <- peek writeVal
  free writeVal
  result `errorOr` toRepository retVal
    
openWithPaths :: GitDir -> Maybe ObjectDir -> Maybe IndexFile -> Maybe WorkTree -> IO Repository
openWithPaths fp od ixf wt = withCString fp $ \str -> do
  odcs  <- maybeCStr od
  ixfcs <- maybeCStr ixf
  wtcs  <- maybeCStr wt
  writeVal <- malloc
  result <- c'git_repository_open2 writeVal str odcs ixfcs wtcs
  mapM_ free [odcs, ixfcs, wtcs]
  retVal <- peek writeVal
  free writeVal
  result `errorOr` toRepository retVal
    
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
index repo = withForeignPtr (repoPrim repo) $ \r -> 
  alloca $ \ix -> do
    result <- c'git_repository_index ix r
    retVal <- peek ix
    result `errorOr` toIndex retVal

initHelper i fp = withCString fp $ \str -> do
  alloca $ \ptr -> do
    result <- c'git_repository_init ptr str i
    retVal <- peek ptr
    result `errorOr` toRepository retVal

init :: FilePath -> IO Repository
init = initHelper 0

initBare :: FilePath -> IO Repository
initBare = initHelper 1

boolHelper f val = withForeignPtr val f >>= return . (/= 0)

isHeadDetached :: Repository -> IO Bool
isHeadDetached = boolHelper c'git_repository_head_detached . repoPrim

isHeadOrphan :: Repository -> IO Bool
isHeadOrphan = boolHelper c'git_repository_head_orphan . repoPrim

isEmpty :: Repository -> IO Bool
isEmpty = boolHelper c'git_repository_is_empty . repoPrim

isBare :: Repository -> IO Bool
isBare = boolHelper c'git_repository_is_bare . repoPrim

-- todo fix use of C'git_repository_pathid to use an enum
pathHelper num repo = do
  cstr <- withForeignPtr (repoPrim repo) (\ptr -> c'git_repository_path ptr num)
  str  <- peekCString cstr -- this is a struct member, so don't need to sweat deallocation.
  return str
  
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