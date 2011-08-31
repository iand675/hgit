{-# LANGUAGE DeriveDataTypeable #-}
module Data.Git.Errors (
  GitError(..),
  errorOr
  ) where
import Data.Typeable
import Bindings.Libgit2.Errors
import Control.Exception
data GitError = Error
              | NotObjectID
              | NotFound
              | NoMemory
              | OSError
              | ObjectType
              | NotARepository
              | InvalidType
              | MissingObjectData
              | PackCorrupted
              | FileLockFail
              | ZLib
              | Busy
              | BareIndex
              | InvalidRefName
              | RefCorrupted
              | TooDeeplyNestedRef
              | PackedRefsCorrupted
              | InvalidPath
              | RevWalkerEmpty
              | InvalidRefState
              | NotImplemented
              | AlreadyExists
              | BufferOverflow
              | NotANum
              | StreamError
              | InvalidArgs
              | ObjectCorrupted
              | AmbiguousObjectIDPrefix
              | BackendPassthrough
              | NoMatch
              | ShortBuffer
              deriving (Show, Typeable, Enum, Ord, Eq)
instance Exception GitError

errorOr errCode alt = case fromIntegral errCode of
  0  -> alt
  n  -> throw $ [Error .. ShortBuffer] !! (abs n - 1)