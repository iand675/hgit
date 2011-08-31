{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Data.Git.Types where
import Foreign.Ptr
import Foreign.ForeignPtr
import Bindings.Libgit2

newtype Repository = Repository {repoPrim :: ForeignPtr C'git_repository}
newtype ObjectDB   = ObjectDB   {odbPrim :: Ptr C'git_odb}
                     
newtype Config = Config {confPrim :: ForeignPtr C'git_config}
newtype Index = Index {ixPrim :: ForeignPtr C'git_index}
newtype Tag = Tag {tagPrim :: ForeignPtr C'git_tag}
newtype IndexEntry = IndexEntry {ixePrim :: ForeignPtr C'git_index_entry}

toRepository ptr = do 
  fptr <- newForeignPtr p'git_repository_free ptr
  return $! Repository fptr

toObjectDB ptr = return $! ObjectDB ptr
    
toIndex ptr = do
  fptr <- newForeignPtr p'git_index_free ptr
  return $! Index fptr

toConfig ptr = do
  fptr <- newForeignPtr p'git_config_free ptr
  return $! Config fptr
