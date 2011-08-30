{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Data.Git.Types where
import Foreign.ForeignPtr
import Bindings.Libgit2

import Prelude hiding ((.), id)
newtype Repository = Repository {repoPrim :: ForeignPtr C'git_repository}
newtype ObjectDB   = ObjectDB   {odbPrim :: ForeignPtr C'git_odb}
newtype Config = Config {confPrim :: ForeignPtr C'git_config}
newtype Index = Index {ixPrim :: ForeignPtr C'git_index}
newtype Tag = Tag {tagPrim :: ForeignPtr C'git_tag}

toRepository ptr = do 
  fptr <- newForeignPtr p'git_repository_free ptr
  return $! Repository fptr

toObjectDB ptr = do  
  fptr <- newForeignPtr p'git_odb_close ptr
  return $! ObjectDB fptr
  
toManagedObjectDB ptr = do
  fptr <- newForeignPtr_ ptr
  return $! ObjectDB fptr
  
toIndex ptr = do
  fptr <- newForeignPtr p'git_index_free ptr
  return $! Index fptr
