{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Dependency where

import Alpm.Internal.Enums
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <alpm.h>

---

-- | Wraps the type @alpm_depend_t@.
data Dependency = Dependency { name :: CString
                             , version :: CString
                             , desc :: CString
                             , hash :: CULong
                             , mode :: ComparisonMode }

instance Storable Dependency where
  sizeOf _ = (#size alpm_depend_t)
  alignment _ = alignment (undefined :: CULong)

  peek ptr = Dependency
    <$> (#peek alpm_depend_t, name) ptr
    <*> (#peek alpm_depend_t, version) ptr
    <*> (#peek alpm_depend_t, desc) ptr
    <*> (#peek alpm_depend_t, name_hash) ptr
    <*> (#peek alpm_depend_t, mod) ptr

  poke ptr (Dependency n v d h m) = do
    (#poke alpm_depend_t, name) ptr n
    (#poke alpm_depend_t, version) ptr v
    (#poke alpm_depend_t, desc) ptr d
    (#poke alpm_depend_t, name_hash) ptr h
    (#poke alpm_depend_t, mod) ptr m
