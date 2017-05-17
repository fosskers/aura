{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm where

import           Alpm.List
import qualified Data.Text as T
import           Foreign
import           Foreign.C
import           System.IO.Unsafe

---

data Handle
data Package
data Database
data Transaction

data PkgGroup = PkgGroup { group_name :: CString
                         , group_pkgs :: Ptr (List Package) }

instance Storable PkgGroup where
  sizeOf _ = 16
  alignment _ = 8

  peek ptr = do
    name <- peekByteOff ptr 0
    pkgs <- peekByteOff ptr 8
    pure $ PkgGroup name pkgs

  poke ptr (PkgGroup n ps) = do
    pokeByteOff ptr 0 n
    pokeByteOff ptr 8 ps

data File = File { file_name :: CString
                 , file_size :: CULong
                 , file_mode :: CULong }

instance Storable File where
  sizeOf _ = 24
  alignment _ = alignment (undefined :: CULong)

  peek ptr = do
    name <- peekByteOff ptr 0
    size <- peekByteOff ptr 8
    mode <- peekByteOff ptr 16
    pure $ File name size mode

  poke ptr (File n s m) = do
    pokeByteOff ptr 0 n
    pokeByteOff ptr 8 s
    pokeByteOff ptr 16 m

foreign import ccall unsafe "alpm.h alpm_version"
  alpm_version :: CString

alpmVersion :: T.Text
alpmVersion = T.pack . unsafePerformIO $ peekCString alpm_version
