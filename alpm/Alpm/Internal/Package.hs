{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Package where

import           Alpm.Internal.Handle
import           Alpm.List
import qualified Data.Text as T
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           System.IO.Unsafe

---

-- | An ALPM package.
type Package = Ptr RawPackage

-- | Wraps the type @alpm_pkg_t@.
data RawPackage

foreign import ccall unsafe "alpm.h alpm_pkg_load"
  alpm_pkg_load :: Handle -> CString -> CInt -> CInt -> IO (Ptr (Ptr RawPackage))

-- | Find a `Package` in a `List` by name.
foreign import ccall unsafe "alpm.h alpm_pkg_find"
  alpm_pkg_find :: Ptr (List RawPackage) -> CString -> Package

-- | Free a `Package`'s memory. Returns @0@ on success, @-1@ on error.
foreign import ccall unsafe "alpm.h alpm_pkg_free"
  alpm_pkg_free :: Package -> IO CInt

-- | Check a `Package`'s validity. @0@ when valid, @-1@ otherwise.
foreign import ccall unsafe "alpm.h alpm_pkg_checkmd5sum"
  alpm_pkg_checkmd5 :: Package -> IO CInt

-- | Does a `Package` have a valid MD5 checksum?
validmd5 :: Package -> Bool
validmd5 p = 0 == unsafePerformIO (alpm_pkg_checkmd5 p)

-- | A list of packages who require the given `Package`. The `List` is newly
-- allocated and must be freed by us when we're done with it.
foreign import ccall unsafe "alpm.h alpm_pkg_compute_requiredby"
  alpm_pkg_requiredby :: Package -> IO (Ptr (List CString))

-- | A list of packages that require the given package as a dependency.
required :: Package -> IO [T.Text]
required p = do
  ptr <- alpm_pkg_requiredby p
  items <- sequence . map peekCString $ toList ptr
  alpm_list_free ptr
  pure $ map T.pack items

-- | A list of packages which optionally require the given `Package`. The
-- `List` is newly allocated and must be freed by us when we're done with it.
foreign import ccall unsafe "alpm.h alpm_pkg_compute_optionalfor"
  alpm_pkg_optionals :: Package -> IO (Ptr (List CString))

-- | A list of packages which have the given package as an optional dependency.
optionals :: Package -> IO [T.Text]
optionals p = do
  ptr <- alpm_pkg_optionals p
  items <- sequence . map peekCString $ toList ptr
  alpm_list_free ptr
  pure $ map T.pack items

-- | Should a given `Package` be ignored? @1@ if the package is present in
-- @IgnorePkg@ or is part of an ignored group, @0@ otherwise.
foreign import ccall unsafe "alpm.h alpm_pkg_should_ignore"
  alpm_pkg_should_ignore :: Handle -> Package -> CInt

-- | Should a given `Package` be ignored?
shouldIgnore :: Handle -> Package -> Bool
shouldIgnore h p = alpm_pkg_should_ignore h p == 1

foreign import ccall unsafe "alpm.h alpm_pkg_get_filename"
  alpm_pkg_get_filename :: Package -> CString

filename :: Package -> T.Text
filename = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_filename
