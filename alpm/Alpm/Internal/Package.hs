{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Package where

import           Alpm.Internal.Database
import           Alpm.Internal.Dependency
import           Alpm.Internal.Enums
import           Alpm.Internal.Handle
import           Alpm.List
import qualified Data.Text as T
import           Data.Versions
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
  items <- mapM peekCString $ toList ptr
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
  items <- mapM peekCString $ toList ptr
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

foreign import ccall unsafe "alpm.h alpm_pkg_get_base"
  alpm_pkg_get_base :: Package -> CString

base :: Package -> T.Text
base = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_base

foreign import ccall unsafe "alpm.h alpm_pkg_get_name"
  alpm_pkg_get_name :: Package -> CString

name :: Package -> T.Text
name = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_name

foreign import ccall unsafe "alpm.h alpm_pkg_get_version"
  alpm_pkg_get_version :: Package -> CString

version :: Package -> Either ParsingError Versioning
version = parseV . T.pack . unsafePerformIO . peekCString . alpm_pkg_get_version

foreign import ccall unsafe "alpm.h alpm_pkg_get_desc"
  alpm_pkg_get_desc :: Package -> CString

description :: Package -> T.Text
description = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_desc

foreign import ccall unsafe "alpm.h alpm_pkg_get_url"
  alpm_pkg_get_url :: Package -> CString

url :: Package -> T.Text
url = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_url

foreign import ccall unsafe "alpm.h alpm_pkg_get_packager"
  alpm_pkg_get_packager :: Package -> CString

packager :: Package -> T.Text
packager = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_packager

foreign import ccall unsafe "alpm.h alpm_pkg_get_md5sum"
  alpm_pkg_get_md5sum :: Package -> CString

md5sum :: Package -> T.Text
md5sum = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_md5sum

foreign import ccall unsafe "alpm.h alpm_pkg_get_sha256sum"
  alpm_pkg_get_sha256sum :: Package -> CString

sha256sum :: Package -> T.Text
sha256sum = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_sha256sum

foreign import ccall unsafe "alpm.h alpm_pkg_get_arch"
  alpm_pkg_get_arch :: Package -> CString

arch :: Package -> T.Text
arch = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_arch

foreign import ccall unsafe "alpm.h alpm_pkg_get_builddate"
  alpm_pkg_get_builddate :: Package -> CLong

-- TODO: Convert this to an actual time type!
-- | The timestamp of when the `Package` was built.
buildDate :: Package -> Int64
buildDate = fromIntegral . alpm_pkg_get_builddate

foreign import ccall unsafe "alpm.h alpm_pkg_get_installdate"
  alpm_pkg_get_installdate :: Package -> CLong

-- TODO: Convert this to an actual time type!
-- | The timestamp of when the `Package` was installed.
installDate :: Package -> Int64
installDate = fromIntegral . alpm_pkg_get_installdate

-- | Where did this `Package` come from?
foreign import ccall unsafe "alpm.h alpm_pkg_get_origin"
  origin :: Package -> PkgOrigin

foreign import ccall unsafe "alpm.h alpm_pkg_get_size"
  alpm_pkg_get_size :: Package -> CLong

-- | The size of this `Package` in bytes. Only available for packages from a
-- sync database or package file.
size :: Package -> Maybe Int64
size p | origin p == po_localdb = Nothing
       | otherwise = Just . fromIntegral $ alpm_pkg_get_size p

foreign import ccall unsafe "alpm.h alpm_pkg_get_isize"
  alpm_pkg_get_isize :: Package -> CLong

installedSize :: Package -> Int64
installedSize = fromIntegral . alpm_pkg_get_isize

-- | Was a `Package` installed explicitely, or as a dependency for another?
foreign import ccall unsafe "alpm.h alpm_pkg_get_reason"
  installedAs :: Package -> InstalledAs

foreign import ccall unsafe "alpm.h alpm_pkg_get_licenses"
  alpm_pkg_get_licenses :: Package -> Ptr (List CString)

licenses :: Package -> [T.Text]
licenses = map T.pack . unsafePerformIO . mapM peekCString . toList . alpm_pkg_get_licenses

foreign import ccall unsafe "alpm.h alpm_pkg_get_groups"
  alpm_pkg_get_groups :: Package -> Ptr (List CString)

-- | Any groups that this `Package` might belong to.
groups :: Package -> [T.Text]
groups = map T.pack . unsafePerformIO . mapM peekCString . toList . alpm_pkg_get_groups

foreign import ccall unsafe "alpm.h alpm_pkg_get_depends"
  alpm_pkg_get_depends :: Package -> Ptr (List Dependency)

-- | Dependencies which this `Package` requires.
depends :: Package -> [Dependency]
depends = toList . alpm_pkg_get_depends

foreign import ccall unsafe "alpm.h alpm_pkg_get_optdepends"
  alpm_pkg_get_optdepends :: Package -> Ptr (List Dependency)

-- | Optional dependencies which can add extra functionality. Not required
-- for installation or building the package.
optdepends :: Package -> [Dependency]
optdepends = toList . alpm_pkg_get_optdepends

foreign import ccall unsafe "alpm.h alpm_pkg_get_conflicts"
  alpm_pkg_get_conflicts :: Package -> Ptr (List Dependency)

-- | A list of `Package`s that cannot exist on the system at the same time
-- as this one. For example, a package @linux-lts@ would at the same time mark
-- @linux@ as a /provided/ package (as in `provides`) and list it as a
-- conflict. This would ensure a user couldn't install two kernels on their
-- machine at the same time.
conflicts :: Package -> [Dependency]
conflicts = toList . alpm_pkg_get_conflicts

foreign import ccall unsafe "alpm.h alpm_pkg_get_provides"
  alpm_pkg_get_provides :: Package -> Ptr (List Dependency)

-- | A list of `Package` "identities" provided by this package. For example,
-- a package named @linux-lts@ could /provide/ @linux@, and any other package
-- requiring @linux@ as a dependency would count it as satisfied by @linux-lts@.
provides :: Package -> [Dependency]
provides = toList . alpm_pkg_get_provides

foreign import ccall unsafe "alpm.h alpm_pkg_get_deltas"
  alpm_pkg_get_deltas :: Package -> Ptr (List CString)

deltas :: Package -> [T.Text]
deltas = map T.pack . unsafePerformIO . mapM peekCString . toList . alpm_pkg_get_deltas

foreign import ccall unsafe "alpm.h alpm_pkg_get_replaces"
  alpm_pkg_get_replaces :: Package -> Ptr (List Dependency)

replaces :: Package -> [Dependency]
replaces = toList . alpm_pkg_get_replaces

foreign import ccall unsafe "alpm.h alpm_pkg_get_db"
  alpm_pkg_get_db :: Package -> Database

-- | The `Database` that this `Package` came from, unless it was loaded from
-- a file.
database :: Package -> Maybe Database
database p | origin p == po_file = Nothing
           | otherwise = Just $ alpm_pkg_get_db p

foreign import ccall unsafe "alpm.h alpm_pkg_get_base64_sig"
  alpm_pkg_get_base64_sig :: Package -> CString

-- | The base 64 encoded package signature.
signature :: Package -> T.Text
signature = T.pack . unsafePerformIO . peekCString . alpm_pkg_get_base64_sig

foreign import ccall unsafe "alpm.h alpm_pkg_get_validation"
  alpm_pkg_get_validation :: Package -> Validation

-- | The methods by which to validate this `Package`.
validation :: Package -> [Validation]
validation = validations . alpm_pkg_get_validation
