{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm
  ( -- * Handlers
    Handle
  , initialize, close
  , rootPath, dbPath, lockfile
  -- * Packages
  , Package
  -- ** Fields
  -- | These are valid pure functions so long as the `Package` hasn't been freed.
  , name, version, description, url, filename, base, packager, md5sum, sha256sum, arch
  , buildDate, installDate, origin, size, installedSize, licenses, groups, signature
  , depends, optdepends, conflicts, provides, deltas, replaces, database, validation
  -- ** Other
  , validmd5
  , required, optionals
  , shouldIgnore
  , PkgGroup(..)
  -- * Databases
  , Database
  -- * Transactions
  , Transaction
  -- * Errors
  , Error(..)
  , currentError, errorMsg
  -- * Misc.
  , File(..)
  , alpmVersion
  ) where

import           Alpm.Internal.Database
import           Alpm.Internal.Error
import           Alpm.Internal.Handle
import           Alpm.Internal.Package
import           Alpm.List
import           Data.Foldable (fold)
import qualified Data.Text as T
import qualified Data.Versions as V
import           Foreign
import           Foreign.C
import           System.IO.Unsafe

---

-- | Wraps the type @alpm_trans_t@.
data Transaction

-- | Wraps the type @alpm_group_t@.
data PkgGroup = PkgGroup { groupName :: CString
                         , groupPkgs :: Ptr (List Package) }

instance Storable PkgGroup where
  sizeOf _ = 16
  alignment _ = 8

  peek ptr = PkgGroup <$> peekByteOff ptr 0 <*> peekByteOff ptr 8

  poke ptr (PkgGroup n ps) = do
    pokeByteOff ptr 0 n
    pokeByteOff ptr 8 ps

data File = File { fileName :: CString
                 , fileSize :: CULong
                 , fileMode :: CULong }

instance Storable File where
  sizeOf _ = 24
  alignment _ = alignment (undefined :: CULong)

  peek ptr = File <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16

  poke ptr (File n s m) = do
    pokeByteOff ptr 0 n
    pokeByteOff ptr 8 s
    pokeByteOff ptr 16 m

-- | Create a `Handle` to underlying ALPM machinery. Expects two FilePaths,
-- the OS's root directory and the location of the ALPM database. These are
-- usually @/@ and @\/var\/lib\/pacman\/@ by default.
initialize :: T.Text -> T.Text -> IO (Either T.Text Handle)
initialize root dbpath = do
  root'   <- newCString $ T.unpack root
  dbpath' <- newCString $ T.unpack dbpath
  alloca $ \errPtr -> do
    h <- alpm_init root' dbpath' errPtr
    if h == nullPtr
      then Left . errorMsg <$> peek errPtr
      else pure $ Right h

foreign import ccall unsafe "alpm.h alpm_version"
  alpm_version :: CString

-- | The current version of /alpm.h/ residing on your system.
alpmVersion :: V.SemVer
alpmVersion = fold . V.semver . T.pack . unsafePerformIO $ peekCString alpm_version

-- | A message corresponding to some error code.
errorMsg :: Error -> T.Text
errorMsg = T.pack . unsafePerformIO . peekCString . alpm_strerror

-- | The current error code. Can change over time, hence `IO`.
foreign import ccall unsafe "alpm.h alpm_errno"
  currentError :: Handle -> IO Error
