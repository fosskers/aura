{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm
  ( -- * Handlers
    Handle
  , initialize, release
  , rootPath, dbPath, lockfile
  -- * Packages
  , Package
  , PkgGroup(..)
  -- * Databases
  , Database
  -- * Transactions
  , Transaction
  -- * Misc.
  , File(..)
  , version
  , errorMsg
  ) where

import           Alpm.Error
import           Alpm.List
import qualified Data.Text as T
import           Foreign
import           Foreign.C
import           System.IO.Unsafe

---

-- | A connection to underlying ALPM machinery. Must be constructed via
-- `initialize` and released via `releaseHandle` when finished.
type Handle = Ptr RawHandle

data RawHandle

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

-- | The current version of /alpm.h/ residing on your system.
version :: T.Text
version = T.pack . unsafePerformIO $ peekCString alpm_version

-- | A message corresponding to some error code.
errorMsg :: Error -> T.Text
errorMsg = T.pack . unsafePerformIO . peekCString . alpm_strerror

-- TODO: Is this thread-safe? Can multiple test suites call this at the same time?
-- Or maybe we just open one Handle at the beginning, and go willy-nilly.
-- | Most other functions can't be used until this is called.
foreign import ccall unsafe "alpm.h alpm_initialize"
  alpm_init :: CString -> CString -> Ptr Error -> IO (Ptr RawHandle)

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

-- | Releases internal ALPM memory, disconnects from the database, and
-- removes the lock file (if present). The library (mostly) can't be used after
-- this is called. Returns @0@ if successful.
foreign import ccall unsafe "alpm.h alpm_release"
  release :: Handle -> IO CInt

foreign import ccall unsafe "alpm.h alpm_option_get_root"
  alpm_option_get_root :: Handle -> CString

-- | The root of the OS, set by `initialize`.
rootPath :: Handle -> T.Text
rootPath = T.pack . unsafePerformIO . peekCString . alpm_option_get_root

foreign import ccall unsafe "alpm.h alpm_option_get_dbpath"
  alpm_option_get_dbpath :: Handle -> CString

-- | The path to the database directory, set by `initialize`.
dbPath :: Handle -> T.Text
dbPath = T.pack . unsafePerformIO . peekCString . alpm_option_get_dbpath

foreign import ccall unsafe "alpm.h alpm_option_get_lockfile"
  alpm_option_get_lockfile :: Handle -> CString

-- | The location of the database lockfile.
lockfile :: Handle -> T.Text
lockfile = T.pack . unsafePerformIO . peekCString . alpm_option_get_lockfile
