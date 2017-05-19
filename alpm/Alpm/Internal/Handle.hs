{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Handle where

import           Alpm.Internal.Error
import qualified Data.Text as T
import           Foreign
import           Foreign.C
import           System.IO.Unsafe

---

-- | A connection to underlying ALPM machinery. Must be constructed via
-- `initialize` and released via `close` when finished.
type Handle = Ptr RawHandle

-- | Wraps the type @alpm_handle_t@.
data RawHandle

-- TODO: Is this thread-safe? Can multiple test suites call this at the same time?
-- Or maybe we just open one Handle at the beginning, and go willy-nilly.
-- | Most other functions can't be used until this is called.
foreign import ccall unsafe "alpm.h alpm_initialize"
  alpm_init :: CString -> CString -> Ptr Error -> IO (Ptr RawHandle)

-- | Releases internal ALPM memory, disconnects from the database, and
-- removes the lock file (if present). The library (mostly) can't be used after
-- this is called. Returns @0@ if successful.
foreign import ccall unsafe "alpm.h alpm_release"
  close :: Handle -> IO CInt

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
