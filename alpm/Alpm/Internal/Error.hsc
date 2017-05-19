{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Error where

import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <alpm.h>

-- | ALPM error codes, wrapping the enum type @alpm_errno_t@.
newtype Error = Error { err :: CInt }

instance Storable Error where
  sizeOf _ = (#size alpm_errno_t)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = Error <$> peekByteOff ptr 0
  poke ptr (Error e) = pokeByteOff ptr 0 e

-- | A message corresponding to some error code.
foreign import ccall unsafe "alpm.h alpm_strerror"
  alpm_strerror :: Error -> CString

#{enum Error, Error,
  err_memory = ALPM_ERR_MEMORY,
  err_system = ALPM_ERR_SYSTEM,
  err_badperms = ALPM_ERR_BADPERMS,
  err_notAFile = ALPM_ERR_NOT_A_FILE,
  err_notADir = ALPM_ERR_NOT_A_DIR,
  err_wrongArgs = ALPM_ERR_WRONG_ARGS,
  err_diskSpace = ALPM_ERR_DISK_SPACE,

  err_handleNull = ALPM_ERR_HANDLE_NULL,
  err_handleNotNull = ALPM_ERR_HANDLE_NOT_NULL,
  err_handleLock = ALPM_ERR_HANDLE_LOCK,

  err_dbOpen = ALPM_ERR_DB_OPEN,
  err_dbCreate = ALPM_ERR_DB_CREATE,
  err_dbNull = ALPM_ERR_DB_NULL,
  err_dbNotNull = ALPM_ERR_DB_NOT_NULL,
  err_dbNotFound = ALPM_ERR_DB_NOT_FOUND,
  err_dbInvalid = ALPM_ERR_DB_INVALID,
  err_dbInvalidSig = ALPM_ERR_DB_INVALID_SIG,
  err_dbVersion = ALPM_ERR_DB_VERSION,
  err_dbWrite = ALPM_ERR_DB_WRITE,
  err_dbRemove = ALPM_ERR_DB_REMOVE,

  err_serverBadUrl = ALPM_ERR_SERVER_BAD_URL,
  err_serverNone = ALPM_ERR_SERVER_NONE,

  err_transNotNull = ALPM_ERR_TRANS_NOT_NULL,
  err_transNull = ALPM_ERR_TRANS_NULL,
  err_transDupTarget = ALPM_ERR_TRANS_DUP_TARGET,
  err_transNotInit = ALPM_ERR_TRANS_NOT_INITIALIZED,
  err_transNotPrep = ALPM_ERR_TRANS_NOT_PREPARED,
  err_transAbort = ALPM_ERR_TRANS_ABORT,
  err_transType = ALPM_ERR_TRANS_TYPE,
  err_transNotLocked = ALPM_ERR_TRANS_NOT_LOCKED,
  err_transHookFailed = ALPM_ERR_TRANS_HOOK_FAILED,

  err_pkgNotFound = ALPM_ERR_PKG_NOT_FOUND,
  err_pkgIgnored = ALPM_ERR_PKG_IGNORED,
  err_pkgInvalid = ALPM_ERR_PKG_INVALID,
  err_pkgChecksum = ALPM_ERR_PKG_INVALID_CHECKSUM,
  err_pkgSig = ALPM_ERR_PKG_INVALID_SIG,
  err_pkgMissingSig = ALPM_ERR_PKG_MISSING_SIG,
  err_pkgOpen = ALPM_ERR_PKG_OPEN,
  err_pkgCantRemove = ALPM_ERR_PKG_CANT_REMOVE,
  err_pkgName = ALPM_ERR_PKG_INVALID_NAME,
  err_pkgArch = ALPM_ERR_PKG_INVALID_ARCH,
  err_pkgRepoNotFound = ALPM_ERR_PKG_REPO_NOT_FOUND,

  err_sigMissing = ALPM_ERR_SIG_MISSING,
  err_sigInvalid = ALPM_ERR_SIG_INVALID,

  err_deltaInvalid = ALPM_ERR_DLT_INVALID,
  err_deltaPatchFailed = ALPM_ERR_DLT_PATCHFAILED,

  err_depsUnsatisfied = ALPM_ERR_UNSATISFIED_DEPS,
  err_depsConflicting = ALPM_ERR_CONFLICTING_DEPS,
  err_depsFileConflict = ALPM_ERR_FILE_CONFLICTS,

  err_retrieve = ALPM_ERR_RETRIEVE,
  err_invalidRegex = ALPM_ERR_INVALID_REGEX,

  err_libarchive = ALPM_ERR_LIBARCHIVE,
  err_libcurl = ALPM_ERR_LIBCURL,
  err_download = ALPM_ERR_EXTERNAL_DOWNLOAD,
  err_gpgme = ALPM_ERR_GPGME
}
