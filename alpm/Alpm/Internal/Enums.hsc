{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Enums where

import Foreign.C.Types

#include <alpm.h>

---

-- | Where did a `Package` come from?
newtype PkgOrigin = PkgOrigin CInt deriving (Eq)

#{enum PkgOrigin, PkgOrigin,
   po_file = ALPM_PKG_FROM_FILE,
   po_localdb = ALPM_PKG_FROM_LOCALDB,
   po_syncdb = ALPM_PKG_FROM_SYNCDB
 }

newtype InstalledAs = InstalledAs CInt deriving (Eq)

#{enum InstalledAs, InstalledAs,
  ia_explicit = ALPM_PKG_REASON_EXPLICIT,
  ia_dependency = ALPM_PKG_REASON_DEPEND
 }
