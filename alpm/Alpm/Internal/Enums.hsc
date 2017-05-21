{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Enums where

import Foreign
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

-- | Wraps the enum @alpm_depmod_t@.
newtype ComparisonMode = ComparisonMode CInt deriving (Eq)

instance Storable ComparisonMode where
  sizeOf _ = (#size alpm_depmod_t)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = ComparisonMode <$> peekByteOff ptr 0
  poke ptr (ComparisonMode e) = pokeByteOff ptr 0 e

#{enum ComparisonMode, ComparisonMode,
   cm_any = ALPM_DEP_MOD_ANY,
   cm_eq = ALPM_DEP_MOD_EQ,
   cm_ge = ALPM_DEP_MOD_GE,
   cm_le = ALPM_DEP_MOD_LE,
   cm_gt = ALPM_DEP_MOD_GT,
   cm_lt = ALPM_DEP_MOD_LT
 }
