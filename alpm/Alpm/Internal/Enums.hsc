{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Enums where

import Data.Bits
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

-- | The method used to validate a package. These values can be bitwise
-- composed within ALPM to represent the desire for multiple validation
-- methods. Use `validations` to split these back up.
newtype Validation = Validation CInt deriving (Eq)

#{enum Validation, Validation,
  vm_unknown = ALPM_PKG_VALIDATION_UNKNOWN,
  vm_none = ALPM_PKG_VALIDATION_NONE,
  vm_md5 = ALPM_PKG_VALIDATION_MD5SUM,
  vm_sha256 = ALPM_PKG_VALIDATION_SHA256SUM,
  vm_sig = ALPM_PKG_VALIDATION_SIGNATURE
 }

-- | Potentially splits up a `Validation` value that might have been bitwise
-- composed by ALPM to represent multiple validation methods in a single value.
validations :: Validation -> [Validation]
validations v@(Validation 0) = [v]  -- UNKNOWN
validations (Validation v) = foldr f [] [0..3]
  where f n acc | testBit v n = Validation (2 ^ n) : acc
                | otherwise = acc
