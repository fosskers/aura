{-# LANGUAGE ForeignFunctionInterface #-}

module Alpm.Internal.Enums
  (
    PkgOrigin(..)
  , po_file, po_localdb, po_syncdb
  , InstalledAs(..)
  , ia_explicit, ia_dependency
  , ComparisonMode(..)
  , cm_any, cm_eq, cm_ge, cm_le, cm_gt, cm_lt
  , SigLevel(..)
  , sl_package, sl_pkg_optional, sl_pkg_marginal, sl_pkg_unknown, sl_database, sl_db_optional
  , sl_db_marginal, sl_db_unknown, sl_default
  , Validation(..)
  , validations
  , vm_unknown, vm_none, vm_md5, vm_sha256, vm_sig
  ) where

import Data.Bits
import Foreign
import Foreign.C.Types

#include <alpm.h>

---

-- | Where did a `Package` come from?
newtype PkgOrigin = PkgOrigin CInt deriving (Eq)

newtype InstalledAs = InstalledAs CInt deriving (Eq)

-- | Wraps the enum @alpm_depmod_t@.
newtype ComparisonMode = ComparisonMode CInt deriving (Eq)

instance Storable ComparisonMode where
  sizeOf _ = (#size alpm_depmod_t)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = ComparisonMode <$> peekByteOff ptr 0
  poke ptr (ComparisonMode e) = pokeByteOff ptr 0 e

-- | Wraps the enum @alpm_siglevel_t@.
newtype SigLevel = SigLevel CInt deriving (Eq)

instance Monoid SigLevel where
  mempty = SigLevel 0

  SigLevel a `mappend` SigLevel b = SigLevel $ a .|. b

-- siglevels :: SigLevel -> [SigLevel]
-- siglevels =

-- | The method used to validate a package. These values can be bitwise
-- composed within ALPM to represent the desire for multiple validation
-- methods. Use `validations` to split these back up.
newtype Validation = Validation CInt deriving (Eq, Show)

instance Monoid Validation where
  mempty = Validation 0

  Validation a `mappend` Validation b = Validation $ a .|. b

-- | Potentially splits up a `Validation` value that might have been bitwise
-- composed by ALPM to represent multiple validation methods in a single value.
validations :: Validation -> [Validation]
validations v@(Validation 0) = [v]  -- UNKNOWN
validations (Validation v) = foldr f [] [0..3]
  where f n acc | testBit v n = Validation (2 ^ n) : acc
                | otherwise = acc

---

#{enum PkgOrigin, PkgOrigin,
   po_file = ALPM_PKG_FROM_FILE,
   po_localdb = ALPM_PKG_FROM_LOCALDB,
   po_syncdb = ALPM_PKG_FROM_SYNCDB
 }

#{enum InstalledAs, InstalledAs,
  ia_explicit = ALPM_PKG_REASON_EXPLICIT,
  ia_dependency = ALPM_PKG_REASON_DEPEND
 }

#{enum ComparisonMode, ComparisonMode,
   cm_any = ALPM_DEP_MOD_ANY,
   cm_eq = ALPM_DEP_MOD_EQ,
   cm_ge = ALPM_DEP_MOD_GE,
   cm_le = ALPM_DEP_MOD_LE,
   cm_gt = ALPM_DEP_MOD_GT,
   cm_lt = ALPM_DEP_MOD_LT
 }

#{enum SigLevel, SigLevel,
   sl_package      = ALPM_SIG_PACKAGE,
   sl_pkg_optional = ALPM_SIG_PACKAGE_OPTIONAL,
   sl_pkg_marginal = ALPM_SIG_PACKAGE_MARGINAL_OK,
   sl_pkg_unknown  = ALPM_SIG_PACKAGE_UNKNOWN_OK,
   sl_database     = ALPM_SIG_DATABASE,
   sl_db_optional  = ALPM_SIG_DATABASE_OPTIONAL,
   sl_db_marginal  = ALPM_SIG_DATABASE_MARGINAL_OK,
   sl_db_unknown   = ALPM_SIG_DATABASE_UNKNOWN_OK,
   sl_default      = ALPM_SIG_USE_DEFAULT
 }

#{enum Validation, Validation,
  vm_unknown = ALPM_PKG_VALIDATION_UNKNOWN,
  vm_none    = ALPM_PKG_VALIDATION_NONE,
  vm_md5     = ALPM_PKG_VALIDATION_MD5SUM,
  vm_sha256  = ALPM_PKG_VALIDATION_SHA256SUM,
  vm_sig     = ALPM_PKG_VALIDATION_SIGNATURE
 }
