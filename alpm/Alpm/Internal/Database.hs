module Alpm.Internal.Database where

import Foreign

---

type Database = Ptr RawDatabase

-- | Wraps the type @alpm_db_t@.
data RawDatabase
