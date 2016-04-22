module Arel.Util where

import Data.Text (unpack)
import Prelude hiding (FilePath)
import Shelly

---

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right b) = Just b

fptos :: FilePath -> String
fptos = unpack . toTextIgnore
