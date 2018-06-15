{-# LANGUAGE MultiWayIf, ViewPatterns #-}

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.Commands.B
    ( saveState
    , restoreState
    , cleanStates
    ) where

import Aura.Core (warn)
import Aura.Languages
import Aura.Monad.Aura
import Aura.Settings
import Aura.State
import Aura.Types
import Aura.Utils (optionalPrompt)
import BasePrelude
import Data.Bitraversable
import Shelly

---

cleanStates :: Word -> Aura (Either Failure ())
cleanStates (fromIntegral -> n) = do
  ss   <- ask
  okay <- optionalPrompt ss $ cleanStates_2 n
  if | not okay  -> fmap Right . warn . cleanStates_3 $ langOf ss
     | otherwise -> getStateFiles >>= bitraverse pure (shelly . traverse_ rm . drop n . reverse)
