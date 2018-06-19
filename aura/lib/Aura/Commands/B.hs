{-# LANGUAGE MultiWayIf, ViewPatterns, FlexibleContexts, TypeApplications, MonoLocalBinds #-}

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
import Aura.Settings
import Aura.State
import Aura.Types
import Aura.Utils (optionalPrompt)
import BasePrelude
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Shelly

---

-- TODO Move this to `States` and delete this module
cleanStates :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Word -> Eff r ()
cleanStates (fromIntegral -> n) = do
  ss   <- ask
  okay <- send . optionalPrompt @IO ss $ cleanStates_2 n
  if | not okay  -> send . warn . cleanStates_3 $ langOf ss
     | otherwise -> getStateFiles >>= send . shelly @IO . traverse_ rm . drop n . reverse
