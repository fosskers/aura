{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Aura.Monad.Aura
  ( Aura
  , MonadIO
  , MonadReader
  , runAura
  , liftIO
  , ask
  , asks
  ) where

import Aura.Settings (Settings)
import BasePrelude
import Control.Monad.Reader

---

-- | The Aura Monad, the main effect type for this program.
-- This used to contain an `ExceptT` but no longer does, meaning
-- that a function that yields some `Aura a` will almost certainly
-- succeed. Functions that can fail will return `Aura (Either Failure a)`.
newtype Aura a = Aura { runA :: ReaderT Settings IO a }
  deriving ( Functor, Applicative, Monad, MonadReader Settings, MonadIO )

runAura :: Aura a -> Settings -> IO a
runAura = runReaderT . runA
