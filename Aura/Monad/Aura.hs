{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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
    , runAura ) where

import Control.Monad.Reader
import Control.Monad.Error

import Aura.Settings (Settings)

---

newtype Aura a = A { runA :: ErrorT AuraError (ReaderT Settings IO) a }
    deriving (Monad, MonadError AuraError, MonadReader Settings, MonadIO)

-- This needs to be expanded.
data AuraError = E | M String deriving (Eq,Show)

instance Error AuraError where
    noMsg  = E
    strMsg = M

runAura :: Aura a -> Settings -> IO (Either AuraError a)
runAura a ss = runReaderT (runErrorT (runA a)) ss
