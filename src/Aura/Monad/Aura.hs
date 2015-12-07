{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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
    , runAura
    , failure
    , catch
    , wrap
    , liftIO
    , ask
    , asks
    ) where

import Data.Void
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict

import Aura.Settings.Base (Settings)

---

{- The Aura Monad. Functions of note:
pure    : yields a successful value.
failure : yields an error and bypasses all other operations.
catch   : catches an error.
wrap    : If given an Either, rewraps it into an Aura Monad.
(>>=)   : fails on the first error.
liftIO  : Perform intermittent IO using `liftIO`.
ask     : Obtain run-time settings.
runAura : Unwraps an Aura action. Must be passed `Settings` as well.
-}
type Aura a = Eff (Exc String :> Reader Settings :> Lift IO :> Void) a

runAura :: Aura a -> Settings -> IO (Either String a)
runAura a s = runLift $ flip runReader s $ runExc a

failure :: String -> Aura a
failure = throwExc

catch :: Aura a -> (String -> Aura a) -> Aura a
catch = catchExc

wrap :: Either String a -> Aura a
wrap = liftEither

liftIO :: IO a -> Aura a
liftIO = lift

asks :: (Settings -> a) -> Aura a
asks = flip fmap ask
