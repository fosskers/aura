{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, FlexibleContexts, Rank2Types #-}

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
    , AuraEff
    , runAura
    , failure
    , catch
    , wrap
    , wrapString
    , liftIO
    , liftShelly
    , ask
    , asks
    ) where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict

import qualified Data.Text as T

import qualified Shelly as S

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
type AuraEff r a = (Member (Exc T.Text) r, Member (Reader Settings) r, SetMember Lift (Lift S.Sh) r) => Eff r a
type Aura a = forall r . AuraEff r a

runAura :: Aura a -> Settings -> IO (Either T.Text a)
runAura a s = S.shelly $ runLift $ flip runReader s $ runExc a

failure :: T.Text -> Aura a
failure = throwExc

catch :: Aura a -> (T.Text -> Aura a) -> Aura a
catch = catchExc

wrap :: Either T.Text a -> Aura a
wrap = liftEither

wrapString :: Either String a -> Aura a
wrapString = either (failure . T.pack) pure

liftIO :: IO a -> Aura a
liftIO = lift . S.liftIO

liftShelly :: S.Sh a -> Aura a
liftShelly = lift

asks :: (Settings -> a) -> Aura a
asks = flip fmap ask
