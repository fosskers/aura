{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Control.Monad.Except
import Control.Monad.Reader

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
newtype Aura a = A { runA :: ExceptT String (ReaderT Settings IO) a }
  deriving ( Monad, MonadError String, MonadReader Settings, MonadIO,
             Functor, Applicative)

runAura :: Aura a -> Settings -> IO (Either String a)
runAura = runReaderT . runExceptT . runA

failure :: String -> Aura a
failure = throwError

catch :: Aura a -> (String -> Aura a) -> Aura a
catch a h = catchError a h

wrap :: Either String a -> Aura a
wrap (Left m)  = failure m
wrap (Right a) = pure a
