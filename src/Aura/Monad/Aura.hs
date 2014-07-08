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
    , AuraError
    , runAura
    , failure
    , catch
    , wrap
    , getErrorMsg
    , liftIO
    , ask
    , asks
    , (<$>)
    ) where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative

import Aura.Settings.Base (Settings)

---

{- The Aura Monad. Functions of note:
return  : yields a successful value.
failure : yields an error and bypasses all other operations.
catch   : catches an error.
wrap    : If given an Either, rewraps it into an Aura Monad.
(>>=)   : fails on the first error.
liftIO  : Perform intermittent IO using `liftIO`.
ask     : Obtain run-time settings.
runAura : Unwraps an Aura action. Must be passed `Settings` as well.
-}
newtype Aura a = A { runA :: ErrorT AuraError (ReaderT Settings IO) a }
  deriving ( Monad, MonadError AuraError, MonadReader Settings, MonadIO
           , Functor, Applicative)

-- This needs to be expanded.
data AuraError = M String deriving (Eq,Show)

instance Error AuraError where
    noMsg  = strMsg "No error message given."
    strMsg = M

runAura :: Aura a -> Settings -> IO (Either AuraError a)
runAura a = runReaderT $ runErrorT (runA a)

failure :: String -> Aura a
failure = throwError . strMsg

catch :: Aura a -> (String -> Aura a) -> Aura a
catch a h = catchError a (\(M m) -> h m)

wrap :: Either AuraError a -> Aura a
wrap (Left (M m)) = failure m
wrap (Right a)    = return a

getErrorMsg :: AuraError -> String
getErrorMsg (M s) = s
