{-# LANGUAGE OverloadedStrings #-}

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

module Aura.Settings.Base where

import           Aura.Languages (Language)
import           BasePrelude hiding (FilePath)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (Manager)
import           Shelly (FilePath)
import           Utilities (Environment, User)

---

-- | Types whose members can be converted to CLI flags.
class Flagable a where
  asFlag :: a -> T.Text

data Truncation = None | Head Int | Tail Int deriving (Eq, Show)

data Makepkg = IgnoreArch | AllSource

instance Flagable Makepkg where
  asFlag IgnoreArch = "--ignorearch"
  asFlag AllSource  = "--allsource"

data BuildConfig = BuildConfig { makepkgFlagsOf  :: S.Set Makepkg
                               , ignoredPkgsOf   :: S.Set Text
                               , buildPathOf     :: Maybe FilePath
                               , buildUserOf     :: Maybe User
                               , truncationOf    :: Truncation  -- For `-As`
                               , buildSwitchesOf :: S.Set BuildSwitch }

defaultConfig :: BuildConfig
defaultConfig = BuildConfig S.empty S.empty Nothing Nothing None S.empty

-- | Extra options for customizing the build process.
data BuildSwitch = LowVerbosity
                 | DeleteMakeDeps
                 | DontSuppressMakepkg
                 | DiffPkgbuilds
                 | RebuildDevel
                 | HotEdit
                 | NoConfirm
                 | NeededOnly
                 | UseCustomizepkg
                 | KeepSource
                 | DryRun
                 | SortAlphabetically  -- For `-As`
                 deriving (Eq, Ord)

-- | Convenient short-hand.
switch :: Settings -> BuildSwitch -> Bool
switch ss bs = S.member bs . buildSwitchesOf $ buildConfigOf ss

-- | The global settings as set by the user with command-line flags.
data Settings = Settings { inputOf       :: [Text]
                         , pacOptsOf     :: [Text]
                         , otherOptsOf   :: [Text]
                         , managerOf     :: Manager
                         , envOf         :: Environment
                         , langOf        :: Language
                         , editorOf      :: Text
                         , cachePathOf   :: FilePath
                         , logFilePathOf :: FilePath
                         , buildConfigOf :: BuildConfig }
