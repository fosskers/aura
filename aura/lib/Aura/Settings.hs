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

module Aura.Settings where

import           Aura.Languages (Language)
import           BasePrelude hiding (FilePath)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (Manager)
import           Shelly (FilePath, toTextIgnore)
import           Utilities (Environment, User, list)

---

-- | Types whose members can be converted to CLI flags.
class Flagable a where
  asFlag :: a -> [T.Text]

data Truncation = None | Head Int | Tail Int deriving (Eq, Show)

data Makepkg = IgnoreArch | AllSource | SkipInteg deriving (Eq, Ord, Show)

instance Flagable Makepkg where
  asFlag IgnoreArch = ["--ignorearch"]
  asFlag AllSource  = ["--allsource"]
  asFlag SkipInteg  = ["--skipinteg"]

-- | Flags that are common to both Aura and Pacman.
-- Aura will react to them, but also pass them through to `pacman`
-- calls if necessary.
data CommonConfig = CommonConfig { cachePathOf      :: Maybe FilePath
                                 , configPathOf     :: Maybe FilePath
                                 , logPathOf        :: Maybe FilePath
                                 , ignoredPkgsOf    :: S.Set Text
                                 , commonSwitchesOf :: S.Set CommonSwitch } deriving (Show)

instance Flagable CommonConfig where
  asFlag (CommonConfig cap cop lfp igs cs) =
    maybe [] (\p -> ["--cachedir", toTextIgnore p]) cap
    ++ maybe [] (\p -> ["--config", toTextIgnore p]) cop
    ++ maybe [] (\p -> ["--logfile", toTextIgnore p]) lfp
    ++ list [] (\xs -> ["--ignore", T.intercalate "," xs]) (toList igs)
    ++ concatMap asFlag (toList cs)

data CommonSwitch = NoConfirm | NeededOnly | Debug | Colour ColourMode deriving (Eq, Ord, Show)

instance Flagable CommonSwitch where
  asFlag NoConfirm  = ["--noconfirm"]
  asFlag NeededOnly = ["--needed"]
  asFlag Debug      = ["--debug"]
  asFlag (Colour m) = "--color" : asFlag m

data ColourMode = Never | Always | Auto deriving (Eq, Ord, Show)

instance Flagable ColourMode where
  asFlag Never  = ["never"]
  asFlag Always = ["always"]
  asFlag Auto   = ["auto"]

data BuildConfig = BuildConfig { makepkgFlagsOf  :: S.Set Makepkg
                               , buildPathOf     :: Maybe FilePath
                               , buildUserOf     :: Maybe User
                               , truncationOf    :: Truncation  -- For `-As`
                               , buildSwitchesOf :: S.Set BuildSwitch } deriving (Show)

-- | Extra options for customizing the build process.
data BuildSwitch = DeleteMakeDeps
                 | DiffPkgbuilds
                 | DontSuppressMakepkg
                 | DryRun
                 | HotEdit
                 | LowVerbosity
                 | RebuildDevel
                 | SortAlphabetically  -- For `-As`
                 | UseCustomizepkg
                 deriving (Eq, Ord, Show)

-- | Convenient short-hand.
switch :: Settings -> BuildSwitch -> Bool
switch ss bs = S.member bs . buildSwitchesOf $ buildConfigOf ss

shared :: Settings -> CommonSwitch -> Bool
shared ss c = S.member c . commonSwitchesOf $ commonConfigOf ss

-- | The global settings as set by the user with command-line flags.
data Settings = Settings { managerOf      :: Manager
                         , envOf          :: Environment
                         , langOf         :: Language
                         , editorOf       :: Text  -- TODO Add to `BuildConfig`?
                         , isTerminal     :: Bool
                         , commonConfigOf :: CommonConfig
                         , buildConfigOf  :: BuildConfig }
