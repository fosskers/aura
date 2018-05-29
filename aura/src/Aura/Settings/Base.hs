{-

Copyright 2012 - 2016 Colin Woodbury <colin@fosskers.ca>

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

import Aura.Languages (Language)
import BasePrelude hiding (FilePath)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Shelly (FilePath)
import Utilities (Environment, User)

---

data SortScheme = ByVote | Alphabetically deriving (Eq, Show)

data Truncation = None | Head Int | Tail Int deriving (Eq, Show)

data Suppression = BeQuiet | BeVerbose deriving (Eq, Show)

-- | The global settings as set by the user with command-line flags.
data Settings = Settings { inputOf         :: [Text]
                         , pacOptsOf       :: [Text]
                         , otherOptsOf     :: [Text]
                         , managerOf       :: Manager
                         , environmentOf   :: Environment
                         , buildUserOf     :: User
                         , langOf          :: Language
                         , pacmanCmdOf     :: Text
                         , editorOf        :: Text
                         , ignoredPkgsOf   :: [Text]
                         , makepkgFlagsOf  :: [Text]
                         , buildPathOf     :: FilePath
                         , cachePathOf     :: FilePath
                         , logFilePathOf   :: FilePath
                         , sortSchemeOf    :: SortScheme  -- For `-As`
                         , truncationOf    :: Truncation  -- For `-As`
                         , beQuiet         :: Bool
                         , suppressMakepkg :: Suppression
                         , delMakeDeps     :: Bool
                         , diffPkgbuilds   :: Bool
                         , rebuildDevel    :: Bool
                         , mayHotEdit      :: Bool
                         , mustConfirm     :: Bool
                         , neededOnly      :: Bool
                         , useCustomizepkg :: Bool
                         , noPowerPill     :: Bool
                         , keepSource      :: Bool
                         , buildABSDeps    :: Bool
                         , dryRun          :: Bool }
