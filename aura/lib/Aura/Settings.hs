{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Settings
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Definition of the runtime environment.

module Aura.Settings
  ( Settings(..)
  , Flagable(..)
    -- * Aura Configuration
  , BuildConfig(..), BuildSwitch(..)
  , switch
  , Truncation(..)
    -- * Pacman Interop
  , CommonConfig(..), CommonSwitch(..)
  , ColourMode(..)
  , shared
    -- * Makepkg Interop
  , Makepkg(..)
  ) where

import           Aura.Languages (Language)
import           Aura.Types (Environment, User, list)
import           BasePrelude hiding (FilePath)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (Manager)
import           Shelly (FilePath, toTextIgnore)

---

-- | Types whose members can be converted to CLI flags.
class Flagable a where
  asFlag :: a -> [T.Text]

-- | How @-As@ should truncate its results.
data Truncation = None | Head Word | Tail Word deriving (Eq, Show)

-- | CLI flags that will be passed down to @makepkg@ when building packages.
data Makepkg = IgnoreArch | AllSource | SkipInteg deriving (Eq, Ord, Show)

instance Flagable Makepkg where
  asFlag IgnoreArch = ["--ignorearch"]
  asFlag AllSource  = ["--allsource"]
  asFlag SkipInteg  = ["--skipinteg"]

-- | Flags that are common to both Aura and Pacman.
-- Aura will react to them, but also pass them through to `pacman`
-- calls if necessary.
data CommonConfig = CommonConfig { cachePathOf      :: Either FilePath FilePath
                                 , configPathOf     :: Either FilePath FilePath
                                 , logPathOf        :: Either FilePath FilePath
                                 , ignoredPkgsOf    :: S.Set Text
                                 , ignoredGroupsOf  :: S.Set Text
                                 , commonSwitchesOf :: S.Set CommonSwitch } deriving (Show)

instance Flagable CommonConfig where
  asFlag (CommonConfig cap cop lfp igs igg cs) =
    either (const []) (\p -> ["--cachedir", toTextIgnore p]) cap
    ++ either (const []) (\p -> ["--config", toTextIgnore p]) cop
    ++ either (const []) (\p -> ["--logfile", toTextIgnore p]) lfp
    ++ list [] (\xs -> ["--ignore", T.intercalate "," $ toList xs]) (toList igs)
    ++ list [] (\xs -> ["--ignoregroup", T.intercalate "," $ toList xs]) (toList igg)
    ++ foldMap asFlag cs

-- | Yes/No-style switches that are common to both Aura and Pacman.
-- Aura acts on them first, then passes them down to @pacman@ if necessary.
data CommonSwitch = NoConfirm | NeededOnly | Debug | Colour ColourMode deriving (Eq, Ord, Show)

instance Flagable CommonSwitch where
  asFlag NoConfirm    = ["--noconfirm"]
  asFlag NeededOnly   = ["--needed"]
  asFlag Debug        = ["--debug"]
  asFlag (Colour m)   = "--color" : asFlag m

-- | Matches Pacman's colour options. `Auto` will ensure that text will only be coloured
-- when the output target is a terminal.
data ColourMode = Never | Always | Auto deriving (Eq, Ord, Show)

instance Flagable ColourMode where
  asFlag Never  = ["never"]
  asFlag Always = ["always"]
  asFlag Auto   = ["auto"]

-- | Settings unique to the AUR package building process.
data BuildConfig = BuildConfig { makepkgFlagsOf  :: S.Set Makepkg
                               , buildPathOf     :: FilePath
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
                 | ForceBuilding
                 deriving (Eq, Ord, Show)

-- | Is some Aura-specific setting turned on for this run?
switch :: Settings -> BuildSwitch -> Bool
switch ss bs = S.member bs . buildSwitchesOf $ buildConfigOf ss

-- | Is some Aura/Pacman common setting turned on for this run?
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
