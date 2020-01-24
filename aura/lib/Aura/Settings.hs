{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Settings
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Definition of the runtime environment.

module Aura.Settings
  ( Settings(..)
    -- * Aura Configuration
  , BuildConfig(..), BuildSwitch(..)
  , switch
  , Truncation(..)
  , defaultBuildDir
    -- * Pacman Interop
  , CommonConfig(..), CommonSwitch(..)
  , ColourMode(..)
  , shared
    -- * Makepkg Interop
  , Makepkg(..)
  ) where

import           Aura.Types
import           BasePrelude
import qualified Data.Set as S
import qualified Data.Text as T
import           Network.HTTP.Client (Manager)
import           System.Path (Absolute, Path, fromAbsoluteFilePath, toFilePath)

---

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
data CommonConfig = CommonConfig
  { cachePathOf      :: !(Either (Path Absolute) (Path Absolute))
  , configPathOf     :: !(Either (Path Absolute) (Path Absolute))
  , logPathOf        :: !(Either (Path Absolute) (Path Absolute))
  , commonSwitchesOf :: !(S.Set CommonSwitch) } deriving (Show, Generic)

instance Flagable CommonConfig where
  asFlag (CommonConfig cap cop lfp cs) =
    either (const []) (\p -> ["--cachedir", T.pack $ toFilePath p]) cap
    <> either (const []) (\p -> ["--config", T.pack $ toFilePath p]) cop
    <> either (const []) (\p -> ["--logfile", T.pack $ toFilePath p]) lfp
    <> asFlag cs

-- | Yes/No-style switches that are common to both Aura and Pacman.
-- Aura acts on them first, then passes them down to @pacman@ if necessary.
data CommonSwitch = NoConfirm | NeededOnly | Debug | Colour ColourMode deriving (Eq, Ord, Show)

instance Flagable CommonSwitch where
  asFlag NoConfirm  = ["--noconfirm"]
  asFlag NeededOnly = ["--needed"]
  asFlag Debug      = ["--debug"]
  asFlag (Colour m) = "--color" : asFlag m

-- | Matches Pacman's colour options. `Auto` will ensure that text will only be coloured
-- when the output target is a terminal.
data ColourMode = Never | Always | Auto deriving (Eq, Ord, Show)

instance Flagable ColourMode where
  asFlag Never  = ["never"]
  asFlag Always = ["always"]
  asFlag Auto   = ["auto"]

-- | Settings unique to the AUR package building process.
data BuildConfig = BuildConfig
  { makepkgFlagsOf  :: !(S.Set Makepkg)
  , buildPathOf     :: !(Path Absolute)
  , buildUserOf     :: !(Maybe User)
  , truncationOf    :: !Truncation  -- For `-As`
  , buildSwitchesOf :: !(S.Set BuildSwitch) } deriving (Show)

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
                 | NoPkgbuildCheck
                 deriving (Eq, Ord, Show)

-- | Is some Aura-specific setting turned on for this run?
switch :: Settings -> BuildSwitch -> Bool
switch ss bs = S.member bs . buildSwitchesOf $ buildConfigOf ss

-- | Is some Aura/Pacman common setting turned on for this run?
shared :: Settings -> CommonSwitch -> Bool
shared ss c = S.member c . commonSwitchesOf $ commonConfigOf ss

-- | The global settings as set by the user with command-line flags.
data Settings = Settings
  { managerOf      :: !Manager
  , envOf          :: !Environment
  , langOf         :: !Language
  , editorOf       :: !FilePath
  , isTerminal     :: !Bool
  , ignoresOf      :: !(S.Set PkgName)
  , commonConfigOf :: !CommonConfig
  , buildConfigOf  :: !BuildConfig }

-- | Unless otherwise specified, packages will be built within @/tmp@.
defaultBuildDir :: Path Absolute
defaultBuildDir = fromAbsoluteFilePath "/tmp"
