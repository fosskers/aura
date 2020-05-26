{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module    : Aura.Settings
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Definition of the runtime environment.

module Aura.Settings
  ( Settings(..)
  , logFuncOfL
    -- * Aura Configuration
  , BuildConfig(..), BuildSwitch(..)
  , buildPathOfL, buildUserOfL, buildSwitchesOfL, allsourcePathOfL, vcsPathOfL
  , switch
  , Truncation(..)
  , defaultBuildDir
    -- * Pacman Interop
  , CommonConfig(..), CommonSwitch(..)
  , cachePathOfL, logPathOfL
  , ColourMode(..)
  , shared
    -- * Makepkg Interop
  , Makepkg(..)
  ) where

import           Aura.Types
import           Network.HTTP.Client (Manager)
import           RIO
import qualified RIO.Set as S
import qualified RIO.Text as T

---

-- | How @-As@ should truncate its results.
data Truncation = None | Head Word | Tail Word deriving (Eq, Show)

-- | CLI flags that will be passed down to @makepkg@ when building packages.
data Makepkg = IgnoreArch | AllSource | SkipInteg | SkipPGP deriving (Eq, Ord, Show)

instance Flagable Makepkg where
  asFlag IgnoreArch = ["--ignorearch"]
  asFlag AllSource  = ["--allsource"]
  asFlag SkipInteg  = ["--skipinteg"]
  asFlag SkipPGP    = ["--skippgpcheck"]

-- | Flags that are common to both Aura and Pacman.
-- Aura will react to them, but also pass them through to `pacman`
-- calls if necessary.
data CommonConfig = CommonConfig
  { cachePathOf      :: !(Either FilePath FilePath)
  , configPathOf     :: !(Either FilePath FilePath)
  , logPathOf        :: !(Either FilePath FilePath)
  , commonSwitchesOf :: !(Set CommonSwitch) } deriving (Show, Generic)

cachePathOfL :: Lens' CommonConfig (Either FilePath FilePath)
cachePathOfL f cc = (\cp -> cc { cachePathOf = cp }) <$> f (cachePathOf cc)

logPathOfL :: Lens' CommonConfig (Either FilePath FilePath)
logPathOfL f cc = (\cp -> cc { logPathOf = cp }) <$> f (logPathOf cc)

instance Flagable CommonConfig where
  asFlag (CommonConfig cap cop lfp cs) =
    either (const []) (\p -> ["--cachedir", T.pack p]) cap
    <> either (const []) (\p -> ["--config", T.pack p]) cop
    <> either (const []) (\p -> ["--logfile", T.pack p]) lfp
    <> asFlag cs

-- | Yes/No-style switches that are common to both Aura and Pacman.
-- Aura acts on them first, then passes them down to @pacman@ if necessary.
data CommonSwitch = NoConfirm | NeededOnly | Debug | Colour ColourMode | Overwrite Text
  deriving (Eq, Ord, Show)

instance Flagable CommonSwitch where
  asFlag NoConfirm     = ["--noconfirm"]
  asFlag NeededOnly    = ["--needed"]
  asFlag Debug         = ["--debug"]
  asFlag (Colour m)    = "--color" : asFlag m
  asFlag (Overwrite t) = "--overwrite" : asFlag t

-- | Matches Pacman's colour options. `Auto` will ensure that text will only be coloured
-- when the output target is a terminal.
data ColourMode = Never | Always | Auto deriving (Eq, Ord, Show)

instance Flagable ColourMode where
  asFlag Never  = ["never"]
  asFlag Always = ["always"]
  asFlag Auto   = ["auto"]

-- | Settings unique to the AUR package building process.
data BuildConfig = BuildConfig
  { makepkgFlagsOf  :: !(Set Makepkg)
  , buildPathOf     :: !(Maybe FilePath)
  , buildUserOf     :: !(Maybe User)
  , allsourcePathOf :: !(Maybe FilePath)
  , vcsPathOf       :: !(Maybe FilePath)
  , truncationOf    :: !Truncation  -- For `-As`
  , buildSwitchesOf :: !(Set BuildSwitch) } deriving (Show)

buildPathOfL :: Lens' BuildConfig (Maybe FilePath)
buildPathOfL f bc = (\bp -> bc { buildPathOf = bp }) <$> f (buildPathOf bc)

buildUserOfL :: Lens' BuildConfig (Maybe User)
buildUserOfL f bc = (\bu -> bc { buildUserOf = bu }) <$> f (buildUserOf bc)

buildSwitchesOfL :: Lens' BuildConfig (Set BuildSwitch)
buildSwitchesOfL f bc = (\bs -> bc { buildSwitchesOf = bs }) <$> f (buildSwitchesOf bc)

allsourcePathOfL :: Lens' BuildConfig (Maybe FilePath)
allsourcePathOfL f bc = (\pth -> bc { allsourcePathOf = pth }) <$> f (allsourcePathOf bc)

vcsPathOfL :: Lens' BuildConfig (Maybe FilePath)
vcsPathOfL f bc = (\pth -> bc { vcsPathOf = pth }) <$> f (vcsPathOf bc)

-- | Extra options for customizing the build process.
data BuildSwitch = DeleteMakeDeps
                 | DiffPkgbuilds
                 | DontSuppressMakepkg
                 | DryRun
                 | HotEdit
                 | LowVerbosity
                 | RebuildDevel
                 | SortAlphabetically  -- For `-As`
                 | ForceBuilding
                 | NoPkgbuildCheck
                 | AsDeps
                 | SkipDepCheck
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
  , ignoresOf      :: !(Set PkgName)
  , commonConfigOf :: !CommonConfig
  , buildConfigOf  :: !BuildConfig
  , logLevelOf     :: !LogLevel
  , logFuncOf      :: !LogFunc }
  deriving stock (Generic)

logFuncOfL :: Lens' Settings LogFunc
logFuncOfL f s = (\lf -> s { logFuncOf = lf }) <$> f (logFuncOf s)

-- | Unless otherwise specified, packages will be built within @/tmp@.
defaultBuildDir :: FilePath
defaultBuildDir = "/tmp"
