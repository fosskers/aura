{-# LANGUAGE OverloadedStrings #-}
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

module Aura.Settings.Enable
    ( getSettings
    , debugOutput ) where

import System.Environment (getEnvironment)
import Data.Maybe         (fromJust)
import Data.Monoid
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as IO

import Aura.Languages (Language, langFromEnv)
import Aura.MakePkg   (makepkgConfFile)
import Aura.Settings.Base
import Aura.Pacman
import Aura.Flags

import Utilities (ifte_, exists)
import Aura.Shell
import Shelly
import Prelude hiding (FilePath)

---

getSettings :: Maybe Language -> ([Flag], [T.Text], [T.Text]) -> Sh Settings
getSettings lang' (auraFlags, input, pacOpts) = do
  confFile    <- getPacmanConf
  pmanCommand <- getPacmanCmd $ noPowerPillStatus auraFlags
  makepkgConf <- getConf $ fromText makepkgConfFile
  buildPath'  <- checkBuildPath (buildPath auraFlags) (getCachePath confFile)
  editor'     <- editor
  buildUser'  <- getUser auraFlags
  language    <- checkLang lang'
  pure Settings { inputOf         = input
                , pacOptsOf       = pacOpts
                , otherOptsOf     = T.pack . show <$> auraFlags
                , buildUserOf     = buildUser'
                , langOf          = language
                , pacmanCmdOf     = fromText pmanCommand
                , editorOf        = editor'
                , carchOf         = singleEntry makepkgConf "CARCH"
                                    "COULDN'T READ $CARCH"
                , ignoredPkgsOf   = getIgnoredPkgs confFile <>
                                    ignoredAuraPkgs auraFlags
                , makepkgFlagsOf  = makepkgFlags auraFlags
                , buildPathOf     = buildPath'
                , cachePathOf     = getCachePath confFile
                , logFilePathOf   = getLogFilePath confFile
                , sortSchemeOf    = sortSchemeStatus auraFlags
                , truncationOf    = truncationStatus auraFlags
                , beQuiet         = quietStatus auraFlags
                , suppressMakepkg = suppressionStatus auraFlags
                , delMakeDeps     = delMakeDepsStatus auraFlags
                , mustConfirm     = confirmationStatus auraFlags
                , neededOnly      = neededStatus auraFlags
                , mayHotEdit      = hotEditStatus auraFlags
                , diffPkgbuilds   = pbDiffStatus auraFlags
                , rebuildDevel    = rebuildDevelStatus auraFlags
                , useCustomizepkg = customizepkgStatus auraFlags
                , noPowerPill     = noPowerPillStatus auraFlags
                , keepSource      = keepSourceStatus auraFlags
                , buildABSDeps    = buildABSDepsStatus auraFlags
                , dryRun          = dryRunStatus auraFlags }

debugOutput :: Settings -> IO ()
debugOutput ss = do
  let yn a = if a then "Yes!" else "No."
  user <- shelly getUser'
  trueuser <- fromJust <$> shelly getTrueUser
  sudop <- shelly $ varExists "SUDO_USER"
  traverse_ IO.putStrLn [ "User              => " <> user
                        , "True User         => " <> trueuser
                        , "Build User        => " <> buildUserOf ss
                        , "Using Sudo?       => " <> yn sudop
                        , "Pacman Flags      => " <> T.unwords (pacOptsOf ss)
                        , "Other Flags       => " <> T.unwords (otherOptsOf ss)
                        , "Other Input       => " <> T.unwords (inputOf ss)
                        , "Language          => " <> T.pack (show (langOf ss))
                        , "Pacman Command    => " <> toTextIgnore (pacmanCmdOf ss)
                        , "Editor            => " <> editorOf ss
                        , "$CARCH            => " <> carchOf ss
                        , "Ignored Pkgs      => " <> T.unwords (ignoredPkgsOf ss)
                        , "Build Path        => " <> toTextIgnore (buildPathOf ss)
                        , "Pkg Cache Path    => " <> toTextIgnore (cachePathOf ss)
                        , "Log File Path     => " <> toTextIgnore (logFilePathOf ss)
                        , "Quiet?            => " <> yn (beQuiet ss)
                        , "Silent Building?  => " <> yn (suppressMakepkg ss)
                        , "Must Confirm?     => " <> yn (mustConfirm ss)
                        , "Needed only?      => " <> yn (neededOnly ss)
                        , "PKGBUILD editing? => " <> yn (mayHotEdit ss)
                        , "Diff PKGBUILDs?   => " <> yn (diffPkgbuilds ss)
                        , "Rebuild Devel?    => " <> yn (rebuildDevel ss)
                        , "Use Customizepkg? => " <> yn (useCustomizepkg ss)
                        , "Forego PowerPill? => " <> yn (noPowerPill ss)
                        , "Keep source?      => " <> yn (keepSource ss) ]

checkLang :: Maybe Language -> Sh Language
checkLang Nothing = langFromEnv <$> lang
checkLang (Just lang') = pure lang'

getUser :: [Flag] -> Sh T.Text
getUser flags = do
  case buildUser flags of
    Just a -> pure a
    Nothing -> do
      user <- getTrueUser
      case user of
        Just a -> pure a
        Nothing -> pure "" -- should not happen, old code used fromJust

-- | Defaults to the cache path if no (legal) build path was given.
checkBuildPath :: FilePath -> FilePath -> Sh FilePath
checkBuildPath bp bp' = ifte_ bp bp' <$> exists bp
