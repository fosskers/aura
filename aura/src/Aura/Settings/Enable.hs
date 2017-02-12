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

import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Monoid
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesDirectoryExist)
import System.Environment (getEnvironment)

import Aura.Flags
import Aura.Languages (Language, langFromLocale)
import Aura.MakePkg (makepkgConfFile)
import Aura.Pacman
import Aura.Settings.Base

import Shell
import Utilities (ifte_, readFileUTF8)

---

getSettings :: Maybe Language -> ([Flag], [String], [String]) -> IO Settings
getSettings lang (auraFlags, input, pacOpts) = do
  confFile    <- getPacmanConf
  environment <- getEnvironment
  pmanCommand <- getPacmanCmd environment $ noPowerPillStatus auraFlags
  makepkgConf <- readFileUTF8 makepkgConfFile
  buildPath'  <- checkBuildPath (buildPath auraFlags) (getCachePath confFile)
  manager     <- newManager tlsManagerSettings
  let language   = checkLang lang environment
      buildUser' = fromMaybe (getTrueUser environment) (buildUser auraFlags)
  pure Settings { inputOf         = input
                , pacOptsOf       = pacOpts
                , otherOptsOf     = show <$> auraFlags
                , managerOf       = manager
                , environmentOf   = environment
                , buildUserOf     = buildUser'
                , langOf          = language
                , pacmanCmdOf     = pmanCommand
                , editorOf        = getEditor environment
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
      env  = environmentOf ss
  traverse_ putStrLn [ "User              => " <> getUser' env
                     , "True User         => " <> getTrueUser env
                     , "Build User        => " <> buildUserOf ss
                     , "Using Sudo?       => " <> yn (varExists "SUDO_USER" env)
                     , "Pacman Flags      => " <> unwords (pacOptsOf ss)
                     , "Other Flags       => " <> unwords (otherOptsOf ss)
                     , "Other Input       => " <> unwords (inputOf ss)
                     , "Language          => " <> show (langOf ss)
                     , "Pacman Command    => " <> pacmanCmdOf ss
                     , "Editor            => " <> editorOf ss
                     , "$CARCH            => " <> carchOf ss
                     , "Ignored Pkgs      => " <> unwords (ignoredPkgsOf ss)
                     , "Build Path        => " <> buildPathOf ss
                     , "Pkg Cache Path    => " <> cachePathOf ss
                     , "Log File Path     => " <> logFilePathOf ss
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

checkLang :: Maybe Language -> Environment -> Language
checkLang Nothing env   = langFromLocale $ getLocale env
checkLang (Just lang) _ = lang

-- | Defaults to the cache path if no (legal) build path was given.
checkBuildPath :: FilePath -> FilePath -> IO FilePath
checkBuildPath bp bp' = ifte_ bp bp' <$> doesDirectoryExist bp
