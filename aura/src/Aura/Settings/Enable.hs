{-# LANGUAGE OverloadedStrings #-}

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colin@fosskers.ca>

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

import           Aura.Flags
import           Aura.Languages (Language, langFromLocale)
import           Aura.Pacman
import           Aura.Settings.Base
import           BasePrelude
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Directory (doesDirectoryExist)
import           System.Environment (getEnvironment)
import           Utilities

---

getSettings :: Maybe Language -> ([Flag], [String], [String]) -> IO (Maybe Settings)
getSettings lang (auraFlags, input, pacOpts) = do
  confFile    <- getPacmanConf
  environment <- M.fromList . map (bimap T.pack T.pack) <$> getEnvironment
  pmanCommand <- getPacmanCmd environment $ noPowerPillStatus auraFlags
  buildPath'  <- checkBuildPath (buildPath auraFlags) (getCachePath confFile)
  manager     <- newManager tlsManagerSettings
  let language   = checkLang lang environment
      buildUser' = fmap T.pack (buildUser auraFlags) <|> getTrueUser environment
  pure $ do
    bu <- buildUser'
    Just Settings { inputOf         = map T.pack input
                  , pacOptsOf       = map T.pack pacOpts
                  , otherOptsOf     = map (T.pack . show) auraFlags
                  , managerOf       = manager
                  , environmentOf   = environment
                  , buildUserOf     = User bu
                  , langOf          = language
                  , pacmanCmdOf     = pmanCommand
                  , editorOf        = getEditor environment
                  , ignoredPkgsOf   = map T.pack $ getIgnoredPkgs confFile <> ignoredAuraPkgs auraFlags
                  , makepkgFlagsOf  = map T.pack $ makepkgFlags auraFlags
                  , buildPathOf     = T.pack buildPath'
                  , cachePathOf     = T.pack $ getCachePath confFile
                  , logFilePathOf   = T.pack $ getLogFilePath confFile
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
                  , buildABSDeps    = False
                  , dryRun          = dryRunStatus auraFlags }

debugOutput :: Settings -> IO ()
debugOutput ss = do
  let yn a = if a then "Yes!" else "No."
      env  = environmentOf ss
  traverse_ T.putStrLn [ "User              => " <> fromMaybe "Unknown!" (M.lookup "USER" env)
                       , "True User         => " <> fromMaybe "Unknown!" (getTrueUser env)
                       , "Build User        => " <> _user (buildUserOf ss)
                       , "Using Sudo?       => " <> yn (M.member "SUDO_USER" env)
                       , "Pacman Flags      => " <> T.unwords (pacOptsOf ss)
                       , "Other Flags       => " <> T.unwords (otherOptsOf ss)
                       , "Other Input       => " <> T.unwords (inputOf ss)
                       , "Language          => " <> T.pack (show $ langOf ss)
                       , "Pacman Command    => " <> pacmanCmdOf ss
                       , "Editor            => " <> editorOf ss
                       , "Ignored Pkgs      => " <> T.unwords (ignoredPkgsOf ss)
                       , "Build Path        => " <> buildPathOf ss
                       , "Pkg Cache Path    => " <> cachePathOf ss
                       , "Log File Path     => " <> logFilePathOf ss
                       , "Quiet?            => " <> yn (beQuiet ss)
                       , "Suppress Makepkg? => " <> T.pack (show $ suppressMakepkg ss)
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
checkBuildPath bp bp' = bool bp' bp <$> doesDirectoryExist bp
