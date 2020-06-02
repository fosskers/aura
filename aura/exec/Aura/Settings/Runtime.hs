{-# LANGUAGE BangPatterns #-}

{-

Copyright 2012 - 2020 Colin Woodbury <colin@fosskers.ca>

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

module Aura.Settings.Runtime ( withEnv ) where

import           Aura.Core (Env(..))
import           Aura.Flags
import           Aura.Languages
import           Aura.Packages.AUR (aurRepo)
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pacman
import           Aura.Settings
import           Aura.Settings.External
import           Aura.Shell
import           Aura.Types
import           Aura.Utils
import           Data.Bifunctor (Bifunctor(..))
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           RIO hiding (first)
import           RIO.Lens (each, _Left, _Right)
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Environment (getEnvironment)
import           Text.Printf

---

-- | Bracket around the `Env` type to properly initialize the `Manager` and
-- `RIO` logger function (among other things).
--
-- Throws in `IO` if there were any errors.
withEnv :: Program -> (Env -> IO a) -> IO a
withEnv (Program op co bc lng ll) f = do
  let ign = S.fromList $ op ^.. _Right . _AurSync . to toList . each . _AurIgnore . to toList . each
      igg = S.fromList $ op ^.. _Right . _AurSync . to toList . each . _AurIgnoreGroup . to toList . each
  confFile    <- getPacmanConf (either id id $ configPathOf co) >>= either throwM pure
  auraConf    <- auraConfig <$> getAuraConf defaultAuraConf
  when (ll == LevelDebug) . printf "%s\n" $ show auraConf
  environment <- M.fromList . map (bimap T.pack T.pack) <$> getEnvironment
  manager     <- newManager tlsManagerSettings
  isTerm      <- hIsTerminalDevice stdout
  fromGroups  <- maybe (pure S.empty) groupPackages . nes
    $ getIgnoredGroups confFile <> igg
  let !bu = buildUserOf bc <|> acUser auraConf <|> getTrueUser environment
  when (isNothing bu) . throwM $ Failure whoIsBuildUser_1
  repos <- (<>) <$> pacmanRepo <*> aurRepo
  lopts <- setLogMinLevel ll . setLogUseLoc True <$> logOptionsHandle stderr True
  withLogFunc lopts $ \logFunc -> do
    let !ss = Settings
          { managerOf      = manager
          , envOf          = environment
          , langOf         = setLang (lng <|> acLang auraConf) environment
          , editorOf       = fromMaybe (getEditor environment) $ acEditor auraConf
          , isTerminal     = isTerm
          , ignoresOf      = getIgnoredPkgs confFile <> fromGroups <> ign
          , commonConfigOf =
              -- | These maintain the precedence order: flags, config file entry, default
              co & cachePathOfL . _Left %~ (\x -> fromMaybe x $ getCachePath confFile)
                 & logPathOfL   . _Left %~ (\x -> fromMaybe x $ getLogFilePath confFile)
          , buildConfigOf =
              bc & buildUserOfL     .~ bu
                 & buildPathOfL     %~ (<|> acBuildPath auraConf)
                 & allsourcePathOfL %~ (<|> acASPath auraConf)
                 & vcsPathOfL       %~ (<|> acVCSPath auraConf)
                 & buildSwitchesOfL %~ (<> maybe S.empty S.singleton (acAnalyse auraConf))
          , logLevelOf = ll
          , logFuncOf = logFunc }
    f (Env repos ss)

setLang :: Maybe Language -> Environment -> Language
setLang Nothing env   = fromMaybe English . langFromLocale $ getLocale env
setLang (Just lang) _ = lang
