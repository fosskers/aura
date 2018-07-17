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

module Settings ( getSettings ) where

import           Aura.Cache (defaultPackageCache)
import           Aura.Languages
import           Aura.Pacman
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding (FilePath)
import           Data.Bitraversable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import           Flags (Program(..))
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Shelly
import           System.Environment (getEnvironment)
import           System.IO (hIsTerminalDevice, stdout)

---

getSettings :: Program -> IO (Either Failure Settings)
getSettings (Program _ co bc lng) = do
  confFile <- getPacmanConf . either id id $ configPathOf co
  join <$> bitraverse pure f confFile
  where f confFile = do
          environment <- M.fromList . map (bimap T.pack T.pack) <$> getEnvironment
          buildPath'  <- checkBuildPath (buildPathOf bc) defaultPackageCache
          manager     <- newManager tlsManagerSettings
          isTerm      <- hIsTerminalDevice stdout
          fromGroups  <- maybe (pure S.empty) groupPackages . NES.fromSet $ getIgnoredGroups confFile <> ignoredGroupsOf co
          let language   = checkLang lng environment
              buildUser' = buildUserOf bc <|> getTrueUser environment
          pure $ do
            bu <- maybe (Left $ Failure whoIsBuildUser_1) Right buildUser'
            Right Settings { managerOf      = manager
                           , envOf          = environment
                           , langOf         = language
                           , editorOf       = getEditor environment
                           , isTerminal     = isTerm
                           , commonConfigOf =
                             -- | These maintain the precedence order: flags, config file entry, default
                             co { cachePathOf   = first (\x -> fromMaybe x $ getCachePath confFile) $ cachePathOf co
                                , logPathOf     = first (\x -> fromMaybe x $ getLogFilePath confFile) $ logPathOf co
                                , ignoredPkgsOf = getIgnoredPkgs confFile <> ignoredPkgsOf co <> fromGroups
                                }
                           , buildConfigOf  =
                             bc { buildPathOf   = buildPath'
                                , buildUserOf   = Just bu
                                }
                           }

checkLang :: Maybe Language -> Environment -> Language
checkLang Nothing env   = langFromLocale $ getLocale env
checkLang (Just lang) _ = lang

checkBuildPath :: MonadIO m => FilePath -> FilePath -> m FilePath
checkBuildPath bp def = bool def bp <$> shelly (test_d bp)
