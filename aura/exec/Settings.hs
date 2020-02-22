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

module Settings ( withEnv ) where

import           Aura.Core (Env(..))
import           Aura.Languages
import           Aura.Packages.AUR (aurRepo)
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pacman
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           Data.Bifunctor (Bifunctor(..))
import qualified Data.Set.NonEmpty as NES
import           Flags
import           Lens.Micro (folded, (^..), _Right)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           RIO hiding (FilePath, first)
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Environment (getEnvironment)

---

-- | Bracket around the `Env` type to properly initialize the `Manager` and
-- `RIO` logger function (among other things).
--
-- Throws in `IO` if there were any errors.
withEnv :: Program -> (Env -> IO a) -> IO a
withEnv (Program op co bc lng ll) f = do
  let ign = S.fromList $ op ^.. _Right . _AurSync . folded . _AurIgnore . folded
      igg = S.fromList $ op ^.. _Right . _AurSync . folded . _AurIgnoreGroup . folded
  confFile    <- getPacmanConf (either id id $ configPathOf co) >>= either throwM pure
  environment <- M.fromList . map (bimap T.pack T.pack) <$> getEnvironment
  manager     <- newManager tlsManagerSettings
  isTerm      <- hIsTerminalDevice stdout
  fromGroups  <- maybe (pure S.empty) groupPackages . NES.nonEmptySet
    $ getIgnoredGroups confFile <> igg
  let language = checkLang lng environment
  bu <- maybe (throwM $ Failure whoIsBuildUser_1) pure
    $ buildUserOf bc <|> getTrueUser environment
  repos <- (<>) <$> pacmanRepo <*> aurRepo
  lopts <- setLogMinLevel ll . setLogUseLoc True <$> logOptionsHandle stderr True
  withLogFunc lopts $ \logFunc -> do
    let !ss = Settings
          { managerOf      = manager
          , envOf          = environment
          , langOf         = language
          , editorOf       = getEditor environment
          , isTerminal     = isTerm
          , ignoresOf      = getIgnoredPkgs confFile <> fromGroups <> ign
          , commonConfigOf =
              -- | These maintain the precedence order: flags, config file entry, default
              co { cachePathOf =
                     first (\x -> fromMaybe x $ getCachePath confFile) $ cachePathOf co
                 , logPathOf   =
                     first (\x -> fromMaybe x $ getLogFilePath confFile) $ logPathOf co }
          , buildConfigOf = bc { buildUserOf = Just bu}
          , logFuncOf = logFunc }
    f (Env repos ss)

checkLang :: Maybe Language -> Environment -> Language
checkLang Nothing env   = langFromLocale $ getLocale env
checkLang (Just lang) _ = lang
