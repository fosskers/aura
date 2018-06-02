{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- An interface to `pacman`.
-- Takes any pacman arguments and applies it to pacman through the shell.

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

module Aura.Pacman
  ( -- * Calling Pacman
    pacman
  , pacmanOutput, pacmanSuccess
  , getPacmanCmd
  , syncDatabase
    -- * Paths
  , lockFile
  , pacmanConfFile
  , getCachePath
  , getLogFilePath
  , singleEntry
    -- * Pacman Config
  , getPacmanConf
  , getIgnoredPkgs
    -- * Misc.
  , getVersionInfo
  , verMsgPad
  , getPacmanHelpMsg
  ) where

import           Aura.Cache
import           Aura.Errors
import           Aura.Languages (pacmanFailure_1)
import           Aura.Monad.Aura
import           Aura.Settings.Base (pacmanCmdOf)
import           BasePrelude
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Shelly as Sh
import           Shelly hiding (FilePath, cmd)
import           System.IO (hFlush, stdout)
import           Text.Regex.PCRE ((=~))
import           Utilities

---

defaultCmd :: T.Text
defaultCmd = "pacman"

powerPillCmd :: Sh.FilePath
powerPillCmd = "/usr/bin/powerpill"

pacmanConfFile :: Sh.FilePath
pacmanConfFile = "/etc/pacman.conf"

defaultLogFile :: FilePath
defaultLogFile = "/var/log/pacman.log"

lockFile :: Sh.FilePath
lockFile = "/var/lib/pacman/db.lck"

getPacmanCmd :: MonadIO m => Environment -> Bool -> m T.Text
getPacmanCmd env nopp =
  case M.lookup "PACMAN" env of
    Just cmd -> pure cmd
    Nothing  -> do  -- Left space for more options later.
      powerPill <- shelly $ test_f powerPillCmd
      if | powerPill && not nopp -> pure $ toTextIgnore powerPillCmd
         | otherwise -> pure defaultCmd

getPacmanConf :: MonadIO m => m T.Text
getPacmanConf = shelly $ readfile pacmanConfFile

getConfFileField :: String -> String -> [String]
getConfFileField confFile field = words $ takeWhile p entry
    where (_, _, entry) = confFile =~ field :: (String, String, String)
          p c = c /= '\n' && c /= '#'

getIgnoredPkgs :: String -> [String]
getIgnoredPkgs confFile = getConfFileField confFile "^IgnorePkg[ ]+=[ ]+"

-- For config file fields that only have one value.
-- Caller must supply an alternative if the given field isn't found.
singleEntry :: String -> String -> String -> String
singleEntry confFile field alt = case getConfFileField confFile regex of
                                      []    -> alt
                                      entry -> noQs $ head entry
    where regex = "^" <> field <> "[ ]*=[ ]*"
          noQs  = filter (`notElem` ("\"" :: String))

getCachePath :: String -> FilePath
getCachePath confFile = singleEntry confFile "CacheDir" (T.unpack defaultPackageCache)

getLogFilePath :: String -> FilePath
getLogFilePath confFile = singleEntry confFile "LogFile" defaultLogFile

----------
-- ACTIONS
----------
pacman :: [T.Text] -> Aura (Either Failure ())
pacman args = do
  cmd <- asks pacmanCmdOf
  flush
  shelly . errExit False $ do
    runHandles (fromText cmd) args [ InHandle Inherit
                                   , OutHandle Inherit
                                   , ErrorHandle Inherit ] n
    code <- lastExitCode
    pure . bool (failure pacmanFailure_1) (Right ()) $ code == 0
    where flush   = liftIO $ hFlush stdout
          n _ _ _ = pure ()

-- | Run some `pacman` process, but only care about whether it succeeded.
pacmanSuccess :: MonadIO m => [T.Text] -> m Bool
pacmanSuccess args = fmap success . shelly . quietSh $ run_ "pacman" args
    where success (ExitSuccess, _) = True
          success _ = False

-- | Runs pacman silently and returns only the stdout.
pacmanOutput :: MonadIO m => [T.Text] -> m T.Text
pacmanOutput = fmap snd . shelly . quietSh . run "pacman"

syncDatabase :: [T.Text] -> Aura (Either Failure ())
syncDatabase pacOpts = pacman $ "-Sy" : pacOpts

getPacmanHelpMsg :: MonadIO m => m [T.Text]
getPacmanHelpMsg = T.lines <$> pacmanOutput ["-h"]

-- | Yields the lines given by `pacman -V` with the pacman image stripped.
getVersionInfo :: MonadIO m => m [T.Text]
getVersionInfo = fmap (T.drop verMsgPad) . T.lines <$> pacmanOutput ["-V"]

-- | The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
