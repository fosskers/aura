{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- An interface to `pacman`.
-- Takes any pacman arguments and applies it to pacman through the shell.

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

module Aura.Pacman where

import System.IO        (hFlush, stdout)
import Control.Applicative (many)
import Data.Attoparsec.Text
import qualified Data.HashMap.Strict as M
import Data.Ini
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Aura.Cache
import Aura.Languages     (pacmanFailure_1)
import Aura.Monad.Aura hiding (liftIO)
import Aura.Settings.Base (pacmanCmdOf)
import Aura.Shell         (shellCmd, quietShellCmd, quietShellCmd')
import Aura.Utils         (scoldAndFail)
import Shelly hiding (cmd)
import Prelude hiding (FilePath)

import Shell (Environment)
import Utilities

---

type ShellArg = T.Text

defaultCmd :: T.Text
defaultCmd = "pacman"

powerPillCmd :: FilePath
powerPillCmd = "/usr/bin/powerpill"

pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

defaultLogFile :: T.Text
defaultLogFile = "/var/log/pacman.log"

lockFile :: FilePath
lockFile = "/var/lib/pacman/db.lck"

getPacmanCmd :: Environment -> Bool -> Sh T.Text
getPacmanCmd _ nopp =
  do pacman <- get_env "PACMAN"
     case pacman of
       Just cmd -> pure cmd
       Nothing  -> do             -- Left space for more options later.
         powerPill <- exists powerPillCmd
         if | powerPill && not nopp -> pure $ toTextIgnore powerPillCmd
            | otherwise -> pure defaultCmd

-----------------------
-- pacman.conf handling
-----------------------

type PacmanConf = (M.HashMap T.Text T.Text, Ini)

getPacmanConf :: Sh (Either String PacmanConf)
getPacmanConf =  parseConf <$> readfile pacmanConfFile

getConf :: FilePath -> Sh (Either String PacmanConf)
getConf file =  parseConf <$> readfile file

parseConf :: T.Text -> Either String PacmanConf
parseConf = parseOnly ( (,) <$> M.fromList <$> many keyValueParser <*> iniParser )

getConfFileField :: Either String PacmanConf -> Maybe T.Text -> T.Text -> Maybe T.Text
getConfFileField (Right (_,confFile)) (Just section) key =
  case lookupValue section key confFile of
       Left _ -> Nothing
       Right a -> Just a
getConfFileField (Right (header,_)) (Nothing) key = M.lookup key header
getConfFileField (Left _) _ _ = Nothing

getIgnoredPkgs :: Either String PacmanConf -> [T.Text]
getIgnoredPkgs confFile = fromMaybe [] $ T.words <$> getConfFileField confFile Nothing "IgnorePkg"

-- For config file fields that only have one value.
-- Caller must supply an alternative if the given field isn't found.
singleEntry :: Either String PacmanConf -> T.Text -> T.Text -> T.Text
singleEntry confFile field alt = fromMaybe alt $ getConfFileField confFile Nothing field

getCachePath :: Either String PacmanConf -> FilePath
getCachePath confFile = fromText $ singleEntry confFile "CacheDir" defaultPackageCache

getLogFilePath :: Either String PacmanConf -> FilePath
getLogFilePath confFile = fromText $ singleEntry confFile "LogFile" defaultLogFile

----------
-- ACTIONS
----------
pacman :: [ShellArg] -> Aura ()
pacman args = asks pacmanCmdOf >>= \cmd -> flush *> shellCmd cmd args
    where flush = liftIO (hFlush stdout)

-- Did a pacman process succeed?
pacmanSuccess :: [ShellArg] -> Aura Bool
pacmanSuccess args = success <$> quietShellCmd' "pacman" args
    where success = (== 0) . tripleFst

-- Handler for pacman call failures.
pacmanFailure :: T.Text -> Aura a
pacmanFailure _ = scoldAndFail pacmanFailure_1

-- Performs a pacmanQuiet and returns only the stdout.
pacmanOutput :: [ShellArg] -> Aura T.Text
pacmanOutput = quietShellCmd "pacman"

syncDatabase :: [ShellArg] -> Aura ()
syncDatabase pacOpts = pacman $ "-Sy" : pacOpts

getPacmanHelpMsg :: Aura [T.Text]
getPacmanHelpMsg = T.lines <$> pacmanOutput ["-h"]

-- Yields the lines given by `pacman -V` with the pacman image stripped.
getVersionInfo :: Aura [T.Text]
getVersionInfo = (fmap (T.drop verMsgPad) . T.lines) <$> pacmanOutput ["-V"]

-- The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
