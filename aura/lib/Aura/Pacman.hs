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
    -- * Pacman Config
  , getPacmanConf
  , getIgnoredPkgs
    -- * Misc.
  , getVersionInfo
  , verMsgPad
  , getPacmanHelpMsg
  ) where

import           Aura.Cache
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Settings.Base (pacmanCmdOf)
import           Aura.Types
import           BasePrelude hiding (some)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Shelly as Sh
import           Shelly hiding (FilePath, cmd)
import           System.IO (hFlush, stdout)
import           Text.Megaparsec hiding (failure)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Utilities

---

-- | The (meaningful) contents of the Pacman config file.
newtype Config = Config (M.Map T.Text [T.Text]) deriving (Show)

config :: Parsec Void T.Text Config
config = Config . M.fromList <$> (garbage *> some line <* eof)

line :: Parsec Void T.Text (T.Text, [T.Text])
line = L.lexeme garbage $ do
  name <- takeWhile1P Nothing (/= ' ')
  space
  char '='
  space
  rest <- T.words <$> takeRest
  pure (name, rest)

-- | Using `[]` as block comment markers is a trick to skip conf file "section" lines.
garbage :: Parsec Void T.Text ()
garbage = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "[" "]")

defaultCmd :: T.Text
defaultCmd = "pacman"

powerPillCmd :: Sh.FilePath
powerPillCmd = "/usr/bin/powerpill"

pacmanConfFile :: Sh.FilePath
pacmanConfFile = "/etc/pacman.conf"

defaultLogFile :: Sh.FilePath
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

getPacmanConf :: MonadIO m => m (Either Failure Config)
getPacmanConf = shelly $ do
  file <- readfile pacmanConfFile
  pure . first (const (Failure confParsing_1)) $ parse config "pacman config" file

getIgnoredPkgs :: Config -> [T.Text]
getIgnoredPkgs (Config c) = maybe [] id $ M.lookup "IgnorePkg" c

getCachePath :: Config -> Sh.FilePath
getCachePath (Config c) = case M.lookup "CacheDir" c of
  Nothing     -> defaultPackageCache
  Just []     -> defaultPackageCache
  Just (fp:_) -> fromText fp

getLogFilePath :: Config -> Sh.FilePath
getLogFilePath (Config c) = case M.lookup "LogFile" c of
  Nothing     -> defaultLogFile
  Just []     -> defaultLogFile
  Just (fp:_) -> fromText fp

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
