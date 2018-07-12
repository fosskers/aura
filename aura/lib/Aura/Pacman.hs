{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

-- |
-- Module    : Aura.Pacman
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- An interface to @pacman@.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Aura.Pacman
  ( -- * Calling Pacman
    pacman
  , pacmanOutput, pacmanSuccess
    -- * Paths
  , lockFile
  , pacmanConfFile
  , defaultLogFile
  , getCachePath
  , getLogFilePath
    -- * Pacman Config
  , Config(..), config
  , getPacmanConf
  , getIgnoredPkgs, getIgnoredGroups
  , groupPackages
    -- * Misc.
  , getVersionInfo
  , verMsgPad
  , getPacmanHelpMsg
  ) where

import           Aura.Languages
import           Aura.Types
import           BasePrelude hiding (some, try)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.GHC ()
import qualified Shelly as Sh
import           Shelly hiding (FilePath, cmd)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Utilities

---

-- | The (meaningful) contents of the Pacman config file.
newtype Config = Config (M.Map T.Text [T.Text]) deriving (Show)

-- | Parse a `Config`, the pacman configuration file.
config :: Parsec Void T.Text Config
config = Config . M.fromList . rights <$> (garbage *> some (fmap Right (try pair) <|> fmap Left single) <* eof)

single :: Parsec Void T.Text ()
single = L.lexeme garbage . void $ manyTill letterChar newline

pair :: Parsec Void T.Text (T.Text, [T.Text])
pair = L.lexeme garbage $ do
  name <- takeWhile1P Nothing (/= ' ')
  space
  char '='
  space
  rest <- T.words <$> takeWhile1P Nothing (/= '\n')
  pure (name, rest)

-- | Using `[]` as block comment markers is a trick to skip conf file "section" lines.
garbage :: Parsec Void T.Text ()
garbage = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "[" "]")

-- | Default location of the pacman config file: \/etc\/pacman.conf
pacmanConfFile :: Sh.FilePath
pacmanConfFile = "/etc/pacman.conf"

-- | Default location of the pacman log flie: \/var\/log\/pacman.log
defaultLogFile :: Sh.FilePath
defaultLogFile = "/var/log/pacman.log"

-- | Default location of the pacman database lock file: \/var\/lib\/pacman\/db.lck
lockFile :: Sh.FilePath
lockFile = "/var/lib/pacman/db.lck"

-- | Given a filepath to the pacman config, try to parse its contents.
getPacmanConf :: Sh.FilePath -> IO (Either Failure Config)
getPacmanConf fp = shelly $ do
  file <- readfile fp
  pure . first (const (Failure confParsing_1)) $ parse config "pacman config" file

-- | Fetches the @IgnorePkg@ entry from the config, if it's there.
getIgnoredPkgs :: Config -> S.Set T.Text
getIgnoredPkgs (Config c) = maybe S.empty S.fromList $ M.lookup "IgnorePkg" c

-- | Fetches the @IgnoreGroup@ entry from the config, if it's there.
getIgnoredGroups :: Config -> S.Set T.Text
getIgnoredGroups (Config c) = maybe S.empty S.fromList $ M.lookup "IgnoreGroup" c

-- | Given a `Set` of package groups, yield all the packages they contain.
groupPackages :: S.Set T.Text -> IO (S.Set T.Text)
groupPackages igs | null igs  = pure S.empty
                  | otherwise = fmap f . pacmanOutput $ "-Qg" : toList igs
  where f = S.fromList . map ((!! 1) . T.words) . T.lines  -- Naughty

-- | Fetches the @CacheDir@ entry from the config, if it's there.
getCachePath :: Config -> Maybe Sh.FilePath
getCachePath (Config c) = c ^? at "CacheDir" . _Just . _head . to fromText

-- | Fetches the @LogFile@ entry from the config, if it's there.
getLogFilePath :: Config -> Maybe Sh.FilePath
getLogFilePath (Config c) = c ^? at "LogFile" . _Just . _head . to fromText

----------
-- ACTIONS
----------
-- | Run a pacman action that may fail. Will never throw an IO exception.
pacman :: [T.Text] -> IO (Either Failure ())
pacman args = do
  code <- shelly . errExit False $ do
    runHandles "pacman" args [ InHandle Inherit, OutHandle Inherit, ErrorHandle Inherit ] n
    lastExitCode
  pure . bool (Left $ Failure pacmanFailure_1) (Right ()) $ code == 0
  where n _ _ _ = pure ()

-- | Run some `pacman` process, but only care about whether it succeeded.
pacmanSuccess :: [T.Text] -> IO Bool
pacmanSuccess args = fmap success . shelly . quietSh $ run_ "pacman" args
    where success (ExitSuccess, _) = True
          success _ = False

-- | Runs pacman silently and returns only the stdout.
pacmanOutput :: [T.Text] -> IO T.Text
pacmanOutput = fmap snd . shelly . quietSh . Sh.run "pacman"

-- | The output of @pacman -h@.
getPacmanHelpMsg :: IO [T.Text]
getPacmanHelpMsg = T.lines <$> pacmanOutput ["-h"]

-- | Yields the lines given by `pacman -V` with the pacman image stripped.
getVersionInfo :: IO [T.Text]
getVersionInfo = fmap (T.drop verMsgPad) . T.lines <$> pacmanOutput ["-V"]

-- | The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
