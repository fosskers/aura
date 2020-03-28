{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : Aura.Pacman
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- An interface to @pacman@.
-- Takes any pacman arguments and applies it to pacman through the shell.

module Aura.Pacman
  ( -- * Calling Pacman
    pacman
  , pacmanOutput, pacmanSuccess, pacmanLines
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
  , versionInfo
  , verMsgPad
  ) where

import           Aura.Languages
import           Aura.Types
import           Data.Bifunctor (first)
import           Lens.Micro (at, (^?), _2, _Just, _head)
import           Lens.Micro.GHC ()
import           RIO hiding (first, some, try)
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL
import           RIO.List.Partial ((!!))
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Path (Absolute, Path, fromAbsoluteFilePath, toFilePath)
import           System.Process.Typed
import           Text.Megaparsec hiding (single)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

---

-- | The (meaningful) contents of the Pacman config file.
newtype Config = Config (Map Text [Text]) deriving (Show)

-- | Parse a `Config`, the pacman configuration file.
config :: Parsec Void Text Config
config = Config . M.fromList . rights <$> (garbage *> some (fmap Right (try pair) <|> fmap Left single) <* eof)

single :: Parsec Void Text ()
single = L.lexeme garbage . void $ manyTill letterChar newline

pair :: Parsec Void Text (Text, [Text])
pair = L.lexeme garbage $ do
  n <- takeWhile1P Nothing (/= ' ')
  space
  void $ char '='
  space
  rest <- T.words <$> takeWhile1P Nothing (/= '\n')
  pure (n, rest)

-- | Using `[]` as block comment markers is a trick to skip conf file "section" lines.
garbage :: Parsec Void Text ()
garbage = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "[" "]")

-- | Default location of the pacman config file: \/etc\/pacman.conf
pacmanConfFile :: Path Absolute
pacmanConfFile = fromAbsoluteFilePath "/etc/pacman.conf"

-- | Default location of the pacman log flie: \/var\/log\/pacman.log
defaultLogFile :: Path Absolute
defaultLogFile = fromAbsoluteFilePath "/var/log/pacman.log"

-- | Default location of the pacman database lock file: \/var\/lib\/pacman\/db.lck
lockFile :: Path Absolute
lockFile = fromAbsoluteFilePath "/var/lib/pacman/db.lck"

-- | Given a filepath to the pacman config, try to parse its contents.
getPacmanConf :: Path Absolute -> IO (Either Failure Config)
getPacmanConf fp = do
  file <- decodeUtf8Lenient <$> BS.readFile (toFilePath fp)
  pure . first (const (Failure confParsing_1)) $ parse config "pacman config" file

-- | Fetches the @IgnorePkg@ entry from the config, if it's there.
getIgnoredPkgs :: Config -> Set PkgName
getIgnoredPkgs (Config c) = maybe S.empty (S.fromList . map PkgName) $ M.lookup "IgnorePkg" c

-- | Fetches the @IgnoreGroup@ entry from the config, if it's there.
getIgnoredGroups :: Config -> Set PkgGroup
getIgnoredGroups (Config c) = maybe S.empty (S.fromList . map PkgGroup) $ M.lookup "IgnoreGroup" c

-- | Given a `Set` of package groups, yield all the packages they contain.
groupPackages :: NonEmpty PkgGroup -> IO (Set PkgName)
groupPackages igs = fmap (f . decodeUtf8Lenient) . pacmanOutput $ "-Qg" : asFlag igs
  where
    f :: Text -> Set PkgName
    f = S.fromList . map (PkgName . (!! 1) . T.words) . T.lines

-- | Fetches the @CacheDir@ entry from the config, if it's there.
getCachePath :: Config -> Maybe (Path Absolute)
getCachePath (Config c) = c ^? at "CacheDir" . _Just . _head . to (fromAbsoluteFilePath . T.unpack)

-- | Fetches the @LogFile@ entry from the config, if it's there.
getLogFilePath :: Config -> Maybe (Path Absolute)
getLogFilePath (Config c) = c ^? at "LogFile" . _Just . _head . to (fromAbsoluteFilePath . T.unpack)

----------
-- ACTIONS
----------

-- | Create a pacman process to run.
pacmanProc :: [String] -> ProcessConfig () () ()
pacmanProc args = setEnv [("LC_ALL", "C")] $ proc "pacman" args

-- | Run a pacman action that may fail.
pacman :: [Text] -> IO ()
pacman (map T.unpack -> args) = do
  ec <- runProcess $ pacmanProc args
  unless (ec == ExitSuccess) $ throwM (Failure pacmanFailure_1)

-- | Run some `pacman` process, but only care about whether it succeeded.
pacmanSuccess :: [T.Text] -> IO Bool
pacmanSuccess = fmap (== ExitSuccess) . runProcess . setStderr closed . setStdout closed . pacmanProc . map T.unpack

-- | Runs pacman silently and returns only the stdout.
pacmanOutput :: [Text] -> IO ByteString
pacmanOutput = fmap (^. _2 . to BL.toStrict) . readProcess . pacmanProc . map T.unpack

-- | Runs pacman silently and returns the stdout as UTF8-decoded `Text` lines.
pacmanLines :: [Text] -> IO [Text]
pacmanLines s = T.lines . decodeUtf8Lenient <$> pacmanOutput s

-- | Yields the lines given by `pacman -V` with the pacman image stripped.
versionInfo :: IO [Text]
versionInfo = map (T.drop verMsgPad) <$> pacmanLines ["-V"]

-- | The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
