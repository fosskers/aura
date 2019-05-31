{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Pacman
-- Copyright : (c) Colin Woodbury, 2012 - 2019
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
import           Aura.Utils (strictText)
import           BasePrelude hiding (some, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Set.NonEmpty (NESet)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Lens.Micro
import           Lens.Micro.GHC ()
import           System.Path (Absolute, Path, fromAbsoluteFilePath, toFilePath)
import           System.Process.Typed
import           Text.Megaparsec hiding (single)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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
  n <- takeWhile1P Nothing (/= ' ')
  space
  void $ char '='
  space
  rest <- T.words <$> takeWhile1P Nothing (/= '\n')
  pure (n, rest)

-- | Using `[]` as block comment markers is a trick to skip conf file "section" lines.
garbage :: Parsec Void T.Text ()
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
  file <- decodeUtf8With lenientDecode <$> BS.readFile (toFilePath fp)
  pure . first (const (Failure confParsing_1)) $ parse config "pacman config" file

-- | Fetches the @IgnorePkg@ entry from the config, if it's there.
getIgnoredPkgs :: Config -> S.Set PkgName
getIgnoredPkgs (Config c) = maybe S.empty (S.fromList . map PkgName) $ M.lookup "IgnorePkg" c

-- | Fetches the @IgnoreGroup@ entry from the config, if it's there.
getIgnoredGroups :: Config -> S.Set PkgGroup
getIgnoredGroups (Config c) = maybe S.empty (S.fromList . map PkgGroup) $ M.lookup "IgnoreGroup" c

-- | Given a `Set` of package groups, yield all the packages they contain.
groupPackages :: NESet PkgGroup -> IO (S.Set PkgName)
groupPackages igs | null igs  = pure S.empty
                  | otherwise = fmap f . pacmanOutput $ "-Qg" : asFlag igs
  where f = S.fromList . map (PkgName . strictText . (!! 1) . BL.words) . BL.lines

-- | Fetches the @CacheDir@ entry from the config, if it's there.
getCachePath :: Config -> Maybe (Path Absolute)
getCachePath (Config c) = c ^? at "CacheDir" . _Just . _head . to (fromAbsoluteFilePath . T.unpack)

-- | Fetches the @LogFile@ entry from the config, if it's there.
getLogFilePath :: Config -> Maybe (Path Absolute)
getLogFilePath (Config c) = c ^? at "LogFile" . _Just . _head . to (fromAbsoluteFilePath . T.unpack)

----------
-- ACTIONS
----------
-- | Run a pacman action that may fail. Will never throw an IO exception.
pacman :: [String] -> IO (Either Failure ())
pacman args = do
  ec <- runProcess $ proc "pacman" args
  pure . bool (Left $ Failure pacmanFailure_1) (Right ()) $ ec == ExitSuccess

-- | Run some `pacman` process, but only care about whether it succeeded.
pacmanSuccess :: [String] -> IO Bool
pacmanSuccess = fmap (== ExitSuccess) . runProcess . setStderr closed . setStdout closed . proc "pacman"

-- | Runs pacman silently and returns only the stdout.
pacmanOutput :: [String] -> IO BL.ByteString
pacmanOutput = fmap (^. _2) . readProcess . proc "pacman"

-- | Runs pacman silently and returns the stdout as UTF8-decoded `Text` lines.
pacmanLines :: [String] -> IO [T.Text]
pacmanLines s = T.lines . TL.toStrict . TL.decodeUtf8With lenientDecode <$> pacmanOutput s

-- | Yields the lines given by `pacman -V` with the pacman image stripped.
versionInfo :: IO [T.Text]
versionInfo = map (T.drop verMsgPad) <$> pacmanLines ["-V"]

-- | The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
