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
  , getPacmanConf
  , getIgnoredPkgs, getIgnoredGroups
  , groupPackages
    -- * Misc.
  , versionInfo
  , verMsgPad
  ) where

import           Aura.Languages
import           Aura.Settings.External
import           Aura.Types
import           RIO hiding (some, try)
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL
import           RIO.FilePath
import           RIO.Lens (_2)
import           RIO.List.Partial ((!!))
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import           System.Process.Typed
import           Text.Megaparsec (parse)

---

-- | Default location of the pacman config file: \/etc\/pacman.conf
pacmanConfFile :: FilePath
pacmanConfFile = "/etc/pacman.conf"

-- | Default location of the pacman log flie: \/var\/log\/pacman.log
defaultLogFile :: FilePath
defaultLogFile = "/var/log/pacman.log"

-- | Default location of the pacman database lock file: \/var\/lib\/pacman\/db.lck
lockFile :: FilePath
lockFile = "/var/lib/pacman/db.lck"

-- | Given a filepath to the pacman config, try to parse its contents.
getPacmanConf :: FilePath -> IO (Either Failure Config)
getPacmanConf fp = do
  file <- decodeUtf8Lenient <$> BS.readFile fp
  pure . first (const (Failure confParsing_1)) $ parse config "pacman config" file

-- | Fetches the @IgnorePkg@ entry from the config, if it's there.
getIgnoredPkgs :: Config -> Set PkgName
getIgnoredPkgs (Config c) = maybe S.empty (S.fromList . map PkgName) $ M.lookup "IgnorePkg" c

-- | Fetches the @IgnoreGroup@ entry from the config, if it's there.
getIgnoredGroups :: Config -> Set PkgGroup
getIgnoredGroups (Config c) = maybe S.empty (S.fromList . map PkgGroup) $ M.lookup "IgnoreGroup" c

-- | Given a `Set` of package groups, yield all the packages they contain.
groupPackages :: Environment -> NonEmpty PkgGroup -> IO (Set PkgName)
groupPackages env igs = fmap (f . decodeUtf8Lenient) . pacmanOutput env $ "-Qg" : asFlag igs
  where
    f :: Text -> Set PkgName
    f = S.fromList . map (PkgName . (!! 1) . T.words) . T.lines

-- | Fetches the @CacheDir@ entry from the config, if it's there.
getCachePath :: Config -> Maybe FilePath
getCachePath (Config c) = do
  fp <- T.unpack <$> (M.lookup "CacheDir" c >>= listToMaybe)
  bool Nothing (Just fp) $ isAbsolute fp

-- | Fetches the @LogFile@ entry from the config, if it's there.
getLogFilePath :: Config -> Maybe FilePath
getLogFilePath (Config c) = do
  fp <- T.unpack <$> (M.lookup "LogFile" c >>= listToMaybe)
  bool Nothing (Just fp) $ isAbsolute fp

----------
-- ACTIONS
----------

-- | Create a pacman process to run.
pacmanProc :: Environment -> [String] -> ProcessConfig () () ()
pacmanProc env args = setEnv vars $ proc "pacman" args
  where
    vars :: [(String, String)]
    vars = ("LC_ALL", "C") : maybe [] (\p -> [("PATH", T.unpack p)]) (M.lookup "PATH" env)

-- | Run a pacman action that may fail.
pacman :: Environment -> [Text] -> IO ()
pacman env (map T.unpack -> args) = do
  ec <- runProcess $ pacmanProc env args
  unless (ec == ExitSuccess) $ throwM (Failure pacmanFailure_1)

-- | Run some `pacman` process, but only care about whether it succeeded.
pacmanSuccess :: Environment -> [T.Text] -> IO Bool
pacmanSuccess env i = fmap (== ExitSuccess) . runProcess . setStderr closed . setStdout closed . pacmanProc env $ map T.unpack i

-- | Runs pacman silently and returns only the stdout.
pacmanOutput :: Environment -> [Text] -> IO ByteString
pacmanOutput env i =
  fmap (^. _2 . to BL.toStrict) . readProcess . pacmanProc env $ map T.unpack i

-- | Runs pacman silently and returns the stdout as UTF8-decoded `Text` lines.
pacmanLines :: Environment -> [Text] -> IO [Text]
pacmanLines env s = T.lines . decodeUtf8Lenient <$> pacmanOutput env s

-- | Yields the lines given by `pacman -V` with the pacman image stripped.
versionInfo :: Environment -> IO [Text]
versionInfo env = map (T.drop verMsgPad) <$> pacmanLines env ["-V"]

-- | The amount of whitespace before text in the lines given by `pacman -V`
verMsgPad :: Int
verMsgPad = 23
