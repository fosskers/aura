{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}
{-# LANGUAGE TypeApplications, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Commands.L
-- Copyright : (c) Colin Woodbury, 2012 - 2018
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-L@ flags - those which involve the pacman log file.

module Aura.Commands.L
  ( viewLogFile
  , searchLogFile
  , logInfoOnPkg
  ) where

import           Aura.Colour (red, dtot)
import           Aura.Core (report)
import           Aura.Languages
import           Aura.Settings
import           Aura.Types (PkgName(..))
import           Aura.Utils
import           BasePrelude hiding (FilePath)
import           Control.Compactable (fmapEither)
import           Control.Monad.Freer
import           Control.Monad.Freer.Reader
import           Data.Generics.Product (field)
import qualified Data.List.NonEmpty as NEL
import           Data.Set.NonEmpty (NonEmptySet)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Lens.Micro ((^.))
import           Shelly

---

-- | The contents of the Pacman log file.
newtype Log = Log [T.Text]

data LogEntry = LogEntry { _pkgName :: PkgName, _firstInstall :: T.Text, _upgrades :: Word, _recent :: [T.Text] }

-- | Pipes the pacman log file through a @less@ session.
viewLogFile :: (Member (Reader Settings) r, Member IO r) => Eff r ()
viewLogFile = do
  pth <- asks (either id id . logPathOf . commonConfigOf)
  void . send . shelly @IO . loudSh $ run_ "less" [toTextIgnore pth]

-- | Print all lines in the log file which contain a given `T.Text`.
searchLogFile :: Settings -> T.Text -> IO ()
searchLogFile ss input = do
  let pth = either id id . logPathOf $ commonConfigOf ss
  logFile <- T.lines <$> shelly (readfile pth)
  traverse_ T.putStrLn $ searchLines input logFile

-- | The result of @-Li@.
logInfoOnPkg :: (Member (Reader Settings) r, Member IO r) => NonEmptySet PkgName -> Eff r ()
logInfoOnPkg pkgs = do
  ss <- ask
  let pth = either id id . logPathOf $ commonConfigOf ss
  logFile <- fmap (Log . T.lines) . send . shelly @IO $ readfile pth
  let (bads, goods) = fmapEither (logLookup logFile) $ toList pkgs
  traverse_ (report red reportNotInLog_1) $ NEL.nonEmpty bads
  send . traverse_ T.putStrLn $ map (renderEntry ss) goods

logLookup :: Log -> PkgName -> Either PkgName LogEntry
logLookup (Log lns) p = case matches of
  []    -> Left p
  (h:t) -> Right $ LogEntry p
                   (T.take 16 $ T.tail h)
                   (fromIntegral . length $ filter (T.isInfixOf " upgraded ") t)
                   (reverse . take 5 $ reverse t)
  where matches = filter (T.isInfixOf (" " <> (p ^. field @"name") <> " (")) lns

renderEntry :: Settings -> LogEntry -> T.Text
renderEntry ss (LogEntry (PkgName pn) fi us rs) =
  dtot . colourCheck ss $ entrify ss fields entries <> hardline <> recent <> hardline
  where fields  = logLookUpFields $ langOf ss
        entries = map pretty [ pn, fi, T.pack (show us), "" ]
        recent  = vsep $ map pretty rs
