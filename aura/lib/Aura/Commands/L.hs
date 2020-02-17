{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

-- |
-- Module    : Aura.Commands.L
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle all @-L@ flags - those which involve the pacman log file.

module Aura.Commands.L
  ( viewLogFile
  , searchLogFile
  , logInfoOnPkg
  ) where

import           Aura.Colour (dtot, red)
import           Aura.Core (Env(..), report)
import           Aura.Languages
import           Aura.Settings
import           Aura.Types (PkgName(..))
import           Aura.Utils
import           Data.Generics.Product (field)
import           Data.Set.NonEmpty (NESet)
import           Data.Text.Prettyprint.Doc
import           RIO hiding (FilePath)
import qualified RIO.NonEmpty as NEL
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import           System.Path (toFilePath)
import           System.Process.Typed (proc, runProcess)

---

-- | The contents of the Pacman log file.
newtype Log = Log [Text]

data LogEntry = LogEntry
  { name         :: PkgName
  , firstInstall :: Text
  , upgrades     :: Word
  , recent       :: [Text] }

-- | Pipes the pacman log file through a @less@ session.
viewLogFile :: RIO Env ()
viewLogFile = do
  pth <- asks (toFilePath . either id id . logPathOf . commonConfigOf . settings)
  void . runProcess $ proc "less" [pth]

-- | Print all lines in the log file which contain a given `Text`.
searchLogFile :: Settings -> Text -> IO ()
searchLogFile ss input = do
  let pth = toFilePath . either id id . logPathOf $ commonConfigOf ss
  logFile <- T.lines . decodeUtf8Lenient <$> readFileBinary pth
  traverse_ putTextLn $ searchLines input logFile

-- | The result of @-Li@.
logInfoOnPkg :: NESet PkgName -> RIO Env ()
logInfoOnPkg pkgs = do
  ss <- asks settings
  let pth = toFilePath . either id id . logPathOf $ commonConfigOf ss
  logFile <- Log . T.lines . decodeUtf8Lenient <$> readFileBinary pth
  let (bads, goods) = fmapEither (logLookup logFile) $ toList pkgs
  traverse_ (report red reportNotInLog_1) $ NEL.nonEmpty bads
  liftIO . traverse_ putTextLn $ map (renderEntry ss) goods

logLookup :: Log -> PkgName -> Either PkgName LogEntry
logLookup (Log lns) p = case matches of
  []    -> Left p
  (h:t) -> Right $
    LogEntry { name = p
             , firstInstall = T.take 16 $ T.tail h
             , upgrades = fromIntegral . length $ filter (T.isInfixOf " upgraded ") t
             , recent = reverse . take 5 $ reverse t }
  where matches = filter (T.isInfixOf (" " <> (p ^. field @"name") <> " (")) lns

renderEntry :: Settings -> LogEntry -> Text
renderEntry ss (LogEntry (PkgName pn) fi us rs) =
  dtot . colourCheck ss $ entrify ss fields entries <> hardline <> recents <> hardline
  where fields  = logLookUpFields $ langOf ss
        entries = map pretty [ pn, fi, T.pack (show us), "" ]
        recents = vsep $ map pretty rs
