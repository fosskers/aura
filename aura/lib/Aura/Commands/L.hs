{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

-- |
-- Module    : Aura.Commands.L
-- Copyright : (c) Colin Woodbury, 2012 - 2019
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
import           BasePrelude hiding (FilePath)
import           Control.Compactable (fmapEither)
import           Control.Effect (Carrier, Member)
import           Control.Effect.Lift (Lift, sendM)
import           Control.Effect.Reader (Reader, asks)
import qualified Data.ByteString.Char8 as BS
import           Data.Generics.Product (field)
import qualified Data.List.NonEmpty as NEL
import           Data.Set.NonEmpty (NESet)
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Lens.Micro ((^.))
import           System.Path (toFilePath)
import           System.Process.Typed (proc, runProcess)

---

-- | The contents of the Pacman log file.
newtype Log = Log [T.Text]

data LogEntry = LogEntry
  { name         :: PkgName
  , firstInstall :: T.Text
  , upgrades     :: Word
  , recent       :: [T.Text] }

-- | Pipes the pacman log file through a @less@ session.
viewLogFile :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) => m ()
viewLogFile = do
  pth <- asks (toFilePath . either id id . logPathOf . commonConfigOf . settings)
  sendM . void . runProcess @IO $ proc "less" [pth]

-- | Print all lines in the log file which contain a given `T.Text`.
searchLogFile :: Settings -> T.Text -> IO ()
searchLogFile ss input = do
  let pth = toFilePath . either id id . logPathOf $ commonConfigOf ss
  logFile <- map (T.decodeUtf8With lenientDecode) . BS.lines <$> BS.readFile pth
  traverse_ T.putStrLn $ searchLines input logFile

-- | The result of @-Li@.
logInfoOnPkg :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) => NESet PkgName -> m ()
logInfoOnPkg pkgs = do
  ss <- asks settings
  let pth = toFilePath . either id id . logPathOf $ commonConfigOf ss
  logFile <- Log . map (T.decodeUtf8With lenientDecode) . BS.lines <$> sendM (BS.readFile pth)
  let (bads, goods) = fmapEither (logLookup logFile) $ toList pkgs
  traverse_ (report red reportNotInLog_1) $ NEL.nonEmpty bads
  sendM . traverse_ T.putStrLn $ map (renderEntry ss) goods

logLookup :: Log -> PkgName -> Either PkgName LogEntry
logLookup (Log lns) p = case matches of
  []    -> Left p
  (h:t) -> Right $
    LogEntry { name = p
             , firstInstall = T.take 16 $ T.tail h
             , upgrades = fromIntegral . length $ filter (T.isInfixOf " upgraded ") t
             , recent = reverse . take 5 $ reverse t }
  where matches = filter (T.isInfixOf (" " <> (p ^. field @"name") <> " (")) lns

renderEntry :: Settings -> LogEntry -> T.Text
renderEntry ss (LogEntry (PkgName pn) fi us rs) =
  dtot . colourCheck ss $ entrify ss fields entries <> hardline <> recents <> hardline
  where fields  = logLookUpFields $ langOf ss
        entries = map pretty [ pn, fi, T.pack (show us), "" ]
        recents = vsep $ map pretty rs
