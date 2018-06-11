{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- Handles all `-L` operations

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

module Aura.Commands.L
  ( viewLogFile
  , searchLogFile
  , logInfoOnPkg
  ) where

import           Aura.Colour.Text (yellow)
import           Aura.Core (badReport)
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Settings.Base
import           Aura.Utils (entrify)
import           BasePrelude hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Shelly
import           Utilities

---

-- | The contents of the Pacman log file.
newtype Log = Log [T.Text]

data LogEntry = LogEntry { _pkgName :: T.Text, _firstInstall :: T.Text, _upgrades :: Word, _recent :: [T.Text] }

viewLogFile :: FilePath -> Aura ()
viewLogFile logFilePath = void . shelly . loudSh $ run_ "less" [toTextIgnore logFilePath]

-- Very similar to `searchCache`. But is this worth generalizing?
searchLogFile :: Settings -> [T.Text] -> IO ()
searchLogFile ss input = do
  logFile <- T.lines <$> shelly (readfile $ logFilePathOf ss)
  traverse_ T.putStrLn $ searchLines (Regex $ T.unwords input) logFile

logInfoOnPkg :: [T.Text] -> Aura ()
logInfoOnPkg []   = pure ()
logInfoOnPkg pkgs = do
  ss <- ask
  logFile <- fmap (Log . T.lines) . shelly . readfile $ logFilePathOf ss
  let (bads, goods) = partitionEithers $ map (logLookup logFile) pkgs
  reportNotInLog bads
  liftIO . traverse_ T.putStrLn $ map (renderEntry ss) goods

logLookup :: Log -> T.Text -> Either T.Text LogEntry
logLookup (Log lns) p = case matches of
  []    -> Left p
  (h:t) -> Right $ LogEntry p
                   (T.take 16 $ T.tail h)
                   (fromIntegral . length $ filter (T.isInfixOf " upgraded ") t)
                   (reverse . take 5 $ reverse t)
  where matches = filter (T.isInfixOf (" " <> p <> " (")) lns

renderEntry :: Settings -> LogEntry -> T.Text
renderEntry ss (LogEntry pn fi us rs) = entrify ss fields entries <> "\n" <> recent
  where fields  = map yellow . logLookUpFields $ langOf ss
        entries = [ pn, fi, T.pack (show us), "" ]
        recent  = T.unlines $ map (" " <>) rs

------------
-- REPORTING
------------
reportNotInLog :: [T.Text] -> Aura ()
reportNotInLog = badReport reportNotInLog_1
