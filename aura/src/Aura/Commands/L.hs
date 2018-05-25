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
import           BasePrelude
import qualified Data.Text as T
import           Shelly hiding (FilePath)
import           Text.Regex.PCRE ((=~))
import           Utilities (searchLines, readFileUTF8, loudSh)

---

viewLogFile :: T.Text -> Aura ()
viewLogFile logFilePath = do
  (ec, _) <- shelly . loudSh $ run_ "less" [logFilePath]
  case ec of
    ExitSuccess -> pure ()
    _ -> failure ""

-- Very similar to `searchCache`. But is this worth generalizing?
searchLogFile :: Settings -> [String] -> IO ()
searchLogFile ss input = do
  logFile <- lines <$> readFileUTF8 (T.unpack $ logFilePathOf ss)
  traverse_ putStrLn $ searchLines (unwords input) logFile

logInfoOnPkg :: [String] -> Aura ()
logInfoOnPkg []   = pure ()
logInfoOnPkg pkgs = ask >>= \ss -> do
  logFile <- liftIO $ readFile (T.unpack $ logFilePathOf ss)
  let inLog p = logFile =~ (" " <> p <> " ")
      reals   = filter inLog pkgs
  reportNotInLog (pkgs \\ reals)
  liftIO $ traverse_ (putStrLn . renderLogLookUp ss logFile) reals

renderLogLookUp :: Settings -> String -> String -> String
renderLogLookUp ss logFile pkg = entrify ss fields entries <> "\n" <> recent
    where fields      = fmap yellow . logLookUpFields . langOf $ ss
          matches     = searchLines (" " <> pkg <> " \\(") $ lines logFile
          installDate = head matches =~ ("\\[[-:0-9 ]+\\]" :: String)
          upgrades    = length $ searchLines " upgraded " matches
          recent      = unlines . fmap (" " <>) . takeLast 5 $ matches
          takeLast n  = reverse . take n . reverse
          entries     = [ pkg
                        , installDate
                        , show upgrades
                        , "" ]

------------
-- REPORTING
------------
reportNotInLog :: [String] -> Aura ()
reportNotInLog = badReport reportNotInLog_1
