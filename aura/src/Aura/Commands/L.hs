-- Handles all `-L` operations

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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
    , logInfoOnPkg ) where

import BasicPrelude hiding (liftIO)

import Data.Foldable   (traverse_)

import Aura.Colour.Text (yellow)
import Aura.Core        (badReport)
import Aura.Shell       (shellCmd)
import Aura.Utils       (entrify)
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import qualified Data.Text as T
import qualified Data.Text.ICU as Re

import Utilities (searchLines, readFileUTF8)

---

viewLogFile :: T.Text -> Aura ()
viewLogFile logFilePath = shellCmd "less" [logFilePath]

-- Very similar to `searchCache`. But is this worth generalizing?
searchLogFile :: [T.Text] -> Aura ()
searchLogFile input = ask >>= \ss -> liftIO $ do
  logFile <- lines <$> readFileUTF8 (logFilePathOf ss)
  traverse_ putStrLn $ searchLines (unwords input) logFile

logInfoOnPkg :: [T.Text] -> Aura ()
logInfoOnPkg []   = pure ()
logInfoOnPkg pkgs = ask >>= \ss -> do
  logFile <- liftIO $ readFileUTF8 (logFilePathOf ss)
  let inLog p = isJust $ Re.find (Re.regex [] p) logFile
      reals   = filter inLog pkgs
  reportNotInLog (pkgs \\ reals)
  liftIO $ traverse_ (putStrLn . renderLogLookUp ss logFile) reals

renderLogLookUp :: Settings -> T.Text -> T.Text -> T.Text
renderLogLookUp ss logFile pkg = entrify ss fields entries <> "\n" <> recent
    where fields      = fmap yellow . logLookUpFields . langOf $ ss
          matches     = searchLines (" " <> pkg <> " \\(") $ T.lines logFile
          installDate =  Re.find (Re.regex [] "\\[[-:0-9 ]+\\]") (head matches) >>= Re.group 0
          upgrades    = length $ searchLines " upgraded " matches
          recent      = T.unlines . fmap (" " <>) . takeLast 5 $ matches
          takeLast n  = reverse . take n . reverse
          entries     = [ pkg
                        , fromMaybe "" installDate
                        , show upgrades
                        , "" ]

------------
-- REPORTING
------------
reportNotInLog :: [T.Text] -> Aura ()
reportNotInLog = badReport reportNotInLog_1
