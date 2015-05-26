{-# LANGUAGE FlexibleContexts #-}
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

import Text.Regex.PCRE ((=~))
import Data.List       ((\\))

import Aura.Colour.Text (yellow)
import Aura.Core        (badReport)
import Aura.Shell       (shellCmd)
import Aura.Utils       (entrify)
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages

import Utilities (searchLines,readFileUTF8)

---

viewLogFile :: FilePath -> Aura ()
viewLogFile logFilePath = shellCmd "less" [logFilePath]

-- Very similar to `searchCache`. But is this worth generalizing?
searchLogFile :: [String] -> Aura ()
searchLogFile input = ask >>= \ss -> liftIO $ do
  logFile <- lines <$> readFileUTF8 (logFilePathOf ss)
  mapM_ putStrLn $ searchLines (unwords input) logFile

logInfoOnPkg :: [String] -> Aura ()
logInfoOnPkg []   = return ()
logInfoOnPkg pkgs = ask >>= \ss -> do
  logFile <- liftIO $ readFile (logFilePathOf ss)
  let inLog p = logFile =~ (" " ++ p ++ " ")
      reals   = filter inLog pkgs
  reportNotInLog (pkgs \\ reals)
  liftIO $ mapM_ (putStrLn . renderLogLookUp ss logFile) reals

renderLogLookUp :: Settings -> String -> String -> String
renderLogLookUp ss logFile pkg = entrify ss fields entries ++ "\n" ++ recent
    where fields      = map yellow . logLookUpFields . langOf $ ss
          matches     = searchLines (" " ++ pkg ++ " \\(") $ lines logFile
          installDate = head matches =~ "\\[[-:0-9 ]+\\]"
          upgrades    = length $ searchLines " upgraded " matches
          recent      = unlines . map ((:) ' ') . takeLast 5 $ matches
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
