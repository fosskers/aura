-- Handles all `-L` operations

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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

import System.Exit (ExitCode)
import Text.Regex.PCRE ((=~))
import Control.Monad (liftM)
import Data.List ((\\))

import Aura.General (badReport, entrify)
import Aura.Languages
import Aura.Settings

import Utilities (searchLines)
import Shell
import Zero

---

viewLogFile :: FilePath -> IO ExitCode
viewLogFile logFilePath = shellCmd "less" [logFilePath]

-- Very similar to `searchCache`. But is this worth generalizing?
searchLogFile :: Settings -> [String] -> IO ExitCode
searchLogFile settings input = do
  logFile <- lines `liftM` readFile (logFilePathOf settings)
  mapM_ putStrLn $ searchLines (unwords input) logFile
  returnSuccess

-- Are you failing at looking up anything,
-- or succeeding at looking up nothing?
logInfoOnPkg :: Settings -> [String] -> IO ExitCode
logInfoOnPkg _ []          = returnFailure  -- Success?
logInfoOnPkg settings pkgs = do
  logFile <- readFile (logFilePathOf settings)
  let inLog p = logFile =~ (" " ++ p ++ " ")
      reals   = filter inLog pkgs
  reportNotInLog (langOf settings) (pkgs \\ reals)
  return reals ?>> do
    mapM_ (putStrLn . renderLogLookUp settings logFile) reals
    returnSuccess

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
reportNotInLog :: Language -> [String] -> IO ()
reportNotInLog lang nons = badReport reportNotInLogMsg1 lang nons
