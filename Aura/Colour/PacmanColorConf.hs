-- Interface for handling the pacman-color conf file.

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

module Aura.Colour.PacmanColorConf
    ( getColours
    , ColourSettings(..) ) where

import System.Directory (doesFileExist)
import Data.Maybe       (catMaybes)

import Aura.Colour.Parser
import Aura.Colour.Text

---

confPath :: FilePath
confPath = "/etc/pacman.d/color.conf"

data ColourSettings = C { redf     :: Colouror
                        , greenf   :: Colouror
                        , yellowf  :: Colouror
                        , bluef    :: Colouror
                        , magentaf :: Colouror
                        , cyanf    :: Colouror
                        , whitef   :: Colouror }

defaultColours :: ColourSettings
defaultColours = C bRed bGreen bYellow bBlue bMagenta bCyan bForeground

getColours :: IO ColourSettings
getColours = do
  exists <- doesFileExist confPath
  if not exists
     then return defaultColours
     else processParse

processParse :: IO ColourSettings
processParse = do
  content <- readFile confPath
  case parseConf content of
    Left e   -> print e >> return defaultColours
    Right ms -> return $ replaceDefaults ms

replaceDefaults :: [Maybe (Colour,Colouror)] -> ColourSettings
replaceDefaults = foldl replace defaultColours . catMaybes

-- Better way to do this...?
replace :: ColourSettings -> (Colour,Colouror) -> ColourSettings
replace cs (Red     ,c) = cs { redf     = c }
replace cs (Green   ,c) = cs { greenf   = c }
replace cs (Yellow  ,c) = cs { yellowf  = c }
replace cs (Blue    ,c) = cs { bluef    = c }
replace cs (Magenta ,c) = cs { magentaf = c }
replace cs (Cyan    ,c) = cs { cyanf    = c }
replace cs (White   ,c) = cs { whitef   = c }
replace _ _             = defaultColours