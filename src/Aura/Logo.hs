-- Library for printing an animated AURA version message.

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

module Aura.Logo where

import Control.Concurrent (threadDelay)
import System.IO          (stdout, hFlush)

import Aura.Colour.Text (yellow)

import Utilities        (prePad)
import Shell            (cursorUpLineCode)

---

data MouthState = Open | Closed deriving (Eq)

-- Taken from: figlet -f small "aura"
auraLogo :: String
auraLogo = " __ _ _  _ _ _ __ _ \n" ++ 
           "/ _` | || | '_/ _` |\n" ++
           "\\__,_|\\_,_|_| \\__,_|"

openMouth :: [String]
openMouth = map yellow
            [ " .--."
            , "/ _.-'"
            , "\\  '-."
            , " '--'" ]

closedMouth :: [String]
closedMouth = map yellow
              [ " .--."
              , "/ _..\\"
              , "\\  ''/"
              , " '--'" ]

pill :: [String]
pill = [ ""
       , ".-."
       , "'-'"
       , "" ]

takeABite :: Int -> IO ()
takeABite pad = drawMouth Closed >> drawMouth Open
    where drawMouth mouth = do
            mapM_ putStrLn $ renderPacmanHead pad mouth
            raiseCursorBy 4
            hFlush stdout
            threadDelay 125000

drawPills :: Int -> IO ()
drawPills numOfPills = mapM_ putStrLn pills
    where pills = renderPills numOfPills

raiseCursorBy :: Int -> IO ()
raiseCursorBy = putStr . raiseCursorBy'

raiseCursorBy' :: Int -> String
raiseCursorBy' = cursorUpLineCode

clearGrid :: IO ()
clearGrid = putStr blankLines >> raiseCursorBy 4
    where blankLines = concat . replicate 4 . padString 23 $ "\n"

renderPill :: Int -> [String]
renderPill pad = map (padString pad) pill

renderPills :: Int -> [String]
renderPills numOfPills = take numOfPills pillPostitions >>= render
    where pillPostitions = [17,12,7]
          render pos = renderPill pos ++ [raiseCursorBy' 5]

renderPacmanHead :: Int -> MouthState -> [String]
renderPacmanHead pad Open   = map (padString pad) openMouth
renderPacmanHead pad Closed = map (padString pad) closedMouth

padString :: Int -> String -> String
padString pad cs = prePad cs ' ' (pad + length cs)
