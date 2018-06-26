{-# LANGUAGE OverloadedStrings #-}

-- Library for printing an animated AURA version message.

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

module Aura.Logo where

import           Aura.Colour (yellow, dtot)
import           Aura.Settings
import           BasePrelude
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           System.IO (stdout, hFlush)
import           Utilities (cursorUpLineCode)

---

data MouthState = Open | Closed deriving (Eq)

-- Taken from: figlet -f small "aura"
auraLogo :: T.Text
auraLogo = " __ _ _  _ _ _ __ _ \n" <>
           "/ _` | || | '_/ _` |\n" <>
           "\\__,_|\\_,_|_| \\__,_|"

openMouth :: Settings -> [T.Text]
openMouth ss = map f
            [ " .--."
            , "/ _.-'"
            , "\\  '-."
            , " '--'" ]
  where f | shared ss (Colour Never) = id
          | otherwise = dtot . yellow . pretty

closedMouth :: Settings -> [T.Text]
closedMouth ss = map f
              [ " .--."
              , "/ _..\\"
              , "\\  ''/"
              , " '--'" ]
  where f | shared ss (Colour Never) = id
          | otherwise = dtot . yellow . pretty

pill :: [T.Text]
pill = [ ""
       , ".-."
       , "'-'"
       , "" ]

takeABite :: Settings -> Int -> IO ()
takeABite ss pad = drawMouth Closed *> drawMouth Open
    where drawMouth mouth = do
            traverse_ T.putStrLn $ renderPacmanHead ss pad mouth
            raiseCursorBy 4
            hFlush stdout
            threadDelay 125000

drawPills :: Int -> IO ()
drawPills numOfPills = traverse_ T.putStrLn pills
    where pills = renderPills numOfPills

raiseCursorBy :: Int -> IO ()
raiseCursorBy = T.putStr . raiseCursorBy'

raiseCursorBy' :: Int -> T.Text
raiseCursorBy' = cursorUpLineCode

clearGrid :: IO ()
clearGrid = T.putStr blankLines *> raiseCursorBy 4
    where blankLines = fold . replicate 4 . padString 23 $ "\n"

renderPill :: Int -> [T.Text]
renderPill pad = padString pad <$> pill

renderPills :: Int -> [T.Text]
renderPills numOfPills = take numOfPills pillPostitions >>= render
    where pillPostitions = [17, 12, 7]
          render pos = renderPill pos <> [raiseCursorBy' 5]

renderPacmanHead :: Settings -> Int -> MouthState -> [T.Text]
renderPacmanHead ss pad Open   = map (padString pad) $ openMouth ss
renderPacmanHead ss pad Closed = map (padString pad) $ closedMouth ss

padString :: Int -> T.Text -> T.Text
padString pad cs = T.justifyRight (pad + T.length cs) ' ' cs
