{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Aura.Logo
-- Copyright : (c) Colin Woodbury, 2012 - 2019
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Print an animated AURA version message.

module Aura.Logo ( animateVersionMsg ) where

import           Aura.Colour (dtot, yellow)
import           Aura.Languages (translatorMsg)
import           Aura.Pacman (verMsgPad)
import           Aura.Settings
import           Aura.Utils
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           RIO
import           System.IO (stdout)

---

-- | Show an animated version message, but only when the output target
-- is a terminal.
animateVersionMsg :: Settings -> T.Text -> [T.Text] -> IO ()
animateVersionMsg ss auraVersion verMsg = do
  when (isTerminal ss) $ do
    hideCursor
    traverse_ (T.putStrLn . padString verMsgPad) verMsg  -- Version message
    raiseCursorBy 7  -- Initial reraising of the cursor.
    drawPills 3
    traverse_ T.putStrLn $ renderPacmanHead ss 0 Open  -- Initial rendering of head.
    raiseCursorBy 4
    takeABite ss 0  -- Initial bite animation.
    traverse_ pillEating pillsAndWidths
    clearGrid
  T.putStrLn auraLogo
  T.putStrLn $ "AURA Version " <> auraVersion
  T.putStrLn " by Colin Woodbury\n"
  traverse_ T.putStrLn . translatorMsg . langOf $ ss
  when (isTerminal ss) showCursor
    where pillEating (p, w) = clearGrid *> drawPills p *> takeABite ss w
          pillsAndWidths    = [(2, 5), (1, 10), (0, 15)]

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

clearGrid :: IO ()
clearGrid = T.putStr blankLines *> raiseCursorBy 4
    where blankLines = fold . replicate 4 . padString 23 $ "\n"

renderPill :: Int -> [T.Text]
renderPill pad = padString pad <$> pill

renderPills :: Int -> [T.Text]
renderPills numOfPills = take numOfPills pillPostitions >>= render
    where pillPostitions = [17, 12, 7]
          render pos = renderPill pos <> [ cursorUpLineCode 5 ]

renderPacmanHead :: Settings -> Int -> MouthState -> [T.Text]
renderPacmanHead ss pad Open   = map (padString pad) $ openMouth ss
renderPacmanHead ss pad Closed = map (padString pad) $ closedMouth ss

padString :: Int -> T.Text -> T.Text
padString pad cs = T.justifyRight (pad + T.length cs) ' ' cs
