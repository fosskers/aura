-- |
-- Module    : Aura.Logo
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Print an animated AURA version message.

module Aura.Logo ( animateVersionMsg ) where

import           Aura.Colour (dtot, yellow)
import           Aura.IO
import           Aura.Languages (translatorMsg)
import           Aura.Pacman (verMsgPad)
import           Aura.Settings
import           Aura.Shell
import           Data.Text.Prettyprint.Doc
import           RIO
import qualified RIO.Text as T

---

-- | Show an animated version message, but only when the output target
-- is a terminal.
animateVersionMsg :: Settings -> Text -> [Text] -> IO ()
animateVersionMsg ss auraVersion verMsg = do
  when (isTerminal ss) $ do
    hideCursor
    traverse_ (putTextLn . padString verMsgPad) verMsg  -- Version message
    raiseCursorBy 7  -- Initial reraising of the cursor.
    drawPills 3
    traverse_ putTextLn $ renderPacmanHead ss 0 Open  -- Initial rendering of head.
    raiseCursorBy 4
    takeABite ss 0  -- Initial bite animation.
    traverse_ pillEating pillsAndWidths
    clearGrid
  putTextLn auraLogo
  putTextLn $ "AURA Version " <> auraVersion
  putTextLn " by Colin Woodbury\n"
  traverse_ putTextLn . translatorMsg . langOf $ ss
  when (isTerminal ss) showCursor
    where pillEating (p, w) = clearGrid *> drawPills p *> takeABite ss w
          pillsAndWidths    = [(2, 5), (1, 10), (0, 15)]

data MouthState = Open | Closed deriving (Eq)

-- Taken from: figlet -f small "aura"
auraLogo :: Text
auraLogo = " __ _ _  _ _ _ __ _ \n" <>
           "/ _` | || | '_/ _` |\n" <>
           "\\__,_|\\_,_|_| \\__,_|"

openMouth :: Settings -> [Text]
openMouth ss = map f
            [ " .--."
            , "/ _.-'"
            , "\\  '-."
            , " '--'" ]
  where f | shared ss (Colour Never) = id
          | otherwise = dtot . yellow . pretty

closedMouth :: Settings -> [Text]
closedMouth ss = map f
              [ " .--."
              , "/ _..\\"
              , "\\  ''/"
              , " '--'" ]
  where f | shared ss (Colour Never) = id
          | otherwise = dtot . yellow . pretty

pill :: [Text]
pill = [ ""
       , ".-."
       , "'-'"
       , "" ]

takeABite :: Settings -> Int -> IO ()
takeABite ss pad = drawMouth Closed *> drawMouth Open
  where
    drawMouth :: MouthState -> IO ()
    drawMouth mouth = do
      traverse_ putTextLn $ renderPacmanHead ss pad mouth
      raiseCursorBy 4
      hFlush stdout
      threadDelay 125000

drawPills :: Int -> IO ()
drawPills numOfPills = traverse_ putTextLn pills
  where pills = renderPills numOfPills

clearGrid :: IO ()
clearGrid = putText blankLines *> raiseCursorBy 4
  where blankLines = fold . replicate 4 . padString 23 $ "\n"

renderPill :: Int -> [Text]
renderPill pad = padString pad <$> pill

renderPills :: Int -> [Text]
renderPills numOfPills = take numOfPills pillPostitions >>= render
  where pillPostitions = [17, 12, 7]
        render pos = renderPill pos <> [ decodeUtf8Lenient $ cursorUpLineCode 5 ]

renderPacmanHead :: Settings -> Int -> MouthState -> [Text]
renderPacmanHead ss pad Open   = map (padString pad) $ openMouth ss
renderPacmanHead ss pad Closed = map (padString pad) $ closedMouth ss

padString :: Int -> Text -> Text
padString pad cs = T.justifyRight (pad + T.length cs) ' ' cs
