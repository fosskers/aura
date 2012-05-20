-- Library for printing an animated AURA version message.

module AuraLogo where

import Control.Concurrent (threadDelay)
import System.IO (stdout, hFlush)

data MouthState = Open | Closed deriving (Eq)

-- Taken from: figlet -f small "aura"
auraLogo :: String
auraLogo = " __ _ _  _ _ _ __ _ \n" ++ 
           "/ _` | || | '_/ _` |\n" ++
           "\\__,_|\\_,_|_| \\__,_|"

openMouth :: [String]
openMouth = [ " .--."
            , "/ _.-'"
            , "\\  '-."
            , " '--'"
            ]

closedMouth :: [String]
closedMouth = [ " .--."
              , "/ _..\\"
              , "\\  ''/"
              , " '--'"
              ]

pill :: [String]
pill = [ ""
       , ".-."
       , "'-'"
       , ""
       ]       

takeABite :: Int -> IO ()
takeABite pad = drawMouth Closed >> drawMouth Open
    where drawMouth mouth = do
            mapM_ putStrLn $ renderPacmanHead pad mouth
            putStr $ raiseCursorBy 4
            hFlush stdout
            threadDelay 125000

drawPills :: Int -> IO ()
drawPills numOfPills = mapM_ (\aPill -> mapM_ putStrLn aPill) pills
    where pills = renderPills numOfPills

raiseCursorBy :: Int -> String
raiseCursorBy 0 = ""
raiseCursorBy n = "\r\b\r" ++ raiseCursorBy (n - 1)

clearGrid :: String
clearGrid = blankLines ++ raiseCursorBy 4
    where blankLines = concat . replicate 4 . padString 23 $ "\n"

renderPill :: Int -> [String]
renderPill pad = map (padString pad) pill

renderPills :: Int -> [[String]]
renderPills numOfPills = map render (take numOfPills pillPostitions)
    where pillPostitions = [17,12,7]
          render pos = renderPill pos ++ [raiseCursorBy 5]

renderPacmanHead :: Int -> MouthState -> [String]
renderPacmanHead pad Open   = map (padString pad) openMouth
renderPacmanHead pad Closed = map (padString pad) closedMouth

padString :: Int -> String -> String
padString pad cs = getPad ++ cs
    where getPad = concat . take pad . repeat $ " "
