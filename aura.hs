-- AURA package manager for Arch Linux

-- System libraries
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import System.Console.GetOpt
import Text.Printf (printf)

-- Custom libraries
import Utilities (replaceByPatt)
import Pacman

data MouthState = Open | Closed deriving (Eq)

data Flag = AURInstall | Version | Help deriving (Eq)

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['A'] ["aursync"] (NoArg AURInstall) aDesc
              ]
    where aDesc = "Install from the AUR."

-- These are intercepted Pacman flags.
pacmanOptions :: [OptDescr Flag]
pacmanOptions = [ Option ['V'] ["version"] (NoArg Version) ""
                , Option ['h'] ["help"]    (NoArg Help)    ""
                ]

auraUsageMsg :: String
auraUsageMsg = usageInfo "AURA only operations:" auraOptions

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

{-
argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options
-}

main = do
  args <- getArgs
  opts <- parseOpts args
  executeOpts opts

parseOpts :: [String] -> IO ([Flag],[String],[String])
parseOpts args = case getOpt' Permute (auraOptions ++ pacmanOptions) args of
                   (opts,nonOpts,pacOpts,_) -> return (opts,nonOpts,pacOpts) 

executeOpts :: ([Flag],[String],[String]) -> IO ()
executeOpts (flags,input,pacOpts) =
    case flags of
      [Help]       -> printHelpMsg pacOpts
      [Version]    -> getVersionInfo >>= animateVersionMsg
      [AURInstall] -> putStrLn "This option isn't ready yet."
      _            -> (pacman $ pacOpts ++ input) >> return ()

printHelpMsg :: [String] -> IO ()
printHelpMsg []      = getPacmanHelpMsg >>= putStrLn . getHelpMsg
printHelpMsg pacOpts = (pacman $ pacOpts ++ ["-h"]) >> return ()

getHelpMsg :: [String] -> String
getHelpMsg pacmanHelpMsg = replacedLines ++ "\n" ++ auraUsageMsg
    where replacedLines = unlines $ map (replaceByPatt patterns) pacmanHelpMsg
          patterns      = [ ("pacman","aura")
                          , ("operations","Inherited Pacamn Operations")
                          ]

-- ANIMATED VERSION MESSAGE
animateVersionMsg :: [String] -> IO ()
animateVersionMsg verMsg = do
  mapM_ putStrLn . map (padString lineHeaderLength) $ verMsg
  putStr $ raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  mapM_ putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
  putStr $ raiseCursorBy 4
  takeABite 0
  mapM_ pillEating pillsAndWidths
  putStr clearGrid
  putStrLn auraLogo
  putStrLn "AURA Version 0.1.1.0\n\n\n"
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]

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
