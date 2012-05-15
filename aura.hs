-- AURA package manager for Arch Linux

-- System libraries
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import System.Console.GetOpt
import Text.Printf (printf)

-- Custom libraries
import Pacman

data MouthState = Open | Closed deriving (Eq)

data Flag = AURInstall | Version | Help deriving (Eq)

options :: [OptDescr Flag]
options = [ Option ['A'] ["aursync"] (NoArg AURInstall) aDesc
          , Option ['V'] []          (NoArg Version)    ""
          , Option ['h'] ["help"]    (NoArg Help)       ""
          ]
    where aDesc = "Install from the AUR."

auraUsageMsg :: String
auraUsageMsg = usageInfo "AURA only operations:" options

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

{-
argError :: String -> a
argError msg = error $ usageInfo (msg ++ "\n" ++ usageMsg) options
-}


main = do
  args <- getArgs
  opts <- parseOpts args
  executeOpts opts

parseOpts :: [String] -> IO ([Flag],[String],[String])
parseOpts args = case getOpt' Permute options args of
                   (opts,nonOpts,pacOpts,_) -> return (opts,nonOpts,pacOpts) 

executeOpts :: ([Flag],[String],[String]) -> IO ()
executeOpts (flags,input,pacOpts) =
    case flags of
      [Help]       -> getPacmanHelpMsg >>= putStrLn . getHelpMsg
      [Version]    -> getVersionInfo >>= animateVersionMsg
      [AURInstall] -> putStrLn "This option isn't ready yet."
      _            -> (pacman $ pacOpts ++ input) >> return ()

-- Crappy temp version.
-- Do this with regexes!
getHelpMsg :: [String] -> String
getHelpMsg pacmanHelpMsg = replacedLines ++ "\n" ++ auraUsageMsg
    where replacedLines = unlines . map replaceWord $ pacmanHelpMsg
          replaceWord   = unwords . map replace . words
          replace "pacman"      = "aura"
          replace "operations:" = "Inherited Pacman Operations:"
          replace otherWord     = otherWord

animateVersionMsg :: [String] -> IO ()
animateVersionMsg verMsg = do
  mapM_ putStrLn . map (padString lineHeaderLength) $ verMsg
  putStr $ raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  mapM_ putStrLn $ renderPacmanHead 0 Open
  putStr $ raiseCursorBy 4
  takeABite 0
  mapM_ pillEating pillsAndWidths
  putStr clearGrid
  putStrLn auraLogo
  putStrLn "AURA Version 0.1.0.0"
  putStr $ replicate 4 '\n'  -- This goes last.
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]

takeABite :: Int -> IO ()
takeABite pad = drawHead Closed >> drawHead Open
    where drawHead mouth = do
            mapM_ putStrLn $ renderPacmanHead pad mouth
            putStr $ raiseCursorBy 4
            hFlush stdout
            threadDelay 175000

drawPills :: Int -> IO ()
drawPills numOfPills = mapM_ (\aPill -> mapM_ putStrLn aPill) pills
    where pills = renderPills numOfPills

raiseCursorBy :: Int -> String
raiseCursorBy 0 = ""
raiseCursorBy n = "\r\b\r" ++ raiseCursorBy (n - 1)

clearGrid :: String
clearGrid = blankLines ++ raiseCursorBy 4
    where blankLines = concat . replicate 4 . padString 23 $ "\n"
