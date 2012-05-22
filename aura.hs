{-# OPTIONS_GHC -O2 #-}

-- AURA package manager for Arch Linux
-- Written by Colin Woodbury <colingw@gmail.com>

-- System Libraries

import System.Environment (getArgs)
import System.Console.GetOpt

-- Custom Libraries
import Utilities (replaceByPatt)
import AuraLogo
import Pacman

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
                          , ("operations","Inherited Pacman Operations")
                          ]

-- ANIMATED VERSION MESSAGE
animateVersionMsg :: [String] -> IO ()
animateVersionMsg verMsg = do
  mapM_ putStrLn $ map (padString lineHeaderLength) verMsg  -- Version message
  putStr $ raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  mapM_ putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
  putStr $ raiseCursorBy 4
  takeABite 0
  mapM_ pillEating pillsAndWidths
  putStr clearGrid
  putStrLn auraLogo
  putStrLn "AURA Version 0.2.0.0"
  putStrLn " by Colin Woodbury\n\n"
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]
