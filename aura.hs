{-# OPTIONS_GHC -O2 #-}

-- AURA package manager for Arch Linux
-- Written by Colin Woodbury <colingw@gmail.com>

-- System Libraries
import Control.Monad (filterM, when)
import Data.List ((\\), nub, delete, sort)
import System.Environment (getArgs)
import System.Console.GetOpt

-- Custom Libraries
import AuraLanguages
import AURPackages
import Utilities
import AuraLogo
import Internet
import Pacman

data Flag = AURInstall | GetPkgbuild | Version | Help | JapOut
            deriving (Eq,Ord)

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['A'] ["aursync"]  (NoArg AURInstall)  aDesc
              , Option ['p'] ["pkgbuild"] (NoArg GetPkgbuild) pDesc
              , Option [] ["japanese"]    (NoArg JapOut)      jDesc
              ]
    where aDesc = "Install from the AUR."
          pDesc = "(With -A) Outputs the contents of a package's PKGBUILD."
          jDesc = "All aura output is given in Japanese."

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
  (auraFlags,pacFlags,input) <- parseOpts args
  let (lang,auraFlags') = getLanguage auraFlags
  executeOpts lang (auraFlags',pacFlags,input)

parseOpts :: [String] -> IO ([Flag],[String],[String])
parseOpts args = case getOpt' Permute (auraOptions ++ pacmanOptions) args of
                   (opts,nonOpts,pacOpts,_) -> return (opts,nonOpts,pacOpts) 

getLanguage :: [Flag] -> (Language,[Flag])
getLanguage flags | JapOut `elem` flags = (japanese, delete JapOut flags)
                  | otherwise           = (english, flags)

executeOpts :: Language -> ([Flag],[String],[String]) -> IO ()
executeOpts lang (flags,input,pacOpts) =
    case flags of
      [Help]          -> printHelpMsg pacOpts
      [Version]       -> getVersionInfo >>= animateVersionMsg
      []              -> (pacman $ pacOpts ++ input)
      (AURInstall:fs) -> case fs of
                           [] -> installPackages lang pacOpts input
                           [GetPkgbuild] -> displayPkgbuild lang input
      
installPackages :: Language -> [String] -> [String] -> IO ()
installPackages lang pacOpts pkgs = do
  uniques     <- filterM isNotInstalled $ nub pkgs
  forPacman   <- filterM isArchPackage uniques
  aurPkgNames <- filterM isAURPackage uniques
  handleNonPackages lang $ uniques \\ (forPacman ++ aurPkgNames)
  aurPackages <- mapM packagify aurPkgNames
  (pacmanDeps,aurDeps) <- determineDeps aurPackages
  let pacmanPkgs = nub $ pacmanDeps ++ forPacman
  when (not $ null pacmanPkgs) (pacman $ ["-S"] ++ pacOpts ++ pacmanPkgs)
  pkgFiles    <- buildPackages lang $ aurDeps ++ aurPackages
  installPackageFiles pkgFiles

displayPkgbuild :: Language -> [String] -> IO ()
displayPkgbuild lang [] = putStrLnA $ displayPkgbuildMsg1 lang
displayPkgbuild lang pkgs = mapM_ displayEach pkgs
    where displayEach pkg = do
            putStrLnA $ displayPkgbuildMsg2 lang pkg
            itExists <- doesUrlExist $ getPkgbuildUrl pkg
            if itExists
            then downloadPkgbuild pkg >>= putStrLn
            else putStrLnA $ displayPkgbuildMsg3 lang

printHelpMsg :: [String] -> IO ()
printHelpMsg []      = getPacmanHelpMsg >>= putStrLn . getHelpMsg
printHelpMsg pacOpts = pacman $ pacOpts ++ ["-h"]

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
