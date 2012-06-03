{-# OPTIONS_GHC -O2 #-}

-- AURA package manager for Arch Linux
-- Written by Colin Woodbury <colingw@gmail.com>

-- System Libraries
import Data.List ((\\), nub, delete, sort)
import Control.Monad (filterM, when)
import System.Environment (getArgs)
import System.Console.GetOpt

-- Custom Libraries
import AuraLanguages
import AURPackages
import Utilities
import AuraLogo
import Internet
import Pkgbuild
import Pacman

data Flag = AURInstall | GetPkgbuild | Languages | Version | Help | JapOut
            deriving (Eq,Ord)

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['A'] ["aursync"]  (NoArg AURInstall)  aDesc
              , Option ['p'] ["pkgbuild"] (NoArg GetPkgbuild) pDesc
              , Option [] ["languages"]   (NoArg Languages)   lDesc
              , Option [] ["japanese"]    (NoArg JapOut)      jDesc
              ]
    where aDesc = "Install from the AUR."
          pDesc = "(With -A) Outputs the contents of a package's PKGBUILD."
          lDesc = "Display the available output languages for aura."
          jDesc = "All aura output is given in Japanese."

-- These are intercepted Pacman flags.
pacmanOptions :: [OptDescr Flag]
pacmanOptions = [ Option ['V'] ["version"] (NoArg Version) ""
                , Option ['h'] ["help"]    (NoArg Help)    ""
                ]

auraUsageMsg :: String
auraUsageMsg = usageInfo "AURA only operations:" auraOptions

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
    case sort flags of
      [Languages]     -> displayOutputLanguages lang
      [Help]          -> printHelpMsg pacOpts
      [Version]       -> getVersionInfo >>= animateVersionMsg
      []              -> (pacman $ pacOpts ++ input)
      (AURInstall:fs) -> case fs of
                           [] -> installPackages lang pacOpts input
                           [GetPkgbuild] -> displayPkgbuild lang input
      
installPackages :: Language -> [String] -> [String] -> IO ()
installPackages lang pacOpts pkgs = do
  let uniques =  nub pkgs
  forPacman   <- filterM isArchPackage uniques
  aurPkgNames <- filterM isAURPackage uniques
  handleNonPackages lang $ uniques \\ (forPacman ++ aurPkgNames)
  aurPackages <- mapM packagify aurPkgNames
  (pacmanDeps,aurDeps) <- getDepsToInstall aurPackages
  let pacmanPkgs = nub $ pacmanDeps ++ forPacman
  when (not $ null pacmanPkgs) (pacman $ ["-S"] ++ pacOpts ++ pacmanPkgs)
  pkgFiles    <- buildPackages lang $ aurDeps ++ aurPackages
  installPackageFiles pkgFiles

displayPkgbuild :: Language -> [String] -> IO ()
displayPkgbuild lang pkgs = do
  mapM_ displayEach pkgs
  --putStrLnA $ displayPkgbuildMsg1 lang
    where displayEach pkg = do
            --putStrLnA $ displayPkgbuildMsg2 lang pkg
            itExists <- doesUrlExist $ getPkgbuildUrl pkg
            if itExists
               then downloadPkgbuild pkg >>= putStrLn
               else putStrLnA $ displayPkgbuildMsg3 lang

displayOutputLanguages :: Language -> IO ()
displayOutputLanguages lang = do
  putStrLnA $ displayOutputLanguagesMsg1 lang
  mapM_ (putStrLn . show) allLanguages

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
  putStrLn "AURA Version 0.2.1.0"
  putStrLn " by Colin Woodbury\n\n"
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]
