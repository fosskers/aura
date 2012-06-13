{-# OPTIONS_GHC -O2 #-}

-- AURA package manager for Arch Linux
-- Written by Colin Woodbury <colingw@gmail.com>

-- System Libraries
import Data.List ((\\), nub, delete, sort, intersperse)
import Control.Monad (filterM, when)
import System.Environment (getArgs, getEnv)
import Text.Regex.Posix ((=~))
import System.FilePath ((</>))
import System.Console.GetOpt

-- Custom Libraries
import AuraLanguages
import Utilities
import AuraLogo
import Internet
import Pkgbuild
import AuraLib
import Pacman

data Flag = AURInstall | Cache | GetPkgbuild | Search | Refresh |
            Languages | Version | Help | JapOut
            deriving (Eq,Ord)

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['A'] ["aursync"]  (NoArg AURInstall)  aDesc
              , Option ['p'] ["pkgbuild"] (NoArg GetPkgbuild) pDesc
              , Option ['C'] ["downgrade"] (NoArg Cache)      cDesc
              , Option ['s'] ["search"]   (NoArg Search)      sDesc 
              ]
    where aDesc = "Install from the [A]UR."
          pDesc = "(With -A) Outputs the contents of a package's PKGBUILD."
          cDesc = "Perform actions involving the package [C]ache. " ++
                  "Default action downgrades given packages."
          sDesc = "(With -C) Search the package cache via a regex pattern."

-- These are intercepted Pacman flags.
pacmanOptions :: [OptDescr Flag]
pacmanOptions = [ Option ['y'] ["refresh"] (NoArg Refresh) ""                 
                , Option ['V'] ["version"] (NoArg Version) ""
                , Option ['h'] ["help"]    (NoArg Help)    ""
                ]

languageOptions :: [OptDescr Flag]
languageOptions = [ Option [] ["languages"] (NoArg Languages) lDesc
                  , Option [] ["japanese"]  (NoArg JapOut)    jDesc
                  ]
    where lDesc = "Display the available output languages for aura."
          jDesc = "All aura output is given in Japanese."

interceptedFlags :: [(Flag,String)]
interceptedFlags = [(Search,"-s"),(Refresh,"-y")]

-- Converts an intercepted Pacman flag back into its raw string form.
reconvertFlag :: Flag -> String
reconvertFlag f = case f `lookup` interceptedFlags of
                    Just x  -> x
                    Nothing -> ""

allFlags :: [OptDescr Flag]
allFlags = auraOptions ++ pacmanOptions ++ languageOptions

auraUsageMsg :: String
auraUsageMsg = usageInfo "AURA only operations:" auraOptions

languageMsg :: String
languageMsg = usageInfo "Language options:" languageOptions

auraVersion :: String
auraVersion = "0.4.0.0"

main = do
  args <- getArgs
  (auraFlags,pacFlags,input) <- parseOpts args
  let (lang,auraFlags') = getLanguage auraFlags
  executeOpts lang (auraFlags',pacFlags,input)

parseOpts :: [String] -> IO ([Flag],[String],[String])
parseOpts args = case getOpt' Permute allFlags args of
                   (opts,nonOpts,pacOpts,_) -> return (opts,nonOpts,pacOpts) 

getLanguage :: [Flag] -> (Language,[Flag])
getLanguage flags | JapOut `elem` flags = (japanese, delete JapOut flags)
                  | otherwise           = (english, flags)

executeOpts :: Language -> ([Flag],[String],[String]) -> IO ()
executeOpts lang (flags,input,pacOpts) =
    case sort flags of
      (AURInstall:fs) -> case fs of
                           [] -> installPackages lang pacOpts input
                           [GetPkgbuild] -> displayPkgbuild lang input
                           (Refresh:fs') -> do 
                              syncDatabase lang
                              executeOpts lang (AURInstall:fs',input,pacOpts)
                           _ -> putStrLnA $ executeOptsMsg1 lang
      (Cache:fs)  -> case fs of
                       []       -> downgradePackages lang input
                       [Search] -> searchPackageCache input
                       _        -> putStrLnA $ executeOptsMsg1 lang
      [Languages] -> displayOutputLanguages lang
      [Help]      -> printHelpMsg pacOpts
      [Version]   -> getVersionInfo >>= animateVersionMsg
      _           -> pacman $ pacOpts ++ input ++ map reconvertFlag flags

--------------------
-- WORKING WITH `-A`
--------------------      
installPackages :: Language -> [String] -> [String] -> IO ()
installPackages lang pacOpts pkgs = do
  confFile <- getPacmanConf
  let uniques   = nub pkgs
      toIgnore  = getIgnoredPkgs confFile
      toInstall = uniques \\ toIgnore
      ignored   = uniques \\ toInstall
  reportIgnoredPackages lang ignored
  (forPacman,aurPkgNames,nonPkgs) <- divideByPkgType toInstall
  reportNonPackages lang nonPkgs
  aurPackages <- mapM makeAURPkg aurPkgNames
  results     <- getDepsToInstall lang aurPackages toIgnore
  case results of
    Left errors -> do
      putStrLnA $ installPackagesMsg1 lang
      mapM_ putStrLn errors
    Right (pacmanDeps,aurDeps) -> do
      let pacPkgs = nub $ pacmanDeps ++ forPacman
          aurPkgsForInstall = aurDeps ++ aurPackages
      if null aurPkgsForInstall
         then return () -- putStrLnA $ installPackagesMsg2 lang
         else do
           reportPkgsToInstall lang pacPkgs aurPkgsForInstall
           response <- yesNoPrompt (installPackagesMsg3 lang) "^y"
           if not response
              then putStrLnA $ installPackagesMsg4 lang
              else proceedWithInstall lang pacOpts pacPkgs aurPkgsForInstall

reportPkgsToInstall :: Language -> [String] -> [AURPkg] -> IO ()
reportPkgsToInstall lang pacPkgs aurPkgs = do
  when (not $ null pacPkgs) (putStrLnA (reportPkgsToInstallMsg1 lang) >>
                             mapM_ putStrLn pacPkgs)
  putStrLnA $ reportPkgsToInstallMsg2 lang
  mapM_ (putStrLn . pkgNameOf) aurPkgs

-- Go ahead and build and install everything.
proceedWithInstall :: Language -> [String] -> [String] -> [AURPkg] -> IO ()
proceedWithInstall lang pacOpts pacPkgs aurPkgs = do
  when (not $ null pacPkgs) (pacman $ ["-S"] ++ pacOpts ++ pacPkgs)
  pkgFiles <- buildPackages lang aurPkgs
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
               else putStrLnA $ displayPkgbuildMsg3 lang pkg

--------------------
-- WORKING WITH `-C`
--------------------
downgradePackages :: Language -> [String] -> IO ()
downgradePackages lang pkgs = do
  cache     <- packageCacheContents
  installed <- filterM isInstalled pkgs
  let notInstalled = pkgs \\ installed
  when (not $ null notInstalled) (reportBadDowngradePkgs lang notInstalled)
  selections <- mapM (getDowngradeChoice lang cache) installed
  pacman $ ["-U"] ++ map (packageCache </>) selections

reportBadDowngradePkgs :: Language -> [String] -> IO ()
reportBadDowngradePkgs lang pkgs = putStrLnA "This isn't quite ready."
               
getDowngradeChoice :: Language -> [String] -> String -> IO String
getDowngradeChoice lang cache pkg = do
  let choices = getChoicesFromCache cache pkg
  return ""  -- TEMPORARY

getChoicesFromCache :: [String] -> String -> [String]
getChoicesFromCache cache pkg = sort choices
    where choices = filter (\p -> p =~ ("^" ++ pkg ++ "-[0-9]")) cache

-- As a one-liner if I felt like it:
-- packageCacheContents >>= mapM_ putStrLn . sort . filter (\p -> p =~ pat)
searchPackageCache :: [String] -> IO ()
searchPackageCache input = do
  cache <- packageCacheContents
  let pattern = unwords input
      matches = sort $ filter (\p -> p =~ pattern) cache
  mapM_ putStrLn matches

--------
-- OTHER
--------
displayOutputLanguages :: Language -> IO ()
displayOutputLanguages lang = do
  putStrLnA $ displayOutputLanguagesMsg1 lang
  mapM_ (putStrLn . show) allLanguages

printHelpMsg :: [String] -> IO ()
printHelpMsg []      = getPacmanHelpMsg >>= putStrLn . getHelpMsg
printHelpMsg pacOpts = pacman $ pacOpts ++ ["-h"]

getHelpMsg :: [String] -> String
getHelpMsg pacmanHelpMsg = concat $ intersperse "\n" allMessages
    where allMessages   = [replacedLines,auraUsageMsg,languageMsg]
          replacedLines = unlines $ map (replaceByPatt patterns) pacmanHelpMsg
          patterns      = [ ("pacman","aura")
                          , ("operations","Inherited Pacman Operations") ]

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
  putStrLn $ "AURA Version " ++ auraVersion
  putStrLn " by Colin Woodbury\n\n"
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]
