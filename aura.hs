{-# OPTIONS_GHC -O2 #-}

-- AURA package manager for Arch Linux
-- Written by Colin Woodbury <colingw@gmail.com>

-- System Libraries
import Data.List ((\\), nub, delete, sort, intersperse)
import System.Directory (getCurrentDirectory)
import Control.Monad (filterM, when)
import System.Environment (getArgs)
import Text.Regex.Posix ((=~))
import System.FilePath ((</>))
import System.Console.GetOpt

-- Custom Libraries
import AuraLanguages
import AurConnection
import Utilities
import AuraLogo
import Internet
import AuraLib
import Pacman

data Flag = AURInstall | Cache | GetPkgbuild | Search | Refresh |
            Upgrade | Download | Languages | Version | Help | JapOut
            deriving (Eq,Ord)

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['A'] ["aursync"]      (NoArg AURInstall)  aDesc
              , Option ['u'] ["sysupgrade"]   (NoArg Upgrade)     uDesc
              , Option ['w'] ["downloadonly"] (NoArg Download)    wDesc
              , Option ['p'] ["pkgbuild"]     (NoArg GetPkgbuild) pDesc
              , Option ['C'] ["downgrade"]    (NoArg Cache)       cDesc
              , Option ['s'] ["search"]       (NoArg Search)      sDesc 
              ]
    where aDesc = "Install from the [A]UR."
          uDesc = "(With -A) Upgrade all installed AUR packages."
          wDesc = "(With -A) Downloads the source tarball only."
          pDesc = "(With -A) Outputs the contents of a package's PKGBUILD."
          cDesc = "Perform actions involving the package [C]ache.\n" ++
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
interceptedFlags = [ (Search,"-s"),(Refresh,"-y"),(Upgrade,"-u")
                   , (Download,"-w")]

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
auraVersion = "0.4.3.1"

main :: IO ()
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
                           [Upgrade]     -> upgradeAURPkgs lang pacOpts input
                           [Download]    -> downloadTarballs lang  input
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
installPackages _ _ [] = return ()
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
  putStrLnA $ installPackagesMsg5 lang
  results     <- getDepsToInstall lang aurPackages toIgnore
  case results of
    Left errors -> do
      printListWithTitle (installPackagesMsg1 lang) errors
    Right (pacmanDeps,aurDeps) -> do
      let pacPkgs = nub $ pacmanDeps ++ forPacman
          pkgsAndOpts = pacOpts ++ pacPkgs
      reportPkgsToInstall lang pacPkgs aurDeps aurPackages 
      response <- yesNoPrompt (installPackagesMsg3 lang) "^y"
      if not response
         then putStrLnA $ installPackagesMsg4 lang
         else do
           when (notNull pacPkgs) (pacman $ ["-S","--asdeps"] ++ pkgsAndOpts)
           mapM_ (buildAndInstallDep lang) aurDeps
           pkgFiles <- buildPackages lang aurPackages
           installPackageFiles [] pkgFiles

printListWithTitle :: String -> [String] -> IO ()
printListWithTitle msg items = putStrLnA msg >> mapM_ putStrLn items
  
-- TODO: Add guesses! "Did you mean xyz instead?"
reportNonPackages :: Language -> [String] -> IO ()
reportNonPackages _ []      = return ()
reportNonPackages lang nons = printListWithTitle msg nons
    where msg = reportNonPackagesMsg1 lang

-- Same as the function above... 
reportIgnoredPackages :: Language -> [String] -> IO ()
reportIgnoredPackages _ []      = return ()
reportIgnoredPackages lang pkgs = printListWithTitle msg pkgs
    where msg = reportIgnoredPackagesMsg1 lang

reportPkgsToInstall :: Language -> [String] -> [AURPkg] -> [AURPkg] -> IO ()
reportPkgsToInstall lang pacPkgs aurDeps aurPkgs = do
  printIfThere putStrLn pacPkgs $ reportPkgsToInstallMsg1 lang
  printIfThere (putStrLn . pkgNameOf) aurDeps $ reportPkgsToInstallMsg2 lang
  printIfThere (putStrLn . pkgNameOf) aurPkgs $ reportPkgsToInstallMsg3 lang
      where printIfThere f ps msg = when (notNull ps) (printPkgs f ps msg)
            printPkgs f ps msg = putStrLnA msg >> mapM_ f ps >> putStrLn ""

buildAndInstallDep :: Language -> AURPkg -> IO ()
buildAndInstallDep lang pkg = do
  path <- buildPackages lang [pkg]
  installPackageFiles ["--asdeps"] path
               
upgradeAURPkgs :: Language -> [String] -> [String] -> IO ()
upgradeAURPkgs lang pacOpts pkgs = do
  putStrLnA $ upgradeAURPkgsMsg1 lang
  installedPkgs <- getInstalledAURPackages
  confFile      <- getPacmanConf
  let toIgnore   = getIgnoredPkgs confFile
      notIgnored = \p -> fst (splitNameAndVer p) `notElem` toIgnore
      legitPkgs  = filter notIgnored installedPkgs
  toCheck <- mapM fetchAndReport legitPkgs
  putStrLnA $ upgradeAURPkgsMsg2 lang
  let toUpgrade = map pkgNameOf . filter (not . isOutOfDate) $ toCheck
  when (notNull toUpgrade) (putStrLnA $ upgradeAURPkgsMsg3 lang)
  installPackages lang pacOpts $ toUpgrade ++ pkgs
    where fetchAndReport p = do
            aurPkg <- makeAURPkg p
            putStrLnA $ upgradeAURPkgsMsg4 lang (pkgNameOf aurPkg)
            return aurPkg

downloadTarballs :: Language -> [String] -> IO ()
downloadTarballs lang pkgs = do
  currDir  <- getCurrentDirectory
  realPkgs <- filterM isAURPackage pkgs
  reportNonPackages lang $ pkgs \\ realPkgs
  mapM_ (downloadEach currDir) realPkgs
      where downloadEach path pkg = do
              putStrLnA $ downloadTarballsMsg1 lang pkg
              downloadSource path pkg

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
reportBadDowngradePkgs lang pkgs = do
  putStrLnA $ reportBadDowngradePkgsMsg1 lang
  mapM_ putStrLn pkgs
               
getDowngradeChoice :: Language -> [String] -> String -> IO String
getDowngradeChoice lang cache pkg = do
  let choices = getChoicesFromCache cache pkg
  putStrLnA $ getDowngradeChoiceMsg1 lang pkg
  getSelection choices

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
