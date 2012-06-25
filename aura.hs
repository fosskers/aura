{-# OPTIONS_GHC -O2 #-}

-- `Aura` package manager for Arch Linux
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

type FlagMap = [(Flag,String)]

data Flag = AURInstall | Cache | GetPkgbuild | Search | Refresh |
            Upgrade | Download | Unsuppress | NoConfirm | Languages |
            Version | Help | JapOut deriving (Eq,Ord)

auraOptions :: [OptDescr Flag]
auraOptions = [ Option ['A'] ["aursync"]      (NoArg AURInstall)  aDesc
              , Option ['u'] ["sysupgrade"]   (NoArg Upgrade)     uDesc
              , Option ['w'] ["downloadonly"] (NoArg Download)    wDesc
              , Option ['p'] ["pkgbuild"]     (NoArg GetPkgbuild) pDesc
              , Option ['x'] ["unsuppress"]   (NoArg Unsuppress)  xDesc
              , Option ['C'] ["downgrade"]    (NoArg Cache)       cDesc
              , Option ['s'] ["search"]       (NoArg Search)      sDesc
              ]
    where aDesc = "Install from the [A]UR."
          uDesc = "(With -A) Upgrade all installed AUR packages."
          wDesc = "(With -A) Download the source tarball only."
          pDesc = "(With -A) Output the contents of a package's PKGBUILD."
          xDesc = "(With -A) Unsuppress makepkg output."
          cDesc = "Perform actions involving the package [C]ache.\n" ++
                  "Default action downgrades given packages."
          sDesc = "(With -C) Search the package cache via a regex pattern."

-- These are intercepted Pacman flags. Their functionality is different.
pacmanOptions :: [OptDescr Flag]
pacmanOptions = [ Option ['y'] ["refresh"] (NoArg Refresh) ""
                , Option ['V'] ["version"] (NoArg Version) ""
                , Option ['h'] ["help"]    (NoArg Help)    ""
                ]

-- Options that have functionality stretching across both Aura and Pacman.
dualOptions :: [OptDescr Flag]
dualOptions = [ Option [] ["noconfirm"] (NoArg NoConfirm) ncDesc ]
    where ncDesc = "Never ask for any Aura or Pacman confirmation."

languageOptions :: [OptDescr Flag]
languageOptions = [ Option [] ["languages"] (NoArg Languages) lDesc
                  , Option [] ["japanese"]  (NoArg JapOut)    jDesc
                  ]
    where lDesc = "Display the available output languages for aura."
          jDesc = "All aura output is given in Japanese."

-- `Hijacked` flags. They have original pacman functionality, but
-- that is masked and made unique in an Aura context.
hijackedFlagMap :: FlagMap
hijackedFlagMap = [ (Search,"-s"),(Refresh,"-y"),(Upgrade,"-u")
                , (Download,"-w") ]

-- 
dualFlagMap :: FlagMap
dualFlagMap = [ (NoConfirm,"--noconfirm") ]

-- Converts an intercepted Pacman flag back into its raw string form.
reconvertFlag :: FlagMap -> Flag -> String
reconvertFlag flagMap f = case f `lookup` flagMap of
                            Just x  -> x
                            Nothing -> ""

allFlags :: [OptDescr Flag]
allFlags = auraOptions ++ pacmanOptions ++ dualOptions ++ languageOptions

auraUsageMsg :: String
auraUsageMsg = usageInfo "AURA only operations:" auraOptions

dualFlagMsg :: String
dualFlagMsg = usageInfo "Dual functionality options:" dualOptions

languageMsg :: String
languageMsg = usageInfo "Language options:" languageOptions

auraVersion :: String
auraVersion = "0.4.4.1"

main :: IO ()
main = do
  args <- getArgs
  (auraFlags,pacFlags,input) <- parseOpts args
  confFile <- getPacmanConf
  let (lang,auraFlags')  = getLanguage auraFlags
      (supp,auraFlags'') = getSuppression auraFlags'
      toIgnore  = getIgnoredPkgs confFile
      cachePath = getCachePath confFile
      settings  = makeSettings lang toIgnore cachePath supp
  executeOpts settings (auraFlags'',pacFlags,input)

parseOpts :: [String] -> IO ([Flag],[String],[String])
parseOpts args = case getOpt' Permute allFlags args of
                   (opts,nonOpts,pacOpts,_) -> return (opts,nonOpts,pacOpts) 

getLanguage :: [Flag] -> (Language,[Flag])
getLanguage flags | JapOut `elem` flags = (japanese, delete JapOut flags)
                  | otherwise           = (english, flags)

-- Similar to `getLanguage`...
getSuppression :: [Flag] -> (Bool,[Flag])
getSuppression flags
    | Unsuppress `elem` flags = (False, delete Unsuppress flags)
    | otherwise               = (True, flags)

executeOpts :: Settings -> ([Flag],[String],[String]) -> IO ()
executeOpts settings (flags,input,pacOpts) = do
    let pacOpts' = pacOpts ++ map (reconvertFlag dualFlagMap) flags
    case sort flags of
      (AURInstall:fs) ->
          case fs of
            []            -> installPackages settings pacOpts' input
            [Upgrade]     -> upgradeAURPkgs settings pacOpts' input
            [Download]    -> downloadTarballs (langOf settings) input
            [GetPkgbuild] -> displayPkgbuild (langOf settings) input
            (Refresh:fs') -> do 
                      syncDatabase (langOf settings)
                      executeOpts settings (AURInstall:fs',input,pacOpts')
            _ -> putStrLnA red $ executeOptsMsg1 (langOf settings)
      (Cache:fs)  ->
          case fs of
            []       -> downgradePackages settings input
            [Search] -> searchPackageCache settings input
            _ -> putStrLnA red $ executeOptsMsg1 (langOf settings)
      [Languages] -> displayOutputLanguages $ langOf settings
      [Help]      -> printHelpMsg pacOpts  -- Not pacOpts'.
      [Version]   -> getVersionInfo >>= animateVersionMsg
      _           -> pacman $ pacOpts' ++ input ++ map (reconvertFlag hijackedFlagMap) flags

--------------------
-- WORKING WITH `-A`
--------------------      
installPackages :: Settings -> [String] -> [String] -> IO ()
installPackages _ _ [] = return ()
installPackages settings pacOpts pkgs = do
  let uniques   = nub pkgs
      toInstall = uniques \\ ignoredPkgsOf settings
      ignored   = uniques \\ toInstall
      lang      = langOf settings
  reportIgnoredPackages lang ignored
  (forPacman,aurPkgNames,nonPkgs) <- divideByPkgType toInstall
  reportNonPackages lang nonPkgs
  aurPackages <- mapM makeAURPkg aurPkgNames
  putStrLnA green $ installPackagesMsg5 lang
  results     <- getDepsToInstall settings aurPackages
  case results of
    Left errors -> do
      printListWithTitle red noColour (installPackagesMsg1 lang) errors
    Right (pacmanDeps,aurDeps) -> do
      let pacPkgs = nub $ pacmanDeps ++ forPacman
          pkgsAndOpts = pacOpts ++ pacPkgs
      reportPkgsToInstall lang pacPkgs aurDeps aurPackages 
      response <- yesNoPrompt (installPackagesMsg3 lang) "^y"
      if not response
         then putStrLnA red $ installPackagesMsg4 lang
         else do
           when (notNull pacPkgs) (pacman $ ["-S","--asdeps"] ++ pkgsAndOpts)
           mapM_ (buildAndInstallDep settings) aurDeps
           pkgFiles <- buildPackages settings aurPackages
           installPackageFiles [] pkgFiles

printListWithTitle :: Colour -> Colour -> String -> [String] -> IO ()
printListWithTitle titleColour itemColour msg items = do
  putStrLnA titleColour msg
  mapM_ (putStrLn . colourize itemColour) items
  putStrLn ""
  
-- TODO: Add guesses! "Did you mean xyz instead?"
reportNonPackages :: Language -> [String] -> IO ()
reportNonPackages _ []      = return ()
reportNonPackages lang nons = printListWithTitle red cyan msg nons
    where msg = reportNonPackagesMsg1 lang

reportIgnoredPackages :: Language -> [String] -> IO ()
reportIgnoredPackages _ []      = return ()
reportIgnoredPackages lang pkgs = printListWithTitle yellow cyan msg pkgs
    where msg = reportIgnoredPackagesMsg1 lang

reportPkgsToInstall :: Language -> [String] -> [AURPkg] -> [AURPkg] -> IO ()
reportPkgsToInstall lang pacPkgs aurDeps aurPkgs = do
  printIfThere printCyan pacPkgs $ reportPkgsToInstallMsg1 lang
  printIfThere printPkgNameCyan aurDeps $ reportPkgsToInstallMsg2 lang
  printIfThere printPkgNameCyan aurPkgs $ reportPkgsToInstallMsg3 lang
      where printIfThere f ps msg = when (notNull ps) (printPkgs f ps msg)
            printPkgs f ps msg = putStrLnA g msg >> mapM_ f ps >> putStrLn ""
            printCyan = putStrLn . colourize cyan
            printPkgNameCyan = putStrLn . colourize cyan . pkgNameOf
            g = green

buildAndInstallDep :: Settings -> AURPkg -> IO ()
buildAndInstallDep settings pkg = do
  path <- buildPackages settings [pkg]
  installPackageFiles ["--asdeps"] path
               
upgradeAURPkgs :: Settings -> [String] -> [String] -> IO ()
upgradeAURPkgs settings pacOpts pkgs = do
  putStrLnA green $ upgradeAURPkgsMsg1 lang
  installedPkgs <- getInstalledAURPackages
  toCheck       <- mapM fetchAndReport $ filter notIgnored installedPkgs
  putStrLnA green $ upgradeAURPkgsMsg2 lang
  let toUpgrade = map pkgNameOf . filter (not . isOutOfDate) $ toCheck
  when (null toUpgrade) (putStrLnA yellow $ upgradeAURPkgsMsg3 lang)
  installPackages settings pacOpts $ toUpgrade ++ pkgs
    where lang       = langOf settings
          toIgnore   = ignoredPkgsOf settings
          notIgnored = \p -> fst (splitNameAndVer p) `notElem` toIgnore
          fetchAndReport p = do
            aurPkg <- makeAURPkg p
            putStrLnA noColour $ upgradeAURPkgsMsg4 lang (pkgNameOf aurPkg)
            return aurPkg

downloadTarballs :: Language -> [String] -> IO ()
downloadTarballs lang pkgs = do
  currDir  <- getCurrentDirectory
  realPkgs <- filterM isAURPackage pkgs
  reportNonPackages lang $ pkgs \\ realPkgs
  mapM_ (downloadEach currDir) realPkgs
      where downloadEach path pkg = do
              putStrLnA green $ downloadTarballsMsg1 lang pkg
              downloadSource path pkg

displayPkgbuild :: Language -> [String] -> IO ()
displayPkgbuild lang pkgs = do
  mapM_ displayEach pkgs
    where displayEach pkg = do
            itExists <- doesUrlExist $ getPkgbuildUrl pkg
            if itExists
               then downloadPkgbuild pkg >>= putStrLn
               else putStrLnA red $ displayPkgbuildMsg1 lang pkg

--------------------
-- WORKING WITH `-C`
--------------------
downgradePackages :: Settings -> [String] -> IO ()
downgradePackages settings pkgs = do
  cache     <- packageCacheContents cachePath
  installed <- filterM isInstalled pkgs
  let notInstalled = pkgs \\ installed
  when (not $ null notInstalled) (reportBadDowngradePkgs lang notInstalled)
  selections <- mapM (getDowngradeChoice lang cache) installed
  pacman $ ["-U"] ++ map (cachePath </>) selections
      where cachePath = cachePathOf settings
            lang      = langOf settings

reportBadDowngradePkgs :: Language -> [String] -> IO ()
reportBadDowngradePkgs lang pkgs = printListWithTitle red cyan msg pkgs
    where msg = reportBadDowngradePkgsMsg1 lang
               
getDowngradeChoice :: Language -> [String] -> String -> IO String
getDowngradeChoice lang cache pkg = do
  let choices = getChoicesFromCache cache pkg
  putStrLnA green $ getDowngradeChoiceMsg1 lang pkg
  getSelection choices

getChoicesFromCache :: [String] -> String -> [String]
getChoicesFromCache cache pkg = sort choices
    where choices = filter (\p -> p =~ ("^" ++ pkg ++ "-[0-9]")) cache

searchPackageCache :: Settings -> [String] -> IO ()
searchPackageCache settings input = do
  cache <- packageCacheContents $ cachePathOf settings
  let pattern = unwords input
      matches = sort $ filter (\p -> p =~ pattern) cache
  mapM_ putStrLn matches

--------
-- OTHER
--------
displayOutputLanguages :: Language -> IO ()
displayOutputLanguages lang = do
  putStrLnA green $ displayOutputLanguagesMsg1 lang
  mapM_ (putStrLn . show) allLanguages

printHelpMsg :: [String] -> IO ()
printHelpMsg []      = getPacmanHelpMsg >>= putStrLn . getHelpMsg
printHelpMsg pacOpts = pacman $ pacOpts ++ ["-h"]

getHelpMsg :: [String] -> String
getHelpMsg pacmanHelpMsg = concat $ intersperse "\n" allMessages
    where allMessages   = [replacedLines,auraUsageMsg,dualFlagMsg,languageMsg]
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
