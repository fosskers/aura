{-# OPTIONS_GHC -O2 #-}

-- `Aura` package manager for Arch Linux
-- Written by Colin Woodbury <colingw@gmail.com>

-- System Libraries
import Data.List ((\\), nub, sort, intersperse, groupBy)
import System.Directory (getCurrentDirectory, copyFile, removeFile)
import System.Posix.Files (fileExist)
import Control.Monad (filterM, when)
import System.Environment (getArgs)
import System.Process (rawSystem)
import Text.Regex.Posix ((=~))
import System.FilePath ((</>))
import Data.Char (isDigit)

-- Custom Libraries
import AuraLanguages
import AurConnection
import AuraFlags
import Utilities
import AuraLogo
import Internet
import AuraLib
import Pacman

auraVersion :: String
auraVersion = "0.5.2.0"

main :: IO ()
main = do
  args <- getArgs
  (auraFlags,input,pacOpts) <- parseOpts args
  confFile <- getPacmanConf
  let language     = getLanguage auraFlags
      suppression  = getSuppression auraFlags
      confirmation = getConfirmation auraFlags
      settings = Settings { langOf          = language
                          , ignoredPkgsOf   = getIgnoredPkgs confFile
                          , cachePathOf     = getCachePath confFile
                          , logFilePathOf   = getLogFilePath confFile
                          , suppressMakepkg = suppression
                          , mustConfirm     = confirmation }
      auraFlags' = filter (`notElem` settingsFlags) auraFlags
      pacOpts'   = pacOpts ++ reconvertFlags auraFlags dualFlagMap
  executeOpts settings (auraFlags', nub input, nub pacOpts')

-- After determining what Flag was given, dispatches a function.
executeOpts :: Settings -> ([Flag],[String],[String]) -> IO ()
executeOpts settings (flags,input,pacOpts) = do
    case sort flags of
      (AURInstall:fs) ->
          case fs of
            []            -> installPackages settings pacOpts input
            [Upgrade]     -> upgradeAURPkgs settings pacOpts input
            [Download]    -> downloadTarballs settings input
            [GetPkgbuild] -> displayPkgbuild settings input
            (Refresh:fs') -> do 
                      syncDatabase pacOpts
                      executeOpts settings (ai:fs',input,pacOpts)
            (DelMDeps:fs')-> removeMakeDeps settings (ai:fs',input,pacOpts)
            badFlags -> scold settings executeOptsMsg1
      (Cache:fs) ->
          case fs of
            []       -> downgradePackages settings input
            [Clean]  -> preCleanCache settings input
            [Search] -> searchPackageCache settings input
            [Backup] -> backupCache settings input
            badFlags -> scold settings executeOptsMsg1
      [ViewLog]   -> viewLogFile $ logFilePathOf settings
      [Orphans]   -> getOrphans >>= mapM_ putStrLn
      [Adopt]     -> pacman $ ["-D","--asexplicit"] ++ input
      [Abandon]   -> getOrphans >>= flip removePkgs pacOpts
      [Languages] -> displayOutputLanguages settings
      [Help]      -> printHelpMsg pacOpts
      [Version]   -> getVersionInfo >>= animateVersionMsg
      pacmanFlags -> pacman $ pacOpts ++ input ++ convertedHijackedFlags
    where convertedHijackedFlags = reconvertFlags flags hijackedFlagMap
          ai = AURInstall  -- This is ugly.
          
--------------------
-- WORKING WITH `-A`
--------------------      
installPackages :: Settings -> [String] -> [String] -> IO ()
installPackages _ _ [] = return ()
installPackages settings pacOpts pkgs = do
  let toInstall = pkgs \\ ignoredPkgsOf settings
      ignored   = pkgs \\ toInstall
      lang      = langOf settings
  reportIgnoredPackages lang ignored
  (forPacman,aurPkgNames,nonPkgs) <- divideByPkgType toInstall
  reportNonPackages lang nonPkgs
  aurPackages <- mapM makeAURPkg aurPkgNames
  notify settings installPackagesMsg5
  results     <- getDepsToInstall settings aurPackages
  case results of
    Left errors -> do
      printListWithTitle red noColour (installPackagesMsg1 lang) errors
    Right (pacmanDeps,aurDeps) -> do
      let pacPkgs     = nub $ pacmanDeps ++ forPacman
          pkgsAndOpts = pacOpts ++ pacPkgs
      reportPkgsToInstall lang pacPkgs aurDeps aurPackages 
      okay <- optionalPrompt (mustConfirm settings) (installPackagesMsg3 lang)
      if not okay
         then scold settings installPackagesMsg4
         else do
           when (notNull pacPkgs) (pacman $ ["-S","--asdeps"] ++ pkgsAndOpts)
           mapM_ (buildAndInstallDep settings pacOpts) aurDeps
           pkgFiles <- buildPackages settings aurPackages
           installPackageFiles pacOpts pkgFiles

buildAndInstallDep :: Settings -> [String] -> AURPkg -> IO ()
buildAndInstallDep settings pacOpts pkg = do
  path <- buildPackages settings [pkg]
  installPackageFiles (["--asdeps"] ++ pacOpts) path
  
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
  printIfThere pacPkgs $ reportPkgsToInstallMsg1 lang
  printIfThere (namesOf aurDeps) $ reportPkgsToInstallMsg2 lang
  printIfThere (namesOf aurPkgs) $ reportPkgsToInstallMsg3 lang
      where namesOf = map pkgNameOf
            printIfThere ps msg = when (notNull ps)
                                  (printListWithTitle green cyan msg ps)
               
upgradeAURPkgs :: Settings -> [String] -> [String] -> IO ()
upgradeAURPkgs settings pacOpts pkgs = do
  notify settings upgradeAURPkgsMsg1
  installedPkgs <- getInstalledAURPackages
  toCheck       <- mapM fetchAndReport $ filter notIgnored installedPkgs
  notify settings upgradeAURPkgsMsg2
  let toUpgrade = map pkgNameOf . filter (not . isOutOfDate) $ toCheck
  when (null toUpgrade) (warn settings upgradeAURPkgsMsg3)
  installPackages settings pacOpts $ toUpgrade ++ pkgs
    where notIgnored p = splitName p `notElem` ignoredPkgsOf settings
          fetchAndReport p = do
            aurPkg <- makeAURPkg p
            say settings (flip upgradeAURPkgsMsg4 $ pkgNameOf aurPkg)
            return aurPkg

downloadTarballs :: Settings -> [String] -> IO ()
downloadTarballs settings pkgs = do
  currDir  <- getCurrentDirectory
  realPkgs <- filterM isAURPackage pkgs
  reportNonPackages (langOf settings) $ pkgs \\ realPkgs
  mapM_ (downloadEach currDir) realPkgs
      where downloadEach path pkg = do
              notify settings (flip downloadTarballsMsg1 pkg)
              downloadSource path pkg

displayPkgbuild :: Settings -> [String] -> IO ()
displayPkgbuild settings pkgs = do
  mapM_ displayEach pkgs
    where displayEach pkg = do
            itExists <- doesUrlExist $ getPkgbuildUrl pkg
            if itExists
               then downloadPkgbuild pkg >>= putStrLn
               else scold settings (flip displayPkgbuildMsg1 pkg)

-- Uninstalls make dependencies that were only necessary for building
-- and are no longer required by anything. This is the very definition of
-- an `orphan` package, thus a before-after comparison of orphan packages
-- is done to determine what needs to be uninstalled.
removeMakeDeps :: Settings -> ([Flag],[String],[String]) -> IO ()
removeMakeDeps settings (flags,input,pacOpts) = do
  orphansBefore <- getOrphans
  executeOpts settings (flags,input,pacOpts)
  orphansAfter  <- getOrphans
  let makedeps = orphansAfter \\ orphansBefore
  when (notNull makedeps) $ notifyAndRemove makedeps
      where notifyAndRemove makedeps = do
              notify settings removeMakeDepsAfterMsg1
              removePkgs makedeps pacOpts

--------------------
-- WORKING WITH `-C`
--------------------
-- Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: Settings -> [String] -> IO ()
downgradePackages settings pkgs = do
  cache     <- packageCacheContents cachePath
  installed <- filterM isInstalled pkgs
  let notInstalled = pkgs \\ installed
  when (not $ null notInstalled) (reportBadDowngradePkgs settings notInstalled)
  selections <- mapM (getDowngradeChoice settings cache) installed
  pacman $ ["-U"] ++ map (cachePath </>) selections
      where cachePath = cachePathOf settings

reportBadDowngradePkgs :: Settings -> [String] -> IO ()
reportBadDowngradePkgs settings pkgs = printListWithTitle red cyan msg pkgs
    where msg = reportBadDowngradePkgsMsg1 $ langOf settings
               
getDowngradeChoice :: Settings -> [String] -> String -> IO String
getDowngradeChoice settings cache pkg = do
  let choices = getChoicesFromCache cache pkg
  notify settings (flip getDowngradeChoiceMsg1 pkg)
  getSelection choices

getChoicesFromCache :: [String] -> String -> [String]
getChoicesFromCache cache pkg = sort choices
    where choices = filter (\p -> p =~ ("^" ++ pkg ++ "-[0-9]")) cache

-- `[]` as input yields the contents of the entire cache.
searchPackageCache :: Settings -> [String] -> IO ()
searchPackageCache settings input = do
  cache <- packageCacheContents $ cachePathOf settings
  let pattern = unwords input
      matches = sort $ filter (\p -> p =~ pattern) cache
  mapM_ putStrLn matches

-- Two conditions must be met for backing-up to begin:
-- 1. The user must be root (or using sudo).
-- 2. The destination folder must already exist.
backupCache :: Settings -> [String] -> IO ()
backupCache settings []      = scold settings backupCacheMsg1
backupCache settings (dir:_) = do
  isRoot <- isUserRoot
  exists <- fileExist dir
  continueIfOkay isRoot exists
      where continueIfOkay isRoot exists
              | not isRoot = scold settings backupCacheMsg2
              | not exists = scold settings backupCacheMsg3
              | otherwise  = do
              cache <- packageCacheContents $ cachePathOf settings
              notify settings $ flip backupCacheMsg4 dir
              notify settings . flip backupCacheMsg5 . length $ cache
              okay <- optionalPrompt (mustConfirm settings)
                      (backupCacheMsg6 $ langOf settings)
              if not okay
                 then scold settings backupCacheMsg7
                 else do
                   notify settings backupCacheMsg8
                   putStrLn ""  -- So that the cursor can rise.
                   copyAndNotify settings dir cache 1

-- Manages the copying and display of the real-time progress notifier.
copyAndNotify :: Settings -> FilePath -> [String] -> Int -> IO ()
copyAndNotify _ _ [] _              = return ()
copyAndNotify settings dir (p:ps) n = do
  putStr $ raiseCursorBy 1
  warn settings (flip copyAndNotifyMsg1 n)
  copyFile (cachePath </> p) (dir </> p)
  copyAndNotify settings dir ps $ n + 1
      where cachePath = cachePathOf settings

preCleanCache :: Settings -> [String] -> IO ()
preCleanCache settings [] = cleanCache settings 0
preCleanCache settings (input:_)  -- Ignores all but first input element.
    | all isDigit input = cleanCache settings $ read input
    | otherwise         = scold settings $ flip preCleanCacheMsg1 input

cleanCache :: Settings -> Int -> IO ()
cleanCache ss toSave
    | toSave < 0  = scold ss cleanCacheMsg1
    | toSave == 0 = warn ss cleanCacheMsg2 >> pacman ["-Scc"]
    | otherwise   = do
        warn ss $ flip cleanCacheMsg3 toSave
        okay <- optionalPrompt (mustConfirm ss) (cleanCacheMsg4 $ langOf ss)
        if not okay
           then scold ss cleanCacheMsg5
           else do
             notify ss cleanCacheMsg6
             cache <- packageCacheContents $ cachePathOf ss
             let grouped = map (take toSave . reverse) $ groupByPkgName cache
                 toRemove  = cache \\ concat grouped
                 filePaths = map (cachePathOf ss </>) toRemove
             mapM_ removeFile filePaths

-- Typically takes the contents of the package cache as an argument.
groupByPkgName :: [String] -> [[String]]
groupByPkgName pkgs = groupBy sameBaseName $ sort pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = tripleFst (p =~ "-[0-9]+" :: (String,String,String))

--------
-- OTHER
--------
viewLogFile :: FilePath -> IO ()
viewLogFile logFilePath = rawSystem "more" [logFilePath] >> return ()

displayOutputLanguages :: Settings -> IO ()
displayOutputLanguages settings = do
  notify settings displayOutputLanguagesMsg1
  mapM_ (putStrLn . show) allLanguages

printHelpMsg :: [String] -> IO ()
printHelpMsg []      = getPacmanHelpMsg >>= putStrLn . getHelpMsg
printHelpMsg pacOpts = pacman $ pacOpts ++ ["-h"]

getHelpMsg :: [String] -> String
getHelpMsg pacmanHelpMsg = concat $ intersperse "\n" allMessages
    where allMessages   = [ replacedLines,auraOperMsg,auraOptMsg
                          , dualFlagMsg,languageMsg ]
          replacedLines = unlines $ map (replaceByPatt patterns) pacmanHelpMsg
          colouredMsg   = colourize yellow "Inherited Pacman Operations" 
          patterns      = [ ("pacman","aura")
                          , ("operations",colouredMsg) ]

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
