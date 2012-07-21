{-# OPTIONS_GHC -O2 #-}

-- `Aura` package manager for Arch Linux.
-- Written by Colin Woodbury <colingw@gmail.com>

-- System Libraries
import System.Directory (getCurrentDirectory, copyFile, removeFile)
import Data.List ((\\), nub, sort, intersperse, groupBy)
import System.Environment (getArgs, getEnvironment)
import Control.Monad (filterM, when, unless)
import System.Exit (exitWith, ExitCode)
import System.Posix.Files (fileExist)
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
import AuraLib
import Pacman
import Shell

auraVersion :: String
auraVersion = "0.7.2.3"

main :: IO a
main = do
  args <- getArgs
  let (auraFlags,input,pacOpts) = parseOpts args
      auraFlags' = filter (`notElem` settingsFlags) auraFlags
      pacOpts'   = pacOpts ++ reconvertFlags auraFlags dualFlagMap
  settings   <- getSettings auraFlags
  exitStatus <- executeOpts settings (auraFlags', nub input, nub pacOpts')
  exitWith exitStatus

getSettings :: [Flag] -> IO Settings
getSettings auraFlags = do
  confFile    <- getPacmanConf
  environment <- getEnvironment
  return $ Settings { environmentOf   = environment
                    , langOf          = getLanguage auraFlags
                    , ignoredPkgsOf   = getIgnoredPkgs confFile
                    , cachePathOf     = getCachePath confFile
                    , logFilePathOf   = getLogFilePath confFile
                    , suppressMakepkg = getSuppression auraFlags
                    , mustConfirm     = getConfirmation auraFlags
                    , mayHotEdit      = getHotEdit auraFlags }

-- After determining what Flag was given, dispatches a function.
-- The `flags` must be sorted to guarantee the pattern matching
-- below will work properly.
executeOpts :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
executeOpts ss (flags,input,pacOpts) = do
  case sort flags of
    (AURInstall:fs) ->
        case fs of
          []             -> ss |+| (ss |$| installPackages ss pacOpts input)
          [Upgrade]      -> ss |+| (ss |$| upgradeAURPkgs ss pacOpts input)
          [Download]     -> downloadTarballs ss input
          [GetPkgbuild]  -> displayPkgbuild ss input
          (Refresh:fs')  -> ss |$| syncAndContinue ss (fs',input,pacOpts)
          (DelMDeps:fs') -> ss |$| removeMakeDeps ss (fs',input,pacOpts)
          badFlags       -> scoldAndFail ss executeOptsMsg1
    (Cache:fs) ->
        case fs of
          []       -> ss |$| downgradePackages ss input
          [Clean]  -> ss |$| preCleanCache ss input
          [Search] -> searchPackageCache ss input
          [Backup] -> ss |$| backupCache ss input
          badFlags -> scoldAndFail ss executeOptsMsg1
    (LogFile:fs) ->
        case fs of
          []       -> viewLogFile $ logFilePathOf ss
          [Search] -> searchLogFile ss input
          [Info]   -> logInfoOnPkg ss input
          badFlags -> scoldAndFail ss executeOptsMsg1
    [Orphans]   -> getOrphans >>= mapM_ putStrLn >> returnSuccess
    [Adopt]     -> ss |$| (pacman $ ["-D","--asexplicit"] ++ input)
    [Abandon]   -> ss |$| (getOrphans >>= flip removePkgs pacOpts)
    [Languages] -> displayOutputLanguages ss
    [Help]      -> printHelpMsg pacOpts
    [Version]   -> getVersionInfo >>= animateVersionMsg
    pacmanFlags -> pacman $ pacOpts ++ input ++ hijackedFlags
    where hijackedFlags = reconvertFlags flags hijackedFlagMap
          
--------------------
-- WORKING WITH `-A`
--------------------
installPackages :: Settings -> [String] -> [String] -> IO ExitCode
installPackages _ _ [] = returnSuccess
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
      returnFailure
    Right (pacmanDeps,aurDeps) -> do
      let pacPkgs     = nub $ pacmanDeps ++ forPacman
          pkgsAndOpts = pacOpts ++ pacPkgs
      reportPkgsToInstall lang pacPkgs aurDeps aurPackages 
      okay <- optionalPrompt (mustConfirm settings) (installPackagesMsg3 lang)
      if not okay
         then scoldAndFail settings installPackagesMsg4
         else do
           unless (null pacPkgs) (pacman' $ ["-S","--asdeps"] ++ pkgsAndOpts)
           mapM_ (buildAndInstallDep settings pacOpts) aurDeps
           pkgFiles <- buildPackages settings aurPackages
           installPackageFiles pacOpts pkgFiles

buildAndInstallDep :: Settings -> [String] -> AURPkg -> IO ExitCode
buildAndInstallDep settings pacOpts pkg = do
  path <- buildPackages settings [pkg]
  installPackageFiles (["--asdeps"] ++ pacOpts) path
  
reportNonPackages :: Language -> [String] -> IO ()
reportNonPackages lang nons = printListWithTitle red cyan msg nons
    where msg = reportNonPackagesMsg1 lang

reportIgnoredPackages :: Language -> [String] -> IO ()
reportIgnoredPackages lang pkgs = printListWithTitle yellow cyan msg pkgs
    where msg = reportIgnoredPackagesMsg1 lang

reportPkgsToInstall :: Language -> [String] -> [AURPkg] -> [AURPkg] -> IO ()
reportPkgsToInstall lang pacPkgs aurDeps aurPkgs = do
  printIfThere pacPkgs $ reportPkgsToInstallMsg1 lang
  printIfThere (namesOf aurDeps) $ reportPkgsToInstallMsg2 lang
  printIfThere (namesOf aurPkgs) $ reportPkgsToInstallMsg3 lang
      where namesOf = map pkgNameOf
            printIfThere ps msg = unless (null ps) $
                                  printListWithTitle green cyan msg ps
               
upgradeAURPkgs :: Settings -> [String] -> [String] -> IO ExitCode
upgradeAURPkgs settings pacOpts pkgs = do
  notify settings upgradeAURPkgsMsg1
  installedPkgs <- getInstalledAURPackages
  toCheck       <- mapM fetchAndReport $ filter notIgnored installedPkgs
  notify settings upgradeAURPkgsMsg2
  let toUpgrade = map pkgNameOf $ filter isOutOfDate toCheck
  when (null toUpgrade) (warn settings upgradeAURPkgsMsg3)
  installPackages settings pacOpts $ toUpgrade ++ pkgs
    where notIgnored p = splitName p `notElem` ignoredPkgsOf settings
          fetchAndReport p = do
            aurPkg <- makeAURPkg p
            say settings (flip upgradeAURPkgsMsg4 $ pkgNameOf aurPkg)
            return aurPkg

downloadTarballs :: Settings -> [String] -> IO ExitCode
downloadTarballs settings pkgs = do
  currDir   <- getCurrentDirectory
  dontExist <- filterM isntAURPackage pkgs
  reportNonPackages (langOf settings) dontExist
  downloadEach currDir $ pkgs \\ dontExist
      where downloadEach _ [] = returnSuccess
            downloadEach path (p:ps) = do
              notify settings $ flip downloadTarballsMsg1 p
              _ <- downloadSource path p
              downloadEach path ps

-- Very similar to `downloadTarballs`...
displayPkgbuild :: Settings -> [String] -> IO ExitCode
displayPkgbuild settings pkgs = do
  dontExist <- filterM isntAURPackage pkgs
  reportNonPackages (langOf settings) dontExist
  dlEach $ pkgs \\ dontExist
    where dlEach []     = returnSuccess
          dlEach (p:ps) = downloadPkgbuild p >>= putStrLn >> dlEach ps

syncAndContinue :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
syncAndContinue settings (flags,input,pacOpts) = do
  _ <- syncDatabase pacOpts
  executeOpts settings (AURInstall:flags,input,pacOpts)

-- Uninstalls `make` dependencies that were only necessary for building
-- and are no longer required by anything. This is the very definition of
-- an `orphan` package, thus a before-after comparison of orphan packages
-- is done to determine what needs to be uninstalled.
removeMakeDeps :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
removeMakeDeps settings (flags,input,pacOpts) = do
  orphansBefore <- getOrphans
  exitStatus    <- executeOpts settings (AURInstall:flags,input,pacOpts)
  if didProcessFail exitStatus
     then returnFailure
     else do
       orphansAfter <- getOrphans
       let makeDeps = orphansAfter \\ orphansBefore
       unless (null makeDeps) $ notify settings removeMakeDepsAfterMsg1
       removePkgs makeDeps pacOpts

--------------------
-- WORKING WITH `-C`
--------------------
-- Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: Settings -> [String] -> IO ExitCode
downgradePackages _ []          = returnSuccess                     
downgradePackages settings pkgs = do
  cache     <- packageCacheContents cachePath
  installed <- filterM isInstalled pkgs
  let notInstalled = pkgs \\ installed
  unless (null notInstalled) (reportBadDowngradePkgs settings notInstalled)
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
searchPackageCache :: Settings -> [String] -> IO ExitCode
searchPackageCache settings input = do
  cache <- packageCacheContents $ cachePathOf settings  
  mapM_ putStrLn . sortPkgs . searchLines (unwords input) $ cache
  returnSuccess

-- The destination folder must already exist for the back-up to being.
backupCache :: Settings -> [String] -> IO ExitCode
backupCache settings []      = scoldAndFail settings backupCacheMsg1
backupCache settings (dir:_) = do
  exists <- fileExist dir
  if not exists
     then scoldAndFail settings backupCacheMsg3
     else do
       cache <- packageCacheContents $ cachePathOf settings
       notify settings $ flip backupCacheMsg4 dir
       notify settings . flip backupCacheMsg5 . length $ cache
       okay <- optionalPrompt (mustConfirm settings)
               (backupCacheMsg6 $ langOf settings)
       if not okay
          then scoldAndFail settings backupCacheMsg7
          else do
            notify settings backupCacheMsg8
            putStrLn ""  -- So that the cursor can rise at first.
            copyAndNotify settings dir cache 1

-- Manages the file copying and display of the real-time progress notifier.
copyAndNotify :: Settings -> FilePath -> [String] -> Int -> IO ExitCode
copyAndNotify _ _ [] _              = returnSuccess
copyAndNotify settings dir (p:ps) n = do
  putStr $ raiseCursorBy 1
  warn settings (flip copyAndNotifyMsg1 n)
  copyFile (cachePath </> p) (dir </> p)
  copyAndNotify settings dir ps $ n + 1
      where cachePath = cachePathOf settings

-- Acts as a filter for the input to `cleanCache`.
preCleanCache :: Settings -> [String] -> IO ExitCode
preCleanCache settings [] = cleanCache settings 0
preCleanCache settings (input:_)  -- Ignores all but first input element.
  | all isDigit input = cleanCache settings $ read input
  | otherwise         = scoldAndFail settings $ flip preCleanCacheMsg1 input

-- Keeps a certain number of package files in the cache according to
-- a number provided by the user. The rest are deleted.
cleanCache :: Settings -> Int -> IO ExitCode
cleanCache ss toSave
    | toSave < 0  = scoldAndFail ss cleanCacheMsg1
    | toSave == 0 = warn ss cleanCacheMsg2 >> pacman ["-Scc"]
    | otherwise   = do
        warn ss $ flip cleanCacheMsg3 toSave
        okay <- optionalPrompt (mustConfirm ss) (cleanCacheMsg4 $ langOf ss)
        if not okay
           then scoldAndFail ss cleanCacheMsg5
           else do
             notify ss cleanCacheMsg6
             cache <- packageCacheContents $ cachePathOf ss
             let grouped = map (take toSave . reverse) $ groupByPkgName cache
                 toRemove  = cache \\ concat grouped
                 filePaths = map (cachePathOf ss </>) toRemove
             mapM_ removeFile filePaths  -- Error handling?
             returnSuccess

-- Typically takes the contents of the package cache as an argument.
groupByPkgName :: [String] -> [[String]]
groupByPkgName pkgs = groupBy sameBaseName $ sortPkgs pkgs
    where sameBaseName a b = baseName a == baseName b
          baseName p = tripleFst (p =~ "-[0-9]+" :: (String,String,String))

--------------------
-- WORKING WITH `-L`
--------------------
viewLogFile :: FilePath -> IO ExitCode
viewLogFile logFilePath = rawSystem "less" [logFilePath]

-- Very similar to `searchCache`. But is this worth generalizing?
searchLogFile :: Settings -> [String] -> IO ExitCode
searchLogFile settings input = do
  logFile <- readFile (logFilePathOf settings) >>= return . lines
  mapM_ putStrLn $ searchLines (unwords input) logFile
  returnSuccess

-- Are you failing at looking up anything,
-- or succeeding at looking up nothing?
logInfoOnPkg :: Settings -> [String] -> IO ExitCode
logInfoOnPkg _ [] = returnFailure  -- Success?
logInfoOnPkg settings pkgs = do
  logFile <- readFile (logFilePathOf settings)
  let toCheck = filter (\p -> logFile =~ (" " ++ p ++ " ")) pkgs
  if null toCheck
     then returnFailure
     else mapM_ (logLookUp settings logFile) toCheck >> returnSuccess

-- Make internal to `logInfoOnPkg`?
-- Assumed: The package to look up _exists_.
logLookUp :: Settings -> String -> String -> IO ()
logLookUp _ _ [] = return ()
logLookUp settings logFile pkg = do
  mapM_ putStrLn $ [ logLookUpMsg1 (langOf settings) pkg
                   , logLookUpMsg2 (langOf settings) installDate
                   , logLookUpMsg3 (langOf settings) upgrades
                   , logLookUpMsg4 (langOf settings) ] ++ takeLast 5 matches
  putStrLn ""
      where matches     = searchLines (" " ++ pkg ++ " \\(") $ lines logFile
            installDate = head matches =~ "\\[[-:0-9 ]+\\]"
            upgrades    = length $ searchLines " upgraded " matches
            takeLast n  = reverse . take n . reverse

--------
-- OTHER
--------
displayOutputLanguages :: Settings -> IO ExitCode
displayOutputLanguages settings = do
  notify settings displayOutputLanguagesMsg1
  mapM_ (putStrLn . show) allLanguages
  returnSuccess

printHelpMsg :: [String] -> IO ExitCode
printHelpMsg [] = getPacmanHelpMsg >>= putStrLn . getHelpMsg >> returnSuccess
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
animateVersionMsg :: [String] -> IO ExitCode
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
  returnSuccess
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]
