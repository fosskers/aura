{-# OPTIONS_GHC -O2 #-}

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

--                       -
--             ---------------------
--       ---------------------------------
--   -----------------------------------------
--  -------------------------------------------
--------------------------------------------------
---------------------------------------------------
-----------------------------------------------------
-------------------------------------------------------
--------------------------------------------------------
---------------------------------------------------------
import Data.List ((\\), nub, sort, intersperse, groupBy) --
import System.Environment (getArgs, getEnvironment) ------
import Control.Monad (liftM, unless, filterM) --------
import System.Exit (exitWith, ExitCode) ----------
import System.Posix.Files (fileExist) --------
import System.FilePath ((</>)) -----------             ______             ___
import Text.Regex.PCRE ((=~)) --------                /      \           /
import Data.Maybe (fromJust) -----                   /        \         /
import Data.Char (isDigit) ---                      /          \       /
--------------------------                          |   aura   |       |   au
import Zero ------------------                      \          /       \
import Shell ---------------------                   \        /         \
import Utilities ---------------------                \______/           \___
import Aura.Logo -------------------------
import Aura.Flags ----------------------------
import Aura.Pacman -------------------------------
import Aura.AuraLib ----------------------------------
import Aura.Languages ------------------------------------
import Aura.AurConnection ---------------------------------
---------------------------------------------------------
--------------------------------------------------------
-------------------------------------------------------
-----------------------------------------------------
---------------------------------------------------
--------------------------------------------------
--  -------------------------------------------
--   -----------------------------------------
--       ---------------------------------
--             ---------------------
--                       -

auraVersion :: String
auraVersion = "1.0.4.0"

main :: IO a
main = do
  args <- getArgs
  let (language,rest) = parseLanguageFlag args
      (auraFlags,input,pacOpts) = parseFlags language rest
      auraFlags' = filter (`notElem` settingsFlags) auraFlags
      pacOpts'   = pacOpts ++ reconvertFlags auraFlags dualFlagMap
  settings <- getSettings language auraFlags
  unless (Debug `notElem` auraFlags) $ debugOutput settings
  exitStatus <- executeOpts settings (auraFlags', nub input, nub pacOpts')
  exitWith exitStatus

getSettings :: Language -> [Flag] -> IO Settings
getSettings lang auraFlags = do
  confFile    <- getPacmanConf
  environment <- getEnvironment
  pmanCommand <- getPacmanCmd environment
  return $ Settings { environmentOf   = environment
                    , langOf          = lang
                    , pacman          = pmanCommand
                    , ignoredPkgsOf   = getIgnoredPkgs confFile
                    , cachePathOf     = getCachePath confFile
                    , logFilePathOf   = getLogFilePath confFile
                    , suppressMakepkg = getSuppression auraFlags
                    , mustConfirm     = getConfirmation auraFlags
                    , mayHotEdit      = getHotEdit auraFlags }

debugOutput :: Settings -> IO ()
debugOutput ss = do
  let yn a = if a then "Yes!" else "No."
      env  = environmentOf ss
  pmanCommand <- getPacmanCmd' env
  mapM_ putStrLn [ "User              => " ++ getUser' env
                 , "True User         => " ++ getTrueUser env
                 , "Using Sudo?       => " ++ yn (varExists "SUDO_USER" env)
                 , "Language          => " ++ show (langOf ss)
                 , "Pacman Command    => " ++ pmanCommand
                 , "Ignored Pkgs      => " ++ unwords (ignoredPkgsOf ss)
                 , "Pkg Cache Path    => " ++ cachePathOf ss
                 , "Log File Path     => " ++ logFilePathOf ss
                 , "Silent Building?  => " ++ yn (suppressMakepkg ss)
                 , "Must Confirm?     => " ++ yn (mustConfirm ss)
                 , "PKGBUILD editing? => " ++ yn (mayHotEdit ss) ]

-- After determining what Flag was given, dispatches a function.
-- The `flags` must be sorted to guarantee the pattern matching
-- below will work properly.
executeOpts :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
executeOpts ss ([],[],[]) = executeOpts ss ([Help],[],[])
executeOpts ss (flags,input,pacOpts) = do
  case sort flags of
    (AURInstall:fs) ->
        case fs of
          []             -> ss |+| (ss |$| installPackages ss pacOpts input)
          [Upgrade]      -> ss |+| (ss |$| upgradeAURPkgs ss pacOpts input)
          [Info]         -> aurPkgInfo ss input
          [Search]       -> aurSearch input
          [ViewDeps]     -> displayPkgDeps ss input
          [Download]     -> downloadTarballs ss input
          [GetPkgbuild]  -> displayPkgbuild input
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
    (Orphans:fs) ->
        case fs of
          []        -> displayOrphans ss input
          [Abandon] -> ss |$| (getOrphans >>= \ps -> removePkgs ss ps pacOpts)
          badFlags  -> scoldAndFail ss executeOptsMsg1
    [ViewConf]  -> viewConfFile
    [Languages] -> displayOutputLanguages ss
    [Help]      -> printHelpMsg ss pacOpts
    [Version]   -> getVersionInfo >>= animateVersionMsg ss
    pacmanFlags -> pacman ss $ pacOpts ++ input ++ hijackedFlags
    where hijackedFlags = reconvertFlags flags hijackedFlagMap
          
--------------------
-- WORKING WITH `-A`
--------------------
{- Ideal look
installPackages pkgs = toAurPkgs pkgs >>= getDeps >>= installDeps >>= install

This could work if Package was a sexy Monad and these operations
could fail silently.

Work in progress in Aura/Build.hs
-}

installPackages :: Settings -> [String] -> [String] -> IO ExitCode
installPackages _ _ [] = returnSuccess
installPackages ss pacOpts pkgs = do
  let toInstall = pkgs \\ ignoredPkgsOf ss
      ignored   = pkgs \\ toInstall
      lang      = langOf ss
  reportIgnoredPackages lang ignored
  (forPacman,aurPkgNames,nonPkgs) <- divideByPkgType toInstall
  reportNonPackages lang nonPkgs
  aurPackages <- mapM makeAURPkg aurPkgNames
  notify ss installPackagesMsg5
  results     <- getDepsToInstall ss aurPackages
  case results of
    Left errors -> do
      printList red noColour (installPackagesMsg1 lang) errors
      returnFailure
    Right (pacmanDeps,aurDeps) -> do
      let repoPkgs    = nub $ pacmanDeps ++ forPacman
          pkgsAndOpts = pacOpts ++ repoPkgs
      reportPkgsToInstall lang repoPkgs aurDeps aurPackages 
      okay <- optionalPrompt (mustConfirm ss) (installPackagesMsg3 lang)
      if not okay
         then scoldAndFail ss installPackagesMsg4
         else do
           unless (null repoPkgs) $ do
                 pacman ss (["-S","--asdeps"] ++ pkgsAndOpts) >> return ()
           mapM_ (buildAndInstallDep ss pacOpts) aurDeps
           pkgFiles <- buildPackages ss aurPackages
           case pkgFiles of
             Just pfs -> installPkgFiles ss pacOpts pfs
             Nothing  -> scoldAndFail ss installPackagesMsg6

buildAndInstallDep :: Settings -> [String] -> AURPkg -> IO ExitCode
buildAndInstallDep ss pacOpts pkg =
  buildPackages ss [pkg] ?>>=
  installPkgFiles ss (["--asdeps"] ++ pacOpts) . fromJust
               
upgradeAURPkgs :: Settings -> [String] -> [String] -> IO ExitCode
upgradeAURPkgs ss pacOpts pkgs = do
  notify ss upgradeAURPkgsMsg1
  foreignPkgs <- filter (\(n,_) -> notIgnored n) `liftM` getForeignPackages
  (aurInfoLookup $ map fst foreignPkgs) ?>>= \pkgInfoEither -> do
    let pkgInfo   = fromRight pkgInfoEither
        aurPkgs   = filter (\(n,_) -> n `elem` map nameOf pkgInfo) foreignPkgs
        toUpgrade = filter isntMostRecent $ zip pkgInfo (map snd aurPkgs)
    notify ss upgradeAURPkgsMsg2
    if null toUpgrade
       then warn ss upgradeAURPkgsMsg3
       else reportPkgsToUpgrade (langOf ss) $ map prettify toUpgrade
    installPackages ss pacOpts $ (map (nameOf . fst) toUpgrade) ++ pkgs
      where notIgnored p   = splitName p `notElem` ignoredPkgsOf ss
            prettify (p,v) = nameOf p ++ " : " ++ v ++ " => " ++ latestVerOf p

aurPkgInfo :: Settings -> [String] -> IO ExitCode
aurPkgInfo ss pkgs = aurInfoLookup pkgs ?>>=
                     mapM_ (displayAurPkgInfo ss) . fromRight >>
                     returnSuccess

displayAurPkgInfo :: Settings -> PkgInfo -> IO ()
displayAurPkgInfo ss info = putStrLn $ renderAurPkgInfo ss info ++ "\n"

renderAurPkgInfo :: Settings -> PkgInfo -> String
renderAurPkgInfo ss info = concat $ intersperse "\n" fieldsAndEntries
    where fieldsAndEntries = map combine $ zip paddedFields entries
          combine (f,e)    = f ++ " : " ++ e
          paddedFields     = map (\x -> postPad x ws longestField) fields
          ws               = whitespace $ langOf ss
          longestField     = maximum $ map length fields
          fields           = infoFields $ langOf ss
          entries          = [ bMagenta "aur"
                             , bForeground $ nameOf info
                             , latestVerOf info
                             , outOfDateMsg (langOf ss) $ isOutOfDate info
                             , cyan $ projectURLOf info
                             , aurURLOf info
                             , licenseOf info
                             , show $ votesOf info
                             , descriptionOf info ]

-- This is quite limited. It only accepts one word/pattern.
aurSearch :: [String] -> IO ExitCode
aurSearch []    = returnFailure
aurSearch regex = aurSearchLookup regex ?>>=
    mapM_ (putStrLn . renderSearchResult (unwords regex)) . fromRight >>
    returnSuccess

renderSearchResult :: String -> PkgInfo -> String
renderSearchResult r info = magenta "aur/" ++ n ++ " " ++ v ++ "\n    " ++ d
    where c cs = case cs =~ ("(?i)" ++ r) of (b,m,a) -> b ++ cyan m ++ a
          n = c $ nameOf info
          d = c $ descriptionOf info
          v | isOutOfDate info = red $ latestVerOf info
            | otherwise        = green $ latestVerOf info

displayPkgDeps :: Settings -> [String] -> IO ExitCode
displayPkgDeps _ []    = returnFailure
displayPkgDeps ss pkgs =
    aurInfoLookup pkgs ?>>= \infoE -> do
      aurPkgs <- mapM makeAURPkg . map nameOf . fromRight $ infoE
      allDeps <- mapM (determineDeps $ langOf ss) aurPkgs
      let (ps,as,_) = foldl groupPkgs ([],[],[]) allDeps
      reportPkgsToInstall (langOf ss) (n ps) (nub as) []
      returnSuccess
          where n = nub . map splitName

downloadTarballs :: Settings -> [String] -> IO ExitCode
downloadTarballs ss pkgs = do
  currDir <- pwd
  filterAURPkgs pkgs ?>>= mapM_ (downloadTBall currDir) >> returnSuccess
    where downloadTBall path pkg = do
              notify ss $ flip downloadTarballsMsg1 pkg
              downloadSource path pkg

displayPkgbuild :: [String] -> IO ExitCode
displayPkgbuild pkgs = filterAURPkgs pkgs ?>>= mapM_ download >> returnSuccess
      where download p = downloadPkgbuild p >>= putStrLn

syncAndContinue :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
syncAndContinue settings (flags,input,pacOpts) = do
  _ <- syncDatabase (pacman settings) pacOpts
  executeOpts settings (AURInstall:flags,input,pacOpts)  -- This is Evil.

removeMakeDeps :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
removeMakeDeps settings (flags,input,pacOpts) = do
  orphansBefore <- getOrphans
  executeOpts settings (AURInstall:flags,input,pacOpts) ?>> do
    orphansAfter <- getOrphans
    let makeDeps = orphansAfter \\ orphansBefore
    unless (null makeDeps) $ notify settings removeMakeDepsAfterMsg1
    removePkgs settings makeDeps pacOpts

--------------------
-- WORKING WITH `-C`
--------------------
-- Interactive. Gives the user a choice as to exactly what versions
-- they want to downgrade to.
downgradePackages :: Settings -> [String] -> IO ExitCode
downgradePackages _ []    = returnSuccess
downgradePackages ss pkgs = do
  reals <- filterM isInstalled pkgs
  reportBadDowngradePkgs (langOf ss) (pkgs \\ reals)
  return reals ?>> do
    cache   <- packageCacheContents cachePath
    choices <- mapM (getDowngradeChoice ss cache) reals
    pacman ss $ ["-U"] ++ map (cachePath </>) choices
      where cachePath = cachePathOf ss
               
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
  cp (cachePath </> p) (dir </> p)
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
    | toSave == 0 = warn ss cleanCacheMsg2 >> pacman ss ["-Scc"]
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
             mapM_ rm filePaths  -- Error handling?
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
viewLogFile logFilePath = shellCmd "less" [logFilePath]

-- Very similar to `searchCache`. But is this worth generalizing?
searchLogFile :: Settings -> [String] -> IO ExitCode
searchLogFile settings input = do
  logFile <- lines `liftM` readFile (logFilePathOf settings)
  mapM_ putStrLn $ searchLines (unwords input) logFile
  returnSuccess

-- Are you failing at looking up anything,
-- or succeeding at looking up nothing?
logInfoOnPkg :: Settings -> [String] -> IO ExitCode
logInfoOnPkg _ []          = returnFailure  -- Success?
logInfoOnPkg settings pkgs = do
  logFile <- readFile (logFilePathOf settings)
  let inLog p = return (logFile =~ (" " ++ p ++ " "))
  reals <- filterM inLog pkgs
  reportNotInLog (langOf settings) (pkgs \\ reals)
  return reals ?>> do
    mapM_ (logLookUp settings logFile) reals
    returnSuccess

-- Make internal to `logInfoOnPkg`?
-- Assumed: The package to look up _exists_.
logLookUp :: Settings -> String -> String -> IO ()
logLookUp _ _ [] = return ()
logLookUp settings logFile pkg = do
  mapM_ putStrLn $ [ logLookUpMsg1 (langOf settings) pkg
                   , logLookUpMsg2 (langOf settings) installDate
                   , logLookUpMsg3 (langOf settings) upgrades
                   , logLookUpMsg4 (langOf settings) ] ++ recentStuff ++ [""]
      where matches     = searchLines (" " ++ pkg ++ " \\(") $ lines logFile
            installDate = head matches =~ "\\[[-:0-9 ]+\\]"
            upgrades    = length $ searchLines " upgraded " matches
            recentStuff = map ((:) ' ') $ takeLast 5 matches
            takeLast n  = reverse . take n . reverse

-------------------
-- WORKING WITH `O`
-------------------
displayOrphans :: Settings -> [String] -> IO ExitCode
displayOrphans _ []    = getOrphans >>= mapM_ putStrLn >> returnSuccess
displayOrphans ss pkgs = adoptPkg ss pkgs

adoptPkg :: Settings -> [String] -> IO ExitCode
adoptPkg ss pkgs = ss |$| (pacman ss $ ["-D","--asexplicit"] ++ pkgs)

----------
-- REPORTS
----------
badReport :: (Language -> String) -> Language -> [String] -> IO ()
badReport m lang pkgs = return pkgs ?>> printList red cyan (m lang) pkgs

reportNonPackages :: Language -> [String] -> IO ()
reportNonPackages lang nons = badReport reportNonPackagesMsg1 lang nons 

reportIgnoredPackages :: Language -> [String] -> IO ()
reportIgnoredPackages lang pkgs = printList yellow cyan msg pkgs
    where msg = reportIgnoredPackagesMsg1 lang

reportPkgsToInstall :: Language -> [String] -> [AURPkg] -> [AURPkg] -> IO ()
reportPkgsToInstall lang pacPkgs aurDeps aurPkgs = do
  printIfThere (sort pacPkgs) $ reportPkgsToInstallMsg1 lang
  printIfThere (sort $ namesOf aurDeps) $ reportPkgsToInstallMsg2 lang
  printIfThere (sort $ namesOf aurPkgs) $ reportPkgsToInstallMsg3 lang
      where namesOf = map pkgNameOf
            printIfThere ps m = unless (null ps) $ printList green cyan m ps

reportPkgsToUpgrade :: Language -> [String] -> IO ()
reportPkgsToUpgrade lang pkgs = printList green cyan msg pkgs
    where msg = reportPkgsToUpgradeMsg1 lang

reportBadDowngradePkgs :: Language -> [String] -> IO ()
reportBadDowngradePkgs lang ps = badReport reportBadDowngradePkgsMsg1 lang ps

reportNotInLog :: Language -> [String] -> IO ()
reportNotInLog lang nons = badReport reportNotInLogMsg1 lang nons

--------
-- OTHER
--------
viewConfFile :: IO ExitCode
viewConfFile = shellCmd "less" [pacmanConfFile]

displayOutputLanguages :: Settings -> IO ExitCode
displayOutputLanguages settings = do
  notify settings displayOutputLanguagesMsg1
  mapM_ (putStrLn . show) allLanguages
  returnSuccess

printHelpMsg :: Settings -> [String] -> IO ExitCode
printHelpMsg settings [] = do
  pacmanHelp <- getPacmanHelpMsg
  putStrLn $ getHelpMsg settings pacmanHelp
  returnSuccess
printHelpMsg settings pacOpts = pacman settings $ pacOpts ++ ["-h"]

getHelpMsg :: Settings -> [String] -> String
getHelpMsg settings pacmanHelpMsg = concat $ intersperse "\n" allMessages
    where lang = langOf settings
          allMessages   = [replacedLines, auraOperMsg lang, manpageMsg lang]
          replacedLines = unlines $ map (replaceByPatt patterns) pacmanHelpMsg
          colouredMsg   = yellow $ inheritedOperTitle lang
          patterns      = [("pacman","aura"), ("operations",colouredMsg)]

-- ANIMATED VERSION MESSAGE
animateVersionMsg :: Settings -> [String] -> IO ExitCode
animateVersionMsg settings verMsg = do
  hideCursor
  mapM_ putStrLn $ map (padString verMsgPad) verMsg  -- Version message
  putStr $ raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  mapM_ putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
  putStr $ raiseCursorBy 4
  takeABite 0
  mapM_ pillEating pillsAndWidths
  putStr clearGrid
  putStrLn auraLogo
  putStrLn $ "AURA Version " ++ auraVersion
  putStrLn " by Colin Woodbury\n"
  mapM_ putStrLn . translatorMsg . langOf $ settings
  showCursor
  returnSuccess
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]
