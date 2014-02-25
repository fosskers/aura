{-# LANGUAGE TupleSections #-}

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

This file is part of Aura.

Aura is free s

             oftwar
        e:youcanredist
     ributeitand/ormodify
    itunderthetermsoftheGN
   UGeneralPublicLicenseasp
  ublishedbytheFreeSoftw
 areFoundation,either     ver        sio        n3o        fth
 eLicense,or(atyou       ropti      on)an      ylate      rvers
ion.Auraisdistr         ibutedi    nthehop    ethatit    willbeu
 seful,butWITHOUTA       NYWAR      RANTY      ;with      outev
 entheimpliedwarranty     ofM        ERC        HAN        TAB
  ILITYorFITNESSFORAPART
   ICULARPURPOSE.SeetheGNUG
    eneralPublicLicensefor
     moredetails.Youshoul
        dhavereceiveda
             copyof

the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

import System.Environment (getArgs)
import Control.Monad      (when)
import System.Exit        (exitSuccess, exitFailure)
import Data.List          (nub, sort, intercalate)

import Aura.Colour.Text (yellow)
import Aura.Shell       (shellCmd)
import Aura.Settings.Enable
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Pacman
import Aura.Flags
import Aura.Utils
import Aura.Core
import Aura.Logo

import Utilities (replaceByPatt)
import Shell     (showCursor, hideCursor)

import Aura.Commands.A as A
import Aura.Commands.B as B
import Aura.Commands.C as C
import Aura.Commands.L as L
import Aura.Commands.M as M
import Aura.Commands.O as O

---

type UserInput = ([Flag],[String],[String])

auraVersion :: String
auraVersion = "1.2.3.1"

main :: IO a
main = getArgs >>= prepSettings . processFlags >>= execute >>= exit

processFlags :: [String] -> (UserInput,Maybe Language)
processFlags args = ((flags,nub input,pacOpts'),language)
    where (language,rest) = parseLanguageFlag args
          (flags,input,pacOpts) = parseFlags language rest
          pacOpts' = nub $ pacOpts ++ reconvertFlags flags dualFlagMap

-- | Set the local environment.
prepSettings :: (UserInput,Maybe Language) -> IO (UserInput,Settings)
prepSettings (ui,lang) = (ui,) `fmap` getSettings lang ui

-- | Hand user input to the Aura Monad and run it.
execute :: (UserInput,Settings) -> IO (Either AuraError ())
execute ((flags,input,pacOpts),ss) = do
  let flags' = filter notSettingsFlag flags
  when (Debug `elem` flags) $ debugOutput ss
  runAura (executeOpts (flags',input,pacOpts)) ss

exit :: Either AuraError () -> IO a
exit (Left e)  = putStrLn (getErrorMsg e) >> exitFailure
exit (Right _) = exitSuccess

-- | After determining what Flag was given, dispatches a function.
-- The `flags` must be sorted to guarantee the pattern matching
-- below will work properly.
-- If no matches on Aura flags are found, the flags are assumed to be
-- pacman's.
executeOpts :: UserInput -> Aura ()
executeOpts ([],_,[]) = executeOpts ([Help],[],[])
executeOpts (flags,input,pacOpts) =
  case sort flags of
    (AURInstall:fs) ->
        case fs of
          []             -> trueRoot (sudo $ A.install pacOpts input)
          [Upgrade]      -> trueRoot (sudo $ A.upgradeAURPkgs pacOpts input)
          [Info]         -> A.aurPkgInfo input
          [Search]       -> A.aurSearch input
          [ViewDeps]     -> A.displayPkgDeps input
          [Download]     -> A.downloadTarballs input
          [GetPkgbuild]  -> A.displayPkgbuild input
          (Refresh:fs')  -> sudo $ syncAndContinue (fs',input,pacOpts)
          badFlags       -> scoldAndFail executeOpts_1
    (ABSInstall:fs) ->
        case fs of
          []             -> trueRoot (sudo $ M.install pacOpts input)
          [TreeSync]     -> sudo $ M.addToTree input
          [Search]       -> M.absSearch input
          [Info]         -> M.absInfo input
          [Clean]        -> sudo M.cleanABSTree
          [ViewDeps]     -> M.displayPkgDeps input
          [GetPkgbuild]  -> M.displayPkgbuild input
          (Refresh:fs')  -> sudo $ syncABSAndContinue (fs',input,pacOpts)
          badFlags       -> scoldAndFail executeOpts_1
    (SaveState:fs) ->
        case fs of
          []             -> sudo B.saveState
          [Clean]        -> sudo $ B.cleanStates input
          [RestoreState] -> sudo B.restoreState
          badFlags       -> scoldAndFail executeOpts_1
    (Cache:fs) ->
        case fs of
          []             -> sudo $ C.downgradePackages pacOpts input
          [Clean]        -> sudo $ C.cleanCache input
          [Clean,Clean]  -> sudo C.cleanNotSaved
          [Search]       -> C.searchCache input
          [CacheBackup]  -> sudo $ C.backupCache input
          badFlags       -> scoldAndFail executeOpts_1
    (LogFile:fs) ->
        case fs of
          []       -> ask >>= L.viewLogFile . logFilePathOf
          [Search] -> L.searchLogFile input
          [Info]   -> L.logInfoOnPkg input
          badFlags -> scoldAndFail executeOpts_1
    (Orphans:fs) ->
        case fs of
          []        -> O.displayOrphans input
          [Abandon] -> sudo $ orphans >>= flip removePkgs pacOpts
          badFlags  -> scoldAndFail executeOpts_1
    [ViewConf]  -> viewConfFile
    [Languages] -> displayOutputLanguages
    [Help]      -> printHelpMsg pacOpts
    [Version]   -> getVersionInfo >>= animateVersionMsg
    pacmanFlags -> catch (pacman $ pacOpts ++ hijackedFlags ++ input)
                      pacmanFailure
    where hijackedFlags = reconvertFlags flags hijackedFlagMap

-- | `-y` was included with `-A`. Sync database before continuing.
syncAndContinue :: UserInput -> Aura ()
syncAndContinue (flags,input,pacOpts) =
  syncDatabase pacOpts >> executeOpts (AURInstall:flags,input,pacOpts)

-- | `-y` was included with `-M`. Sync local ABS tree before continuing.
syncABSAndContinue :: UserInput -> Aura ()
syncABSAndContinue (flags,input,pacOpts) =
  M.absSync >> executeOpts (ABSInstall:flags,input,pacOpts)

----------
-- GENERAL
----------
viewConfFile :: Aura ()
viewConfFile = shellCmd "less" [pacmanConfFile]

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  notify displayOutputLanguages_1
  liftIO $ mapM_ print allLanguages

printHelpMsg :: [String] -> Aura ()
printHelpMsg [] = ask >>= \ss -> do
  pacmanHelp <- getPacmanHelpMsg
  liftIO . putStrLn . getHelpMsg ss $ pacmanHelp
printHelpMsg pacOpts = pacman $ pacOpts ++ ["-h"]

getHelpMsg :: Settings -> [String] -> String
getHelpMsg settings pacmanHelpMsg = intercalate "\n" allMessages
    where lang = langOf settings
          allMessages   = [replacedLines, auraOperMsg lang, manpageMsg lang]
          replacedLines = unlines $ map (replaceByPatt patterns) pacmanHelpMsg
          colouredMsg   = yellow $ inheritedOperTitle lang
          patterns      = [("pacman","aura"), ("operations",colouredMsg)]

-- | Animated version message.
animateVersionMsg :: [String] -> Aura ()
animateVersionMsg verMsg = ask >>= \ss -> liftIO $ do
  hideCursor
  mapM_ (putStrLn . padString verMsgPad) verMsg  -- Version message
  raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  mapM_ putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
  raiseCursorBy 4
  takeABite 0  -- Initial bite animation.
  mapM_ pillEating pillsAndWidths
  clearGrid
  version ss
  showCursor
    where pillEating (p,w) = clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]

version :: Settings -> IO ()
version ss = do
  putStrLn auraLogo
  putStrLn $ "AURA Version " ++ auraVersion
  putStrLn " by Colin Woodbury\n"
  mapM_ putStrLn . translatorMsg . langOf $ ss
