{-# OPTIONS_GHC -O2 #-}

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

import Data.List (intersperse, (\\), nub, sort)
import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs)
import Control.Monad (unless)

import Aura.State (restoreState, saveState)
import Aura.Colour.TextColouring (yellow)
import Aura.Shell (shellCmd)
import Aura.Settings.Enable
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.General
import Aura.Pacman
import Aura.Flags
import Aura.Logo

import Utilities (replaceByPatt)
import Shell hiding (shellCmd)

import qualified Aura.Commands.A as A
import qualified Aura.Commands.C as C
import qualified Aura.Commands.L as L
import qualified Aura.Commands.O as O

---

auraVersion :: String
auraVersion = "1.1.0.0"

main :: IO a
main = do
  args <- getArgs
  let (language,rest) = parseLanguageFlag args
      (auraFlags,input,pacOpts) = parseFlags language rest
      auraFlags' = filter (`notElem` settingsFlags) auraFlags
      pacOpts'   = pacOpts ++ reconvertFlags auraFlags dualFlagMap
  settings <- getSettings language auraFlags
  unless (Debug `notElem` auraFlags) $ debugOutput settings
  result <- runAura (executeOpts (auraFlags',nub input,nub pacOpts')) settings
  case result of
    Left e  -> putStrLn (getErrorMsg e) >> (exitWith $ ExitFailure 1)
    Right _ -> exitWith ExitSuccess

-- After determining what Flag was given, dispatches a function.
-- The `flags` must be sorted to guarantee the pattern matching
-- below will work properly.
executeOpts :: ([Flag],[String],[String]) -> Aura ()
executeOpts ([],[],[]) = executeOpts ([Help],[],[])
executeOpts (flags,input,pacOpts) = ask >>= \ss -> do
  case sort flags of
    (AURInstall:fs) ->
        case fs of
          []             -> trueRoot (sudo $ A.installPackages pacOpts input)
          [Upgrade]      -> trueRoot (sudo $ A.upgradeAURPkgs pacOpts input)
          [Info]         -> A.aurPkgInfo input
          [Search]       -> A.aurSearch input
          [ViewDeps]     -> A.displayPkgDeps input
          [Download]     -> A.downloadTarballs input
          [GetPkgbuild]  -> A.displayPkgbuild input
          (Refresh:fs')  -> sudo $ syncAndContinue (fs',input,pacOpts)
          (DelMDeps:fs') -> sudo $ removeMakeDeps (fs',input,pacOpts)
          badFlags       -> scoldAndFail executeOptsMsg1
    (Cache:fs) ->
        case fs of
          []       -> sudo $ C.downgradePackages input
          [Clean]  -> sudo $ C.cleanCache input
          [Search] -> C.searchCache input
          [Backup] -> sudo $ C.backupCache input
          badFlags -> scoldAndFail executeOptsMsg1
    (LogFile:fs) ->
        case fs of
          []       -> L.viewLogFile $ logFilePathOf ss
          [Search] -> L.searchLogFile input
          [Info]   -> L.logInfoOnPkg input
          badFlags -> scoldAndFail executeOptsMsg1
    (Orphans:fs) ->
        case fs of
          []        -> O.displayOrphans input
          [Abandon] -> sudo $ getOrphans >>= flip removePkgs pacOpts
          badFlags  -> scoldAndFail executeOptsMsg1
    [SaveState]    -> sudo saveState
    [RestoreState] -> sudo restoreState
    [ViewConf]     -> viewConfFile
    [Languages]    -> displayOutputLanguages
    [Help]         -> printHelpMsg pacOpts
    [Version]      -> getVersionInfo >>= animateVersionMsg
    pacmanFlags    -> pacman $ pacOpts ++ input ++ hijackedFlags
    where hijackedFlags = reconvertFlags flags hijackedFlagMap

-- This two functions contain evil, and must be in `aura.hs` to work.
syncAndContinue :: ([Flag],[String],[String]) -> Aura ()
syncAndContinue (flags,input,pacOpts) = do
  syncDatabase pacOpts
  executeOpts (AURInstall:flags,input,pacOpts)  -- This is Evil.

removeMakeDeps :: ([Flag],[String],[String]) -> Aura ()
removeMakeDeps (flags,input,pacOpts) = do
  orphansBefore <- getOrphans
  executeOpts (AURInstall:flags,input,pacOpts)
  orphansAfter <- getOrphans
  let makeDeps = orphansAfter \\ orphansBefore
  unless (null makeDeps) $ notify removeMakeDepsAfterMsg1
  removePkgs makeDeps pacOpts

----------
-- GENERAL
----------
viewConfFile :: Aura ()
viewConfFile = shellCmd "less" [pacmanConfFile]

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  notify displayOutputLanguagesMsg1
  liftIO $ mapM_ (putStrLn . show) allLanguages

printHelpMsg :: [String] -> Aura ()
printHelpMsg [] = ask >>= \ss -> do
  pacmanHelp <- getPacmanHelpMsg
  liftIO . putStrLn . getHelpMsg ss $ pacmanHelp
printHelpMsg pacOpts = pacman $ pacOpts ++ ["-h"]

getHelpMsg :: Settings -> [String] -> String
getHelpMsg settings pacmanHelpMsg = concat $ intersperse "\n" allMessages
    where lang = langOf settings
          allMessages   = [replacedLines, auraOperMsg lang, manpageMsg lang]
          replacedLines = unlines $ map (replaceByPatt patterns) pacmanHelpMsg
          colouredMsg   = yellow $ inheritedOperTitle lang
          patterns      = [("pacman","aura"), ("operations",colouredMsg)]

-- ANIMATED VERSION MESSAGE
animateVersionMsg :: [String] -> Aura ()
animateVersionMsg verMsg = ask >>= \ss -> liftIO $ do
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
  mapM_ putStrLn . translatorMsg . langOf $ ss
  showCursor
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]
