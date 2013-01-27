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

import System.Environment (getArgs)
import Control.Monad      (unless, liftM)
import System.Exit        (exitSuccess, exitFailure)
import Data.List          (nub, sort, intercalate)

import Aura.Colour.Text (yellow)
import Aura.Shell       (shellCmd)
import Aura.State       (restoreState, saveState)
import Aura.Settings.Enable
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.General
import Aura.Pacman
import Aura.Flags
import Aura.Utils
import Aura.Logo

import Utilities (replaceByPatt, tripleFst)
import Shell hiding (shellCmd)

import qualified Aura.Commands.A as A
import qualified Aura.Commands.C as C
import qualified Aura.Commands.L as L
import qualified Aura.Commands.O as O

---

type UserInput = ([Flag],[String],[String])

auraVersion :: String
auraVersion = "1.1.2.0"

main :: IO a
main = getArgs >>= prepSettings . processFlags >>= execute >>= exit

processFlags :: [String] -> (UserInput,Language)
processFlags args = ((flags,nub input,pacOpts'),language)
    where (language,rest) = parseLanguageFlag args
          (flags,input,pacOpts) = parseFlags language rest
          pacOpts' = nub $ pacOpts ++ reconvertFlags flags dualFlagMap

-- | Set the local environment.
prepSettings :: (UserInput,Language) -> IO (UserInput,Settings)
prepSettings (ui,lang) = (,) ui `liftM` getSettings lang (tripleFst ui)

-- | Hand user input to the Aura Monad and run it.
execute :: (UserInput,Settings) -> IO (Either AuraError ())
execute ((flags,input,pacOpts),ss) = do
  let flags' = filterSettingsFlags flags
  unless (Debug `notElem` flags) $ debugOutput ss
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
          []             -> trueRoot (sudo $ A.installPackages pacOpts input)
          [Upgrade]      -> trueRoot (sudo $ A.upgradeAURPkgs pacOpts input)
          [Info]         -> A.aurPkgInfo input
          [Search]       -> A.aurSearch input
          [ViewDeps]     -> A.displayPkgDeps input
          [Download]     -> A.downloadTarballs input
          [GetPkgbuild]  -> A.displayPkgbuild input
          (Refresh:fs')  -> sudo $ syncAndContinue (fs',input,pacOpts)
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
          []       -> ask >>= L.viewLogFile . logFilePathOf
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
    pacmanFlags    -> catch (pacman $ pacOpts ++ input ++ hijackedFlags)
                      pacmanFailure
    where hijackedFlags = reconvertFlags flags hijackedFlagMap

-- `-y` was included in the flags. Sync database before continuing.
syncAndContinue :: UserInput -> Aura ()
syncAndContinue (flags,input,pacOpts) = do
  syncDatabase pacOpts
  executeOpts (AURInstall:flags,input,pacOpts)

----------
-- GENERAL
----------
viewConfFile :: Aura ()
viewConfFile = shellCmd "less" [pacmanConfFile]

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  notify displayOutputLanguagesMsg1
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
