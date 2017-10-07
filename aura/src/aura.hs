{-

Copyright 2012 - 2017 Colin Woodbury <colingw@gmail.com>

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

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (nub, sort, intercalate)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)

import Aura.Colour.Text (yellow)
import Aura.Core
import Aura.Flags
import Aura.Languages
import Aura.Logo
import Aura.Monad.Aura
import Aura.Pacman
import Aura.Settings.Base
import Aura.Settings.Enable
import Aura.Shell (shellCmd)
import Aura.Utils

import Shell (showCursor, hideCursor)
import Utilities (replaceByPatt)

import Aura.Commands.A as A
import Aura.Commands.B as B
import Aura.Commands.C as C
import Aura.Commands.L as L
import Aura.Commands.O as O

---

type UserInput = ([Flag], [String], [String])

auraVersion :: String
auraVersion = "1.3.9"

main :: IO a
main = getArgs >>= prepSettings . processFlags >>= execute >>= exit

processFlags :: [String] -> (UserInput, Maybe Language)
processFlags args = ((flags, nub input, pacOpts'), language)
    where (language, _) = parseLanguageFlag args
          (flags, input, pacOpts) = parseFlags language args
          pacOpts' = nub $ pacOpts <> reconvertFlags dualFlagMap flags
                   <> reconvertFlags pacmanFlagMap flags

-- | Set the local environment.
prepSettings :: (UserInput, Maybe Language) -> IO (UserInput, Settings)
prepSettings (ui, lang) = (,) ui <$> getSettings lang ui

-- | Hand user input to the Aura Monad and run it.
execute :: (UserInput, Settings) -> IO (Either String ())
execute ((flags, input, pacOpts), ss) = do
  let flags' = filter notSettingsFlag flags
  when (Debug `elem` flags) $ debugOutput ss
  runAura (executeOpts (flags', input, pacOpts)) ss

exit :: Either String () -> IO a
exit (Left e)  = putStrLn e *> exitFailure
exit (Right _) = exitSuccess

-- | After determining what Flag was given, dispatches a function.
-- The `flags` must be sorted to guarantee the pattern matching
-- below will work properly.
-- If no matches on Aura flags are found, the flags are assumed to be
-- pacman's.
executeOpts :: UserInput -> Aura ()
executeOpts ([], _, []) = executeOpts ([Help], [], [])
executeOpts (flags, input, pacOpts) =
  case sort flags of
    (AURInstall:fs) ->
        case fs of
          []             -> trueRoot (sudo $ A.install pacOpts input)
          [Upgrade]      -> trueRoot (sudo $ A.upgradeAURPkgs pacOpts input)
          [Info]         -> A.aurPkgInfo input
          [Search]       -> A.aurPkgSearch input
          [ViewDeps]     -> A.displayPkgDeps input
          [Download]     -> A.downloadTarballs input
          [GetPkgbuild]  -> A.displayPkgbuild input
          (Refresh:fs')  -> sudo $ syncAndContinue (fs', input, pacOpts)
          _              -> scoldAndFail executeOpts_1
    (SaveState:fs) ->
        case fs of
          []             -> sudo B.saveState
          [Clean]        -> sudo $ B.cleanStates input
          [RestoreState] -> sudo B.restoreState
          _              -> scoldAndFail executeOpts_1
    (Cache:fs) ->
        case fs of
          []             -> sudo $ C.downgradePackages pacOpts input
          [Clean]        -> sudo $ C.cleanCache input
          [Clean, Clean]  -> sudo C.cleanNotSaved
          [Search]       -> C.searchCache input
          [CacheBackup]  -> sudo $ C.backupCache input
          _              -> scoldAndFail executeOpts_1
    (LogFile:fs) ->
        case fs of
          []       -> ask >>= L.viewLogFile . logFilePathOf
          [Search] -> L.searchLogFile input
          [Info]   -> L.logInfoOnPkg input
          _        -> scoldAndFail executeOpts_1
    (Orphans:fs) ->
        case fs of
          []        -> O.displayOrphans input
          [Abandon] -> sudo $ orphans >>= flip removePkgs pacOpts
          _         -> scoldAndFail executeOpts_1
    [ViewConf]  -> viewConfFile
    [Languages] -> displayOutputLanguages
    [Help]      -> printHelpMsg pacOpts
    [Version]   -> getVersionInfo >>= animateVersionMsg
    _ -> catch (pacman $ pacOpts <> hijackedFlags <> input) pacmanFailure
    where hijackedFlags = reconvertFlags hijackedFlagMap flags

-- | `-y` was included with `-A`. Sync database before continuing.
syncAndContinue :: UserInput -> Aura ()
syncAndContinue (flags, input, pacOpts) =
  syncDatabase pacOpts *> executeOpts (AURInstall:flags, input, pacOpts)

----------
-- GENERAL
----------
viewConfFile :: Aura ()
viewConfFile = shellCmd "less" [pacmanConfFile]

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  notify displayOutputLanguages_1
  liftIO $ traverse_ print allLanguages

printHelpMsg :: [String] -> Aura ()
printHelpMsg [] = ask >>= \ss -> do
  pacmanHelp <- getPacmanHelpMsg
  liftIO . putStrLn . getHelpMsg ss $ pacmanHelp
printHelpMsg pacOpts = pacman $ pacOpts <> ["-h"]

getHelpMsg :: Settings -> [String] -> String
getHelpMsg settings pacmanHelpMsg = intercalate "\n" allMessages
    where lang = langOf settings
          allMessages   = [replacedLines, auraOperMsg lang, manpageMsg lang]
          replacedLines = unlines (replaceByPatt patterns <$> pacmanHelpMsg)
          colouredMsg   = yellow $ inheritedOperTitle lang
          patterns      = [("pacman", "aura"), ("operations", colouredMsg)]

-- | Animated version message.
animateVersionMsg :: [String] -> Aura ()
animateVersionMsg verMsg = ask >>= \ss -> liftIO $ do
  hideCursor
  traverse_ (putStrLn . padString verMsgPad) verMsg  -- Version message
  raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  traverse_ putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
  raiseCursorBy 4
  takeABite 0  -- Initial bite animation.
  traverse_ pillEating pillsAndWidths
  clearGrid
  version ss
  showCursor
    where pillEating (p, w) = clearGrid *> drawPills p *> takeABite w
          pillsAndWidths   = [(2, 5), (1, 10), (0, 15)]

version :: Settings -> IO ()
version ss = do
  putStrLn auraLogo
  putStrLn $ "AURA Version " <> auraVersion
  putStrLn " by Colin Woodbury\n"
  traverse_ putStrLn . translatorMsg . langOf $ ss
