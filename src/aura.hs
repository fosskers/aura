{-

Copyright 2012, 2013, 2014, 2015, 2016 Colin Woodbury <colingw@gmail.com>

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

import BasicPrelude hiding (catch, liftIO)

import System.Exit        (exitSuccess, exitFailure)
import Data.Foldable      (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import qualified Shelly as S

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
import Aura.Shell     (showCursor, hideCursor)

import Aura.Commands.A as A
import Aura.Commands.B as B
import Aura.Commands.C as C
import Aura.Commands.L as L
import Aura.Commands.M as M
import Aura.Commands.O as O

---

type UserInput = ([Flag], [T.Text], [T.Text])

auraVersion :: T.Text
auraVersion = "1.4.0"

main :: IO a
main = getArgs >>= prepSettings . processFlags >>= execute >>= exit

processFlags :: [Text] -> (UserInput, Maybe Language)
processFlags args' = ((flags, nub input, pacOpts'), language)
    where args = map T.unpack args'
          (language, _) = parseLanguageFlag args
          (flags, input, pacOpts) = parseFlags language args
          pacOpts' = nub $ pacOpts <> reconvertFlags dualFlagMap flags
                   <> reconvertFlags pacmanFlagMap flags

-- | Set the local environment.
prepSettings :: (UserInput, Maybe Language) -> IO (UserInput, Settings)
prepSettings (ui, lang) =  (,) ui <$> S.shelly (getSettings lang ui)

-- | Hand user input to the Aura Monad and run it.
execute :: (UserInput, Settings) -> IO (Either T.Text ())
execute ((flags, input, pacOpts), ss) = do
  let flags' = filter notSettingsFlag flags
  when (Debug `elem` flags) $ debugOutput ss
  runAura (executeOpts (flags', input, pacOpts)) ss

exit :: Either T.Text () -> IO a
exit (Left e)  = IO.putStrLn e *> exitFailure
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
    (ABSInstall:fs) ->
        case fs of
          []             -> trueRoot (sudo $ M.install pacOpts input)
          [TreeSync]     -> sudo $ M.addToTree input
          [Search]       -> M.absSearch input
          [Info]         -> M.absInfo input
          [Clean]        -> sudo M.cleanABSTree
          [ViewDeps]     -> M.displayPkgDeps input
          [GetPkgbuild]  -> M.displayPkgbuild input
          (Refresh:fs')  -> sudo $ syncABSAndContinue (fs', input, pacOpts)
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
          [CacheBackup]  -> sudo $ C.backupCache $ map S.fromText input
          _              -> scoldAndFail executeOpts_1
    (LogFile:fs) ->
        case fs of
          []       -> ask >>= L.viewLogFile . S.toTextIgnore . logFilePathOf
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

-- | `-y` was included with `-M`. Sync local ABS tree before continuing.
syncABSAndContinue :: UserInput -> Aura ()
syncABSAndContinue (flags, input, pacOpts) =
  M.absSync *> executeOpts (ABSInstall:flags, input, pacOpts)

----------
-- GENERAL
----------
viewConfFile :: Aura ()
viewConfFile = shellCmd "less" [S.toTextIgnore pacmanConfFile]

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  notify displayOutputLanguages_1
  liftIO $ traverse_ print allLanguages

printHelpMsg :: [T.Text] -> Aura ()
printHelpMsg [] = ask >>= \ss -> do
  pacmanHelp <- getPacmanHelpMsg
  liftIO . IO.putStrLn . getHelpMsg ss $ pacmanHelp
printHelpMsg pacOpts = pacman $ pacOpts <> ["-h"]

getHelpMsg :: Settings -> [T.Text] -> T.Text
getHelpMsg settings pacmanHelpMsg = T.intercalate "\n" allMessages
    where lang = langOf settings
          allMessages   = [replacedLines, auraOperMsg lang, manpageMsg lang]
          replacedLines = T.unlines (replaceByPatt patterns <$> pacmanHelpMsg)
          colouredMsg   = yellow $ inheritedOperTitle lang
          patterns      = [("pacman", "aura"), ("operations", colouredMsg)]

-- | Animated version message.
animateVersionMsg :: [T.Text] -> Aura ()
animateVersionMsg verMsg = ask >>= \ss -> liftIO $ do
  hideCursor
  traverse_ (IO.putStrLn . padString verMsgPad) verMsg  -- Version message
  raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  traverse_ IO.putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
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
  IO.putStrLn auraLogo
  IO.putStrLn $ "AURA Version " <> auraVersion
  IO.putStrLn " by Colin Woodbury\n"
  traverse_ IO.putStrLn . translatorMsg . langOf $ ss
