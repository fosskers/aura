{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

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

import           Aura.Colour.Text (yellow)
import           Aura.Commands.A as A
import           Aura.Commands.B as B
import           Aura.Commands.C as C
import           Aura.Commands.L as L
import           Aura.Commands.O as O
import           Aura.Core
import           Aura.Flags
import           Aura.Languages
import           Aura.Logo
import           Aura.Monad.Aura
import           Aura.Pacman
import           Aura.Settings.Base
import           Aura.Settings.Enable
import           Aura.Utils
import           BasePrelude hiding (Version, catch)
import qualified Data.Text as T
import           Shelly (fromText, shelly, run_)
import           Utilities

---

type UserInput = ([Flag], [String], [String])

auraVersion :: String
auraVersion = "1.4.0"

main :: IO ()
main = do
  mus  <- getArgs >>= prepSettings . processFlags
  case mus of
    Nothing   -> putStrLn "There was a problem initializing the runtime environment."
    Just args -> execute args >>= exit

processFlags :: [String] -> (UserInput, Maybe Language)
processFlags args = ((flags, nub input, pacOpts'), language)
    where (language, _) = parseLanguageFlag args
          (flags, input, pacOpts) = parseFlags language args
          pacOpts' = nub $ pacOpts <> reconvertFlags dualFlagMap flags
                   <> reconvertFlags pacmanFlagMap flags

-- | Set the local environment.
prepSettings :: (UserInput, Maybe Language) -> IO (Maybe (UserInput, Settings))
prepSettings (ui, lang) = fmap (ui,) <$> getSettings lang ui

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
          []             -> trueRoot (sudo $ A.install (map T.pack pacOpts) (map T.pack input))
          [Upgrade]      -> trueRoot (sudo $ A.upgradeAURPkgs (map T.pack pacOpts) (map T.pack input))
          [Info]         -> A.aurPkgInfo input
          [Search]       -> A.aurPkgSearch (map T.pack input)
          [ViewDeps]     -> A.displayPkgDeps (map T.pack input)
          [Download]     -> A.downloadTarballs (map T.pack input)
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
          []             -> sudo $ C.downgradePackages (map T.pack pacOpts) (map T.pack input)
          [Clean]        -> sudo $ C.cleanCache input
          [Clean, Clean]  -> sudo C.cleanNotSaved
          [Search]       -> C.searchCache (map T.pack input)
          [CacheBackup]  -> sudo $ C.backupCache (map (fromText . T.pack) input)
          _              -> scoldAndFail executeOpts_1
    (LogFile:fs) ->
        case fs of
          []       -> ask >>= L.viewLogFile . logFilePathOf
          [Search] -> ask >>= liftIO . flip L.searchLogFile input
          [Info]   -> L.logInfoOnPkg input
          _        -> scoldAndFail executeOpts_1
    (Orphans:fs) ->
        case fs of
          []        -> O.displayOrphans (map T.pack input)
          [Abandon] -> sudo $ orphans >>= flip removePkgs (map T.pack pacOpts)
          _         -> scoldAndFail executeOpts_1
    [ViewConf]  -> viewConfFile
    [Languages] -> displayOutputLanguages
    [Help]      -> printHelpMsg (map T.pack pacOpts)
    [Version]   -> getVersionInfo >>= animateVersionMsg . map T.unpack
    _ -> catch (pacman $ (map T.pack pacOpts) <> (map T.pack hijackedFlags) <> (map T.pack input)) pacmanFailure
    where hijackedFlags = reconvertFlags hijackedFlagMap flags

-- | `-y` was included with `-A`. Sync database before continuing.
syncAndContinue :: UserInput -> Aura ()
syncAndContinue (flags, input, pacOpts) =
  syncDatabase (map T.pack pacOpts) *> executeOpts (AURInstall:flags, input, pacOpts)

----------
-- GENERAL
----------
viewConfFile :: Aura ()
viewConfFile = shelly $ run_ "less" [pacmanConfFile]

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  asks langOf >>= notify . displayOutputLanguages_1
  liftIO $ traverse_ print allLanguages

printHelpMsg :: [T.Text] -> Aura ()
printHelpMsg [] = do
  ss <- ask
  pacmanHelp <- getPacmanHelpMsg
  liftIO . putStrLn . getHelpMsg ss . map T.unpack $ pacmanHelp
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
