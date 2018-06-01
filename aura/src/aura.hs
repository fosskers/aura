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
import           Aura.Errors
import           Aura.Flags
import           Aura.Languages
import           Aura.Logo
import           Aura.Monad.Aura
import           Aura.Pacman
import           Aura.Settings.Base
import           Aura.Settings.Enable
import           BasePrelude hiding (Version)
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
  first (($ langOf ss) . _failure) <$> runAura (executeOpts (flags', input, pacOpts)) ss

exit :: Either String () -> IO a
exit (Left e)  = scold e *> exitFailure
exit (Right _) = exitSuccess

-- | After determining what Flag was given, dispatches a function.
-- The `flags` must be sorted to guarantee the pattern matching
-- below will work properly.
-- If no matches on Aura flags are found, the flags are assumed to be
-- pacman's.
executeOpts :: UserInput -> Aura (Either Failure ())
executeOpts ([], _, []) = executeOpts ([Help], [], [])
executeOpts (flags, input, pacOpts) =
  case sort flags of
    (AURInstall:fs) ->
        case fs of
          []             -> fmap (join . join) . trueRoot . sudo $ A.install (map T.pack pacOpts) (map T.pack input)
          [Upgrade]      -> fmap (join . join) . trueRoot . sudo $ A.upgradeAURPkgs (map T.pack pacOpts) (map T.pack input)
          [Info]         -> Right <$> A.aurPkgInfo input
          [Search]       -> Right <$> A.aurPkgSearch (map T.pack input)
          [ViewDeps]     -> A.displayPkgDeps (map T.pack input)
          [Download]     -> Right <$> A.downloadTarballs (map T.pack input)
          [GetPkgbuild]  -> Right <$> A.displayPkgbuild input
          (Refresh:fs')  -> fmap join . sudo $ syncAndContinue (fs', input, pacOpts)
          _              -> pure $ failure executeOpts_1
    (SaveState:fs) ->
        case fs of
          []             -> sudo B.saveState
          [Clean]        -> fmap join . sudo $ B.cleanStates input
          [RestoreState] -> join <$> sudo B.restoreState
          _              -> pure $ failure executeOpts_1
    (Cache:fs) ->
        case fs of
          []             -> fmap join . sudo $ C.downgradePackages (map T.pack pacOpts) (map T.pack input)
          [Clean]        -> fmap join . sudo $ C.cleanCache input
          [Clean, Clean] -> join <$> sudo C.cleanNotSaved
          [Search]       -> fmap Right . C.searchCache $ map T.pack input
          [CacheBackup]  -> fmap join . sudo $ C.backupCache (map (fromText . T.pack) input)
          _              -> pure $ failure executeOpts_1
    (LogFile:fs) ->
        case fs of
          []       -> ask >>= fmap Right . L.viewLogFile . logFilePathOf
          [Search] -> ask >>= fmap Right . liftIO . flip L.searchLogFile input
          [Info]   -> Right <$> L.logInfoOnPkg input
          _        -> pure $ failure executeOpts_1
    (Orphans:fs) ->
        case fs of
          []        -> O.displayOrphans (map T.pack input)
          [Abandon] -> fmap join . sudo $ orphans >>= flip removePkgs (map T.pack pacOpts)
          _         -> pure $ failure executeOpts_1
    [ViewConf]  -> Right <$> viewConfFile
    [Languages] -> Right <$> displayOutputLanguages
    [Help]      -> printHelpMsg $ map T.pack pacOpts
    [Version]   -> getVersionInfo >>= fmap Right . animateVersionMsg . map T.unpack
    _ -> pacman $ map T.pack pacOpts <> map T.pack hijackedFlags <> map T.pack input
    where hijackedFlags = reconvertFlags hijackedFlagMap flags

-- | `-y` was included with `-A`. Sync database before continuing.
syncAndContinue :: UserInput -> Aura (Either Failure ())
syncAndContinue (flags, input, pacOpts) = do
  syncDatabase (map T.pack pacOpts)
  executeOpts (AURInstall:flags, input, pacOpts)

----------
-- GENERAL
----------
viewConfFile :: Aura ()
viewConfFile = shelly $ run_ "less" [pacmanConfFile]

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  asks langOf >>= notify . displayOutputLanguages_1
  liftIO $ traverse_ print allLanguages

printHelpMsg :: [T.Text] -> Aura (Either Failure ())
printHelpMsg [] = do
  ss <- ask
  pacmanHelp <- getPacmanHelpMsg
  fmap Right . liftIO . putStrLn . getHelpMsg ss . map T.unpack $ pacmanHelp
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
