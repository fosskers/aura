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

module Main ( main ) where

import           Aura.Commands.A as A
import           Aura.Commands.B as B
import           Aura.Commands.C as C
import           Aura.Commands.L as L
import           Aura.Commands.O as O
import           Aura.Core
import           Aura.Languages
import           Aura.Logo
import           Aura.Monad.Aura
import           Aura.Pacman
import           Aura.Settings
import           Aura.Types
import           BasePrelude hiding (Version)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Flags
import           Options.Applicative
import           Settings
import           Text.Pretty.Simple (pPrintNoColor)
import           Utilities

---

auraVersion :: T.Text
auraVersion = "1.5.0"

main :: IO ()
main = do
  options   <- execParser opts
  esettings <- getSettings options
  case esettings of
    Left err -> T.putStrLn . ($ English) $ _failure err
    Right ss -> execute ss options >>= exit

execute :: Settings -> Program -> IO (Either T.Text ())
execute ss p = first (($ langOf ss) . _failure) <$> runAura (executeOpts p) ss

exit :: Either T.Text () -> IO a
exit (Left e)  = scold e *> exitFailure
exit (Right _) = exitSuccess

executeOpts :: Program -> Aura (Either Failure ())
executeOpts (Program ops _ _ _) = do
  ss <- ask
  when (shared ss Debug) $ do
    pPrintNoColor ops
    pPrintNoColor (buildConfigOf ss)
    pPrintNoColor (commonConfigOf ss)
  case ops of
    Left o -> pacman $ asFlag o ++ asFlag (commonConfigOf ss)
    Right (AurSync o) ->
      case o of
        Right ps              -> fmap (join . join) . trueRoot . sudo $ A.install ps
        Left (AurDeps ps)     -> A.displayPkgDeps ps
        Left (AurInfo ps)     -> Right <$> A.aurPkgInfo ps
        Left (AurPkgbuild ps) -> Right <$> A.displayPkgbuild ps
        Left (AurSearch s)    -> Right <$> A.aurPkgSearch s
        Left (AurUpgrade ps)  -> fmap (join . join) . trueRoot . sudo $ A.upgradeAURPkgs ps
        Left (AurTarball ps)  -> Right <$> A.downloadTarballs ps
    Right (Backup o) ->
      case o of
        Nothing              -> sudo B.saveState
        Just (BackupClean n) -> fmap join . sudo $ B.cleanStates n
        Just BackupRestore   -> join <$> sudo B.restoreState
    Right (Cache o) ->
      case o of
        Right ps               -> fmap Right . liftIO $ traverse_ T.putStrLn ps
        Left (CacheSearch s)   -> Right <$> C.searchCache s
        Left (CacheClean n)    -> fmap join . sudo $ C.cleanCache n
        Left (CacheBackup pth) -> fmap join . sudo $ C.backupCache pth
    Right (Log o) ->
      case o of
        Nothing            -> Right <$> L.viewLogFile
        Just (LogInfo ps)  -> Right <$> L.logInfoOnPkg ps
        Just (LogSearch s) -> ask >>= fmap Right . liftIO . flip L.searchLogFile s
    Right (Orphans o) ->
      case o of
        Nothing               -> O.displayOrphans
        Just OrphanAbandon    -> fmap join . sudo $ orphans >>= removePkgs
        Just (OrphanAdopt ps) -> O.adoptPkg ps
    Right Version -> getVersionInfo >>= fmap Right . animateVersionMsg
    Right Languages -> Right <$> displayOutputLanguages

displayOutputLanguages :: Aura ()
displayOutputLanguages = do
  asks langOf >>= notify . displayOutputLanguages_1
  liftIO $ traverse_ print [English ..]

-- | Animated version message.
animateVersionMsg :: [T.Text] -> Aura ()
animateVersionMsg verMsg = ask >>= \ss -> liftIO $ do
  hideCursor
  traverse_ (T.putStrLn . padString verMsgPad) verMsg  -- Version message
  raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  traverse_ T.putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
  raiseCursorBy 4
  takeABite 0  -- Initial bite animation.
  traverse_ pillEating pillsAndWidths
  clearGrid
  T.putStrLn auraLogo
  T.putStrLn $ "AURA Version " <> auraVersion
  T.putStrLn " by Colin Woodbury\n"
  traverse_ T.putStrLn . translatorMsg . langOf $ ss
  showCursor
    where pillEating (p, w) = clearGrid *> drawPills p *> takeABite w
          pillsAndWidths   = [(2, 5), (1, 10), (0, 15)]
