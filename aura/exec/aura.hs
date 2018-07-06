{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleContexts, MonoLocalBinds, TypeApplications #-}

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

import           Aura.Colour (dtot)
import           Aura.Commands.A as A
import           Aura.Commands.B as B
import           Aura.Commands.C as C
import           Aura.Commands.L as L
import           Aura.Commands.O as O
import           Aura.Core
import           Aura.Languages
import           Aura.Logo
import           Aura.Pacman
import           Aura.Settings
import           Aura.Types
import           BasePrelude hiding (Version)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Flags
import           Options.Applicative (execParser)
import           Settings
import           Shelly (shelly, run_, toTextIgnore)
import           Text.Pretty.Simple (pPrintNoColor)
import           Utilities

---

auraVersion :: T.Text
auraVersion = "2.0.0"

main :: IO ()
main = do
  options   <- execParser opts
  esettings <- getSettings options
  case esettings of
    Left err -> T.putStrLn . dtot . ($ English) $ _failure err
    Right ss -> execute ss options >>= exit ss

execute :: Settings -> Program -> IO (Either (Doc AnsiStyle) ())
execute ss p = first (($ langOf ss) . _failure) <$> (runM . runReader ss . runError . executeOpts $ _operation p)

exit :: Settings -> Either (Doc AnsiStyle) () -> IO a
exit ss (Left e)  = scold ss e *> exitFailure
exit _  (Right _) = exitSuccess

executeOpts :: Either PacmanOp AuraOp -> Eff '[Error Failure, Reader Settings, IO] ()
executeOpts ops = do
  ss <- ask
  when (shared ss Debug) $ do
    pPrintNoColor ops
    pPrintNoColor (buildConfigOf ss)
    pPrintNoColor (commonConfigOf ss)
  case ops of
    Left o -> rethrow . pacman $ asFlag o ++ asFlag (commonConfigOf ss) ++ bool [] ["--quiet"] (switch ss LowVerbosity)
    Right (AurSync o) ->
      case o of
        Right ps              -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.install ps
        Left (AurDeps ps)     -> A.displayPkgDeps ps
        Left (AurInfo ps)     -> A.aurPkgInfo ps
        Left (AurPkgbuild ps) -> A.displayPkgbuild ps
        Left (AurSearch s)    -> A.aurPkgSearch s
        Left (AurUpgrade ps)  -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.upgradeAURPkgs ps
        Left (AurTarball ps)  -> A.downloadTarballs ps
        Left (AurJson ps)     -> A.aurJson ps
    Right (Backup o) ->
      case o of
        Nothing              -> sudo B.saveState
        Just (BackupClean n) -> sudo $ B.cleanStates n
        Just BackupRestore   -> sudo B.restoreState
        Just BackupList      -> B.listStates
    Right (Cache o) ->
      case o of
        Right ps                -> sudo $ C.downgradePackages ps
        Left (CacheSearch s)    -> C.searchCache s
        Left (CacheClean n)     -> sudo $ C.cleanCache n
        Left CacheCleanNotSaved -> sudo C.cleanNotSaved
        Left (CacheBackup pth)  -> sudo $ C.backupCache pth
    Right (Log o) ->
      case o of
        Nothing            -> L.viewLogFile
        Just (LogInfo ps)  -> L.logInfoOnPkg ps
        Just (LogSearch s) -> ask >>= send . flip L.searchLogFile s
    Right (Orphans o) ->
      case o of
        Nothing               -> send O.displayOrphans
        Just OrphanAbandon    -> sudo $ send orphans >>= removePkgs
        Just (OrphanAdopt ps) -> O.adoptPkg ps
    Right Version   -> send getVersionInfo >>= animateVersionMsg
    Right Languages -> displayOutputLanguages
    Right ViewConf  -> viewConfFile

displayOutputLanguages :: (Member (Reader Settings) r, Member IO r) => Eff r ()
displayOutputLanguages = do
  ss <- ask
  send . notify ss . displayOutputLanguages_1 $ langOf ss
  send $ traverse_ print [English ..]

viewConfFile :: (Member (Reader Settings) r, Member IO r) => Eff r ()
viewConfFile = do
  pth <- asks (either id id . configPathOf . commonConfigOf)
  void . send . shelly @IO . loudSh $ run_ "less" [toTextIgnore pth]

-- | Animated version message.
animateVersionMsg :: (Member (Reader Settings) r, Member IO r) => [T.Text] -> Eff r ()
animateVersionMsg verMsg = ask >>= \ss -> send $ do
  when (isTerminal ss) $ do
    hideCursor
    traverse_ (T.putStrLn . padString verMsgPad) verMsg  -- Version message
    raiseCursorBy 7  -- Initial reraising of the cursor.
    drawPills 3
    traverse_ T.putStrLn $ renderPacmanHead ss 0 Open  -- Initial rendering of head.
    raiseCursorBy 4
    takeABite ss 0  -- Initial bite animation.
    traverse_ (pillEating ss) pillsAndWidths
    clearGrid
  T.putStrLn auraLogo
  T.putStrLn $ "AURA Version " <> auraVersion
  T.putStrLn " by Colin Woodbury\n"
  traverse_ T.putStrLn . translatorMsg . langOf $ ss
  when (isTerminal ss) showCursor
    where pillEating ss (p, w) = clearGrid *> drawPills p *> takeABite ss w
          pillsAndWidths       = [(2, 5), (1, 10), (0, 15)]
