{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-

Copyright 2012 - 2020 Colin Woodbury <colin@fosskers.ca>

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
import           Aura.Utils (putTextLn)
import           Data.Bifunctor (first)
import qualified Data.Set.NonEmpty as NES
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Flags
import           Options.Applicative (execParser)
import           RIO hiding (first)
import           Settings
import           System.Path (toFilePath)
import           System.Process.Typed (proc, runProcess)
import           Text.Pretty.Simple (pPrintNoColor)

---

#ifndef CURRENT_PACKAGE_VERSION
#define CURRENT_PACKAGE_VERSION "UNKNOWN"
#endif

auraVersion :: Text
auraVersion = CURRENT_PACKAGE_VERSION

main :: IO ()
main = do
  options <- execParser opts
  res <- try $ withEnv options $ \env ->
    execute env options >>= exit (settings env)
  case res of
    Left err -> putTextLn (dtot . ($ English) $ failure err) *> exitFailure
    Right r  -> pure r

-- | Won't throw due to the `try`.
execute :: Env -> Program -> IO (Either (Doc AnsiStyle) ())
execute env p = first f <$> try (runRIO env . execOpts $ _operation p)
  where
    f (Failure fl) = fl $ langOf (settings env)

exit :: Settings -> Either (Doc AnsiStyle) () -> IO a
exit ss (Left e)  = scold ss e *> exitFailure
exit _  (Right _) = exitSuccess

execOpts :: Either (PacmanOp, Set MiscOp) AuraOp -> RIO Env ()
execOpts ops = do
  logDebug "Interpreting CLI options."
  ss <- asks settings
  when (shared ss Debug) $ do
    liftIO . pPrintNoColor $ ops
    liftIO . pPrintNoColor $ buildConfigOf ss
    liftIO . pPrintNoColor $ commonConfigOf ss
  let p (ps, ms) = liftIO . pacman $
        asFlag ps
        ++ foldMap asFlag ms
        ++ asFlag (commonConfigOf ss)
        ++ bool [] ["--quiet"] (switch ss LowVerbosity)
  case ops of
    Left o@(Sync (Left sops) _, _)
      | any isUpgrade sops -> sudo (liftIO $ B.saveState ss) *> p o
    Left o -> logDebug "Performing a pacman operation." >> p o
    Right (AurSync o _) ->
      case o of
        Right ps              -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.install ps
        Left (AurDeps ps)     -> A.displayPkgDeps ps
        Left (AurInfo ps)     -> A.aurPkgInfo ps
        Left (AurPkgbuild ps) -> A.displayPkgbuild ps
        Left (AurSearch s)    -> A.aurPkgSearch s
        Left (AurUpgrade ps)  -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.upgradeAURPkgs ps
        Left (AurJson ps)     -> A.aurJson ps
        Left (AurTarball ps)  -> A.fetchTarball ps
    Right (Backup o) ->
      case o of
        Nothing              -> sudo . liftIO $ B.saveState ss
        Just (BackupClean n) -> sudo . liftIO $ B.cleanStates ss n
        Just BackupRestore   -> sudo B.restoreState
        Just BackupList      -> liftIO B.listStates
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
        Just (LogSearch s) -> asks settings >>= liftIO . flip L.searchLogFile s
    Right (Orphans o) ->
      case o of
        Nothing               -> liftIO O.displayOrphans
        Just OrphanAbandon    -> sudo $ liftIO orphans >>= traverse_ removePkgs . NES.nonEmptySet
        Just (OrphanAdopt ps) -> O.adoptPkg ps
    Right Version   -> liftIO $ versionInfo >>= animateVersionMsg ss auraVersion
    Right Languages -> displayOutputLanguages
    Right ViewConf  -> viewConfFile

isUpgrade :: SyncOp -> Bool
isUpgrade (SyncUpgrade _) = True
isUpgrade _               = False

displayOutputLanguages :: RIO Env ()
displayOutputLanguages = do
  ss <- asks settings
  liftIO . notify ss . displayOutputLanguages_1 $ langOf ss
  liftIO $ traverse_ (putTextLn . tshow) [English ..]

viewConfFile :: RIO Env ()
viewConfFile = do
  pth <- asks (either id id . configPathOf . commonConfigOf . settings)
  void . runProcess $ proc "less" [toFilePath pth]
