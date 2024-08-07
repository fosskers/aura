{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

{-

Copyright 2012 - 2024 Colin Woodbury <colin@fosskers.ca>

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
import           Aura.Commands.P as P
import           Aura.Core
import           Aura.Flags
import           Aura.IO
import           Aura.Languages
import           Aura.Logo
import           Aura.Pacman
import           Aura.Settings
import           Aura.Settings.Runtime
import           Aura.Types
import           Aura.Utils (nes)
import           Options.Applicative (execParser)
import           RIO
import           RIO.FilePath
import qualified RIO.Set as S
import           System.Process.Typed (proc, runProcess)

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
execute :: Env -> Program -> IO (Either Failure ())
execute env p = try (runRIO env . execOpts $ _operation p)

exit :: Settings -> Either Failure () -> IO a
exit _  (Left Silent)                = exitFailure
exit ss (Left (Failure (FailMsg e))) = scold ss e *> exitFailure
exit _  (Right _)                    = exitSuccess

execOpts :: Either (PacmanOp, Set MiscOp) AuraOp -> RIO Env ()
execOpts ops = do
  logDebug "Interpreting CLI options."
  ss <- asks settings
  let !env = envOf ss
  when (logLevelOf ss == LevelDebug) $ do
    logDebug $ displayShow ops
    logDebug . displayShow $ buildConfigOf ss
    logDebug . displayShow $ commonConfigOf ss

  let p :: (PacmanOp, Set MiscOp) -> RIO Env ()
      p (ps, ms) = liftIO . pacman env $
        asFlag ps
        ++ foldMap asFlag ms
        ++ asFlag (commonConfigOf ss)
        ++ bool [] ["--quiet"] (switch ss LowVerbosity)

  case ops of
    Left o@(Sync (Left sops) _, _)
      | any isUpgrade sops -> sudo (liftIO $ B.saveState ss) >> p o
    Left o -> logDebug "Performing a pacman operation." >> p o
    Right (AurSync o sws) -> do
      when (any isYWithA sws)
        $ sudo $ p (Sync (Right mempty) (S.singleton SyncRefresh), mempty)
      case o of
        Right ps              -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.install ps
        Left (AurDeps ps)     -> A.displayPkgDeps ps
        Left (AurInfo ps)     -> A.aurPkgInfo ps
        Left (AurPkgbuild ps) -> A.displayPkgbuild ps
        Left (AurSearch s)
          | not (S.member AurWideSearch sws) -> A.aurPkgSearch s
          | otherwise -> do
              aur <- try @(RIO Env) @Failure $ A.aurPkgSearch s
              -- If the AUR lookup succeeded but the -S didn't, the exit code should still be success.
              let term = SyncSearch s :| []
              repo <- try @(RIO Env) @Failure $ p (Sync (Left term) S.empty, S.empty)
              case (aur, repo) of
                (Left _, Left e) -> throwM e
                (_, _)           -> pure ()
        Left (AurUpgrade ps)  -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.upgradeAURPkgs ps
        Left (AurJson ps)     -> A.aurJson ps
        Left (AurTarball ps)  -> A.fetchTarball ps
    Right (Backup o) -> case o of
      Nothing              -> sudo . liftIO $ B.saveState ss
      Just (BackupClean n) -> sudo . liftIO $ B.cleanStates ss n
      Just BackupRestore   -> sudo B.restoreState
      Just BackupList      -> liftIO B.listStates
    Right (Cache o) -> case o of
      Right ps                -> sudo $ C.downgradePackages ps
      Left (CacheSearch s)    -> C.searchCache s
      Left (CacheClean n u)   -> sudo $ C.cleanCache n u
      Left CacheCleanNotSaved -> sudo C.cleanNotSaved
      Left (CacheBackup pth)  -> sudo $ C.backupCache pth
      Left CacheCleanVCS      -> sudo C.cleanDir
    Right (Log o) -> case o of
      Nothing            -> L.viewLogFile
      Just (LogInfo ps)  -> L.logInfoOnPkg ps
      Just (LogSearch s) -> asks settings >>= liftIO . flip L.searchLogFile s
    Right (Orphans o) -> case o of
      Nothing               -> liftIO $ O.displayOrphans env
      Just OrphanAbandon    -> sudo $ liftIO (orphans env) >>= traverse_ removePkgs . nes
      Just (OrphanAdopt ps) -> O.adoptPkg ps
    Right (Analysis o) -> case o of
      Nothing                -> P.exploitsFromStdin
      Just (AnalysisFile fp) -> P.exploitsFromFile fp
      Just (AnalysisDir fp)  -> P.exploitsFromFile $ fp </> "PKGBUILD"
      Just AnalysisAudit     -> P.audit
    Right Version   -> liftIO $ versionInfo env >>= animateVersionMsg ss auraVersion
    Right Languages -> displayOutputLanguages
    Right ViewConf  -> viewConfFile

isUpgrade :: SyncOp -> Bool
isUpgrade (SyncUpgrade _) = True
isUpgrade _               = False

-- | Did the user supply a `-y` alongside an `-A` command?
isYWithA :: AurSwitch -> Bool
isYWithA AurRepoSync = True
isYWithA _           = False

displayOutputLanguages :: RIO Env ()
displayOutputLanguages = do
  ss <- asks settings
  notify ss displayOutputLanguages_1
  traverse_ (putTextLn . tshow) [English ..]

viewConfFile :: RIO Env ()
viewConfFile = do
  pth <- asks (either id id . configPathOf . commonConfigOf . settings)
  void . runProcess $ proc "less" [pth]
