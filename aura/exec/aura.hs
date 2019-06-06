{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-

Copyright 2012 - 2019 Colin Woodbury <colin@fosskers.ca>

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
import           Aura.Packages.AUR (aurRepo)
import           Aura.Packages.Repository (pacmanRepo)
import           Aura.Pacman
import           Aura.Settings
import           Aura.Types
import           BasePrelude hiding (Version)
import           Control.Effect (Carrier, Member)
import           Control.Effect.Error (Error, runError)
import           Control.Effect.Lift (Lift, runM, sendM)
import           Control.Effect.Reader (Reader, asks, runReader)
import           Data.Set (Set)
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Flags
import           Options.Applicative (execParser)
import           Settings
import           System.Path (toFilePath)
import           System.Process.Typed (proc, runProcess)
import           Text.Pretty.Simple (pPrintNoColor)

---

auraVersion :: T.Text
auraVersion = "2.0.1"

main :: IO ()
main = do
  options   <- execParser opts
  esettings <- getSettings options
  repos     <- (<>) <$> pacmanRepo <*> aurRepo
  case esettings of
    Left err -> T.putStrLn . dtot . ($ English) $ failure err
    Right ss -> execute ss repos options >>= exit ss

execute :: Settings -> Repository -> Program -> IO (Either (Doc AnsiStyle) ())
execute ss r p = first (($ langOf ss) . failure) <$> (runM . runReader (Env r ss) . runError . executeOpts $ _operation p)

exit :: Settings -> Either (Doc AnsiStyle) () -> IO a
exit ss (Left e)  = scold ss e *> exitFailure
exit _  (Right _) = exitSuccess

executeOpts :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) =>
  Either (PacmanOp, Set MiscOp) AuraOp -> m ()
executeOpts ops = do
  ss <- asks settings
  when (shared ss Debug) $ do
    sendM @IO . pPrintNoColor $ ops
    sendM @IO . pPrintNoColor $ buildConfigOf ss
    sendM @IO . pPrintNoColor $ commonConfigOf ss
  let p (ps, ms) = liftEitherM . sendM . pacman $
        asFlag ps
        ++ foldMap asFlag ms
        ++ asFlag (commonConfigOf ss)
        ++ bool [] ["--quiet"] (switch ss LowVerbosity)
  case ops of
    Left o@(Sync (Left (SyncUpgrade _)) _, _) -> sudo (sendM $ B.saveState ss) *> p o
    Left o -> p o
    Right (AurSync o _) ->
      case o of
        Right ps              -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.install ps
        Left (AurDeps ps)     -> A.displayPkgDeps ps
        Left (AurInfo ps)     -> A.aurPkgInfo ps
        Left (AurPkgbuild ps) -> A.displayPkgbuild ps
        Left (AurSearch s)    -> A.aurPkgSearch s
        Left (AurUpgrade ps)  -> bool (trueRoot . sudo) id (switch ss DryRun) $ A.upgradeAURPkgs ps
        Left (AurJson ps)     -> A.aurJson ps
    Right (Backup o) ->
      case o of
        Nothing              -> sudo . sendM $ B.saveState ss
        Just (BackupClean n) -> sudo . sendM $ B.cleanStates ss n
        Just BackupRestore   -> sudo B.restoreState
        Just BackupList      -> sendM B.listStates
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
        Just (LogSearch s) -> asks settings >>= sendM . flip L.searchLogFile s
    Right (Orphans o) ->
      case o of
        Nothing               -> sendM O.displayOrphans
        Just OrphanAbandon    -> sudo $ sendM orphans >>= traverse_ removePkgs . NES.nonEmptySet
        Just (OrphanAdopt ps) -> O.adoptPkg ps
    Right Version   -> sendM $ versionInfo >>= animateVersionMsg ss auraVersion
    Right Languages -> displayOutputLanguages
    Right ViewConf  -> viewConfFile

displayOutputLanguages :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) => m ()
displayOutputLanguages = do
  ss <- asks settings
  sendM . notify ss . displayOutputLanguages_1 $ langOf ss
  sendM $ traverse_ print [English ..]

viewConfFile :: (Carrier sig m, Member (Reader Env) sig, Member (Lift IO) sig) => m ()
viewConfFile = do
  pth <- asks (either id id . configPathOf . commonConfigOf . settings)
  sendM . void . runProcess @IO $ proc "less" [toFilePath pth]
