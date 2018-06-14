{-# LANGUAGE OverloadedStrings #-}

module Flags
  ( Program(..), opts
  , PacmanOp(..)
  , AuraOp(..), AurOp(..), BackupOp(..), CacheOp(..), LogOp(..), OrphanOp(..)
  ) where

import           Aura.Settings.Base
import           Aura.Types (Language(..))
import           BasePrelude hiding (Version, FilePath, option, log)
import qualified Data.Set as S
import qualified Data.Text as T
import           Options.Applicative
import           Shelly hiding (command)
import           Utilities (User(..))

---

-- | A description of a run of Aura to attempt.
data Program = Program {
  -- ^ Whether Aura handles everything, or the ops and input are just passed down to Pacman.
  _operation   :: Either PacmanOp AuraOp
  -- ^ Settings common to both Aura and Pacman.
  , _commons   :: CommonConfig
  -- ^ Settings specific to building packages.
  , _buildConf :: BuildConfig
  -- ^ The human language of text output.
  , _language  :: Maybe Language }

-- | Inherited operations that are fed down to Pacman.
data PacmanOp = Database (Either DatabaseOp [T.Text]) (S.Set MiscOp)
              | Files
              | Query
              | Remove
              | Sync
              | TestDeps
              | Upgrade

instance Flagable PacmanOp where
  asFlag (Database (Left o) ms) = "-D" : asFlag o ++ concatMap asFlag (toList ms)
  asFlag _ = []  -- TODO fill this out

data DatabaseOp = DBCheck
                | DBAsDeps [T.Text]
                | DBAsExplicit [T.Text]

instance Flagable DatabaseOp where
  asFlag DBCheck           = ["--check"]
  asFlag (DBAsDeps ps)     = "--asdeps" : ps
  asFlag (DBAsExplicit ps) = "--asexplicit" : ps

data MiscOp = MiscArch    FilePath
            | MiscDBPath  FilePath
            | MiscRoot    FilePath
            | MiscVerbose
            | MiscColor   T.Text
            | MiscGpgDir  FilePath
            | MiscHookDir FilePath
            deriving (Eq, Ord)

instance Flagable MiscOp where
  asFlag (MiscArch p)    = ["--arch", toTextIgnore p]
  asFlag (MiscDBPath p)  = ["--dbpath", toTextIgnore p]
  asFlag (MiscRoot p)    = ["--root", toTextIgnore p]
  asFlag MiscVerbose     = ["--verbose"]
  asFlag (MiscColor c)   = ["--color", c]
  asFlag (MiscGpgDir p)  = ["--gpgdir", toTextIgnore p]
  asFlag (MiscHookDir p) = ["--hookdir", toTextIgnore p]

-- | Operations unique to Aura.
data AuraOp = AurSync (Either AurOp [T.Text])
            | Backup  (Maybe  BackupOp)
            | Cache   (Either CacheOp [T.Text])
            | Log     (Maybe  LogOp)
            | Orphans (Maybe  OrphanOp)
            | Version

data AurOp = AurDeps     [T.Text]
           | AurInfo     [T.Text]
           | AurPkgbuild [T.Text]
           | AurSearch    T.Text
           | AurUpgrade  [T.Text]
           | AurTarball  [T.Text]

data BackupOp = BackupClean Word | BackupRestore

data CacheOp = CacheBackup FilePath | CacheClean Word | CacheSearch T.Text

data LogOp = LogInfo [T.Text] | LogSearch T.Text

data OrphanOp = OrphanAbandon | OrphanAdopt [T.Text]

opts :: ParserInfo Program
opts = info (program <**> helper) (fullDesc <> header "Aura - Package manager for Arch Linux and the AUR.")

program :: Parser Program
program = Program
  <$> (fmap Right aurOps <|> fmap Left pacOps)
  <*> commonConfig
  <*> buildConfig
  <*> optional language
  where aurOps = aursync <|> backups <|> cache <|> log <|> orphans <|> version
        pacOps = database

aursync :: Parser AuraOp
aursync = AurSync <$> (bigA *> (fmap Right someArgs <|> fmap Left mods))
  where bigA = flag' () (long "aursync" <> short 'A' <> help "Install packages from the AUR.")
        mods = deps <|> ainfo <|> pkgbuild <|> search <|> upgrade <|> tarball
        deps = AurDeps <$>
          (flag' () (long "deps" <> short 'd' <> help "View dependencies of an AUR package.") *> someArgs)
        ainfo = AurInfo <$>
          (flag' () (long "info" <> short 'i' <> help "View AUR package information.") *> someArgs)
        pkgbuild = AurPkgbuild <$>
          (flag' () (long "pkgbuild" <> short 'p' <> help "View an AUR package's PKGBUILD file.") *> someArgs)
        search = AurSearch <$>
          strOption (long "search" <> short 's' <> metavar "STRING" <> help "Search the AUR via a search string.")
        upgrade = AurUpgrade <$>
          (flag' () (long "sysupgrade" <> short 'u' <> help "Upgrade all installed AUR packages.") *> manyArgs)
        tarball = AurTarball <$>
          (flag' () (long "downloadonly" <> short 'w' <> help "Download a package's source tarball.") *> someArgs)

backups :: Parser AuraOp
backups = Backup <$> (bigB *> optional mods)
  where bigB = flag' () (long "save" <> short 'B' <> help "Save a package state.")
        mods = clean <|> restore
        clean = BackupClean <$>
          option auto (long "clean" <> short 'c' <> metavar "N" <> help "Keep the most recent N states, delete the rest.")
        restore = flag' BackupRestore (long "restore" <> short 'r' <> help "Restore a previous package state.")

cache :: Parser AuraOp
cache = Cache <$> (bigC *> (fmap Left mods <|> fmap Right someArgs))
  where bigC = flag' () (long "downgrade" <> short 'C' <> help "Interact with the package cache.")
        mods = backup <|> clean <|> search
        backup = CacheBackup <$>
          strOption (long "backup"
                      <> metavar "PATH"
                      <> help "Backup the package cache to a given directory."
                      <> hidden)
        clean  = CacheClean <$>
          option auto (long "clean"
                        <> short 'c'
                        <> metavar "N"
                        <> help "Save the most recent N versions of a package in the cache, deleting the rest."
                        <> hidden)
        search = CacheSearch <$>
          strOption (long "search"
                      <> short 's'
                      <> metavar "STRING"
                      <> help "Search the package cache via a search string."
                      <> hidden)

log :: Parser AuraOp
log = Log <$> (bigL *> optional mods)
  where bigL = flag' () (long "viewlog" <> short 'L' <> help "View the Pacman log.")
        mods = inf <|> search
        inf  = LogInfo <$>
          (flag' () (long "info"
                      <> short 'i'
                      <> help "Display the installation history for given packages."
                      <> hidden) *> someArgs)
        search = LogSearch <$>
          strOption (long "search"
                      <> short 's'
                      <> metavar "STRING"
                      <> help "Search the Pacman log via a search string."
                      <> hidden)

orphans :: Parser AuraOp
orphans = Orphans <$> (bigO *> optional mods)
  where bigO    = flag' () (long "orphans" <> short 'O' <> help "Display all orphan packages.")
        mods    = abandon <|> adopt
        abandon = flag' OrphanAbandon (long "abandon" <> short 'j' <> help "Uninstall all orphan packages.")
        adopt   = OrphanAdopt <$>
          (flag' () (long "adopt" <> help "Mark some packages' install reason as 'Explicit'.") *> someArgs)

version :: Parser AuraOp
version = flag' Version (long "version" <> short 'V' <> help "Display Aura's version.")

buildConfig :: Parser BuildConfig
buildConfig = BuildConfig <$> makepkg <*> ignored <*> optional bp <*> optional bu <*> trunc <*> buildSwitches
  where makepkg = S.fromList <$> many (ia <|> as)
        ia      = flag' IgnoreArch (long "ignorearch" <> help "Exposed makepkg flag.")
        as      = flag' AllSource (long "allsource" <> help "Exposed makepkg flag.")
        ignored = maybe S.empty (S.fromList . T.split (== ',')) <$>
          optional (strOption (long "aurignore" <> metavar "PKG(,PKG,...)" <> help "Ignore given AUR packages."))
        bp      = strOption (long "build" <> metavar "PATH" <> help "Directory in which to build packages.")
        bu      = User <$> strOption (long "builduser" <> metavar "USER" <> help "User account to build as.")
        trunc   = fmap Head (option auto (long "head" <> metavar "N" <> help "Only show top N search results."))
          <|> fmap Tail (option auto (long "tail" <> metavar "N" <> help "Only show last N search results."))
          <|> pure None

buildSwitches :: Parser (S.Set BuildSwitch)
buildSwitches = S.fromList <$> many (lv <|> dmd <|> dsm <|> dpb <|> rbd <|> he <|> ucp <|> dr <|> sa)
  where lv  = flag' LowVerbosity (long "quiet" <> short 'q' <> help "Display less information.")
        dmd = flag' DeleteMakeDeps (long "delmakedeps" <> short 'a' <> help "Uninstall makedeps after building.")
        dsm = flag' DontSuppressMakepkg (long "unsuppress" <> short 'x' <> help "Unsuppress makepkg output.")
        dpb = flag' DiffPkgbuilds (long "diff" <> short 'k' <> help "Show PKGBUILD diffs.")
        rbd = flag' RebuildDevel (long "devel" <> help "Rebuild all git/hg/svn/darcs-based packages.")
        he  = flag' HotEdit (long "hotedit" <> help "Edit a PKGBUILD before building.")
        ucp = flag' UseCustomizepkg (long "custom" <> help "Run customizepkg before building.")
        dr  = flag' DryRun (long "dryrun" <> help "Run dependency checks and PKGBUILD diffs, but don't build.")
        sa  = flag' SortAlphabetically (long "abc" <> help "Sort search results alphabetically.")

commonConfig :: Parser CommonConfig
commonConfig = CommonConfig <$> optional cap <*> optional cop <*> optional lfp <*> commonSwitches
  where cap = strOption (long "cachedir" <> help "Use an alternate package cache location.")
        cop = strOption (long "config"   <> help "Use an alternate Pacman config file.")
        lfp = strOption (long "logfile"  <> help "Use an alternate Pacman log.")

commonSwitches :: Parser (S.Set CommonSwitch)
commonSwitches = S.fromList <$> many (nc <|> no <|> dbg)
  where nc  = flag' NoConfirm  (long "noconfirm" <> help "Never ask for Aura or Pacman confirmation.")
        no  = flag' NeededOnly (long "needed"    <> help "Don't rebuild/reinstall up-to-date packages.")
        dbg = flag' Debug      (long "debug"     <> help "Print useful debugging info.")

database :: Parser PacmanOp
database = Database <$> (bigD *> (fmap Right someArgs <|> fmap Left mods)) <*> misc
  where bigD   = flag' () (long "database" <> short 'D' <> help "Interact with the package database.")
        mods   = check <|> asdeps <|> asexp
        check  = flag' DBCheck (long "check" <> short 'k' <> help "Test local database validity.")
        asdeps = DBAsDeps <$> (flag' () (long "asdeps" <> help "Mark packages as being dependencies.") *> someArgs)
        asexp  = DBAsExplicit <$> (flag' () (long "asexplicit" <> help "Mark packages as being explicitely installed.") *> someArgs)

misc :: Parser (S.Set MiscOp)
misc = S.fromList <$> many (ar <|> dbp <|> roo <|> ver <|> clr <|> gpg <|> hd)
  where ar  = MiscArch    <$> strOption (long "arch" <> metavar "ARCH" <> help "Use an alternate architecture.")
        dbp = MiscDBPath  <$> strOption (long "dbpath" <> short 'b' <> metavar "PATH" <> help "Use an alternate database location.")
        roo = MiscRoot    <$> strOption (long "root" <> short 'r' <> metavar "PATH" <> help "Use an alternate installation root.")
        ver = flag' MiscVerbose (long "verbose" <> short 'v' <> help "Be more verbose.")
        clr = MiscColor   <$> strOption (long "color" <> metavar "WHEN" <> help "Colourize the output.")
        gpg = MiscGpgDir  <$> strOption (long "gpgdir" <> metavar "PATH" <> help "Use an alternate GnuGPG directory.")
        hd  = MiscHookDir <$> strOption (long "hookdir" <> metavar "PATH" <> help "Use an alternate hook directory.")

-- | One or more arguments.
someArgs :: Parser [T.Text]
someArgs = some (argument str (metavar "PACKAGES"))

-- | Zero or more arguments.
manyArgs :: Parser [T.Text]
manyArgs = many (argument str (metavar "PACKAGES"))

language :: Parser Language
language = pure English
