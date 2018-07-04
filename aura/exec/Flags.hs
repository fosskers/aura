{-# LANGUAGE OverloadedStrings #-}

module Flags
  ( Program(..), opts
  , PacmanOp(..)
  , AuraOp(..), AurOp(..), BackupOp(..), CacheOp(..), LogOp(..), OrphanOp(..)
  ) where

import           Aura.Cache (defaultPackageCache)
import           Aura.Pacman (pacmanConfFile, defaultLogFile)
import           Aura.Settings
import           Aura.Types (Language(..))
import           BasePrelude hiding (Version, FilePath, option, log, exp)
import qualified Data.Set as S
import qualified Data.Text as T
import           Options.Applicative
import           Shelly
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
  , _language  :: Maybe Language } deriving (Show)

-- | Inherited operations that are fed down to Pacman.
data PacmanOp = Database (Either DatabaseOp (S.Set T.Text)) (S.Set MiscOp)
              | Files    (S.Set FilesOp) (S.Set MiscOp)
              | Query    (Either QueryOp (S.Set QueryFilter, S.Set T.Text)) (S.Set MiscOp)
              | Remove   (S.Set RemoveOp) (S.Set T.Text) (S.Set MiscOp)
              | Sync     (Either SyncOp (S.Set T.Text)) (S.Set SyncSwitch) (S.Set MiscOp)
              | TestDeps (S.Set T.Text) (S.Set MiscOp)
              | Upgrade  (Maybe UpgradeSwitch) (S.Set T.Text) (S.Set MiscOp)
              deriving (Show)

instance Flagable PacmanOp where
  asFlag (Database (Left o) ms)      = "-D" : asFlag o ++ concatMap asFlag (toList ms)
  asFlag (Database (Right fs) ms)    = "-D" : toList fs ++ concatMap asFlag (toList ms)
  asFlag (Files os ms)               = "-F" : concatMap asFlag (toList os) ++ concatMap asFlag (toList ms)
  asFlag (Query (Left o) ms)         = "-Q" : asFlag o ++ concatMap asFlag (toList ms)
  asFlag (Query (Right (fs, ps)) ms) = "-Q" : toList ps ++ concatMap asFlag (toList fs) ++ concatMap asFlag (toList ms)
  asFlag (Remove os ps ms)           = "-R" : concatMap asFlag (toList os) ++ toList ps ++ concatMap asFlag (toList ms)
  asFlag (Sync (Left o) ss ms)       = "-S" : concatMap asFlag (toList ss) ++ asFlag o ++ concatMap asFlag (toList ms)
  asFlag (Sync (Right ps) ss ms)     = "-S" : concatMap asFlag (toList ss) ++ toList ps ++ concatMap asFlag (toList ms)
  asFlag (TestDeps ps ms)            = "-T" : toList ps ++ concatMap asFlag (toList ms)
  asFlag (Upgrade s ps ms)           = "-U" : maybe [] asFlag s ++ toList ps ++ concatMap asFlag (toList ms)

data DatabaseOp = DBCheck
                | DBAsDeps     (S.Set T.Text)
                | DBAsExplicit (S.Set T.Text)
                deriving (Show)

instance Flagable DatabaseOp where
  asFlag DBCheck           = ["--check"]
  asFlag (DBAsDeps ps)     = "--asdeps" : toList ps
  asFlag (DBAsExplicit ps) = "--asexplicit" : toList ps

data FilesOp = FilesList  (S.Set T.Text)
             | FilesOwns   T.Text
             | FilesSearch T.Text
             | FilesRegex
             | FilesRefresh
             | FilesMachineReadable
             deriving (Eq, Ord, Show)

instance Flagable FilesOp where
  asFlag (FilesList fs)       = "--list" : toList fs
  asFlag (FilesOwns f)        = ["--owns", f]
  asFlag (FilesSearch f)      = ["--search", f]
  asFlag FilesRegex           = ["--regex"]
  asFlag FilesRefresh         = ["--refresh"]
  asFlag FilesMachineReadable = ["--machinereadable"]

data QueryOp = QueryChangelog (S.Set T.Text)
             | QueryGroups    (S.Set T.Text)
             | QueryInfo      (S.Set T.Text)
             | QueryCheck     (S.Set T.Text)
             | QueryList      (S.Set T.Text)
             | QueryOwns      (S.Set T.Text)
             | QueryFile      (S.Set T.Text)
             | QuerySearch     T.Text
             deriving (Show)

instance Flagable QueryOp where
  asFlag (QueryChangelog ps) = "--changelog" : toList ps
  asFlag (QueryGroups ps)    = "--groups" : toList ps
  asFlag (QueryInfo ps)      = "--info" : toList ps
  asFlag (QueryCheck ps)     = "--check" : toList ps
  asFlag (QueryList ps)      = "--list" : toList ps
  asFlag (QueryOwns ps)      = "--owns" : toList ps
  asFlag (QueryFile ps)      = "--file" : toList ps
  asFlag (QuerySearch t)     = ["--search", t]

data QueryFilter = QueryDeps
                 | QueryExplicit
                 | QueryForeign
                 | QueryNative
                 | QueryUnrequired
                 | QueryUpgrades
                 deriving (Eq, Ord, Show)

instance Flagable QueryFilter where
  asFlag QueryDeps       = ["--deps"]
  asFlag QueryExplicit   = ["--explicit"]
  asFlag QueryForeign    = ["--foreign"]
  asFlag QueryNative     = ["--native"]
  asFlag QueryUnrequired = ["--unrequired"]
  asFlag QueryUpgrades   = ["--upgrades"]

data RemoveOp = RemoveCascade
              | RemoveNoSave
              | RemoveRecursive
              | RemoveUnneeded
              deriving (Eq, Ord, Show)

instance Flagable RemoveOp where
  asFlag RemoveCascade   = ["--cascade"]
  asFlag RemoveNoSave    = ["--nosave"]
  asFlag RemoveRecursive = ["--recursive"]
  asFlag RemoveUnneeded  = ["--unneeded"]

data SyncOp = SyncClean
            | SyncGroups   (S.Set T.Text)
            | SyncInfo     (S.Set T.Text)
            | SyncList      T.Text
            | SyncSearch    T.Text
            | SyncUpgrade  (S.Set T.Text)
            | SyncDownload (S.Set T.Text)
            deriving (Show)

instance Flagable SyncOp where
  asFlag SyncClean         = ["--clean"]
  asFlag (SyncGroups gs)   = "--groups" : toList gs
  asFlag (SyncInfo ps)     = "--info" : toList ps
  asFlag (SyncList r)      = ["--list", r]
  asFlag (SyncSearch s)    = ["--search", s]
  asFlag (SyncUpgrade ps)  = "--sysupgrade" : toList ps
  asFlag (SyncDownload ps) = "--downloadonly" : toList ps

data SyncSwitch = SyncRefresh deriving (Eq, Ord, Show)

instance Flagable SyncSwitch where
  asFlag SyncRefresh = ["--refresh"]

data UpgradeSwitch = UpgradeAsDeps | UpgradeAsExplicit deriving (Show)

instance Flagable UpgradeSwitch where
  asFlag UpgradeAsDeps     = ["--asdeps"]
  asFlag UpgradeAsExplicit = ["--asexplicit"]

-- | Flags common to several Pacman operations.
data MiscOp = MiscArch    FilePath
            | MiscAssumeInstalled T.Text
            | MiscColor   T.Text
            | MiscConfirm
            | MiscDBOnly
            | MiscDBPath  FilePath
            | MiscGpgDir  FilePath
            | MiscHookDir FilePath
            | MiscNoDeps
            | MiscNoProgress
            | MiscNoScriptlet
            | MiscPrint
            | MiscPrintFormat T.Text
            | MiscRoot    FilePath
            | MiscVerbose
            deriving (Eq, Ord, Show)

instance Flagable MiscOp where
  asFlag (MiscArch p)            = ["--arch", toTextIgnore p]
  asFlag (MiscAssumeInstalled p) = ["--assume-installed", p]
  asFlag (MiscColor c)           = ["--color", c]
  asFlag (MiscDBPath p)          = ["--dbpath", toTextIgnore p]
  asFlag (MiscGpgDir p)          = ["--gpgdir", toTextIgnore p]
  asFlag (MiscHookDir p)         = ["--hookdir", toTextIgnore p]
  asFlag (MiscPrintFormat s)     = ["--print-format", s]
  asFlag (MiscRoot p)            = ["--root", toTextIgnore p]
  asFlag MiscConfirm             = ["--confirm"]
  asFlag MiscDBOnly              = ["--dbonly"]
  asFlag MiscNoDeps              = ["--nodeps"]
  asFlag MiscNoProgress          = ["--noprogressbar"]
  asFlag MiscNoScriptlet         = ["--noscriptlet"]
  asFlag MiscPrint               = ["--print"]
  asFlag MiscVerbose             = ["--verbose"]

-- | Operations unique to Aura.
data AuraOp = AurSync (Either AurOp (S.Set T.Text))
            | Backup  (Maybe  BackupOp)
            | Cache   (Either CacheOp (S.Set T.Text))
            | Log     (Maybe  LogOp)
            | Orphans (Maybe  OrphanOp)
            | Version
            | Languages
            deriving (Show)

data AurOp = AurDeps     (S.Set T.Text)
           | AurInfo     (S.Set T.Text)
           | AurPkgbuild (S.Set T.Text)
           | AurSearch    T.Text
           | AurUpgrade  (S.Set T.Text)
           | AurTarball  (S.Set T.Text)
           | AurJson     (S.Set T.Text)
           deriving (Show)

data BackupOp = BackupClean Word | BackupRestore | BackupList deriving (Show)

data CacheOp = CacheBackup FilePath | CacheClean Word | CacheCleanNotSaved | CacheSearch T.Text deriving (Show)

data LogOp = LogInfo (S.Set T.Text) | LogSearch T.Text deriving (Show)

data OrphanOp = OrphanAbandon | OrphanAdopt (S.Set T.Text) deriving (Show)

opts :: ParserInfo Program
opts = info (program <**> helper) (fullDesc <> header "Aura - Package manager for Arch Linux and the AUR.")

program :: Parser Program
program = Program
  <$> (fmap Right aurOps <|> fmap Left pacOps)
  <*> commonConfig
  <*> buildConfig
  <*> optional language
  where aurOps = aursync <|> backups <|> cache <|> log <|> orphans <|> version <|> languages
        pacOps = database <|> files <|> queries <|> remove <|> sync <|> testdeps <|> upgrades

aursync :: Parser AuraOp
aursync = bigA *> (AurSync <$> (fmap (Right . S.map T.toLower) someArgs <|> fmap Left mods))
  where bigA = flag' () (long "aursync" <> short 'A' <> help "Install packages from the AUR.")
        mods     = deps <|> ainfo <|> pkgbuild <|> search <|> upgrade <|> tarball <|> aur
        deps     = AurDeps <$> (flag' () (long "deps" <> short 'd' <> hidden <> help "View dependencies of an AUR package.") *> someArgs')
        ainfo    = AurInfo <$> (flag' () (long "info" <> short 'i' <> hidden <> help "View AUR package information.") *> someArgs')
        pkgbuild = AurPkgbuild <$> (flag' () (long "pkgbuild" <> short 'p' <> hidden <> help "View an AUR package's PKGBUILD file.") *> someArgs')
        search   = AurSearch <$> strOption (long "search" <> short 's' <> metavar "STRING" <> hidden <> help "Search the AUR via a search string.")
        upgrade  = AurUpgrade <$> (flag' () (long "sysupgrade" <> short 'u' <> hidden <> help "Upgrade all installed AUR packages.") *> manyArgs')
        tarball  = AurTarball <$> (flag' () (long "downloadonly" <> short 'w' <> hidden <> help "Download a package's source tarball.") *> someArgs')
        aur      = AurJson <$> (flag' () (long "json" <> hidden <> help "Retrieve package JSON straight from the AUR.") *> someArgs')

backups :: Parser AuraOp
backups = bigB *> (Backup <$> optional mods)
  where bigB = flag' () (long "save" <> short 'B' <> help "Save a package state.")
        mods = clean <|> restore <|> list
        clean = BackupClean <$> option auto (long "clean" <> short 'c' <> metavar "N" <> hidden <> help "Keep the most recent N states, delete the rest.")
        restore = flag' BackupRestore (long "restore" <> short 'r' <> hidden <> help "Restore a previous package state.")
        list = flag' BackupList (long "list" <> short 'l' <> hidden <> help "Show all saved package state filenames.")

cache :: Parser AuraOp
cache = bigC *> (Cache <$> (fmap Left mods <|> fmap Right someArgs))
  where bigC = flag' () (long "downgrade" <> short 'C' <> help "Interact with the package cache.")
        mods = backup <|> clean <|> clean' <|> search
        backup = CacheBackup <$> strOption (long "backup" <> short 'b' <> metavar "PATH" <> help "Backup the package cache to a given directory." <> hidden)
        clean  = CacheClean  <$> option auto (long "clean" <> short 'c' <> metavar "N" <> help "Save the most recent N versions of a package in the cache, deleting the rest." <> hidden)
        clean' = flag' CacheCleanNotSaved (long "notsaved" <> help "Clean out any cached package files which doesn't appear in any saved state." <> hidden)
        search = CacheSearch <$> strOption (long "search" <> short 's' <> metavar "STRING" <> help "Search the package cache via a search string." <> hidden)

log :: Parser AuraOp
log = bigL *> (Log <$> optional mods)
  where bigL = flag' () (long "viewlog" <> short 'L' <> help "View the Pacman log.")
        mods = inf <|> sch
        inf  = LogInfo <$> (flag' () (long "info" <> short 'i' <> help "Display the installation history for given packages." <> hidden) *> someArgs')
        sch  = LogSearch <$> strOption (long "search" <> short 's' <> metavar "STRING" <> help "Search the Pacman log via a search string." <> hidden)

orphans :: Parser AuraOp
orphans = bigO *> (Orphans <$> optional mods)
  where bigO    = flag' () (long "orphans" <> short 'O' <> help "Display all orphan packages.")
        mods    = abandon <|> adopt
        abandon = flag' OrphanAbandon (long "abandon" <> short 'j' <> hidden <> help "Uninstall all orphan packages.")
        adopt   = OrphanAdopt <$> (flag' () (long "adopt" <> hidden <> help "Mark some packages' install reason as 'Explicit'.") *> someArgs')

version :: Parser AuraOp
version = flag' Version (long "version" <> short 'V' <> help "Display Aura's version.")

languages :: Parser AuraOp
languages = flag' Languages (long "languages" <> help "Show all human languages available for output.")

buildConfig :: Parser BuildConfig
buildConfig = BuildConfig <$> makepkg <*> bp <*> optional bu <*> trunc <*> buildSwitches
  where makepkg = S.fromList <$> many (ia <|> as <|> si)
        ia      = flag' IgnoreArch (long "ignorearch" <> hidden <> help "Exposed makepkg flag.")
        as      = flag' AllSource (long "allsource" <> hidden <> help "Exposed makepkg flag.")
        si      = flag' SkipInteg (long "skipinteg" <> hidden <> help "Skip all makepkg integrity checks.")
        bp      = strOption (long "build" <> metavar "PATH" <> hidden <> help "Directory in which to build packages.")
                  <|> pure defaultPackageCache
        bu      = User <$> strOption (long "builduser" <> metavar "USER" <> hidden <> help "User account to build as.")
        trunc   = fmap Head (option auto (long "head" <> metavar "N" <> hidden <> help "Only show top N search results."))
          <|> fmap Tail (option auto (long "tail" <> metavar "N" <> hidden <> help "Only show last N search results."))
          <|> pure None

buildSwitches :: Parser (S.Set BuildSwitch)
buildSwitches = S.fromList <$> many (lv <|> dmd <|> dsm <|> dpb <|> rbd <|> he <|> ucp <|> dr <|> sa <|> fo)
  where dmd = flag' DeleteMakeDeps (long "delmakedeps" <> short 'a' <> hidden <> help "Uninstall makedeps after building.")
        dsm = flag' DontSuppressMakepkg (long "unsuppress" <> short 'x' <> hidden <> help "Unsuppress makepkg output.")
        dpb = flag' DiffPkgbuilds (long "diff" <> short 'k' <> hidden <> help "Show PKGBUILD diffs.")
        rbd = flag' RebuildDevel (long "devel" <> hidden <> help "Rebuild all git/hg/svn/darcs-based packages.")
        he  = flag' HotEdit (long "hotedit" <> hidden <> help "Edit a PKGBUILD before building.")
        ucp = flag' UseCustomizepkg (long "custom" <> hidden <> help "Run customizepkg before building.")
        dr  = flag' DryRun (long "dryrun" <> hidden <> help "Run dependency checks and PKGBUILD diffs, but don't build.")
        sa  = flag' SortAlphabetically (long "abc" <> hidden <> help "Sort search results alphabetically.")
        lv  = flag' LowVerbosity (long "quiet" <> short 'q' <> hidden <> help "Display less information.")
        fo  = flag' ForceBuilding (long "force" <> hidden <> help "Always (re)build specified packages.")

commonConfig :: Parser CommonConfig
commonConfig = CommonConfig <$> cap <*> cop <*> lfp <*> ign <*> igg <*> commonSwitches
  where cap = fmap Right (strOption (long "cachedir" <> hidden <> help "Use an alternate package cache location."))
              <|> pure (Left defaultPackageCache)
        cop = fmap Right (strOption (long "config"   <> hidden <> help "Use an alternate Pacman config file."))
              <|> pure (Left pacmanConfFile)
        lfp = fmap Right (strOption (long "logfile"  <> hidden <> help "Use an alternate Pacman log."))
              <|> pure (Left defaultLogFile)
        ign = maybe S.empty (S.fromList . T.split (== ',')) <$>
          optional (strOption (long "ignore" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore given packages."))
        igg = maybe S.empty (S.fromList . T.split (== ',')) <$>
          optional (strOption (long "ignoregroup" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore packages from the given groups."))

commonSwitches :: Parser (S.Set CommonSwitch)
commonSwitches = S.fromList <$> many (nc <|> no <|> dbg <|> clr)
  where nc  = flag' NoConfirm  (long "noconfirm" <> hidden <> help "Never ask for Aura or Pacman confirmation.")
        no  = flag' NeededOnly (long "needed"    <> hidden <> help "Don't rebuild/reinstall up-to-date packages.")
        dbg = flag' Debug      (long "debug"     <> hidden <> help "Print useful debugging info.")
        clr = Colour . f <$> strOption (long "color" <> metavar "WHEN" <> hidden <> help "Colourize the output.")
        f "never"  = Never
        f "always" = Always
        f _        = Auto

database :: Parser PacmanOp
database = bigD *> (Database <$> (fmap Right someArgs <|> fmap Left mods) <*> misc)
  where bigD   = flag' () (long "database" <> short 'D' <> help "Interact with the package database.")
        mods   = check <|> asdeps <|> asexp
        check  = flag' DBCheck (long "check" <> short 'k' <> hidden <> help "Test local database validity.")
        asdeps = DBAsDeps <$> (flag' () (long "asdeps" <> hidden <> help "Mark packages as being dependencies.") *> someArgs')
        asexp  = DBAsExplicit <$> (flag' () (long "asexplicit" <> hidden <> help "Mark packages as being explicitely installed.") *> someArgs')

files :: Parser PacmanOp
files = bigF *> (Files <$> fmap S.fromList (many mods) <*> misc)
  where bigF = flag' () (long "files" <> short 'F' <> help "Interact with the file database.")
        mods = lst <|> own <|> sch <|> rgx <|> rfr <|> mch
        lst  = FilesList <$> (flag' () (long "list" <> short 'l' <> hidden <> help "List the files owned by given packages.") *> someArgs')
        own  = FilesOwns <$> strOption (long "owns" <> short 'o' <> metavar "FILE" <> hidden <> help "Query the package that owns FILE.")
        sch  = FilesSearch <$> strOption (long "search" <> short 's' <> metavar "FILE" <> hidden <> help "Find package files that match the given FILEname.")
        rgx  = flag' FilesRegex (long "regex" <> short 'x' <> hidden <> help "Interpret the input of -Fs as a regex.")
        rfr  = flag' FilesRefresh (long "refresh" <> short 'y' <> hidden <> help "Download fresh package databases.")
        mch  = flag' FilesMachineReadable (long "machinereadable" <> hidden <> help "Produce machine-readable output.")

queries :: Parser PacmanOp
queries = bigQ *> (Query <$> (fmap Right query <|> fmap Left mods) <*> misc)
  where bigQ  = flag' () (long "query" <> short 'Q' <> help "Interact with the local package database.")
        query = (,) <$> queryFilters <*> manyArgs
        mods  = chl <|> gps <|> inf <|> lst <|> own <|> fls <|> sch
        chl   = QueryChangelog <$> (flag' () (long "changelog" <> short 'c' <> hidden <> help "View a package's changelog.") *> someArgs')
        gps   = QueryGroups <$> (flag' () (long "groups" <> short 'g' <> hidden <> help "View all members of a package group.") *> someArgs')
        inf   = QueryInfo <$> (flag' () (long "info" <> short 'i' <> hidden <> help "View package information.") *> someArgs')
        lst   = QueryList <$> (flag' () (long "list" <> short 'l' <> hidden <> help "List files owned by a package.") *> someArgs')
        own   = QueryOwns <$> (flag' () (long "owns" <> short 'o' <> hidden <> help "Find the package some file belongs to.") *> someArgs')
        fls   = QueryFile <$> (flag' () (long "file" <> short 'p' <> hidden <> help "Query a package file.") *> someArgs')
        sch   = QuerySearch <$> strOption (long "search" <> short 's' <> metavar "REGEX" <> hidden <> help "Search the local database.")

queryFilters :: Parser (S.Set QueryFilter)
queryFilters = S.fromList <$> many (dps <|> exp <|> frg <|> ntv <|> urq <|> upg)
  where dps = flag' QueryDeps (long "deps" <> short 'd' <> hidden <> help "[filter] Only list packages installed as deps.")
        exp = flag' QueryExplicit (long "explicit" <> short 'e' <> hidden <> help "[filter] Only list explicitly installed packages.")
        frg = flag' QueryForeign (long "foreign" <> short 'm' <> hidden <> help "[filter] Only list AUR packages.")
        ntv = flag' QueryNative (long "native" <> short 'n' <> hidden <> help "[filter] Only list official packages.")
        urq = flag' QueryUnrequired (long "unrequired" <> short 't' <> hidden <> help "[filter] Only list packages not required as a dependency to any other.")
        upg = flag' QueryUpgrades (long "upgrades" <> short 'u' <> hidden <> help "[filter] Only list outdated packages.")

remove :: Parser PacmanOp
remove = bigR *> (Remove <$> mods <*> someArgs <*> misc)
  where bigR     = flag' () (long "remove" <> short 'R' <> help "Uninstall packages.")
        mods     = S.fromList <$> many (cascade <|> nosave <|> recurse <|> unneeded)
        cascade  = flag' RemoveCascade (long "cascade" <> short 'c' <> hidden <> help "Remove packages and all others that depend on them.")
        nosave   = flag' RemoveNoSave (long "nosave" <> short 'n' <> hidden <> help "Remove configuration files as well.")
        recurse  = flag' RemoveRecursive (long "recursive" <> short 's' <> hidden <> help "Remove unneeded dependencies.")
        unneeded = flag' RemoveUnneeded (long "unneeded" <> short 'u' <> hidden <> help "Remove unneeded packages.")

sync :: Parser PacmanOp
sync = bigS *> (Sync <$> (fmap Right someArgs <|> fmap Left mods) <*> ref <*> misc)
  where bigS = flag' () (long "sync" <> short 'S' <> help "Install official packages.")
        ref  = S.fromList <$> many (flag' SyncRefresh (long "refresh" <> short 'y' <> hidden <> help "Update the package database."))
        mods = cln <|> gps <|> inf <|> lst <|> sch <|> upg <|> dnl
        cln  = flag' SyncClean (long "clean" <> short 'c' <> hidden <> help "Remove old packages from the cache.")
        gps  = SyncGroups <$> (flag' () (long "groups" <> short 'g' <> hidden <> help "View members of a package group.") *> someArgs')
        inf  = SyncInfo <$> (flag' () (long "info" <> short 'i' <> hidden <> help "View package information.") *> someArgs')
        lst  = SyncList <$> strOption (long "list" <> short 'l' <> metavar "REPO" <> hidden <> help "List the packages in a REPO.")
        sch  = SyncSearch <$> strOption (long "search" <> short 's' <> metavar "REGEX" <> hidden <> help "Search the official package repos.")
        upg  = SyncUpgrade <$> (flag' () (long "sysupgrade" <> short 'u' <> hidden <> help "Upgrade installed packages.") *> manyArgs')
        dnl  = SyncDownload <$> (flag' () (long "downloadonly" <> short 'w' <> hidden <> help "Download package tarballs.") *> someArgs')


misc :: Parser (S.Set MiscOp)
misc = S.fromList <$> many (ar <|> dbp <|> roo <|> ver <|> gpg <|> hd <|> con <|> dbo <|> nop <|> nos <|> pf <|> nod <|> prt <|> asi)
  where ar  = MiscArch    <$> strOption (long "arch" <> metavar "ARCH" <> hidden <> help "Use an alternate architecture.")
        dbp = MiscDBPath  <$> strOption (long "dbpath" <> short 'b' <> metavar "PATH" <> hidden <> help "Use an alternate database location.")
        roo = MiscRoot    <$> strOption (long "root" <> short 'r' <> metavar "PATH" <> hidden <> help "Use an alternate installation root.")
        ver = flag' MiscVerbose (long "verbose" <> short 'v' <> hidden <> help "Be more verbose.")
        gpg = MiscGpgDir  <$> strOption (long "gpgdir" <> metavar "PATH" <> hidden <> help "Use an alternate GnuGPG directory.")
        hd  = MiscHookDir <$> strOption (long "hookdir" <> metavar "PATH" <> hidden <> help "Use an alternate hook directory.")
        con = flag' MiscConfirm (long "confirm" <> hidden <> help "Always ask for confirmation.")
        dbo = flag' MiscDBOnly (long "dbonly" <> hidden <> help "Only modify database entries, not package files.")
        nop = flag' MiscNoProgress (long "noprogressbar" <> hidden <> help "Don't show a progress bar when downloading.")
        nos = flag' MiscNoScriptlet (long "noscriptlet" <> hidden <> help "Don't run available install scriptlets.")
        pf  = MiscPrintFormat <$> strOption (long "print-format" <> metavar "STRING" <> hidden <> help "Specify how targets should be printed.")
        nod = flag' MiscNoDeps (long "nodeps" <> short 'd' <> hidden <> help "Skip dependency version checks.")
        prt = flag' MiscPrint (long "print" <> short 'p' <> hidden <> help "Print the targets instead of performing the operation.")
        asi = MiscAssumeInstalled <$> strOption (long "assume-installed" <> metavar "<package=version>" <> hidden <> help "Add a virtual package to satisfy dependencies.")

testdeps :: Parser PacmanOp
testdeps = bigT *> (TestDeps <$> someArgs <*> misc)
  where bigT = flag' () (long "deptest" <> short 'T' <> help "Test dependencies - useful for scripts.")

upgrades :: Parser PacmanOp
upgrades = bigU *> (Upgrade <$> optional mods <*> someArgs <*> misc)
  where bigU = flag' () (long "upgrade" <> short 'U' <> help "Install given package files.")
        mods = flag' UpgradeAsDeps (long "asdeps" <> hidden) <|> flag' UpgradeAsExplicit (long "asexplicit" <> hidden)

-- | One or more arguments.
someArgs :: Parser (S.Set T.Text)
someArgs = S.fromList <$> some (argument str (metavar "PACKAGES"))

-- | Same as `someArgs`, but the help message "brief display" won't show PACKAGES.
someArgs' :: Parser (S.Set T.Text)
someArgs' = S.fromList <$> some (argument str (metavar "PACKAGES" <> hidden))

-- | Zero or more arguments.
manyArgs :: Parser (S.Set T.Text)
manyArgs = S.fromList <$> many (argument str (metavar "PACKAGES"))

-- | Zero or more arguments.
manyArgs' :: Parser (S.Set T.Text)
manyArgs' = S.fromList <$> many (argument str (metavar "PACKAGES" <> hidden))

language :: Parser Language
language = foldr1 (<|>) $ map (\(f, v) -> flag' v (long f <> hidden)) langs
  where langs = [ ( "japanese",   Japanese ),   ( "日本語",     Japanese )
                , ( "polish",     Polish ),     ( "polski",    Polish )
                , ( "croatian",   Croatian ),   ( "hrvatski",  Croatian )
                , ( "swedish",    Swedish ),    ( "svenska",   Swedish )
                , ( "german",     German ),     ( "deutsch",   German )
                , ( "spanish",    Spanish ),    ( "español",   Spanish )
                , ( "portuguese", Portuguese ), ( "português", Portuguese )
                , ( "french",     French),      ( "français",  French )
                , ( "russian",    Russian ),    ( "русский",   Russian )
                , ( "italian",    Italian ),    ( "italiano",  Italian )
                , ( "serbian",    Serbian ),    ( "српски",    Serbian )
                , ( "norwegian",  Norwegian ),  ( "norsk",     Norwegian )
                , ( "indonesian", Indonesia )
                , ( "chinese",    Chinese ),    ( "中文",       Chinese ) ]
