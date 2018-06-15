{-# LANGUAGE OverloadedStrings #-}

module Flags
  ( Program(..), opts
  , PacmanOp(..)
  , AuraOp(..), AurOp(..), BackupOp(..), CacheOp(..), LogOp(..), OrphanOp(..)
  ) where

import           Aura.Settings.Base
import           Aura.Types (Language(..))
import           BasePrelude hiding (Version, FilePath, option, log, exp)
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
data PacmanOp = Database (Either DatabaseOp (S.Set T.Text)) (S.Set MiscOp)
              | Files (S.Set FilesOp) (S.Set MiscOp)
              | Query (Either QueryOp (S.Set QueryFilter, (S.Set T.Text))) (S.Set MiscOp)
              | Remove (S.Set RemoveOp) (S.Set T.Text) (S.Set MiscOp)
              | Sync
              | TestDeps
              | Upgrade
              deriving (Show)

instance Flagable PacmanOp where
  asFlag (Database (Left o) ms)      = "-D" : asFlag o ++ concatMap asFlag (toList ms)
  asFlag (Database (Right fs) ms)    = "-D" : toList fs ++ concatMap asFlag (toList ms)
  asFlag (Files os ms)               = "-F" : concatMap asFlag (toList os) ++ concatMap asFlag (toList ms)
  asFlag (Query (Left o) ms)         = "-Q" : asFlag o ++ concatMap asFlag (toList ms)
  asFlag (Query (Right (fs, ps)) ms) = "-Q" : toList ps ++ concatMap asFlag (toList fs) ++ concatMap asFlag (toList ms)
  asFlag (Remove os ps ms)           = "-R" : concatMap asFlag (toList os) ++ toList ps ++ concatMap asFlag (toList ms)

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

-- | Flags common to several Pacman operations.
data MiscOp = MiscArch    FilePath
            | MiscDBPath  FilePath
            | MiscRoot    FilePath
            | MiscVerbose
            | MiscColor   T.Text
            | MiscGpgDir  FilePath
            | MiscHookDir FilePath
            | MiscConfirm
            | MiscDBOnly
            | MiscNoProgress
            | MiscNoScriptlet
            | MiscPrintFormat T.Text
            | MiscNoDeps
            | MiscPrint
            | MiscAssumeInstalled T.Text
            | MiscIgnoreGroup (S.Set T.Text)
            deriving (Eq, Ord, Show)

instance Flagable MiscOp where
  asFlag (MiscArch p)            = ["--arch", toTextIgnore p]
  asFlag (MiscDBPath p)          = ["--dbpath", toTextIgnore p]
  asFlag (MiscRoot p)            = ["--root", toTextIgnore p]
  asFlag MiscVerbose             = ["--verbose"]
  asFlag (MiscColor c)           = ["--color", c]
  asFlag (MiscGpgDir p)          = ["--gpgdir", toTextIgnore p]
  asFlag (MiscHookDir p)         = ["--hookdir", toTextIgnore p]
  asFlag MiscConfirm             = ["--confirm"]
  asFlag MiscDBOnly              = ["--dbonly"]
  asFlag MiscNoProgress          = ["--noprogressbar"]
  asFlag MiscNoScriptlet         = ["--noscriptlet"]
  asFlag (MiscPrintFormat s)     = ["--print-format", s]
  asFlag MiscNoDeps              = ["--nodeps"]
  asFlag MiscPrint               = ["--print"]
  asFlag (MiscAssumeInstalled p) = ["--assume-installed", p]
  asFlag (MiscIgnoreGroup ps)    = "--ignoregroup" : toList ps

-- | Operations unique to Aura.
data AuraOp = AurSync (Either AurOp (S.Set T.Text))
            | Backup  (Maybe  BackupOp)
            | Cache   (Either CacheOp (S.Set T.Text))
            | Log     (Maybe  LogOp)
            | Orphans (Maybe  OrphanOp)
            | Version
            deriving (Show)

data AurOp = AurDeps     (S.Set T.Text)
           | AurInfo     (S.Set T.Text)
           | AurPkgbuild (S.Set T.Text)
           | AurSearch    T.Text
           | AurUpgrade  (S.Set T.Text)
           | AurTarball  (S.Set T.Text)
           deriving (Show)

data BackupOp = BackupClean Word | BackupRestore deriving (Show)

data CacheOp = CacheBackup FilePath | CacheClean Word | CacheSearch T.Text deriving (Show)

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
  where aurOps = aursync <|> backups <|> cache <|> log <|> orphans <|> version
        pacOps = database <|> files <|> queries <|> remove

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
buildConfig = BuildConfig <$> makepkg <*> optional bp <*> optional bu <*> trunc <*> buildSwitches
  where makepkg = S.fromList <$> many (ia <|> as)
        ia      = flag' IgnoreArch (long "ignorearch" <> help "Exposed makepkg flag.")
        as      = flag' AllSource (long "allsource" <> help "Exposed makepkg flag.")
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
commonConfig = CommonConfig <$> optional cap <*> optional cop <*> optional lfp <*> ign <*> commonSwitches
  where cap = strOption (long "cachedir" <> help "Use an alternate package cache location.")
        cop = strOption (long "config"   <> help "Use an alternate Pacman config file.")
        lfp = strOption (long "logfile"  <> help "Use an alternate Pacman log.")
        ign = maybe S.empty (S.fromList . T.split (== ',')) <$>
          optional (strOption (long "ignore" <> metavar "PKG(,PKG,...)" <> help "Ignore given packages."))

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

files :: Parser PacmanOp
files = Files <$> (bigF *> fmap S.fromList (many mods)) <*> misc
  where bigF = flag' () (long "files" <> short 'F' <> help "Interact with the file database.")
        mods = lst <|> own <|> sch <|> rgx <|> rfr <|> mch
        lst  = FilesList <$> (flag' () (long "list" <> short 'l' <> help "List the files owned by given packages.") *> someArgs)
        own  = FilesOwns <$> strOption (long "owns" <> short 'o' <> metavar "FILE" <> help "Query the package that owns FILE.")
        sch  = FilesSearch <$> strOption (long "search" <> short 's' <> metavar "FILE" <> help "Find package files that match the given FILEname.")
        rgx  = flag' FilesRegex (long "regex" <> short 'x' <> help "Interpret the input of -Fs as a regex.")
        rfr  = flag' FilesRefresh (long "refresh" <> short 'y' <> help "Download fresh package databases.")
        mch  = flag' FilesMachineReadable (long "machinereadable" <> help "Produce machine-readable output.")

queries :: Parser PacmanOp
queries = Query <$> (bigQ *> (fmap Right query <|> fmap Left mods)) <*> misc
  where bigQ  = flag' () (long "query" <> short 'Q' <> help "Interact with the local package database.")
        query = (,) <$> queryFilters <*> manyArgs
        mods  = chl <|> gps <|> inf <|> lst <|> own <|> fls <|> sch
        chl   = QueryChangelog <$> (flag' () (long "changelog" <> short 'c' <> help "View a package's changelog.") *> someArgs)
        gps   = QueryGroups <$> (flag' () (long "groups" <> short 'g' <> help "View all members of a package group.") *> someArgs)
        inf   = QueryInfo <$> (flag' () (long "info" <> short 'i' <> help "View package information.") *> someArgs)
        lst   = QueryList <$> (flag' () (long "list" <> short 'l' <> help "List files owned by a package.") *> someArgs)
        own   = QueryOwns <$> (flag' () (long "owns" <> short 'o' <> help "Find the package some file belongs to.") *> someArgs)
        fls   = QueryFile <$> (flag' () (long "file" <> short 'p' <> help "Query a package file.") *> someArgs)
        sch   = QuerySearch <$> strOption (long "search" <> short 's' <> metavar "REGEX" <> help "Search the local database.")

queryFilters :: Parser (S.Set QueryFilter)
queryFilters = S.fromList <$> many (dps <|> exp <|> frg <|> ntv <|> urq <|> upg)
  where dps = flag' QueryDeps (long "deps" <> short 'd' <> help "[filter] Only list packages installed as deps.")
        exp = flag' QueryExplicit (long "explicit" <> short 'e' <> help "[filter] Only list explicitly installed packages.")
        frg = flag' QueryForeign (long "foreign" <> short 'm' <> help "[filter] Only list AUR packages.")
        ntv = flag' QueryNative (long "native" <> short 'n' <> help "[filter] Only list official packages.")
        urq = flag' QueryUnrequired (long "unrequired" <> short 't' <> help "[filter] Only list packages not required as a dependency to any other.")
        upg = flag' QueryUpgrades (long "upgrades" <> short 'u' <> help "[filter] Only list outdated packages.")

remove :: Parser PacmanOp
remove = bigR *> (Remove <$> mods <*> someArgs <*> misc)
  where bigR     = flag' () (long "remove" <> short 'R' <> help "Uninstall packages.")
        mods     = S.fromList <$> many (cascade <|> nosave <|> recurse <|> unneeded)
        cascade  = flag' RemoveCascade (long "cascade" <> short 'c' <> help "Remove packages and all others that depend on them.")
        nosave   = flag' RemoveNoSave (long "nosave" <> short 'n' <> help "Remove configuration files as well.")
        recurse  = flag' RemoveRecursive (long "recursive" <> short 's' <> help "Remove unneeded dependencies.")
        unneeded = flag' RemoveUnneeded (long "unneeded" <> short 'u' <> help "Remove unneeded packages.")

misc :: Parser (S.Set MiscOp)
misc = S.fromList <$> many (ar <|> dbp <|> roo <|> ver <|> clr <|> gpg <|> hd <|> con <|> dbo <|> nop <|> nos <|> pf <|> nod <|> prt <|> asi <|> igg)
  where ar  = MiscArch    <$> strOption (long "arch" <> metavar "ARCH" <> help "Use an alternate architecture.")
        dbp = MiscDBPath  <$> strOption (long "dbpath" <> short 'b' <> metavar "PATH" <> help "Use an alternate database location.")
        roo = MiscRoot    <$> strOption (long "root" <> short 'r' <> metavar "PATH" <> help "Use an alternate installation root.")
        ver = flag' MiscVerbose (long "verbose" <> short 'v' <> help "Be more verbose.")
        clr = MiscColor   <$> strOption (long "color" <> metavar "WHEN" <> help "Colourize the output.")
        gpg = MiscGpgDir  <$> strOption (long "gpgdir" <> metavar "PATH" <> help "Use an alternate GnuGPG directory.")
        hd  = MiscHookDir <$> strOption (long "hookdir" <> metavar "PATH" <> help "Use an alternate hook directory.")
        con = flag' MiscConfirm (long "confirm" <> help "Always ask for confirmation.")
        dbo = flag' MiscDBOnly (long "dbonly" <> help "Only modify database entries, not package files.")
        nop = flag' MiscNoProgress (long "noprogressbar" <> help "Don't show a progress bar when downloading.")
        nos = flag' MiscNoScriptlet (long "noscriptlet" <> help "Don't run available install scriptlets.")
        pf  = MiscPrintFormat <$> strOption (long "print-format" <> metavar "STRING" <> help "Specify how targets should be printed.")
        nod = flag' MiscNoDeps (long "nodeps" <> short 'd' <> help "Skip dependency version checks.")
        prt = flag' MiscPrint (long "print" <> short 'p' <> help "Print the targets instead of performing the operation.")
        asi = MiscAssumeInstalled <$> strOption (long "assume-installed" <> metavar "<package=version>" <> help "Add a virtual package to satisfy dependencies.")
        igg = MiscIgnoreGroup . maybe S.empty (S.fromList . T.split (== ',')) <$>
          optional (strOption (long "ignoregroup" <> metavar "PKG(,PKG,...)" <> help "Ignore given package groups."))

-- | One or more arguments.
someArgs :: Parser (S.Set T.Text)
someArgs = S.fromList <$> some (argument str (metavar "PACKAGES"))

-- | Zero or more arguments.
manyArgs :: Parser (S.Set T.Text)
manyArgs = S.fromList <$> many (argument str (metavar "PACKAGES"))

language :: Parser Language
language = pure English
