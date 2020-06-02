module Aura.Flags
  ( Program(..), opts
  , PacmanOp( Sync ), SyncOp( SyncUpgrade ), SyncSwitch(..), MiscOp
  , AuraOp(..), AurSwitch(..), _AurSync, _AurIgnore, _AurIgnoreGroup
  , AurOp(..), BackupOp(..), CacheOp(..), LogOp(..), OrphanOp(..), AnalysisOp(..)
  ) where

import           Aura.Cache (defaultPackageCache)
import           Aura.Pacman (defaultLogFile, pacmanConfFile)
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (Traversal')
import           Options.Applicative
import           RIO hiding (exp, log)
import           RIO.FilePath
import           RIO.List.Partial (foldr1)
import qualified RIO.NonEmpty as NEL
import qualified RIO.NonEmpty.Partial as NELP
import qualified RIO.Set as S
import qualified RIO.Text as T

---

-- | A description of a run of Aura to attempt.
data Program = Program {
  _operation   :: Either (PacmanOp, Set MiscOp) AuraOp
  -- ^ Whether Aura handles everything, or the ops and input are just passed down to Pacman.
  , _commons   :: CommonConfig
  -- ^ Settings common to both Aura and Pacman.
  , _buildConf :: BuildConfig
  -- ^ Settings specific to building packages.
  , _language  :: Maybe Language
  -- ^ The human language of text output.
  , _logLevel  :: LogLevel
  -- ^ The default RIO logging level.
  } deriving (Show)

-- | Inherited operations that are fed down to Pacman.
data PacmanOp
  = Database (Either DatabaseOp (NonEmpty PkgName))
  | Files    (Set FilesOp)
  | Query    (Either QueryOp (Set QueryFilter, Set PkgName))
  | Remove   (Set RemoveOp) (NonEmpty PkgName)
  | Sync     (Either (NonEmpty SyncOp) (Set PkgName)) (Set SyncSwitch)
  | TestDeps (NonEmpty Text)
  | Upgrade  (Set UpgradeSwitch) (NonEmpty PkgName)
  deriving (Show)

instance Flagable PacmanOp where
  asFlag (Database (Left o))      = "-D" : asFlag o
  asFlag (Database (Right fs))    = "-D" : asFlag fs
  asFlag (Files os)               = "-F" : asFlag os
  asFlag (Query (Left o))         = "-Q" : asFlag o
  asFlag (Query (Right (fs, ps))) = "-Q" : asFlag ps ++ asFlag fs
  asFlag (Remove os ps)           = "-R" : asFlag os ++ asFlag ps
  asFlag (Sync (Left o) ss)       = "-S" : asFlag ss ++ asFlag o
  asFlag (Sync (Right ps) ss)     = "-S" : asFlag ss ++ asFlag ps
  asFlag (TestDeps ps)            = "-T" : asFlag ps
  asFlag (Upgrade s ps)           = "-U" : asFlag s ++ asFlag ps

data DatabaseOp
  = DBCheck
  | DBAsDeps     (NonEmpty Text)
  | DBAsExplicit (NonEmpty Text)
  deriving (Show)

instance Flagable DatabaseOp where
  asFlag DBCheck           = ["--check"]
  asFlag (DBAsDeps ps)     = "--asdeps" : asFlag ps
  asFlag (DBAsExplicit ps) = "--asexplicit" : asFlag ps

data FilesOp
  = FilesList  (NonEmpty Text)
  | FilesOwns   Text
  | FilesSearch Text
  | FilesRegex
  | FilesRefresh
  | FilesMachineReadable
  deriving (Eq, Ord, Show)

instance Flagable FilesOp where
  asFlag (FilesList fs)       = "--list" : asFlag fs
  asFlag (FilesOwns f)        = ["--owns", f]
  asFlag (FilesSearch f)      = ["--search", f]
  asFlag FilesRegex           = ["--regex"]
  asFlag FilesRefresh         = ["--refresh"]
  asFlag FilesMachineReadable = ["--machinereadable"]

data QueryOp
  = QueryChangelog (NonEmpty Text)
  | QueryGroups    (NonEmpty Text)
  | QueryInfo      (NonEmpty Text)
  | QueryCheck     (NonEmpty Text)
  | QueryList      (NonEmpty Text)
  | QueryOwns      (NonEmpty Text)
  | QueryFile      (NonEmpty Text)
  | QuerySearch     Text
  deriving (Show)

instance Flagable QueryOp where
  asFlag (QueryChangelog ps) = "--changelog" : asFlag ps
  asFlag (QueryGroups ps)    = "--groups" : asFlag ps
  asFlag (QueryInfo ps)      = "--info" : asFlag ps
  asFlag (QueryCheck ps)     = "--check" : asFlag ps
  asFlag (QueryList ps)      = "--list" : asFlag ps
  asFlag (QueryOwns ps)      = "--owns" : asFlag ps
  asFlag (QueryFile ps)      = "--file" : asFlag ps
  asFlag (QuerySearch t)     = ["--search", t]

data QueryFilter
  = QueryDeps
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

data RemoveOp
  = RemoveCascade
  | RemoveNoSave
  | RemoveRecursive
  | RemoveUnneeded
  deriving (Eq, Ord, Show)

instance Flagable RemoveOp where
  asFlag RemoveCascade   = ["--cascade"]
  asFlag RemoveNoSave    = ["--nosave"]
  asFlag RemoveRecursive = ["--recursive"]
  asFlag RemoveUnneeded  = ["--unneeded"]

data SyncOp
  = SyncClean
  | SyncGroups   (NonEmpty Text)
  | SyncInfo     (NonEmpty Text)
  | SyncList      Text
  | SyncSearch   (NonEmpty Text)
  | SyncUpgrade  (Set Text)
  | SyncDownload (NonEmpty Text)
  deriving (Eq, Ord, Show)

instance Flagable SyncOp where
  asFlag SyncClean         = ["--clean"]
  asFlag (SyncGroups gs)   = "--groups" : asFlag gs
  asFlag (SyncInfo ps)     = "--info" : asFlag ps
  asFlag (SyncList r)      = ["--list", r]
  asFlag (SyncSearch s)    = "--search" : asFlag s
  asFlag (SyncUpgrade ps)  = "--sysupgrade" : asFlag ps
  asFlag (SyncDownload ps) = "--downloadonly" : asFlag ps

data SyncSwitch
  = SyncRefresh
  | SyncIgnore      (Set PkgName)
  | SyncIgnoreGroup (Set PkgGroup)
  | SyncOverwrite   Text
  deriving (Eq, Ord, Show)

instance Flagable SyncSwitch where
  asFlag SyncRefresh          = ["--refresh"]
  asFlag (SyncIgnore ps)      = ["--ignore", T.intercalate "," $ asFlag ps ]
  asFlag (SyncIgnoreGroup gs) = ["--ignoregroup" , T.intercalate "," $ asFlag gs ]
  asFlag (SyncOverwrite glob) = "--overwrite" : asFlag glob

data UpgradeSwitch
  = UpgradeAsDeps
  | UpgradeAsExplicit
  | UpgradeIgnore      (Set PkgName)
  | UpgradeIgnoreGroup (Set PkgGroup)
  | UpgradeOverwrite   Text
  deriving (Eq, Ord, Show)

instance Flagable UpgradeSwitch where
  asFlag UpgradeAsDeps           = ["--asdeps"]
  asFlag UpgradeAsExplicit       = ["--asexplicit"]
  asFlag (UpgradeIgnore ps)      = ["--ignore", T.intercalate "," $ asFlag ps ]
  asFlag (UpgradeIgnoreGroup gs) = ["--ignoregroup", T.intercalate "," $ asFlag gs ]
  asFlag (UpgradeOverwrite glob) = "--overwrite" : asFlag glob

-- | Flags common to several Pacman operations.
data MiscOp
  = MiscArch    FilePath
  | MiscAssumeInstalled Text
  | MiscColor   Text
  | MiscConfirm
  | MiscDBOnly
  | MiscDBPath  FilePath
  | MiscGpgDir  FilePath
  | MiscHookDir FilePath
  | MiscNoDeps
  | MiscNoProgress
  | MiscNoScriptlet
  | MiscPrint
  | MiscPrintFormat Text
  | MiscRoot    FilePath
  | MiscVerbose
  deriving (Eq, Ord, Show)

instance Flagable MiscOp where
  asFlag (MiscArch p)            = ["--arch", T.pack p]
  asFlag (MiscAssumeInstalled p) = ["--assume-installed", p]
  asFlag (MiscColor c)           = ["--color", c]
  asFlag (MiscDBPath p)          = ["--dbpath", T.pack p]
  asFlag (MiscGpgDir p)          = ["--gpgdir", T.pack p]
  asFlag (MiscHookDir p)         = ["--hookdir", T.pack p]
  asFlag (MiscPrintFormat s)     = ["--print-format", s]
  asFlag (MiscRoot p)            = ["--root", T.pack p]
  asFlag MiscConfirm             = ["--confirm"]
  asFlag MiscDBOnly              = ["--dbonly"]
  asFlag MiscNoDeps              = ["--nodeps"]
  asFlag MiscNoProgress          = ["--noprogressbar"]
  asFlag MiscNoScriptlet         = ["--noscriptlet"]
  asFlag MiscPrint               = ["--print"]
  asFlag MiscVerbose             = ["--verbose"]

-- | Operations unique to Aura.
data AuraOp
  = AurSync (Either AurOp (NonEmpty PkgName)) (Set AurSwitch)
  | Backup  (Maybe  BackupOp)
  | Cache   (Either CacheOp (NonEmpty PkgName))
  | Log     (Maybe  LogOp)
  | Orphans (Maybe  OrphanOp)
  | Analysis (Maybe AnalysisOp)
  | Version
  | Languages
  | ViewConf
  deriving (Show)

_AurSync :: Traversal' AuraOp (Set AurSwitch)
_AurSync f (AurSync o s) = AurSync o <$> f s
_AurSync _ x             = pure x

data AurOp
  = AurDeps     (NonEmpty PkgName)
  | AurInfo     (NonEmpty PkgName)
  | AurPkgbuild (NonEmpty PkgName)
  | AurSearch    Text
  | AurUpgrade  (Set PkgName)
  | AurJson     (NonEmpty PkgName)
  | AurTarball  (NonEmpty PkgName)
  deriving (Show)

data AurSwitch
  = AurIgnore      (Set PkgName)
  | AurIgnoreGroup (Set PkgGroup)
  | AurRepoSync
  deriving (Eq, Ord, Show)

_AurIgnore :: Traversal' AurSwitch (Set PkgName)
_AurIgnore f (AurIgnore s) = AurIgnore <$> f s
_AurIgnore _ x             = pure x

_AurIgnoreGroup :: Traversal' AurSwitch (Set PkgGroup)
_AurIgnoreGroup f (AurIgnoreGroup s) = AurIgnoreGroup <$> f s
_AurIgnoreGroup _ x                  = pure x

data BackupOp
  = BackupClean Word
  | BackupRestore
  | BackupList deriving (Show)

data CacheOp
  = CacheBackup FilePath
  | CacheClean Word
  | CacheCleanNotSaved
  | CacheSearch Text
  deriving (Show)

data LogOp
  = LogInfo (NonEmpty PkgName)
  | LogSearch Text
  deriving (Show)

data OrphanOp
  = OrphanAbandon
  | OrphanAdopt (NonEmpty PkgName)
  deriving (Show)

data AnalysisOp
  = AnalysisFile FilePath
  | AnalysisDir FilePath
  | AnalysisAudit
  deriving (Show)

opts :: ParserInfo Program
opts = info (program <**> helper)
  (fullDesc <> header "Aura - Package manager for Arch Linux and the AUR.")

program :: Parser Program
program = Program
  <$> (fmap Right aurOps <|> (curry Left <$> pacOps <*> misc))
  <*> commonConfig
  <*> buildConfig
  <*> optional language
  <*> logLevel
  where
    aurOps = aursync <|> backups <|> cache <|> log <|> orphans <|> analysis <|> version' <|> languages <|> viewconf
    pacOps = database <|> files <|> queries <|> remove <|> sync <|> testdeps <|> upgrades

aursync :: Parser AuraOp
aursync = bigA *>
  (AurSync
   <$> (fmap (Right . NEL.map (PkgName . T.toLower)) someArgs <|> fmap Left mods)
   <*> (S.fromList <$> many switches)
  )
  where bigA    = flag' () (long "aursync" <> short 'A' <> help "Install packages from the AUR.")
        mods    = ds <|> ainfo <|> pkgb <|> search <|> upgrade <|> aur <|> tarball
        ds      = AurDeps <$> (flag' () (long "deps" <> short 'd' <> hidden <> help "View dependencies of an AUR package.") *> somePkgs')
        ainfo   = AurInfo <$> (flag' () (long "info" <> short 'i' <> hidden <> help "View AUR package information.") *> somePkgs')
        pkgb    = AurPkgbuild <$> (flag' () (long "pkgbuild" <> short 'p' <> hidden <> help "View an AUR package's PKGBUILD file.") *> somePkgs')
        search  = AurSearch <$> strOption (long "search" <> short 's' <> metavar "STRING" <> hidden <> help "Search the AUR via a search string.")
        upgrade = AurUpgrade <$> (flag' () (long "sysupgrade" <> short 'u' <> hidden <> help "Upgrade all installed AUR packages.") *> fmap (S.map PkgName) manyArgs')
        aur     = AurJson <$> (flag' () (long "json" <> hidden <> help "Retrieve package JSON straight from the AUR.") *> somePkgs')
        tarball = AurTarball <$> (flag' () (long "downloadonly" <> short 'w' <> hidden <> help "Download a package tarball.") *> somePkgs')
        switches = ign <|> igg <|> y
        ign  = AurIgnore . S.fromList . map PkgName . T.split (== ',')
          <$> strOption (long "ignore" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore given packages.")
        igg  = AurIgnoreGroup . S.fromList . map PkgGroup . T.split (== ',')
          <$> strOption (long "ignoregroup" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore packages from the given groups.")
        y    = flag' AurRepoSync (short 'y' <> hidden <> help "Do an -Sy before continuing.")

backups :: Parser AuraOp
backups = bigB *> (Backup <$> optional mods)
  where bigB = flag' () (long "save" <> short 'B' <> help "Save a package state.")
        mods = clean <|> restore <|> lst
        clean = BackupClean <$> option auto (long "clean" <> short 'c' <> metavar "N" <> hidden <> help "Keep the most recent N states, delete the rest.")
        restore = flag' BackupRestore (long "restore" <> short 'r' <> hidden <> help "Restore a previous package state.")
        lst = flag' BackupList (long "list" <> short 'l' <> hidden <> help "Show all saved package state filenames.")

cache :: Parser AuraOp
cache = bigC *> (Cache <$> (fmap Left mods <|> fmap Right somePkgs))
  where bigC = flag' () (long "downgrade" <> short 'C' <> help "Interact with the package cache.")
        mods = backup <|> clean <|> clean' <|> search
        backup = CacheBackup <$> option (eitherReader absFilePath) (long "backup" <> short 'b' <> metavar "PATH" <> help "Backup the package cache to a given directory." <> hidden)
        clean  = CacheClean  <$> option auto (long "clean" <> short 'c' <> metavar "N" <> help "Save the most recent N versions of a package in the cache, deleting the rest." <> hidden)
        clean' = flag' CacheCleanNotSaved (long "notsaved" <> help "Clean out any cached package files which doesn't appear in any saved state." <> hidden)
        search = CacheSearch <$> strOption (long "search" <> short 's' <> metavar "STRING" <> help "Search the package cache via a search string." <> hidden)

log :: Parser AuraOp
log = bigL *> (Log <$> optional mods)
  where bigL = flag' () (long "viewlog" <> short 'L' <> help "View the Pacman log.")
        mods = inf <|> sch
        inf  = LogInfo <$> (flag' () (long "info" <> short 'i' <> help "Display the installation history for given packages." <> hidden) *> somePkgs')
        sch  = LogSearch <$> strOption (long "search" <> short 's' <> metavar "STRING" <> help "Search the Pacman log via a search string." <> hidden)

orphans :: Parser AuraOp
orphans = bigO *> (Orphans <$> optional mods)
  where bigO    = flag' () (long "orphans" <> short 'O' <> help "Display all orphan packages.")
        mods    = abandon <|> adopt
        abandon = flag' OrphanAbandon (long "abandon" <> short 'j' <> hidden <> help "Uninstall all orphan packages.")
        adopt   = OrphanAdopt <$> (flag' () (long "adopt" <> short 'a' <> hidden <> help "Mark some packages' install reason as 'Explicit'.") *> somePkgs')

analysis :: Parser AuraOp
analysis = bigP *> (Analysis <$> optional mods)
  where
    bigP = flag' () (long "analysis" <> short 'P' <> help "Analyse PKGBUILDs for malicious bash code.")
    mods = file <|> dir <|> audit
    file = AnalysisFile <$> strOption (long "file" <> short 'f' <> metavar "PATH" <> hidden <> help "Path to a PKGBUILD file.")
    dir = AnalysisDir <$> strOption (long "dir" <> short 'd' <> metavar "PATH" <> hidden <> help "Path to a directory containing a PKGBUILD file.")
    audit = flag' AnalysisAudit (long "audit" <> short 'a' <> hidden <> help "Analyse PKGBUILDs of installed AUR packages.")

version' :: Parser AuraOp
version' = flag' Version (long "version" <> short 'V' <> help "Display Aura's version.")

languages :: Parser AuraOp
languages = flag' Languages (long "languages" <> help "Show all human languages available for output.")

viewconf :: Parser AuraOp
viewconf = flag' ViewConf (long "viewconf" <> help "View the Pacman config file.")

buildConfig :: Parser BuildConfig
buildConfig = BuildConfig <$> makepkg <*> bp <*> bu <*> asp <*> vp <*> trunc <*> buildSwitches
  where makepkg = S.fromList <$> many (ia <|> as <|> si <|> sp)
        ia      = flag' IgnoreArch (long "ignorearch" <> hidden <> help "Exposed makepkg flag.")
        as      = flag' AllSource (long "allsource" <> hidden <> help "Exposed makepkg flag.")
        si      = flag' SkipInteg (long "skipinteg" <> hidden <> help "Skip all makepkg integrity checks.")
        sp      = flag' SkipPGP (long "skippgpcheck" <> hidden <> help "Skip all makepkg PGP checks.")
        bp      = optional $ option (eitherReader absFilePath) (long "build" <> metavar "PATH" <> hidden <> help "Directory in which to build packages.")
        bu      = optional $ User <$> strOption (long "builduser" <> metavar "USER" <> hidden <> help "User account to build as.")
        asp     = optional $ option (eitherReader absFilePath) (long "allsourcepath" <> metavar "PATH" <> hidden <> help "Directory in which to store the output of --allsource.")
        vp      = optional $ option (eitherReader absFilePath) (long "vcspath" <> metavar "PATH" <> hidden <> help "Directory in which to build and store VCS packages.")
        trunc   = fmap Head (option auto (long "head" <> metavar "N" <> hidden <> help "Only show top N search results."))
          <|> fmap Tail (option auto (long "tail" <> metavar "N" <> hidden <> help "Only show last N search results."))
          <|> pure None

buildSwitches :: Parser (Set BuildSwitch)
buildSwitches = S.fromList <$> many (lv <|> dmd <|> dsm <|> dpb <|> rbd <|> he <|> dr <|> sa <|> fo <|> npc <|> asd <|> sdc)
  where dmd = flag' DeleteMakeDeps (long "delmakedeps" <> short 'a' <> hidden <> help "Uninstall makedeps after building.")
        dsm = flag' DontSuppressMakepkg (long "unsuppress" <> short 'x' <> hidden <> help "Unsuppress makepkg output.")
        dpb = flag' DiffPkgbuilds (long "diff" <> short 'k' <> hidden <> help "Show PKGBUILD diffs.")
        rbd = flag' RebuildDevel (long "devel" <> hidden <> help "Rebuild all git/hg/svn/darcs-based packages.")
        he  = flag' HotEdit (long "hotedit" <> hidden <> help "Edit a PKGBUILD before building.")
        dr  = flag' DryRun (long "dryrun" <> hidden <> help "Run dependency checks and PKGBUILD diffs, but don't build.")
        sa  = flag' SortAlphabetically (long "abc" <> hidden <> help "Sort search results alphabetically.")
        lv  = flag' LowVerbosity (long "quiet" <> short 'q' <> hidden <> help "Display less information.")
        fo  = flag' ForceBuilding (long "force" <> hidden <> help "Always (re)build specified packages.")
        npc = flag' NoPkgbuildCheck (long "noanalysis" <> hidden <> help "Do not analyse PKGBUILDs for security flaws.")
        asd = flag' AsDeps (long "asdeps" <> hidden <> help "All installed packages will be marked as dependencies.")
        sdc = flag' SkipDepCheck (long "skipdepcheck" <> hidden <> help "Don't perform dependency solving.")

commonConfig :: Parser CommonConfig
commonConfig = CommonConfig <$> cap <*> cop <*> lfp <*> commonSwitches
  where cap = fmap Right
                   (option (eitherReader absFilePath) (long "cachedir" <> hidden <> help "Use an alternate package cache location."))
              <|> pure (Left defaultPackageCache)
        cop = fmap Right
                   (option (eitherReader absFilePath) (long "config"   <> hidden <> help "Use an alternate Pacman config file."))
              <|> pure (Left pacmanConfFile)
        lfp = fmap Right
                   (option (eitherReader absFilePath) (long "logfile"  <> hidden <> help "Use an alternate Pacman log."))
              <|> pure (Left defaultLogFile)

commonSwitches :: Parser (Set CommonSwitch)
commonSwitches = S.fromList <$> many (nc <|> no <|> dbg <|> clr <|> ovr)
  where nc  = flag' NoConfirm  (long "noconfirm" <> hidden <> help "Never ask for Aura or Pacman confirmation.")
        no  = flag' NeededOnly (long "needed"    <> hidden <> help "Don't rebuild/reinstall up-to-date packages.")
        dbg = flag' Debug      (long "debug"     <> hidden <> help "Print useful debugging info.")
        ovr = Overwrite <$> strOption (long "overwrite" <> hidden <> help "Bypass file conflict checks." <> metavar "GLOB")
        clr = Colour . f <$> strOption (long "color" <> metavar "WHEN" <> hidden <> help "Colourize the output.")
        f :: String -> ColourMode
        f "never"  = Never
        f "always" = Always
        f _        = Auto

database :: Parser PacmanOp
database = bigD *> (Database <$> (fmap Right somePkgs <|> fmap Left mods))
  where bigD   = flag' () (long "database" <> short 'D' <> help "Interact with the package database.")
        mods   = check <|> asdeps <|> asexp
        check  = flag' DBCheck (long "check" <> short 'k' <> hidden <> help "Test local database validity.")
        asdeps = DBAsDeps <$> (flag' () (long "asdeps" <> hidden <> help "Mark packages as being dependencies.") *> someArgs')
        asexp  = DBAsExplicit <$> (flag' () (long "asexplicit" <> hidden <> help "Mark packages as being explicitely installed.") *> someArgs')

files :: Parser PacmanOp
files = bigF *> (Files <$> fmap S.fromList (many mods))
  where bigF = flag' () (long "files" <> short 'F' <> help "Interact with the file database.")
        mods = lst <|> own <|> sch <|> rgx <|> rfr <|> mch
        lst  = FilesList <$> (flag' () (long "list" <> short 'l' <> hidden <> help "List the files owned by given packages.") *> someArgs')
        own  = FilesOwns <$> strOption (long "owns" <> short 'o' <> metavar "FILE" <> hidden <> help "Query the package that owns FILE.")
        sch  = FilesSearch <$> strOption (long "search" <> short 's' <> metavar "FILE" <> hidden <> help "Find package files that match the given FILEname.")
        rgx  = flag' FilesRegex (long "regex" <> short 'x' <> hidden <> help "Interpret the input of -Fs as a regex.")
        rfr  = flag' FilesRefresh (long "refresh" <> short 'y' <> hidden <> help "Download fresh package databases.")
        mch  = flag' FilesMachineReadable (long "machinereadable" <> hidden <> help "Produce machine-readable output.")

queries :: Parser PacmanOp
queries = bigQ *> (Query <$> (fmap Right query <|> fmap Left mods))
  where bigQ  = flag' () (long "query" <> short 'Q' <> help "Interact with the local package database.")
        query = curry (second (S.map PkgName)) <$> queryFilters <*> manyArgs
        mods  = chl <|> gps <|> inf <|> lst <|> own <|> fls <|> sch <|> chk
        chl   = QueryChangelog <$> (flag' () (long "changelog" <> short 'c' <> hidden <> help "View a package's changelog.") *> someArgs')
        gps   = QueryGroups <$> (flag' () (long "groups" <> short 'g' <> hidden <> help "View all members of a package group.") *> someArgs')
        inf   = QueryInfo <$> (flag' () (long "info" <> short 'i' <> hidden <> help "View package information.") *> someArgs')
        lst   = QueryList <$> (flag' () (long "list" <> short 'l' <> hidden <> help "List files owned by a package.") *> someArgs')
        chk = QueryCheck <$> (flag' () (long "check" <> short 'k' <> hidden <> help "Check that package files exist.") *> someArgs')
        own   = QueryOwns <$> (flag' () (long "owns" <> short 'o' <> hidden <> help "Find the package some file belongs to.") *> someArgs')
        fls   = QueryFile <$> (flag' () (long "file" <> short 'p' <> hidden <> help "Query a package file.") *> someArgs')
        sch   = QuerySearch <$> strOption (long "search" <> short 's' <> metavar "REGEX" <> hidden <> help "Search the local database.")

queryFilters :: Parser (Set QueryFilter)
queryFilters = S.fromList <$> many (dps <|> exp <|> frg <|> ntv <|> urq <|> upg)
  where dps = flag' QueryDeps (long "deps" <> short 'd' <> hidden <> help "[filter] Only list packages installed as deps.")
        exp = flag' QueryExplicit (long "explicit" <> short 'e' <> hidden <> help "[filter] Only list explicitly installed packages.")
        frg = flag' QueryForeign (long "foreign" <> short 'm' <> hidden <> help "[filter] Only list AUR packages.")
        ntv = flag' QueryNative (long "native" <> short 'n' <> hidden <> help "[filter] Only list official packages.")
        urq = flag' QueryUnrequired (long "unrequired" <> short 't' <> hidden <> help "[filter] Only list packages not required as a dependency to any other.")
        upg = flag' QueryUpgrades (long "upgrades" <> short 'u' <> hidden <> help "[filter] Only list outdated packages.")

remove :: Parser PacmanOp
remove = bigR *> (Remove <$> mods <*> somePkgs)
  where bigR     = flag' () (long "remove" <> short 'R' <> help "Uninstall packages.")
        mods     = S.fromList <$> many (cascade <|> nosave <|> recurse <|> unneeded)
        cascade  = flag' RemoveCascade (long "cascade" <> short 'c' <> hidden <> help "Remove packages and all others that depend on them.")
        nosave   = flag' RemoveNoSave (long "nosave" <> short 'n' <> hidden <> help "Remove configuration files as well.")
        recurse  = flag' RemoveRecursive (long "recursive" <> short 's' <> hidden <> help "Remove unneeded dependencies.")
        unneeded = flag' RemoveUnneeded (long "unneeded" <> short 'u' <> hidden <> help "Remove unneeded packages.")

sync :: Parser PacmanOp
sync = bigS *> (Sync <$> (fmap (Right . S.map PkgName) manyArgs <|> fmap Left mods) <*> (S.fromList <$> many (ref <|> ign <|> igg)))
  where bigS = flag' () (long "sync" <> short 'S' <> help "Install official packages.")
        ref  = flag' SyncRefresh (long "refresh" <> short 'y' <> hidden <> help "Update the package database.")
        mods = NELP.fromList <$> some (cln <|> gps <|> inf <|> lst <|> sch <|> upg <|> dnl)
        cln  = flag' SyncClean (long "clean" <> short 'c' <> hidden <> help "Remove old packages from the cache.")
        gps  = SyncGroups <$> (flag' () (long "groups" <> short 'g' <> hidden <> help "View members of a package group.") *> someArgs')
        inf  = SyncInfo <$> (flag' () (long "info" <> short 'i' <> hidden <> help "View package information.") *> someArgs')
        lst  = SyncList <$> strOption (long "list" <> short 'l' <> metavar "REPO" <> hidden <> help "List the packages in a REPO.")
        sch  = SyncSearch <$> (flag' () (long "search" <> short 's' <> hidden <> help "Search the official package repos.") *> someArgs')
        upg  = SyncUpgrade <$> (flag' () (long "sysupgrade" <> short 'u' <> hidden <> help "Upgrade installed packages.") *> manyArgs')
        dnl  = SyncDownload <$> (flag' () (long "downloadonly" <> short 'w' <> hidden <> help "Download package tarballs.") *> someArgs')
        ign  = SyncIgnore . S.fromList . map PkgName . T.split (== ',') <$>
          strOption (long "ignore" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore given packages.")
        igg  = SyncIgnoreGroup . S.fromList . map PkgGroup . T.split (== ',') <$>
          strOption (long "ignoregroup" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore packages from the given groups.")

misc :: Parser (Set MiscOp)
misc = S.fromList <$> many (ar <|> dbp <|> roo <|> ver <|> gpg <|> hd <|> con <|> dbo <|> nop <|> nos <|> pf <|> nod <|> prt <|> asi)
  where ar  = MiscArch
              <$> option (eitherReader absFilePath) (long "arch" <> metavar "ARCH" <> hidden <> help "Use an alternate architecture.")
        dbp = MiscDBPath
              <$> option (eitherReader absFilePath) (long "dbpath" <> short 'b' <> metavar "PATH" <> hidden <> help "Use an alternate database location.")
        roo = MiscRoot
              <$> option (eitherReader absFilePath) (long "root" <> short 'r' <> metavar "PATH" <> hidden <> help "Use an alternate installation root.")
        ver = flag' MiscVerbose (long "verbose" <> short 'v' <> hidden <> help "Be more verbose.")
        gpg = MiscGpgDir
              <$> option (eitherReader absFilePath) (long "gpgdir" <> metavar "PATH" <> hidden <> help "Use an alternate GnuGPG directory.")
        hd  = MiscHookDir
              <$> option (eitherReader absFilePath) (long "hookdir" <> metavar "PATH" <> hidden <> help "Use an alternate hook directory.")
        con = flag' MiscConfirm (long "confirm" <> hidden <> help "Always ask for confirmation.")
        dbo = flag' MiscDBOnly (long "dbonly" <> hidden <> help "Only modify database entries, not package files.")
        nop = flag' MiscNoProgress (long "noprogressbar" <> hidden <> help "Don't show a progress bar when downloading.")
        nos = flag' MiscNoScriptlet (long "noscriptlet" <> hidden <> help "Don't run available install scriptlets.")
        pf  = MiscPrintFormat <$> strOption (long "print-format" <> metavar "STRING" <> hidden <> help "Specify how targets should be printed.")
        nod = flag' MiscNoDeps (long "nodeps" <> short 'd' <> hidden <> help "Skip dependency version checks.")
        prt = flag' MiscPrint (long "print" <> short 'p' <> hidden <> help "Print the targets instead of performing the operation.")
        asi = MiscAssumeInstalled <$> strOption (long "assume-installed" <> metavar "<package=version>" <> hidden <> help "Add a virtual package to satisfy dependencies.")

testdeps :: Parser PacmanOp
testdeps = bigT *> (TestDeps <$> someArgs)
  where bigT = flag' () (long "deptest" <> short 'T' <> help "Test dependencies - useful for scripts.")

upgrades :: Parser PacmanOp
upgrades = bigU *> (Upgrade <$> (S.fromList <$> many mods) <*> somePkgs)
  where bigU = flag' () (long "upgrade" <> short 'U' <> help "Install given package files.")
        mods = asd <|> ase <|> ign <|> igg
        asd = flag' UpgradeAsDeps (long "asdeps" <> hidden)
        ase = flag' UpgradeAsExplicit (long "asexplicit" <> hidden)
        ign  = UpgradeIgnore . S.fromList . map PkgName . T.split (== ',') <$>
          strOption (long "ignore" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore given packages.")
        igg  = UpgradeIgnoreGroup . S.fromList . map PkgGroup . T.split (== ',') <$>
          strOption (long "ignoregroup" <> metavar "PKG(,PKG,...)" <> hidden <> help "Ignore packages from the given groups.")

somePkgs :: Parser (NonEmpty PkgName)
somePkgs = NELP.fromList . map PkgName <$> some (argument str (metavar "PACKAGES"))

-- | Same as `someArgs`, but the help message "brief display" won't show PACKAGES.
somePkgs' :: Parser (NonEmpty PkgName)
somePkgs' = NELP.fromList . map PkgName <$> some (argument str (metavar "PACKAGES" <> hidden))

-- | One or more arguments.
someArgs :: Parser (NonEmpty Text)
someArgs = NEL.nub . NELP.fromList <$> some (argument str (metavar "PACKAGES"))

-- | Same as `someArgs`, but the help message "brief display" won't show PACKAGES.
someArgs' :: Parser (NonEmpty Text)
someArgs' = NEL.nub . NELP.fromList <$> some (argument str (metavar "PACKAGES" <> hidden))

-- | Zero or more arguments.
manyArgs :: Parser (Set Text)
manyArgs = S.fromList <$> many (argument str (metavar "PACKAGES"))

-- | Zero or more arguments.
manyArgs' :: Parser (Set Text)
manyArgs' = S.fromList <$> many (argument str (metavar "PACKAGES" <> hidden))

language :: Parser Language
language = foldr1 (<|>) $ map (\(f, v) -> flag' v (long f <> hidden)) langs
  where langs = [ ( "japanese",   Japanese ),   ( "日本語",        Japanese )
                , ( "polish",     Polish ),     ( "polski",     Polish )
                , ( "croatian",   Croatian ),   ( "hrvatski",   Croatian )
                , ( "swedish",    Swedish ),    ( "svenska",    Swedish )
                , ( "german",     German ),     ( "deutsch",    German )
                , ( "spanish",    Spanish ),    ( "español",    Spanish )
                , ( "portuguese", Portuguese ), ( "português",  Portuguese )
                , ( "french",     French),      ( "français",   French )
                , ( "russian",    Russian ),    ( "русский",    Russian )
                , ( "italian",    Italian ),    ( "italiano",   Italian )
                , ( "serbian",    Serbian ),    ( "српски",     Serbian )
                , ( "norwegian",  Norwegian ),  ( "norsk",      Norwegian )
                , ( "indonesian", Indonesia )
                , ( "chinese",    Chinese ),    ( "中文",         Chinese )
                , ( "esperanto",  Esperanto )
                , ( "dutch",      Dutch ),      ( "nederlands", Dutch ) ]

logLevel :: Parser LogLevel
logLevel = option (eitherReader l)
  (long "log-level" <> metavar "debug|info|warn|error" <> value LevelInfo
   <> help "The minimum level of log messages to display (default: info)")
  where
    l :: String -> Either String LogLevel
    l "debug" = Right LevelDebug
    l "info"  = Right LevelInfo
    l "warn"  = Right LevelWarn
    l "error" = Right LevelError
    l _       = Left "Must be one of debug|info|warn|error"

absFilePath :: String -> Either String FilePath
absFilePath fp = bool (Left $ "Not absolute: " <> fp) (Right fp) $ isAbsolute fp
