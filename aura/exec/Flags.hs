module Flags where

import           BasePrelude hiding (Version, FilePath, option)
import qualified Data.Text as T
import           Options.Applicative
import           Shelly

---

-- | A description of a run of Aura to attempt.
data Program = Program {
  -- ^ Whether Aura handles everything, or the ops and input are just passed down to Pacman.
  _operation :: Either PacmanOp AuraOp
  -- ^ Flags common to both Aura and Pacman.
  , _commons :: [Common]
  -- ^ Exposed flags to be passed to makepkg.
  , _makepkg :: [Makepkg]
  -- ^ Other input, usually package names.
  , _input   :: [T.Text] }

data Common = NoConfirm | Needed

data Makepkg = IgnoreArch | AllSource

-- | Inherited operations that are fed down to Pacman.
data PacmanOp = Database | Files | Query | Remove | Sync | TestDeps | Upgrade

-- | Operations unique to Aura.
data AuraOp = AurSync | Backup | Cache CacheOp | Log LogOp | Orphans | Version

data CacheOp = CacheDowngrade [T.Text] | CacheBackup FilePath | CacheClean Word | CacheSearch T.Text

data LogOp = LogView | LogInfo [T.Text] | LogSearch T.Text

-- For accepting an arbitrary number of args
-- some (argument str (metavar "FILES..."))

version :: Parser AuraOp
version = flag' Version (long "version" <> short 'V' <> help "Display Aura's version.")

-- aursync :: Parser

cache :: Parser AuraOp
cache = Cache <$> ((bigC' *> (backup <|> clean <|> search)) <|> bigC)
  where bigC = CacheDowngrade <$>
          option auto (long "downgrade" <> short 'C' <> metavar "PACKAGES" <> help "Downgrade PACKAGES.")
        bigC' = flag' () (long "downgrade" <> short 'C')
        backup = CacheBackup <$>
          strOption (long "backup"
                      <> metavar "PATH"
                      <> help "Backup the package cache to a given directory.")
        clean  = CacheClean <$>
          option auto (long "clean"
                        <> short 'c'
                        <> metavar "N"
                        <> help "Save the most recent N versions of a package in the cache, deleting the rest.")
        search = CacheSearch <$>
          strOption (long "search"
                      <> short 's'
                      <> metavar "STRING"
                      <> help "Search the package cache via a search string.")

log :: Parser AuraOp
log = Log <$> ((bigL *> (inf <|> search)) <|> bigL)
  where bigL = flag' LogView (long "viewlog" <> short 'L' <> help "View the Pacman log.")
        inf = LogInfo <$>
          option auto (long "info"
                        <> short 'i'
                        <> metavar "PACKAGES"
                        <> help "Display the installation history for given packages.")
        search = LogSearch <$>
          strOption (long "search"
                      <> short 's'
                      <> metavar "STRING"
                      <> help "Search the Pacman log via a search string.")
