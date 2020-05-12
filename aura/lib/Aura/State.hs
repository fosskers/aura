-- |
-- Module    : Aura.State
-- Copyright : (c) Colin Woodbury, 2012 - 2020
-- License   : GPL3
-- Maintainer: Colin Woodbury <colin@fosskers.ca>
--
-- Handle the saving and restoring of installed package states.

module Aura.State
    ( PkgState(..)
    , saveState
    , restoreState
    , inState
    , readState
    , stateCache
    , getStateFiles
    ) where

import           Aura.Cache
import           Aura.Colour (red)
import           Aura.Core (Env(..), notify, report, warn)
import           Aura.IO
import           Aura.Languages
import           Aura.Pacman (pacman, pacmanLines)
import           Aura.Settings
import           Aura.Types
import           Aura.Utils (hush)
import           Data.Aeson
import           Data.Versions
import           RIO
import qualified RIO.ByteString.Lazy as BL
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.List as L
import           RIO.List.Partial ((!!))
import qualified RIO.Map as M
import qualified RIO.Map.Unchecked as M
import qualified RIO.NonEmpty as NEL
import qualified RIO.Text as T
import           RIO.Time
import           Text.Printf (printf)

---

-- | All packages installed at some specific `ZonedTime`. Any "pinned" PkgState will
-- never be deleted by `-Bc`.
data PkgState = PkgState
  { timeOf   :: !ZonedTime
  , pinnedOf :: !Bool
  , pkgsOf   :: !(Map PkgName Versioning) }

instance ToJSON PkgState where
  toJSON (PkgState t pnd ps) = object
    [ "time" .= t
    , "pinned" .= pnd
    , "packages" .= fmap prettyV ps ]

instance FromJSON PkgState where
  parseJSON = withObject "PkgState" $ \v -> PkgState
    <$> v .: "time"
    <*> v .: "pinned"
    <*> fmap f (v .: "packages")
    where f = M.mapMaybe (hush . versioning)

data StateDiff = StateDiff
  { _toAlter  :: ![SimplePkg]
  , _toRemove :: ![PkgName] }

-- | The default location of all saved states: \/var\/cache\/aura\/states
stateCache :: FilePath
stateCache = "/var/cache/aura/states"

-- | Does a given package have an entry in a particular `PkgState`?
inState :: SimplePkg -> PkgState -> Bool
inState (SimplePkg n v) s = (Just v ==) . M.lookup n $ pkgsOf s

rawCurrentState :: IO [SimplePkg]
rawCurrentState = mapMaybe simplepkg' <$> pacmanLines ["-Q"]

currentState :: IO PkgState
currentState = do
  pkgs <- rawCurrentState
  time <- getZonedTime
  pure . PkgState time False . M.fromAscList $ map (\(SimplePkg n v) -> (n, v)) pkgs

compareStates :: PkgState -> PkgState -> StateDiff
compareStates old curr = tcar { _toAlter = olds old curr <> _toAlter tcar }
  where tcar = toChangeAndRemove old curr

-- | All packages that were changed and newly installed.
toChangeAndRemove :: PkgState -> PkgState -> StateDiff
toChangeAndRemove old curr = uncurry StateDiff . M.foldrWithKey status ([], []) $ pkgsOf curr
    where status k v (d, r) = case M.lookup k (pkgsOf old) of
                               Nothing -> (d, k : r)
                               Just v' | v == v' -> (d, r)
                                       | otherwise -> (SimplePkg k v' : d, r)

-- | Packages that were uninstalled since the last record.
olds :: PkgState -> PkgState -> [SimplePkg]
olds old curr = map (uncurry SimplePkg) . M.assocs $ M.difference (pkgsOf old) (pkgsOf curr)

-- | The filepaths of every saved package state.
getStateFiles :: IO [FilePath]
getStateFiles = do
  createDirectoryIfMissing True stateCache
  L.sort . map (stateCache </>) <$> listDirectory stateCache

-- | Save a package state.
-- In writing the first state file, the `states` directory is created automatically.
saveState :: Settings -> IO ()
saveState ss = do
  state <- currentState
  let filename = stateCache </> dotFormat (timeOf state) <.> "json"
  createDirectoryIfMissing True stateCache
  BL.writeFile filename $ encode state
  notify ss saveState_1

dotFormat :: ZonedTime -> String
dotFormat (ZonedTime t _) = L.intercalate "." items
    where items = [ show ye
                  , printf "%02d(%s)" mo (mnths !! (mo - 1))
                  , printf "%02d" da
                  , printf "%02d" (todHour $ localTimeOfDay t)
                  , printf "%02d" (todMin $ localTimeOfDay t)
                  , printf "%02d" ((round . todSec $ localTimeOfDay t) :: Int) ]
          (ye, mo, da) = toGregorian $ localDay t
          mnths :: [String]
          mnths = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]

-- | Does its best to restore a state chosen by the user.
restoreState :: RIO Env ()
restoreState =
  liftIO getStateFiles >>= maybe (throwM $ Failure restoreState_2) f . NEL.nonEmpty
  where f :: NonEmpty FilePath -> RIO Env ()
        f sfs = do
          ss  <- asks settings
          let pth = either id id . cachePathOf $ commonConfigOf ss
          mpast  <- liftIO $ selectState sfs >>= readState
          case mpast of
            Nothing   -> throwM $ Failure readState_1
            Just past -> do
              curr <- liftIO currentState
              Cache cache <- liftIO $ cacheContents pth
              let StateDiff rein remo = compareStates past curr
                  (okay, nope)        = L.partition (`M.member` cache) rein
              traverse_ (report red restoreState_1 . fmap spName) $ NEL.nonEmpty nope
              reinstallAndRemove (mapMaybe (`M.lookup` cache) okay) remo

selectState :: NonEmpty FilePath -> IO FilePath
selectState = getSelection T.pack

-- | Given a `FilePath` to a package state file, attempt to read and parse
-- its contents. As of Aura 2.0, only state files in JSON format are accepted.
readState :: FilePath -> IO (Maybe PkgState)
readState = fmap decode . BL.readFile

-- | `reinstalling` can mean true reinstalling, or just altering.
reinstallAndRemove :: [PackagePath] -> [PkgName] -> RIO Env ()
reinstallAndRemove [] [] = asks settings >>= \ss -> warn ss reinstallAndRemove_1
reinstallAndRemove down remo
  | null remo = reinstall
  | null down = remove
  | otherwise = reinstall *> remove
  where
    remove    = liftIO . pacman $ "-R" : asFlag remo
    reinstall = liftIO . pacman $ "-U" : map (T.pack . ppPath) down
