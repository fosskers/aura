{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}

-- |
-- Module    : Aura.State
-- Copyright : (c) Colin Woodbury, 2012 - 2018
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
import           Aura.Core (warn, notify, rethrow, report)
import           Aura.Languages
import           Aura.Pacman (pacmanOutput, pacman)
import           Aura.Settings
import           Aura.Types
import           BasePrelude hiding (Version, FilePath, mapMaybe)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time
import           Data.Versions
import           Data.Witherable (mapMaybe)
import           Shelly hiding (time)
import           Utilities (list, getSelection)

---

-- | All packages installed at some specific `ZonedTime`. Any "pinned" PkgState will
-- never be deleted by `-Bc`.
data PkgState = PkgState { timeOf :: ZonedTime, pinnedOf :: Bool, pkgsOf :: M.Map T.Text Versioning }

instance ToJSON PkgState where
  toJSON (PkgState t pnd ps) = object [ "time" .= t, "pinned" .= pnd, "packages" .= fmap prettyV ps ]

instance FromJSON PkgState where
  parseJSON (Object v) = PkgState
    <$> v .: "time"
    <*> v .: "pinned"
    <*> fmap f (v .: "packages")
    where f = mapMaybe (either (const Nothing) Just . versioning)
  parseJSON invalid = typeMismatch "PkgState" invalid

data StateDiff = StateDiff { _toAlter :: [SimplePkg], _toRemove :: [T.Text] }

-- | The default location of all saved states: \/var\/cache\/aura\/states
stateCache :: FilePath
stateCache = "/var/cache/aura/states"

-- | Does a given package have an entry in a particulr `PkgState`?
inState :: SimplePkg -> PkgState -> Bool
inState (SimplePkg n v) s = maybe False (v ==) . M.lookup n $ pkgsOf s

rawCurrentState :: IO [SimplePkg]
rawCurrentState = mapMaybe simplepkg' . T.lines <$> pacmanOutput ["-Q"]

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
getStateFiles :: (Member (Error Failure) r, Member IO r) => Eff r [FilePath]
getStateFiles = send f >>= list (throwError $ Failure restoreState_2) pure
  where f = shelly @IO $ mkdir_p stateCache >> fmap sort (ls stateCache)

-- | Save a package state.
-- In writing the first state file, the `states` directory is created automatically.
saveState :: (Member (Reader Settings) r, Member IO r) => Eff r ()
saveState = do
  ss <- ask
  state <- send currentState
  let filename = T.unpack . toTextIgnore $ stateCache </> dotFormat (timeOf state) <.> "json"
  send . shelly @IO $ mkdir_p stateCache
  send . BL.writeFile filename $ encode state
  asks langOf >>= send . notify ss . saveState_1

dotFormat :: ZonedTime -> String
dotFormat (ZonedTime t _) = intercalate "." items
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
restoreState :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Eff r ()
restoreState = do
  sfs <- getStateFiles
  ss  <- ask
  let pth = either id id . cachePathOf $ commonConfigOf ss
  mpast  <- send $ selectState sfs >>= readState
  case mpast of
    Nothing   -> throwError $ Failure readState_1
    Just past -> do
      curr <- send currentState
      Cache cache <- send . shelly @IO $ cacheContents pth
      let StateDiff rein remo = compareStates past curr
          (okay, nope)        = partition (`M.member` cache) rein
      unless (null nope) . report red restoreState_1 $ map _spName nope
      reinstallAndRemove (mapMaybe (`M.lookup` cache) okay) remo

selectState :: [FilePath] -> IO FilePath
selectState = fmap fromText . getSelection . map toTextIgnore

-- | Given a `FilePath` to a package state file, attempt to read and parse
-- its contents. As of Aura 2.0, only state files in JSON format are accepted.
readState :: FilePath -> IO (Maybe PkgState)
readState = fmap decode . BL.readFile . T.unpack . toTextIgnore

-- | `reinstalling` can mean true reinstalling, or just altering.
reinstallAndRemove :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  [PackagePath] -> [T.Text] -> Eff r ()
reinstallAndRemove [] [] = ask >>= \ss -> send (warn ss . reinstallAndRemove_1 $ langOf ss)
reinstallAndRemove down remo
  | null remo = reinstall
  | null down = remove
  | otherwise = reinstall *> remove
  where remove    = rethrow . pacman $ "-R" : remo
        reinstall = rethrow . pacman $ "-U" : map (toTextIgnore . _pkgpath) down
