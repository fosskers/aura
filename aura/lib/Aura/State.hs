{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

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
import           Aura.Core (Env(..), liftEitherM, notify, report, warn)
import           Aura.Languages
import           Aura.Pacman (pacman, pacmanOutput)
import           Aura.Settings
import           Aura.Types
import           Aura.Utils
import           BasePrelude hiding (Version)
import           Control.Compactable (fmapMaybe)
import           Control.Effect (Carrier, Member)
import           Control.Effect.Error (Error, throwError)
import           Control.Effect.Lift (Lift, sendM)
import           Control.Effect.Reader (Reader, asks)
import           Control.Error.Util (hush)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Generics.Product (field)
import           Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time
import           Data.Versions
import           Lens.Micro ((^.))
import           System.Path
import           System.Path.IO (createDirectoryIfMissing, getDirectoryContents)

---

-- | All packages installed at some specific `ZonedTime`. Any "pinned" PkgState will
-- never be deleted by `-Bc`.
data PkgState = PkgState { timeOf :: ZonedTime, pinnedOf :: Bool, pkgsOf :: M.Map PkgName Versioning }

instance ToJSON PkgState where
  toJSON (PkgState t pnd ps) = object [ "time" .= t, "pinned" .= pnd, "packages" .= fmap prettyV ps ]

instance FromJSON PkgState where
  parseJSON (Object v) = PkgState
    <$> v .: "time"
    <*> v .: "pinned"
    <*> fmap f (v .: "packages")
    where f = fmapMaybe (hush . versioning)
  parseJSON invalid = typeMismatch "PkgState" invalid

data StateDiff = StateDiff { _toAlter :: [SimplePkg], _toRemove :: [PkgName] }

-- | The default location of all saved states: \/var\/cache\/aura\/states
stateCache :: Path Absolute
stateCache = fromAbsoluteFilePath "/var/cache/aura/states"

-- | Does a given package have an entry in a particular `PkgState`?
inState :: SimplePkg -> PkgState -> Bool
inState (SimplePkg n v) s = maybe False (v ==) . M.lookup n $ pkgsOf s

rawCurrentState :: IO [SimplePkg]
rawCurrentState = mapMaybe (simplepkg' . strictText) . BL.lines <$> pacmanOutput ["-Q"]

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
getStateFiles :: IO [Path Absolute]
getStateFiles = do
  createDirectoryIfMissing True stateCache
  sort . map (stateCache </>) <$> getDirectoryContents stateCache

-- | Save a package state.
-- In writing the first state file, the `states` directory is created automatically.
saveState :: Settings -> IO ()
saveState ss = do
  state <- currentState
  let filename = stateCache </> fromUnrootedFilePath (dotFormat (timeOf state)) <.> FileExt "json"
  createDirectoryIfMissing True stateCache
  BL.writeFile (toFilePath filename) $ encode state
  notify ss . saveState_1 $ langOf ss

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
restoreState :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) => m ()
restoreState =
  sendM getStateFiles >>= maybe (throwError $ Failure restoreState_2) f . nonEmpty
  where f :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) => NonEmpty (Path Absolute) -> m ()
        f sfs = do
          ss  <- asks settings
          let pth = either id id . cachePathOf $ commonConfigOf ss
          mpast  <- sendM $ selectState sfs >>= readState
          case mpast of
            Nothing   -> throwError $ Failure readState_1
            Just past -> do
              curr <- sendM currentState
              Cache cache <- sendM $ cacheContents pth
              let StateDiff rein remo = compareStates past curr
                  (okay, nope)        = partition (`M.member` cache) rein
              traverse_ (report red restoreState_1 . fmap (^. field @"name")) $ nonEmpty nope
              reinstallAndRemove (mapMaybe (`M.lookup` cache) okay) remo

selectState :: NonEmpty (Path Absolute) -> IO (Path Absolute)
selectState = getSelection (T.pack . toFilePath)

-- | Given a `FilePath` to a package state file, attempt to read and parse
-- its contents. As of Aura 2.0, only state files in JSON format are accepted.
readState :: Path Absolute -> IO (Maybe PkgState)
readState = fmap decode . BL.readFile . toFilePath

-- | `reinstalling` can mean true reinstalling, or just altering.
reinstallAndRemove :: (Carrier sig m, Member (Reader Env) sig, Member (Error Failure) sig, Member (Lift IO) sig) =>
  [PackagePath] -> [PkgName] -> m ()
reinstallAndRemove [] [] = asks settings >>= \ss ->
  sendM (warn ss . reinstallAndRemove_1 $ langOf ss)
reinstallAndRemove down remo
  | null remo = reinstall
  | null down = remove
  | otherwise = reinstall *> remove
  where
    remove    = liftEitherM . sendM . pacman $ "-R" : asFlag remo
    reinstall = liftEitherM . sendM . pacman $ "-U" : map (T.pack . toFilePath . path) down
