{-# LANGUAGE OverloadedStrings #-}

-- A library for saving and restoring the state of installed packages.

{-

Copyright 2012 - 2018 Colin Woodbury <colin@fosskers.ca>

This file is part of Aura.

Aura is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Aura is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Aura.  If not, see <http://www.gnu.org/licenses/>.

-}

module Aura.State
    ( saveState
    , restoreState
    , inState
    , readState
    , stateCache
    , getStateFiles ) where

import           Aura.Cache
import           Aura.Colour.Text (cyan, red)
import           Aura.Core (warn, notify)
import           Aura.Errors
import           Aura.Languages
import           Aura.Monad.Aura
import           Aura.Pacman (pacmanOutput, pacman)
import           Aura.Settings.Base
import           Aura.Time
import           Aura.Utils (printList)
import           Aura.Utils.Numbers
import           BasePrelude hiding (Version, FilePath)
import           Data.Bitraversable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Shelly hiding (time)
import           Utilities (list, getSelection)

---

data PkgState = PkgState { timeOf :: Time
                         , pkgsOf :: M.Map T.Text (Maybe Version) }
                deriving (Eq, Show, Read)

-- ([toAlter], [toRemove])
type StateDiff = ([SimplePkg], [T.Text])

stateCache :: FilePath
stateCache = "/var/cache/aura/states"

inState :: SimplePkg -> PkgState -> Bool
inState (SimplePkg n v) s = case M.lookup n $ pkgsOf s of
                              Nothing -> False
                              Just v' -> v == v'

rawCurrentState :: MonadIO m => m [T.Text]
rawCurrentState = T.lines <$> pacmanOutput ["-Q"]

currentState :: MonadIO m => m PkgState
currentState = do
  pkgs <- rawCurrentState
  time <- liftIO localTime
  let namesVers    = map (pair . T.words) pkgs
      pair (x:y:_) = (x, version $ T.unpack y)
  pure . PkgState time . M.fromAscList $ namesVers

compareStates :: PkgState -> PkgState -> StateDiff
compareStates old curr = first (olds old curr <>) $ toChangeAndRemove old curr

-- | All packages that were changed and newly installed.
toChangeAndRemove :: PkgState -> PkgState -> StateDiff
toChangeAndRemove old curr = M.foldrWithKey status ([], []) $ pkgsOf curr
    where status k v (d, r) = case M.lookup k (pkgsOf old) of
                               Nothing -> (d, k : r)
                               Just v' | v == v' -> (d, r)
                                       | otherwise -> (SimplePkg k v' : d, r)

-- | Packages that were uninstalled since the last record.
olds :: PkgState -> PkgState -> [SimplePkg]
olds old curr = map (uncurry SimplePkg) . M.assocs $ M.difference (pkgsOf old) (pkgsOf curr)

getStateFiles :: MonadIO m => m (Either Failure [FilePath])
getStateFiles = list (failure restoreState_2) Right <$> shelly f
  where f = mkdir_p stateCache >> fmap sort (ls stateCache)

-- | In writing the first state file, the `states` directory is created
-- automatically.
saveState :: Aura ()
saveState = do
  state <- currentState
  let filename = stateCache </> dotFormat (timeOf state)
  shelly $ writefile filename (T.pack $ show state)  -- TODO Using `show` is dumb.
  asks langOf >>= notify . saveState_1

-- | Does its best to restore a state chosen by the user.
restoreState :: Aura (Either Failure ())
restoreState = getStateFiles >>= fmap join . bitraverse pure f
  where f sfs = do
          ss    <- ask
          past  <- liftIO $ selectState sfs >>= readState
          curr  <- currentState
          cache <- shelly . cacheContents $ cachePathOf ss
          let (rein, remo) = compareStates past curr
              (okay, nope) = partition (`M.member` cache) rein
              message      = restoreState_1 $ langOf ss
          unless (null nope) $ printList red cyan message (map (T.unpack . _spName) nope)
          reinstallAndRemove (mapMaybe (`M.lookup` cache) okay) remo

-- TODO Bad bad string conversion
selectState :: [FilePath] -> IO FilePath
selectState = fmap (fromText . T.pack) . getSelection . map (T.unpack . toTextIgnore)

-- TODO Using `read` here is terrible.
readState :: MonadIO m => FilePath -> m PkgState
readState name = read . T.unpack <$> shelly (readfile $ stateCache </> name)

-- How does pacman do simultaneous removals and upgrades?
-- I've seen it happen plenty of times.
-- | `reinstalling` can mean true reinstalling, or just altering.
reinstallAndRemove :: [T.Text] -> [T.Text] -> Aura (Either Failure ())
reinstallAndRemove [] [] = asks langOf >>= fmap Right . warn . reinstallAndRemove_1
reinstallAndRemove down remo
    | null remo = reinstall
    | null down = remove
    | otherwise = reinstall *> remove
    where remove    = pacman $ "-R" : remo
          reinstall = ask >>= \ss ->
                      pacman $ "-U" : map (toTextIgnore . (cachePathOf ss </>)) down
