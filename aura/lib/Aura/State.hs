{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, TypeApplications, MonoLocalBinds #-}

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
import           Aura.Core (warn, notify, rethrow)
import           Aura.Languages
import           Aura.Pacman (pacmanOutput, pacman)
import           Aura.Settings
import           Aura.Time
import           Aura.Types
import           Aura.Utils (printList)
import           BasePrelude hiding (Version, FilePath)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Versions
import           Shelly hiding (time)
import           Utilities (list, getSelection)

---

data PkgState = PkgState { timeOf :: Time
                         , pkgsOf :: M.Map T.Text Versioning }
                deriving (Eq, Show)

data StateDiff = StateDiff { _toAlter :: [SimplePkg], _toRemove :: [T.Text] }

stateCache :: FilePath
stateCache = "/var/cache/aura/states"

inState :: SimplePkg -> PkgState -> Bool
inState (SimplePkg n v) s = case M.lookup n $ pkgsOf s of
                              Nothing -> False
                              Just v' -> v == v'

rawCurrentState :: IO [SimplePkg]
rawCurrentState = mapMaybe simplepkg' . T.lines <$> pacmanOutput ["-Q"]

currentState :: IO PkgState
currentState = do
  pkgs <- rawCurrentState
  time <- localTime
  pure . PkgState time . M.fromAscList $ map (\(SimplePkg n v) -> (n, v)) pkgs

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

getStateFiles :: (Member (Error Failure) r, Member IO r) => Eff r [FilePath]
getStateFiles = send f >>= list (throwError $ Failure restoreState_2) pure
  where f = shelly @IO $ mkdir_p stateCache >> fmap sort (ls stateCache)

-- | In writing the first state file, the `states` directory is created
-- automatically.
saveState :: (Member (Reader Settings) r, Member IO r) => Eff r ()
saveState = do
  state <- send currentState
  let filename = stateCache </> dotFormat (timeOf state)
  send . shelly @IO $ writefile filename (T.pack $ show state)  -- TODO Using `show` is dumb.
  asks langOf >>= send . notify . saveState_1

-- | Does its best to restore a state chosen by the user.
-- restoreState :: Aura (Either Failure ())
restoreState :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) => Eff r ()
restoreState = do
  sfs <- getStateFiles
  ss  <- ask
  let pth = fromMaybe defaultPackageCache . cachePathOf $ commonConfigOf ss
  past  <- send $ selectState sfs >>= readState
  curr  <- send currentState
  Cache cache <- send . shelly @IO $ cacheContents pth
  let StateDiff rein remo = compareStates past curr
      (okay, nope) = partition (`M.member` cache) rein
      message      = restoreState_1 $ langOf ss
  send . unless (null nope) $ printList @IO red cyan message (map _spName nope)
  reinstallAndRemove (mapMaybe (`M.lookup` cache) okay) remo

selectState :: [FilePath] -> IO FilePath
selectState = fmap fromText . getSelection . map toTextIgnore

-- TODO Using `read` here is terrible. Make it JSON.
readState :: FilePath -> IO PkgState
readState = undefined -- read . T.unpack <$> shelly (readfile $ stateCache </> name)

-- | `reinstalling` can mean true reinstalling, or just altering.
reinstallAndRemove :: (Member (Reader Settings) r, Member (Error Failure) r, Member IO r) =>
  [PackagePath] -> [T.Text] -> Eff r ()
reinstallAndRemove [] [] = asks langOf >>= send . warn . reinstallAndRemove_1
reinstallAndRemove down remo
  | null remo = reinstall
  | null down = remove
  | otherwise = reinstall *> remove
  where remove    = rethrow . pacman $ "-R" : remo
        reinstall = do
          pth <- asks (fromMaybe defaultPackageCache . cachePathOf . commonConfigOf)
          rethrow . pacman $ "-U" : map (toTextIgnore . (\pp -> pth </> _pkgpath pp)) down
