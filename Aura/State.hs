-- A library for saving and restoring the state of installed packages.

{-

Copyright 2012, 2013, 2014 Colin Woodbury <colingw@gmail.com>

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

import qualified Data.Map.Lazy as M

import System.FilePath ((</>))
import Control.Monad   (unless)
import Control.Arrow   (first)
import Data.Maybe      (mapMaybe)
import Data.List       (partition,sort)

import Aura.Colour.Text (cyan, red)
import Aura.Pacman      (pacmanOutput, pacman)
import Aura.Utils       (comparableVer,printList)
import Aura.Core        (warn,notify)
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Cache
import Aura.Time

import Utilities (getSelection, readFileUTF8)
import Shell     (ls')

---

data PkgState = PkgState { timeOf :: Time
                         , pkgsOf :: M.Map String [Int] }
                deriving (Eq,Show,Read)

-- ([toAlter],[toRemove])
type StateDiff = ([SimplePkg],[String])

stateCache :: FilePath
stateCache = "/var/cache/aura/states"

inState :: SimplePkg -> PkgState -> Bool
inState (n,v) s = case M.lookup n $ pkgsOf s of
                    Nothing -> False
                    Just v' -> v == v'

rawCurrentState :: Aura [String]
rawCurrentState = lines <$> pacmanOutput ["-Q"]

currentState :: Aura PkgState
currentState = do
  pkgs <- rawCurrentState
  time <- liftIO localTime
  let namesVers = map (pair . words) pkgs
      pair      = \(x:y:_) -> (x, comparableVer y)
  return . PkgState time . M.fromAscList $ namesVers

compareStates :: PkgState -> PkgState -> StateDiff
compareStates old curr = first (olds old curr ++) $ toChangeAndRemove old curr

-- | All packages that were changed and newly installed.
toChangeAndRemove :: PkgState -> PkgState -> StateDiff
toChangeAndRemove old curr = M.foldrWithKey status ([],[]) $ pkgsOf curr
    where status k v (d,r) = case M.lookup k (pkgsOf old) of
                               Nothing -> (d, k : r)
                               Just v' -> if v == v'
                                          then (d,r)
                                          else ((k,v') : d, r)

-- | Packages that were uninstalled since the last record.
olds :: PkgState -> PkgState -> [SimplePkg]
olds old curr = M.assocs $ M.difference (pkgsOf old) (pkgsOf curr)

getStateFiles :: Aura [FilePath]
getStateFiles = sort <$> liftIO (ls' stateCache)

saveState :: Aura ()
saveState = do
  state <- currentState
  let filename = stateCache </> dotFormat (timeOf state)
  liftIO $ writeFile filename (show state)
  notify saveState_1

-- | Does its best to restore a state chosen by the user.
restoreState :: Aura ()
restoreState = ask >>= \ss -> do
  curr  <- currentState
  past  <- getStateFiles >>= liftIO . getSelection >>= readState
  cache <- cacheContents $ cachePathOf ss
  let (rein,remo) = compareStates past curr
      (okay,nope) = partition (alterable cache) rein
      message     = restoreState_1 $ langOf ss
  unless (null nope) $ printList red cyan message (map fst nope)
  reinstallAndRemove (mapMaybe (getFilename cache) okay) remo

readState :: FilePath -> Aura PkgState
readState name = liftIO (read <$> readFileUTF8 (stateCache </> name))

-- How does pacman do simultaneous removals and upgrades?
-- I've seen it happen plenty of times.
-- | `reinstalling` can mean true reinstalling, or just altering.
reinstallAndRemove :: [FilePath] -> [String] -> Aura ()
reinstallAndRemove [] [] = warn reinstallAndRemove_1
reinstallAndRemove down remo
    | null remo = reinstall
    | null down = remove
    | otherwise = reinstall >> remove
    where remove    = pacman $ "-R" : remo
          reinstall = ask >>= \ss ->
                      pacman $ "-U" : map (cachePathOf ss </>) down
