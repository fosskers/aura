-- A library for saving and restoring the state of installed packages.

{-

Copyright 2012, 2013 Colin Woodbury <colingw@gmail.com>

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

module Aura.State where

import qualified Data.Map.Lazy as M

import System.FilePath ((</>))
import Control.Monad   (liftM, unless)
import Data.Maybe      (mapMaybe)
import Data.List       (partition)

import Aura.Colour.Text (cyan, red)
import Aura.General     (warn)
import Aura.Pacman      (pacmanOutput, pacman)
import Aura.Utils       (comparableVer,printList)
import Aura.Settings.Base
import Aura.Monad.Aura
import Aura.Languages
import Aura.Cache
import Aura.Time

import Utilities (getSelection)
import Shell     (ls')

---

data State = State { timeOf :: SimpleTime 
                   , pkgsOf :: M.Map String [Int] }
             deriving (Eq,Show,Read)

-- ([toDowngrade],[toRemove])
type StateDiff = ([SimplePkg],[String])

stateCache :: FilePath
stateCache = "/var/cache/aura/states"

rawCurrentState :: Aura [String]
rawCurrentState = lines `liftM` pacmanOutput ["-Q"]

currentState :: Aura State
currentState = do
  pkgs <- rawCurrentState
  time <- (toSimpleTime . toUTCTime) `liftM` liftIO getClockTime
  let namesVers    = map (pair . words) pkgs
      pair (x:y:_) = (x, comparableVer y)
  return . State time . M.fromAscList $ namesVers

compareStates :: State -> State -> StateDiff
compareStates old curr = M.foldrWithKey status ([],[]) $ pkgsOf curr
    where status k v (d,r) = case M.lookup k (pkgsOf old) of
                               Nothing -> (d, k : r)
                               Just v' -> if v == v'
                                          then (d,r)
                                          else ((k,v') : d, r)

getStateFiles :: Aura [FilePath]
getStateFiles = liftIO $ ls' stateCache

saveState :: Aura ()
saveState = do
  state <- currentState
  let filename = stateCache </> dotFormat (timeOf state)
  liftIO $ writeFile filename (show state)

restoreState :: Aura ()
restoreState = ask >>= \ss -> do
  curr  <- currentState
  past  <- getStateFiles >>= liftIO . getSelection >>= readState
  cache <- cacheContents $ cachePathOf ss
  let (cand,remo) = compareStates past curr
      (down,nope) = partition (flip downgradable cache) cand
      message     = restoreStateMsg1 $ langOf ss
  unless (null nope) $ printList red cyan message (map fst nope)
  downgradeAndRemove (mapMaybe (flip getFilename cache) down) remo

readState :: FilePath -> Aura State
readState name = liftIO (read `liftM` readFile (stateCache </> name))

-- How does pacman do simultaneous removals and upgrades?
-- I've seen it happen plenty of times.
downgradeAndRemove :: [FilePath] -> [String] -> Aura ()
downgradeAndRemove [] [] = warn downgradeAndRemoveMsg1
downgradeAndRemove down remo
    | null remo = downgrade
    | null down = remove
    | otherwise = downgrade >> remove
    where remove    = pacman $ "-R" : remo
          downgrade = ask >>= \ss ->
                      pacman $ "-U" : map (cachePathOf ss </>) down
