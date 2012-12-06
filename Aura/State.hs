-- A library for saving and restoring the state of installed packages.

{-

Copyright 2012 Colin Woodbury <colingw@gmail.com>

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
import Control.Monad (liftM)

import Aura.Pacman (pacmanOutput)
import Aura.Time
import Shell (ls')

--------
-- TYPES
--------
data State = State { timeOf :: SimpleTime 
                   , pkgsOf :: (M.Map String String) }
             deriving (Eq,Show,Read)

-- ([toDowngrade],[toRemove])
type StateDiff = ([String],[String])

stateCache :: FilePath
stateCache = "/var/cache/aura/states"

rawCurrentState :: IO [String]
rawCurrentState = lines `liftM` pacmanOutput ["-Q"]

currentState :: IO State
currentState = do
  pkgs <- rawCurrentState
  time <- (toSimpleTime . toUTCTime) `liftM` getClockTime
  let namesVers = map (\p -> (\(x:y:_) -> (x,y)) $ words p) $ pkgs  -- Fugly
  return . State time . M.fromAscList $ namesVers

compareStates :: State -> State -> StateDiff
compareStates old curr = M.foldrWithKey status ([],[]) $ pkgsOf curr
    where status k v (d,r) = case M.lookup k (pkgsOf old) of
                               Nothing -> (d, k : r)
                               Just v' -> if v == v'
                                          then (d,r)
                                          else (k : d, r)

pkgsToDowngrade :: StateDiff -> [String]
pkgsToDowngrade = fst

pkgsToRemove :: StateDiff -> [String]
pkgsToRemove = snd

getStateFiles :: IO [FilePath]
getStateFiles = ls' stateCache

saveState :: IO ()
saveState = do
  state <- currentState
  let filename = stateCache </> dotFormat (timeOf state)
  writeFile filename $ show state

restoreState :: IO ()
restoreState = undefined

readState :: FilePath -> IO State
readState name = read `liftM` readFile (stateCache </> name)
