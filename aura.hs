{-# OPTIONS_GHC -O2 #-}

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

import Data.List (intersperse, (\\), nub, sort)
import Control.Monad (unless)
import System.Exit (ExitCode, exitWith)
import System.Environment (getArgs)

import Aura.State (restoreState, saveState)
import Aura.Languages
import Aura.Settings
import Aura.General
import Aura.Pacman
import Aura.Flags
import Aura.Logo

import Utilities (replaceByPatt)
import Shell
import Zero

import qualified Aura.A as A
import qualified Aura.C as C
import qualified Aura.L as L
import qualified Aura.O as O

---

auraVersion :: String
auraVersion = "1.0.8.0"

main :: IO a
main = do
  args <- getArgs
  let (language,rest) = parseLanguageFlag args
      (auraFlags,input,pacOpts) = parseFlags language rest
      auraFlags' = filter (`notElem` settingsFlags) auraFlags
      pacOpts'   = pacOpts ++ reconvertFlags auraFlags dualFlagMap
  settings <- getSettings language auraFlags
  unless (Debug `notElem` auraFlags) $ debugOutput settings
  exitStatus <- executeOpts settings (auraFlags', nub input, nub pacOpts')
  exitWith exitStatus

-- After determining what Flag was given, dispatches a function.
-- The `flags` must be sorted to guarantee the pattern matching
-- below will work properly.
executeOpts :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
executeOpts ss ([],[],[]) = executeOpts ss ([Help],[],[])
executeOpts ss (flags,input,pacOpts) = do
  case sort flags of
    (AURInstall:fs) ->
        case fs of
          []             -> ss |+| (ss |$| A.installPackages ss pacOpts input)
          [Upgrade]      -> ss |+| (ss |$| A.upgradeAURPkgs ss pacOpts input)
          [Info]         -> A.aurPkgInfo ss input
          [Search]       -> A.aurSearch input
          [ViewDeps]     -> A.displayPkgDeps ss input
          [Download]     -> A.downloadTarballs ss input
          [GetPkgbuild]  -> A.displayPkgbuild input
          (Refresh:fs')  -> ss |$| syncAndContinue ss (fs',input,pacOpts)
          (DelMDeps:fs') -> ss |$| removeMakeDeps ss (fs',input,pacOpts)
          badFlags       -> scoldAndFail ss executeOptsMsg1
    (Cache:fs) ->
        case fs of
          []       -> ss |$| C.downgradePackages ss input
          [Clean]  -> ss |$| C.cleanCache ss input
          [Search] -> C.searchCache ss input
          [Backup] -> ss |$| C.backupCache ss input
          badFlags -> scoldAndFail ss executeOptsMsg1
    (LogFile:fs) ->
        case fs of
          []       -> L.viewLogFile $ logFilePathOf ss
          [Search] -> L.searchLogFile ss input
          [Info]   -> L.logInfoOnPkg ss input
          badFlags -> scoldAndFail ss executeOptsMsg1
    (Orphans:fs) ->
        case fs of
          []        -> O.displayOrphans ss input
          [Abandon] -> ss |$| (getOrphans >>= \ps -> removePkgs ss ps pacOpts)
          badFlags  -> scoldAndFail ss executeOptsMsg1
    [SaveState]    -> ss |$| (saveState >> returnSuccess)
    [RestoreState] -> ss |$| restoreState ss
    [ViewConf]     -> viewConfFile
    [Languages]    -> displayOutputLanguages ss
    [Help]         -> printHelpMsg ss pacOpts
    [Version]      -> getVersionInfo >>= animateVersionMsg ss
    pacmanFlags    -> pacman ss $ pacOpts ++ input ++ hijackedFlags
    where hijackedFlags = reconvertFlags flags hijackedFlagMap

-- This two functions contain evil, and must be in `aura.hs` to work.
syncAndContinue :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
syncAndContinue settings (flags,input,pacOpts) = do
  _ <- syncDatabase (pacman settings) pacOpts
  executeOpts settings (AURInstall:flags,input,pacOpts)  -- This is Evil.

removeMakeDeps :: Settings -> ([Flag],[String],[String]) -> IO ExitCode
removeMakeDeps settings (flags,input,pacOpts) = do
  orphansBefore <- getOrphans
  executeOpts settings (AURInstall:flags,input,pacOpts) ?>> do
    orphansAfter <- getOrphans
    let makeDeps = orphansAfter \\ orphansBefore
    unless (null makeDeps) $ notify settings removeMakeDepsAfterMsg1
    removePkgs settings makeDeps pacOpts

----------
-- GENERAL
----------
viewConfFile :: IO ExitCode
viewConfFile = shellCmd "less" [pacmanConfFile]

displayOutputLanguages :: Settings -> IO ExitCode
displayOutputLanguages settings = do
  notify settings displayOutputLanguagesMsg1
  mapM_ (putStrLn . show) allLanguages
  returnSuccess

printHelpMsg :: Settings -> [String] -> IO ExitCode
printHelpMsg settings [] = do
  pacmanHelp <- getPacmanHelpMsg
  putStrLn $ getHelpMsg settings pacmanHelp
  returnSuccess
printHelpMsg settings pacOpts = pacman settings $ pacOpts ++ ["-h"]

getHelpMsg :: Settings -> [String] -> String
getHelpMsg settings pacmanHelpMsg = concat $ intersperse "\n" allMessages
    where lang = langOf settings
          allMessages   = [replacedLines, auraOperMsg lang, manpageMsg lang]
          replacedLines = unlines $ map (replaceByPatt patterns) pacmanHelpMsg
          colouredMsg   = yellow $ inheritedOperTitle lang
          patterns      = [("pacman","aura"), ("operations",colouredMsg)]

-- ANIMATED VERSION MESSAGE
animateVersionMsg :: Settings -> [String] -> IO ExitCode
animateVersionMsg settings verMsg = do
  hideCursor
  mapM_ putStrLn $ map (padString verMsgPad) verMsg  -- Version message
  putStr $ raiseCursorBy 7  -- Initial reraising of the cursor.
  drawPills 3
  mapM_ putStrLn $ renderPacmanHead 0 Open  -- Initial rendering of head.
  putStr $ raiseCursorBy 4
  takeABite 0
  mapM_ pillEating pillsAndWidths
  putStr clearGrid
  putStrLn auraLogo
  putStrLn $ "AURA Version " ++ auraVersion
  putStrLn " by Colin Woodbury\n"
  mapM_ putStrLn . translatorMsg . langOf $ settings
  showCursor
  returnSuccess
    where pillEating (p,w) = putStr clearGrid >> drawPills p >> takeABite w
          pillsAndWidths   = [(2,5),(1,10),(0,15)]
