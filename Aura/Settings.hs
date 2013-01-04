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

module Aura.Settings
    ( getSettings
    , debugOutput
    , Settings(..) ) where

import System.Environment (getEnvironment)

import Aura.Colour.TextColouring (Colouror)
import Aura.Colour.PacmanColorConf
import Aura.Languages
import Aura.Pacman
import Aura.Flags

import Shell

---

-- The global settings as set by the user with command-line flags.
data Settings = Settings { environmentOf   :: Environment
                         , langOf          :: Language
                         , pacman          :: Pacman
                         , editorOf        :: String
                         , ignoredPkgsOf   :: [String]
                         , cachePathOf     :: FilePath
                         , logFilePathOf   :: FilePath
                         , suppressMakepkg :: Bool
                         , mustConfirm     :: Bool
                         , mayHotEdit      :: Bool
                         , diffPkgbuilds   :: Bool
                         , pcRed           :: Colouror
                         , pcGreen         :: Colouror
                         , pcYellow        :: Colouror
                         , pcBlue          :: Colouror
                         , pcMagenta       :: Colouror
                         , pcCyan          :: Colouror
                         , pcWhite         :: Colouror }

getSettings :: Language -> [Flag] -> IO Settings
getSettings lang auraFlags = do
  confFile    <- getPacmanConf
  environment <- getEnvironment
  pmanCommand <- getPacmanCmd environment
  colourFuncs <- getColours
  return $ Settings { environmentOf   = environment
                    , langOf          = lang
                    , pacman          = pmanCommand
                    , editorOf        = getEditor environment
                    , ignoredPkgsOf   = getIgnoredPkgs confFile
                    , cachePathOf     = getCachePath confFile
                    , logFilePathOf   = getLogFilePath confFile
                    , suppressMakepkg = getSuppression auraFlags
                    , mustConfirm     = getConfirmation auraFlags
                    , mayHotEdit      = getHotEdit auraFlags
                    , diffPkgbuilds   = getDiffStatus auraFlags
                    , pcRed           = redf colourFuncs
                    , pcGreen         = greenf colourFuncs
                    , pcYellow        = yellowf colourFuncs
                    , pcBlue          = bluef colourFuncs
                    , pcMagenta       = magentaf colourFuncs
                    , pcCyan          = cyanf colourFuncs
                    , pcWhite         = whitef colourFuncs }

debugOutput :: Settings -> IO ()
debugOutput ss = do
  let yn a = if a then "Yes!" else "No."
      env  = environmentOf ss
  pmanCommand <- getPacmanCmd' env
  mapM_ putStrLn [ "User              => " ++ getUser' env
                 , "True User         => " ++ getTrueUser env
                 , "Using Sudo?       => " ++ yn (varExists "SUDO_USER" env)
                 , "Language          => " ++ show (langOf ss)
                 , "Pacman Command    => " ++ pmanCommand
                 , "Editor            => " ++ editorOf ss
                 , "Ignored Pkgs      => " ++ unwords (ignoredPkgsOf ss)
                 , "Pkg Cache Path    => " ++ cachePathOf ss
                 , "Log File Path     => " ++ logFilePathOf ss
                 , "Silent Building?  => " ++ yn (suppressMakepkg ss)
                 , "Must Confirm?     => " ++ yn (mustConfirm ss)
                 , "PKGBUILD editing? => " ++ yn (mayHotEdit ss) 
                 , "Diff PKGBUILDs?   => " ++ yn (diffPkgbuilds ss) ]

{-
sampleSettings :: Settings
sampleSettings = Settings { environmentOf = undefined
                          , langOf        = English
                          , pacman        = pacmanCmd "pacman"
                          , ignoredPkgsOf = []
                          , cachePathOf   = "/var/cache/pacman/pkg"
                          , logFilePathOf = "/var/log/pacman.log"
                          , suppressMakepkg = True
                          , mustConfirm   = False
                          , mayHotEdit    = False
                          , diffPkgbuilds = False }
-}
