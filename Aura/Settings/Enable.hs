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

module Aura.Settings.Enable
    ( getSettings
    , debugOutput ) where

import System.Environment (getEnvironment)

import Aura.Colour.PacmanColorConf
import Aura.Settings.BadPackages
import Aura.Languages (Language)
import Aura.Settings.Base
import Aura.Pacman
import Aura.Flags

import Shell

---

getSettings :: Language -> [Flag] -> IO Settings
getSettings lang auraFlags = do
  confFile    <- getPacmanConf
  environment <- getEnvironment
  pmanCommand <- getPacmanCmd environment
  colourFuncs <- getColours
  return $ Settings { environmentOf   = environment
                    , langOf          = lang
                    , pacmanCmdOf     = pmanCommand
                    , editorOf        = getEditor environment
                    , ignoredPkgsOf   = getIgnoredPkgs confFile ++
                                        getIgnoredAuraPkgs auraFlags
                    , wontBuildOf     = getBadPackages lang
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
  mapM_ putStrLn [ "User              => " ++ getUser' env
                 , "True User         => " ++ getTrueUser env
                 , "Using Sudo?       => " ++ yn (varExists "SUDO_USER" env)
                 , "Language          => " ++ show (langOf ss)
                 , "Pacman Command    => " ++ pacmanCmdOf ss
                 , "Editor            => " ++ editorOf ss
                 , "Ignored Pkgs      => " ++ unwords (ignoredPkgsOf ss)
                 , "Pkg Cache Path    => " ++ cachePathOf ss
                 , "Log File Path     => " ++ logFilePathOf ss
                 , "Silent Building?  => " ++ yn (suppressMakepkg ss)
                 , "Must Confirm?     => " ++ yn (mustConfirm ss)
                 , "PKGBUILD editing? => " ++ yn (mayHotEdit ss) 
                 , "Diff PKGBUILDs?   => " ++ yn (diffPkgbuilds ss) ]
