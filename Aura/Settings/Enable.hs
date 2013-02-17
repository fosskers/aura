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

import Aura.Languages (Language( NotSpecified ), getLanguageFromEnvironment)
import Aura.MakePkg   (makepkgConfFile)
import Aura.Colour.PacmanColorConf
import Aura.Settings.BadPackages
import Aura.Settings.Base
import Aura.Pacman
import Aura.Flags

import Shell

---

getSettings :: Language -> ([Flag],[String],[String]) -> IO Settings
getSettings lang (auraFlags,input,pacOpts) = do
  confFile    <- getPacmanConf
  environment <- getEnvironment
  pmanCommand <- getPacmanCmd environment
  colourFuncs <- getColours
  makepkgConf <- readFile makepkgConfFile
  return Settings { inputOf         = input
                  , pacOptsOf       = pacOpts
                  , otherOpsOf      = map show auraFlags
                  , environmentOf   = environment
                  , langOf          = checkLang lang environment
                  , pacmanCmdOf     = pmanCommand
                  , editorOf        = getEditor environment
                  , carchOf         = singleEntry makepkgConf "CARCH"
                                      "COULDN'T READ $CARCH"
                  , ignoredPkgsOf   = getIgnoredPkgs confFile ++
                                      getIgnoredAuraPkgs auraFlags
                  , wontBuildOf     = getBadPackages lang
                  , cachePathOf     = getCachePath confFile
                  , logFilePathOf   = getLogFilePath confFile
                  , suppressMakepkg = getSuppression auraFlags
                  , delMakeDeps     = getDelMakeDeps auraFlags
                  , mustConfirm     = getConfirmation auraFlags
                  , mayHotEdit      = getHotEdit auraFlags
                  , diffPkgbuilds   = getDiffStatus auraFlags
                  , rebuildDevel    = getRebuildDevel auraFlags
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
                 , "Pacman Flags      => " ++ unwords (pacOptsOf ss)
                 , "Other Flags       => " ++ unwords (otherOpsOf ss)
                 , "Other input       => " ++ unwords (inputOf ss)
                 , "Language          => " ++ show (langOf ss)
                 , "Pacman Command    => " ++ pacmanCmdOf ss
                 , "Editor            => " ++ editorOf ss
                 , "$CARCH            => " ++ carchOf ss
                 , "Ignored Pkgs      => " ++ unwords (ignoredPkgsOf ss)
                 , "Pkg Cache Path    => " ++ cachePathOf ss
                 , "Log File Path     => " ++ logFilePathOf ss
                 , "Silent Building?  => " ++ yn (suppressMakepkg ss)
                 , "Must Confirm?     => " ++ yn (mustConfirm ss)
                 , "PKGBUILD editing? => " ++ yn (mayHotEdit ss) 
                 , "Diff PKGBUILDs?   => " ++ yn (diffPkgbuilds ss)
                 , "Rebuild Devel?    => " ++ yn (rebuildDevel ss)
                 , "Colour Test       => " ++ pcRed ss "RED"         ++
                                              pcGreen ss "GREEN"     ++
                                              pcYellow ss "YELLOW"   ++
                                              pcBlue ss "BLUE"       ++
                                              pcMagenta ss "MAGENTA" ++
                                              pcCyan ss "CYAN"       ++
                                              pcWhite ss "WHITE" ]

checkLang :: Language -> Environment -> Language
checkLang NotSpecified env = getLanguageFromEnvironment $ getLangVar env
checkLang lang         _   = lang

