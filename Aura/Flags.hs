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

module Aura.Flags
    ( parseLanguageFlag
    , parseFlags 
    , settingsFlags
    , reconvertFlags
    , dualFlagMap
    , hijackedFlagMap
    , getSuppression
    , getDelMakeDeps
    , getConfirmation
    , getHotEdit
    , getDiffStatus
    , getRebuildDevel
    , filterSettingsFlags
    , getIgnoredAuraPkgs
    , auraOperMsg
    , Flag(..) ) where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

import Aura.Colour.Text (yellow)
import Aura.Languages

import Utilities (notNull, split)

---

type FlagMap = [(Flag,String)]

data Flag = AURInstall
          | SaveState
          | Cache
          | LogFile
          | Orphans
          | Search
          | Info
          | Refresh
          | GetPkgbuild
          | ViewDeps
          | DelMDeps
          | Upgrade
          | Download
          | Unsuppress
          | HotEdit
          | NoConfirm
          | Ignore String
          | DiffPkgbuilds
          | Devel
          | Debug
          | CacheBackup
          | Clean
          | Abandon
          | ViewConf
          | RestoreState
          | Languages
          | Version
          | Help
          | JapOut
          | PolishOut
          | CroatianOut
          | SwedishOut
          | GermanOut
          | SpanishOut
          | PortuOut
          | FrenchOut
          | RussianOut
          | ItalianOut
            deriving (Eq,Ord)

allFlags :: Language -> [OptDescr Flag]
allFlags lang = concat [ auraOperations lang
                       , auraOptions
                       , pacmanOptions
                       , dualOptions ]

simpleMakeOption :: ([Char],[String],Flag) -> OptDescr Flag
simpleMakeOption (c,s,f) = Option c s (NoArg f) ""

auraOperations :: Language -> [OptDescr Flag]
auraOperations lang =
    [ Option ['A'] ["aursync"]   (NoArg AURInstall) (aurSy lang)
    , Option ['B'] ["save"]      (NoArg SaveState)  (saveS lang)
    , Option ['C'] ["downgrade"] (NoArg Cache)      (downG lang)
    , Option ['L'] ["viewlog"]   (NoArg LogFile)    (viewL lang)
    , Option ['O'] ["orphans"]   (NoArg Orphans)    (orpha lang) ]

auraOptions :: [OptDescr Flag]
auraOptions = Option [] ["aurignore"] (ReqArg Ignore "") "" :
              map simpleMakeOption
              [ ( ['a'], ["delmakedeps"],  DelMDeps      )
              , ( ['b'], ["backup"],       CacheBackup   )
              , ( ['c'], ["clean"],        Clean         )
              , ( ['d'], ["deps"],         ViewDeps      )
              , ( ['j'], ["abandon"],      Abandon       )
              , ( ['k'], ["diff"],         DiffPkgbuilds )
              , ( ['i'], ["info"],         Info          )
              , ( ['p'], ["pkgbuild"],     GetPkgbuild   )
              , ( ['r'], ["restore"],      RestoreState  )
              , ( ['s'], ["search"],       Search        )
              , ( ['u'], ["sysupgrade"],   Upgrade       )
              , ( ['w'], ["downloadonly"], Download      )
              , ( ['x'], ["unsuppress"],   Unsuppress    )
              , ( [],    ["devel"],        Devel         )
              , ( [],    ["hotedit"],      HotEdit       )
              , ( [],    ["viewconf"],     ViewConf      ) 
              , ( [],    ["languages"],    Languages     ) 
              , ( [],    ["auradebug"],    Debug         ) ]

-- These are intercepted Pacman flags. Their functionality is different.
pacmanOptions :: [OptDescr Flag]
pacmanOptions = map simpleMakeOption
                [ ( ['y'], ["refresh"], Refresh )
                , ( ['V'], ["version"], Version )
                , ( ['h'], ["help"],    Help    ) ]

-- Options that have functionality stretching across both Aura and Pacman.
dualOptions :: [OptDescr Flag]
dualOptions = map simpleMakeOption
              [ ( [], ["noconfirm"], NoConfirm ) ]

languageOptions :: [OptDescr Flag]
languageOptions = map simpleMakeOption
                  [ ( [], ["japanese","日本語"],      JapOut      )
                  , ( [], ["polish","polski"],        PolishOut   )
                  , ( [], ["croatian","hrvatski"],    CroatianOut )
                  , ( [], ["swedish","svenska"],      SwedishOut  )
                  , ( [], ["german","deutsch"],       GermanOut   )
                  , ( [], ["spanish","español"],      SpanishOut  )
                  , ( [], ["portuguese","português"], PortuOut    )
                  , ( [], ["french","français"],      FrenchOut   )
                  , ( [], ["russian","русский"],      RussianOut  )
                  , ( [], ["italian","italiano"],     ItalianOut  ) ]

-- `Hijacked` flags. They have original pacman functionality, but
-- that is masked and made unique in an Aura context.
hijackedFlagMap :: FlagMap
hijackedFlagMap = [ (CacheBackup,"-b")
                  , (Clean,"-c")
                  , (ViewDeps,"-d")
                  , (Info,"-i")
                  , (DiffPkgbuilds,"-k")
                  , (RestoreState,"-r")
                  , (Search,"-s")
                  , (Upgrade,"-u")
                  , (Download,"-w")
                  , (Refresh,"-y") ]

dualFlagMap :: FlagMap
dualFlagMap = [ (NoConfirm,"--noconfirm") ]
 
-- Does the whole lot and filters out the garbage.
reconvertFlags :: [Flag] -> FlagMap -> [String]
reconvertFlags flags fm = filter notNull $ map (reconvertFlag fm) flags

-- Converts an intercepted Pacman flag back into its raw string form.
reconvertFlag :: FlagMap -> Flag -> String
reconvertFlag flagMap f = fromMaybe "" $ f `lookup` flagMap

settingsFlags :: [Flag]
settingsFlags = [ Unsuppress,NoConfirm,HotEdit,DiffPkgbuilds,Debug,Devel
                , DelMDeps ]

filterSettingsFlags :: [Flag] -> [Flag]
filterSettingsFlags []              = []
filterSettingsFlags (Ignore _ : fs) = filterSettingsFlags fs
filterSettingsFlags (f:fs) | f `elem` settingsFlags = filterSettingsFlags fs
                           | otherwise = f : filterSettingsFlags fs

auraOperMsg :: Language -> String
auraOperMsg lang = usageInfo (yellow $ auraOperTitle lang) $ auraOperations lang

-- Extracts desirable results from given Flags.
-- Callers must supply an alternate value for when there are no matches.
fishOutFlag :: [(Flag,a)] -> a -> [Flag] -> a
fishOutFlag [] alt _             = alt
fishOutFlag ((f,r):fs) alt flags | f `elem` flags = r
                                 | otherwise      = fishOutFlag fs alt flags

getLanguage :: [Flag] -> Language
getLanguage = fishOutFlag flagsAndResults english
    where flagsAndResults = zip langFlags langFuns
          langFlags       = [ JapOut,PolishOut,CroatianOut,SwedishOut
                            , GermanOut,SpanishOut,PortuOut,FrenchOut
                            , RussianOut,ItalianOut ]
          langFuns        = [ japanese,polish,croatian,swedish,german
                            , spanish,portuguese,french,russian,italian ]

getIgnoredAuraPkgs :: [Flag] -> [String]
getIgnoredAuraPkgs [] = []
getIgnoredAuraPkgs (Ignore ps : _) = split ',' ps
getIgnoredAuraPkgs (_:fs) = getIgnoredAuraPkgs fs

getSuppression :: [Flag] -> Bool
getSuppression = fishOutFlag [(Unsuppress,False)] True

getDelMakeDeps :: [Flag] -> Bool
getDelMakeDeps = fishOutFlag [(DelMDeps,True)] False

getConfirmation :: [Flag] -> Bool
getConfirmation = fishOutFlag [(NoConfirm,False)] True

getHotEdit :: [Flag] -> Bool
getHotEdit = fishOutFlag [(HotEdit,True)] False

getDiffStatus :: [Flag] -> Bool
getDiffStatus = fishOutFlag [(DiffPkgbuilds,True)] False

getRebuildDevel :: [Flag] -> Bool
getRebuildDevel = fishOutFlag [(Devel,True)] False

parseLanguageFlag :: [String] -> (Language,[String])
parseLanguageFlag args =
    case getOpt' Permute languageOptions args of
      (langs,nonOpts,otherOpts,_) -> (getLanguage langs, nonOpts ++ otherOpts)

-- Errors are dealt with manually in `aura.hs`.
parseFlags :: Language -> [String] -> ([Flag],[String],[String])
parseFlags lang args = case getOpt' Permute (allFlags lang) args of
                         (opts,nonOpts,pacOpts,_) -> (opts,nonOpts,pacOpts) 
