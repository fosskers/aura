module Aura.Flags where

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

-- System Libraries
import System.Console.GetOpt

-- Custom Libraries
import Utilities (notNull)
import Shell (yellow)
import Aura.Languages

type FlagMap = [(Flag,String)]

data Flag = AURInstall
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
          | Backup
          | Clean
          | Abandon
          | ViewConf
          | Languages
          | Version
          | Help
          | JapOut
          | PolishOut
          | CroatianOut
          | SwedishOut
          | GermanOut
          | SpanishOut
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
    , Option ['C'] ["downgrade"] (NoArg Cache)      (downG lang)
    , Option ['L'] ["viewlog"]   (NoArg LogFile)    (viewL lang)
    , Option ['O'] ["orphans"]   (NoArg Orphans)    (orpha lang) ]

auraOptions :: [OptDescr Flag]
auraOptions = map simpleMakeOption
              [ ( ['a'], ["delmakedeps"],  DelMDeps    )
              , ( ['b'], ["backup"],       Backup      )
              , ( ['c'], ["clean"],        Clean       )
              , ( ['d'], ["deps"],         ViewDeps    )
              , ( ['j'], ["abandon"],      Abandon     )
              , ( ['i'], ["info"],         Info        )
              , ( ['p'], ["pkgbuild"],     GetPkgbuild )
              , ( ['s'], ["search"],       Search      )
              , ( ['u'], ["sysupgrade"],   Upgrade     )
              , ( ['w'], ["downloadonly"], Download    )
              , ( ['x'], ["unsuppress"],   Unsuppress  )
              , ( [],    ["hotedit"],      HotEdit     )
              , ( [],    ["conf"],         ViewConf    ) 
              , ( [],    ["languages"],    Languages   ) ]

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
                  [ ( [], ["japanese","日本語"],   JapOut      )
                  , ( [], ["polish","polski"],     PolishOut   )
                  , ( [], ["croatian","hrvatski"], CroatianOut )
                  , ( [], ["swedish","svenska"],   SwedishOut  )
                  , ( [], ["german", "deutsch"],   GermanOut   )
                  , ( [], ["spanish", "español"],  SpanishOut  ) ]

-- `Hijacked` flags. They have original pacman functionality, but
-- that is masked and made unique in an Aura context.
hijackedFlagMap :: FlagMap
hijackedFlagMap = [ (Backup,"-b")
                  , (Clean,"-c")
                  , (ViewDeps,"-d")
                  , (Info,"-i")
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
reconvertFlag flagMap f = case f `lookup` flagMap of
                            Just x  -> x
                            Nothing -> ""

settingsFlags :: [Flag]
settingsFlags = [Unsuppress,NoConfirm,HotEdit,JapOut]

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
          langFlags       = [JapOut,PolishOut,CroatianOut,SwedishOut,GermanOut,SpanishOut]
          langFuns        = [japanese,polish,croatian,swedish,german,spanish]

getSuppression :: [Flag] -> Bool
getSuppression = fishOutFlag [(Unsuppress,False)] True

getConfirmation :: [Flag] -> Bool
getConfirmation = fishOutFlag [(NoConfirm,False)] True

getHotEdit :: [Flag] -> Bool
getHotEdit = fishOutFlag [(HotEdit,True)] False

parseLanguageFlag :: [String] -> (Language,[String])
parseLanguageFlag args =
    case getOpt' Permute languageOptions args of
      (langs,nonOpts,otherOpts,_) -> (getLanguage langs, nonOpts ++ otherOpts)

-- Errors are dealt with manually in `aura.hs`.
parseFlags :: Language -> [String] -> ([Flag],[String],[String])
parseFlags lang args = case getOpt' Permute (allFlags lang) args of
                         (opts,nonOpts,pacOpts,_) -> (opts,nonOpts,pacOpts) 
